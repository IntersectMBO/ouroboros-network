{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE RankNTypes #-}

module Cardano.KESAgent.Evolution
where

import Cardano.Crypto.KES.Class
import Cardano.Crypto.DSIGN.Class as DSIGN
import Cardano.KESAgent.OCert

import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Control.Monad.Class.MonadTime

import Data.Time (NominalDiffTime, nominalDiffTimeToSeconds)

getCurrentKESPeriod :: MonadTime m => Integer -> m KESPeriod
getCurrentKESPeriod genesisTimestamp = do
  now <- utcTimeToPOSIXSeconds <$> getCurrentTime
  getCurrentKESPeriodWith (pure now) genesisTimestamp

getCurrentKESPeriodWith :: Monad m => m NominalDiffTime -> Integer -> m KESPeriod
getCurrentKESPeriodWith now genesisTimestamp =
  KESPeriod
    . floor
    .  (/ (36 * 3600))
    . (subtract (realToFrac genesisTimestamp))
    . nominalDiffTimeToSeconds
    <$> now

updateKESToCurrent :: KESSignAlgorithm m (KES v)
                   => MonadTime m
                   => Integer
                   -> ContextKES (KES v)
                   -> OCert v
                   -> SignKeyWithPeriodKES (KES v)
                   -> m (Maybe (SignKeyWithPeriodKES (KES v)))
updateKESToCurrent genesisTimestamp context cert skp = do
  currentPeriod <- getCurrentKESPeriod genesisTimestamp
  updateKESTo context currentPeriod cert skp

updateKESTo :: KESSignAlgorithm m (KES v)
            => ContextKES (KES v)
            -> KESPeriod
            -> OCert v
            -> SignKeyWithPeriodKES (KES v)
            -> m (Maybe (SignKeyWithPeriodKES (KES v)))
updateKESTo context currentPeriod cert skp = do
  let targetEvolution =
        unKESPeriod currentPeriod -
        unKESPeriod (ocertKESPeriod cert)
  if periodKES skp >= targetEvolution then
    return (Just skp) -- key is already up to date or newer
  else do
    skp'May <- updateKESWithPeriod context skp
    case skp'May of
      Nothing ->
        -- update failed (t >= totalPeriodsKES)
        return Nothing
      Just skp' ->
        -- update succeeded
        if periodKES skp' == targetEvolution then
          -- reached target evolution, so we're done
          return (Just skp')
        else
          -- Still not enough, keep going.
          -- This also captures the (theoretical) case where we have updated
          -- past the last evolution, but the recursive call will catch this.
          updateKESTo context currentPeriod cert skp'
