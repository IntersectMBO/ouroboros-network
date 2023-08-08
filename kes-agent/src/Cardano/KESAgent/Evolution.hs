{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.KESAgent.Evolution
  where

import Cardano.KESAgent.OCert

import Cardano.Crypto.DSIGN.Class as DSIGN
import Cardano.Crypto.KES.Class

import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime
import Data.Time ( NominalDiffTime, nominalDiffTimeToSeconds )
import Data.Time.Clock.POSIX ( utcTimeToPOSIXSeconds )

getCurrentKESPeriod :: MonadTime m => Integer -> m KESPeriod
getCurrentKESPeriod genesisTimestamp = do
  now <- utcTimeToPOSIXSeconds <$> getCurrentTime
  getCurrentKESPeriodWith (pure now) genesisTimestamp

getCurrentKESPeriodWith :: Monad m => m NominalDiffTime -> Integer -> m KESPeriod
getCurrentKESPeriodWith now genesisTimestamp =
  KESPeriod
    . floor
    .  (/ (36 * 3600))
    . subtract (realToFrac genesisTimestamp)
    . nominalDiffTimeToSeconds
    <$> now

updateKESToCurrent :: KESAlgorithm (KES v)
                   => MonadTime m
                   => MonadST m
                   => MonadThrow m
                   => Integer
                   -> ContextKES (KES v)
                   -> OCert v
                   -> SignKeyWithPeriodKES (KES v)
                   -> m (Maybe (SignKeyWithPeriodKES (KES v)))
updateKESToCurrent genesisTimestamp context cert skp = do
  currentPeriod <- getCurrentKESPeriod genesisTimestamp
  updateKESTo context currentPeriod cert skp

updateKESTo :: KESAlgorithm (KES v)
            => MonadST m
            => MonadThrow m
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
