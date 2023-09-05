{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.KESAgent.KES.Evolution
  where

import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.KES.Crypto

import Cardano.Crypto.DSIGN.Class as DSIGN
import Cardano.Crypto.KES.Class

import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime
import Data.Time ( NominalDiffTime, nominalDiffTimeToSeconds )
import Data.Time.Clock.POSIX ( utcTimeToPOSIXSeconds, posixSecondsToUTCTime )

import Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.TH as JSON

data EvolutionConfig =
  EvolutionConfig
    { slotLength :: Int
    , slotsPerKESPeriod :: Int
    , systemStart :: UTCTime
    }
    deriving (Show, Eq, Ord)

deriveJSON JSON.defaultOptions ''EvolutionConfig

defEvolutionConfig :: EvolutionConfig
defEvolutionConfig =
  EvolutionConfig
    { systemStart = posixSecondsToUTCTime 1506203091 -- real-world genesis on the production ledger
    , slotsPerKESPeriod = 129600
    , slotLength = 1
    }

evolutionConfigFromGenesisFile :: FilePath -> IO (Either String EvolutionConfig)
evolutionConfigFromGenesisFile = JSON.eitherDecodeFileStrict'

getCurrentKESPeriod :: MonadTime m => EvolutionConfig -> m KESPeriod
getCurrentKESPeriod = do
  getCurrentKESPeriodWith getCurrentTime

getCurrentKESPeriodWith :: Monad m => m UTCTime -> EvolutionConfig -> m KESPeriod
getCurrentKESPeriodWith getNow ec = do
  now <- getNow
  let diffSecs = floor (nominalDiffTimeToSeconds $ diffUTCTime now (systemStart ec))
      kesPeriodDuration = fromIntegral (slotLength ec) * fromIntegral (slotsPerKESPeriod ec)
  return $ KESPeriod (diffSecs `div` kesPeriodDuration)

updateKESToCurrent :: KESAlgorithm (KES v)
                   => MonadTime m
                   => MonadST m
                   => MonadThrow m
                   => EvolutionConfig
                   -> ContextKES (KES v)
                   -> OCert v
                   -> SignKeyWithPeriodKES (KES v)
                   -> m (Maybe (SignKeyWithPeriodKES (KES v)))
updateKESToCurrent ec context cert skp = do
  currentPeriod <- getCurrentKESPeriod ec
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
