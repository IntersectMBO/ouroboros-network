{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | Functionality for handling key evolution within a KES agent or KES agent
-- client.
module Cardano.KESAgent.KES.Evolution
where

import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.OCert

import Cardano.Crypto.KES.Class

import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime
import Data.Time (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import qualified Data.Aeson as JSON
import Data.Aeson.TH (deriveJSON)

-- | Evolution parameters used to determine which evolution of a given key is
-- current, and when a key should evolve. Fields are named after the matching
-- fields of the genesis JSON file.
data EvolutionConfig
  = EvolutionConfig
  { slotLength :: Int
  , slotsPerKESPeriod :: Int
  , systemStart :: UTCTime
  }
  deriving (Show, Eq, Ord)

deriveJSON JSON.defaultOptions ''EvolutionConfig

-- | Default evolution parameters, based on the parameters used on the
-- real-world production ledger at the time of writing.
defEvolutionConfig :: EvolutionConfig
defEvolutionConfig =
  EvolutionConfig
    { systemStart = posixSecondsToUTCTime 1506203091
    , slotsPerKESPeriod = 129600
    , slotLength = 1
    }

-- | Load evolution parameters from a genesis JSON file.
evolutionConfigFromGenesisFile :: FilePath -> IO (Either String EvolutionConfig)
evolutionConfigFromGenesisFile = JSON.eitherDecodeFileStrict'

-- | Determine the current KES period from the local host's RTC.
getCurrentKESPeriod :: MonadTime m => EvolutionConfig -> m KESPeriod
getCurrentKESPeriod = do
  getCurrentKESPeriodWith getCurrentTime

-- | Determine the current KES period from the local host's RTC, based on the
-- given evolution parameters.
getCurrentKESPeriodWith :: Monad m => m UTCTime -> EvolutionConfig -> m KESPeriod
getCurrentKESPeriodWith getNow ec = do
  now <- getNow
  let diffSecs = floor (nominalDiffTimeToSeconds $ diffUTCTime now (systemStart ec))
      kesPeriodDuration = fromIntegral (slotLength ec) * fromIntegral (slotsPerKESPeriod ec)
  return $ KESPeriod (diffSecs `div` kesPeriodDuration)

-- | Evolve a KES key to the current period. The old key will be released as
-- appropriate. If the current period exceeds the key's available evolutions,
-- return 'Nothing'. If the specified period is before the key's current
-- evolution, return 'Just' the key at its current evolution.
updateKESToCurrent ::
  KESAlgorithm (KES v) =>
  MonadTime m =>
  MonadST m =>
  MonadThrow m =>
  EvolutionConfig ->
  ContextKES (KES v) ->
  OCert v ->
  SignKeyWithPeriodKES (KES v) ->
  m (Maybe (SignKeyWithPeriodKES (KES v)))
updateKESToCurrent ec context cert skp = do
  currentPeriod <- getCurrentKESPeriod ec
  updateKESTo context currentPeriod cert skp

-- | Evolve a KES key to the specified period. The old key will be released as
-- appropriate. If the specified period exceeds the key's available evolutions,
-- return 'Nothing'. If the specified period is before the key's current
-- evolution, return 'Just' the key at its current evolution.
updateKESTo ::
  KESAlgorithm (KES v) =>
  MonadST m =>
  MonadThrow m =>
  ContextKES (KES v) ->
  KESPeriod ->
  OCert v ->
  SignKeyWithPeriodKES (KES v) ->
  m (Maybe (SignKeyWithPeriodKES (KES v)))
updateKESTo context currentPeriod cert skp = do
  let targetEvolution =
        unKESPeriod currentPeriod
          - unKESPeriod (ocertKESPeriod cert)
  if periodKES skp >= targetEvolution
    then
      return (Just skp) -- key is already up to date or newer
    else do
      skp'May <- updateKESWithPeriod context skp
      case skp'May of
        Nothing ->
          -- update failed (t >= totalPeriodsKES)
          return Nothing
        Just skp' ->
          -- update succeeded
          if periodKES skp' == targetEvolution
            then
              -- reached target evolution, so we're done
              return (Just skp')
            else
              -- Still not enough, keep going.
              -- This also captures the (theoretical) case where we have updated
              -- past the last evolution, but the recursive call will catch this.
              updateKESTo context currentPeriod cert skp'
