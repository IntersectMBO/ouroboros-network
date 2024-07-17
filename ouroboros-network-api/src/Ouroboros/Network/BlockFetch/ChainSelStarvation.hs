{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Ouroboros.Network.BlockFetch.ChainSelStarvation
  ( ChainSelStarvationHandle
  , ChainSelStarvationConfig (..)
  , initStarvationHandle
  , startStarvation
  , receivedBlockAt
  , isStarvationExceeded
  , resetStarvation
  ) where

import Control.Monad.Class.MonadSTM
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadTime.SI
         (DiffTime, MonadMonotonicTime, Time (..), diffTime, getMonotonicTime)

import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)


-- | The state of ChainSel starvation
--
-- BlockFetch peers are allowed to starve ChainSel for a while. We
-- keep here a cumulative sum of all the time that ChainSel has been starved
-- when downloading the last blocks.
--
-- The bulk sync fetch decision logic needs to decide whether the current
-- current peer has starved ChainSel too much.
data ChainSelStarvationState =
    ChainSelStarvationState {
      -- | The last starvation event
      cssLastEvent            :: ChainSelStarvationEvent,
      -- | The amount of time that ChainSel waited for new blocks to come
      cssCumulativeStarvation :: DiffTime,
      -- | The amount of blocks to download before reseting the cumulative starvation
      cssRemainingBlocks      :: Word64,
      -- | Whether the allowed starvation time was exceeded
      cssExceeded             :: Bool
    }
  deriving (Eq, Show, NoThunks, Generic)

-- | Timestamps for ChainSel starvations
data ChainSelStarvationEvent = ChainSelStarvationStartedAt Time | ChainSelStarvationEndedAt Time
  deriving (Eq, Show, NoThunks, Generic)

-- | A handle to measure and check starvation of ChainSel
--
-- When the starvation exceeds the configured limit, the handle
-- remembers so until it is reset with 'resetStarvation'.
data ChainSelStarvationHandle m =
    ChainSelStarvationHandle ChainSelStarvationConfig (StrictTVar m ChainSelStarvationState)

data ChainSelStarvationConfig = ChainSelStarvationConfig
    { csscWindow :: Word64 -- ^ How many blocks are in the window for cumulative starvation
    , csscLimit  :: DiffTime -- ^ How large is the cumulative starvation allowed to be
    }

-- | Initializes a handle from the given configuration.
initStarvationHandle :: MonadSTM m => ChainSelStarvationConfig -> STM m (ChainSelStarvationHandle m)
initStarvationHandle config = do
    -- TODO: throw an error if the configuration values are non-positive?
    starvationVar <- newTVar ChainSelStarvationState
      { cssLastEvent = ChainSelStarvationEndedAt (Time 0)
      , cssCumulativeStarvation = 0
      , cssRemainingBlocks = csscWindow config
      , cssExceeded = False
      }
    pure $ ChainSelStarvationHandle config starvationVar

-- | Records an starvation as started at the given time.
startStarvation
  :: MonadSTM m => ChainSelStarvationHandle m -> Time -> STM m ()
startStarvation (ChainSelStarvationHandle _config starvationVar) t0 = do
    prevStarvation <- readTVar starvationVar
    case cssLastEvent prevStarvation of
      ChainSelStarvationEndedAt{} -> do
        v <- readTVar starvationVar
        let starvation = v{cssLastEvent = ChainSelStarvationStartedAt t0}
        writeTVar starvationVar starvation
      _ ->
        pure ()

-- | Records a block as received at the given time. If an starvation was
-- ongoing, it is terminated.
receivedBlockAt :: MonadSTM m => ChainSelStarvationHandle m -> Time -> STM m ()
receivedBlockAt (ChainSelStarvationHandle config starvationVar) tf =
    modifyTVar starvationVar $ \v ->
    -- If there was a starvation ongoing, we need to report that it is done.
    let (lastEvent, duration) = case cssLastEvent v of
          ChainSelStarvationStartedAt t0 ->
            (ChainSelStarvationEndedAt tf, diffTime tf t0)
          e -> (e, 0)
        cumulative =
          if cssRemainingBlocks v > 0 then
            duration + cssCumulativeStarvation v
          else
            0
        remainingBlocks =
          if cssRemainingBlocks v > 0 then
            cssRemainingBlocks v - 1
          else
            csscWindow config - 1
     in ChainSelStarvationState
          { cssLastEvent = lastEvent
          , cssCumulativeStarvation = cumulative
          , cssRemainingBlocks = remainingBlocks
          , cssExceeded = cssExceeded v || cumulative > csscLimit config
          }

-- | Tells if the allowed starvation time has been exceeded in the past
--
-- The excess can be forgotten with 'resetStarvation'.
isStarvationExceeded :: (MonadSTM m, MonadMonotonicTime m) => ChainSelStarvationHandle m -> m Bool
isStarvationExceeded (ChainSelStarvationHandle config starvationVar) = do
    v <- atomically $ readTVar starvationVar
    ongoingStarvationDuration <- case cssLastEvent v of
      ChainSelStarvationStartedAt t0 -> do
        tf <- getMonotonicTime
        pure (diffTime tf t0)
      _ ->
        pure 0
    pure $ cssExceeded v ||
           ongoingStarvationDuration + cssCumulativeStarvation v > csscLimit config

-- | Resets the cumulative starvation and forgets past excesses
--
-- If there is an ongoing starvation, it is counted as if it had
-- started now.
--
resetStarvation
  :: (MonadMonotonicTime m, MonadSTM m) => ChainSelStarvationHandle m -> m ()
resetStarvation (ChainSelStarvationHandle config starvationVar) = do
    now <- getMonotonicTime
    atomically $ modifyTVar starvationVar $ \v ->
      v{ cssRemainingBlocks = csscWindow config
       , cssCumulativeStarvation = 0
       , cssExceeded = False
       , cssLastEvent = case cssLastEvent v of
           ChainSelStarvationStartedAt{} -> ChainSelStarvationStartedAt now
           e -> e
       }
