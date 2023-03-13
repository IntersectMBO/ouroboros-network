{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.BlockchainTime.WallClock.Simple (
    simpleBlockchainTime
    -- * Low-level API (exported primarily for testing)
  , getWallClockSlot
  , waitUntilNextSlot
  ) where

import           Control.Monad
import           Data.Bifunctor
import           Data.Fixed (divMod')
import           Data.Time (NominalDiffTime)
import           Data.Void
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime.API
import           Ouroboros.Consensus.BlockchainTime.WallClock.Types
import           Ouroboros.Consensus.BlockchainTime.WallClock.Util
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.Time

-- | Real blockchain time
--
-- WARNING: if the start time is in the future, 'simpleBlockchainTime' will
-- block until the start time has come.
simpleBlockchainTime :: forall m. IOLike m
                     => ResourceRegistry m
                     -> SystemTime m
                     -> SlotLength
                     -> NominalDiffTime -- ^ Max clock rewind
                     -> m (BlockchainTime m)
simpleBlockchainTime registry time slotLen maxClockRewind = do
    systemTimeWait time

    -- Fork thread that continuously updates the current slot
    firstSlot <- fst <$> getWallClockSlot time slotLen
    slotVar   <- newTVarIO firstSlot
    void $ forkLinkedThread registry "simpleBlockchainTime" $
             loop slotVar firstSlot

    -- The API is now a simple STM one
    return BlockchainTime {
        getCurrentSlot = CurrentSlot <$> readTVar slotVar
      }
  where
    -- In each iteration of the loop, we recompute how long to wait until
    -- the next slot. This minimizes clock skew.
    loop :: StrictTVar m SlotNo
         -> SlotNo
         -> m Void
    loop slotVar = go
      where
        go :: SlotNo -> m Void
        go current = do
          next <- waitUntilNextSlot time slotLen maxClockRewind current
          atomically $ writeTVar slotVar next
          go next

{-------------------------------------------------------------------------------
  Pure calculations
-------------------------------------------------------------------------------}

slotFromUTCTime :: SlotLength -> RelativeTime -> (SlotNo, NominalDiffTime)
slotFromUTCTime slotLen (RelativeTime now) =
    first SlotNo $ now `divMod'` getSlotLength slotLen

delayUntilNextSlot :: SlotLength -> RelativeTime -> NominalDiffTime
delayUntilNextSlot slotLen now =
    getSlotLength slotLen - timeSpent
  where
    (_curSlot, timeSpent) = slotFromUTCTime slotLen now

{-------------------------------------------------------------------------------
  Stateful wrappers around the pure calculations
-------------------------------------------------------------------------------}

-- | Get current slot and time spent in that slot
getWallClockSlot :: IOLike m
                 => SystemTime m
                 -> SlotLength
                 -> m (SlotNo, NominalDiffTime)
getWallClockSlot SystemTime{..} slotLen =
    slotFromUTCTime slotLen <$> systemTimeCurrent

-- | Wait until the next slot
--
-- Takes the current slot number to guard against system clock changes. If the
-- clock changes back further than the max clock rewind parameter, a fatal
-- 'SystemClockMovedBack' exception will be thrown. When this exception is
-- thrown, the node will shut down, and should be restarted with (full?)
-- validation enabled: it is conceivable that blocks got moved to the immutable
-- DB that, due to the clock change, should not be considered immutable anymore.
--
-- If the clock changed back less than the max clock rewind parameter, we stay
-- in the same slot for longer and don't throw an exception.
waitUntilNextSlot :: IOLike m
                  => SystemTime m
                  -> SlotLength
                  -> NominalDiffTime  -- ^ Max clock rewind
                  -> SlotNo           -- ^ Current slot number
                  -> m SlotNo
waitUntilNextSlot time@SystemTime{..} slotLen maxClockRewind oldCurrent = do
    now <- systemTimeCurrent

    let delay = delayUntilNextSlot slotLen now
    threadDelay (nominalDelay delay)

    -- At this point we expect to be in 'nextSlot', but the actual now-current
    -- slot might be different:
    --
    -- o If the system is under heavy load, we might have missed some slots. If
    --   this is the case, that's okay, and we just report the actual
    --   now-current slot as the next slot.
    -- o If the system clock is adjusted back a tiny bit (maybe due to an NTP
    --   client running on the system), it's possible that we are still in the
    --   /old/ current slot. If this happens, we just wait again; nothing bad
    --   has happened, we just stay in one slot for longer.
    -- o If the system clock is adjusted back more than that, we might be in a
    --   slot number /before/ the old current slot. In that case, if the
    --   adjustment is <= the max rewind parameter, we allow it, but stay in the
    --   same slot. Just like the previous case, we will stay in one slot for
    --   longer.
    -- o If the system clock is adjusted back more than the max rewind
    --   parameter, we throw an exception (see discussion above).

    afterDelay <- systemTimeCurrent
    let (newCurrent, _timeInNewCurrent) = slotFromUTCTime slotLen afterDelay

    if | newCurrent > oldCurrent ->
           return newCurrent
       | newCurrent <= oldCurrent,
         now `diffRelTime` afterDelay <= maxClockRewind ->
           waitUntilNextSlot time slotLen maxClockRewind oldCurrent
       | otherwise ->
           throwIO $ SystemClockMovedBack oldCurrent newCurrent
