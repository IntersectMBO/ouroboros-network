{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.BlockchainTime.Simple (
    simpleBlockchainTime
    -- * Low-level API (exported primarily for testing)
  , getWallClockSlot
  , waitUntilNextSlot
  ) where

import           Control.Monad
import           Data.Time (NominalDiffTime, diffUTCTime)
import           Data.Void

import           Control.Tracer (Tracer, traceWith)

import           Ouroboros.Network.Block (SlotNo)

import           Ouroboros.Consensus.BlockchainTime.API
import           Ouroboros.Consensus.BlockchainTime.SlotLengths
import           Ouroboros.Consensus.BlockchainTime.WallClock
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.Time

-- | Real blockchain time
--
-- WARNING: if the start time is in the future, 'simpleBlockchainTime' will
-- block until the start time has come.
simpleBlockchainTime :: forall m. IOLike m
                     => ResourceRegistry m
                     -> Tracer m TraceBlockchainTimeEvent
                     -> SystemStart
                     -> SlotLength
                     -> m (BlockchainTime m)
simpleBlockchainTime registry tracer start slotLen = do
    now   <- getCurrentTime
    lsVar <- newTVarM ls

    -- Wait until system start if necessary
    when (getSystemStart start > now) $ do
      let delay = getSystemStart start `diffUTCTime` now
      traceWith tracer $ TraceStartTimeInTheFuture start delay
      threadDelay (nominalDelay delay)

    -- Fork thread that continuously updates the current slot
    first   <- fst <$> getWallClockSlot start lsVar
    slotVar <- newTVarM first
    void $ forkLinkedThread registry "simpleBlockchainTime" $ do
      loop lsVar slotVar first

    -- The API is now a simple STM one
    return BlockchainTime {
        getCurrentSlot = readTVar slotVar
      }
  where
    ls :: FocusedSlotLengths
    ls = focusSlotLengths (singletonSlotLengths slotLen)

    -- In each iteration of the loop, we recompute how long to wait until
    -- the next slot. This minimizes clock skew.
    loop :: StrictTVar m FocusedSlotLengths
         -> StrictTVar m SlotNo
         -> SlotNo
         -> m Void
    loop lsVar slotVar = go
      where
        go :: SlotNo -> m Void
        go current = do
          next <- waitUntilNextSlot start lsVar current
          atomically $ writeTVar slotVar next
          go next

{-------------------------------------------------------------------------------
  Stateful wrappers around Ouroboros.Consensus.BlockchainTime.SlotLengths
-------------------------------------------------------------------------------}

-- | Get current slot and time spent in that slot
getWallClockSlot :: IOLike m
                 => SystemStart
                 -> StrictTVar m FocusedSlotLengths
                 -> m (SlotNo, NominalDiffTime)
getWallClockSlot start lsVar = do
    now <- getCurrentTime
    atomically $ updateTVar lsVar $ slotFromUTCTime start now

-- | Wait until the next slot
--
-- Takes the current slot number to guard against system clock changes. Any
-- clock changes that would result in the slot number to /decrease/ will result
-- in a fatal 'SystemClockMovedBackException'. When this exception is thrown,
-- the node will shut down, and should be restarted with (full?) validation
-- enabled: it is conceivable that blocks got moved to the immutable DB that,
-- due to the clock change, should not be considered immutable anymore.
waitUntilNextSlot :: IOLike m
                  => SystemStart
                  -> StrictTVar m FocusedSlotLengths
                  -> SlotNo    -- ^ Current slot number
                  -> m SlotNo
waitUntilNextSlot start lsVar oldCurrent = do
    now                <- getCurrentTime
    (delay, _nextSlot) <- atomically $ updateTVar lsVar $
                                        delayUntilNextSlot start now
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
    -- o If the system clock is adjusted back more than that, we might be in
    --   a slot number /before/ the old current slot. In that case, we throw
    --   an exception (see discussion above).

    (newCurrent, _timeInNewCurrent) <- getWallClockSlot start lsVar

    if | newCurrent > oldCurrent ->
           return newCurrent
       | newCurrent == oldCurrent ->
           waitUntilNextSlot start lsVar oldCurrent
       | otherwise ->
           throwM $ SystemClockMovedBack now oldCurrent newCurrent
