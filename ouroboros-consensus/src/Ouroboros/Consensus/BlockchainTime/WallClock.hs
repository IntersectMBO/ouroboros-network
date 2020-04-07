{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.BlockchainTime.WallClock (
    realBlockchainTime
  , TraceBlockchainTimeEvent(..)
  , SystemClockMovedBackException(..)
    -- * Low-level API (exported primarily for testing)
  , getWallClockSlot
  , waitUntilNextSlot
  , nominalDelay
  ) where

import           Control.Exception (Exception)
import           Control.Monad
import           Data.Time (NominalDiffTime, diffUTCTime)
import           Data.Void

import           Control.Tracer (Tracer, traceWith)

import           Ouroboros.Network.Block (SlotNo)

import           Ouroboros.Consensus.BlockchainTime.API
import           Ouroboros.Consensus.BlockchainTime.SlotLengths
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry

-- | Events emitted by 'realBlockchainTime'.
data TraceBlockchainTimeEvent
  = TraceStartTimeInTheFuture SystemStart NominalDiffTime
    -- ^ The start time of the blockchain time is in the future. We have to
    -- block (for 'NominalDiffTime') until that time comes.
  deriving (Show)

-- | Real blockchain time
--
-- WARNING: if the start time is in the future, 'realBlockchainTime' will
-- block until the start time has come.
realBlockchainTime :: forall m. IOLike m
                   => ResourceRegistry m
                   -> Tracer m TraceBlockchainTimeEvent
                   -> SystemStart
                   -> FocusedSlotLengths
                   -> m (BlockchainTime m)
realBlockchainTime registry tracer start ls = do
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
    void $ forkLinkedThread registry "realBlockchainTime" $ do
      loop lsVar slotVar first

    -- The API is now a simple STM one
    return BlockchainTime {
        getCurrentSlot = readTVar slotVar
      }
  where
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

data SystemClockMovedBackException =
    -- | The system clock got moved back so far that the slot number decreased
    --
    -- We record the time at which we discovered the clock change, the slot
    -- number before the clock change, and the slot number after the change.
    SystemClockMovedBack UTCTime SlotNo SlotNo
  deriving (Show)

instance Exception SystemClockMovedBackException

{-------------------------------------------------------------------------------
  Auxiliary: conversions
-------------------------------------------------------------------------------}

nominalDelay :: NominalDiffTime -> DiffTime
nominalDelay = realToFrac
