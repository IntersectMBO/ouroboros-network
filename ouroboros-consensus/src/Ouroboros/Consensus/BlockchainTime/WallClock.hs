{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.BlockchainTime.WallClock (
    realBlockchainTime
  , TraceBlockchainTimeEvent(..)
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
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.STM

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
    first <- fst <$> getWallClockSlot start lsVar
    slot  <- newTVarM first
    void $ forkLinkedThread registry $ loop lsVar slot

    -- The API is now a simple STM one
    return BlockchainTime {
        getCurrentSlot = readTVar slot
      , onSlotChange_  = onEachChange registry id (Just first) (readTVar slot)
      }
  where
    -- In each iteration of the loop, we recompute how long to wait until
    -- the next slot. This minimizes clock skew.
    loop :: StrictTVar m FocusedSlotLengths -> StrictTVar m SlotNo -> m Void
    loop lsVar slot = forever $ do
      next <- waitUntilNextSlot start lsVar
      atomically $ writeTVar slot next

{-------------------------------------------------------------------------------
  Stateful wrappers around Ouroboros.Consensus.BlockchainTime.SlotLengths
-------------------------------------------------------------------------------}

getWallClockSlot :: IOLike m
                 => SystemStart
                 -> StrictTVar m FocusedSlotLengths
                 -> m (SlotNo, NominalDiffTime)
getWallClockSlot start lsVar = do
    now <- getCurrentTime
    atomically $ updateTVar lsVar $ slotFromUTCTime start now

waitUntilNextSlot :: IOLike m
                  => SystemStart
                  -> StrictTVar m FocusedSlotLengths
                  -> m SlotNo
waitUntilNextSlot start lsVar = do
    now               <- getCurrentTime
    (delay, nextSlot) <- atomically $ updateTVar lsVar $
                                        delayUntilNextSlot start now
    threadDelay (nominalDelay delay)
    return nextSlot

{-------------------------------------------------------------------------------
  Auxiliary: conversions
-------------------------------------------------------------------------------}

nominalDelay :: NominalDiffTime -> DiffTime
nominalDelay = realToFrac
