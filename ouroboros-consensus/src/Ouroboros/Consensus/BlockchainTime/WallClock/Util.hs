{-# LANGUAGE RecordWildCards #-}

-- | Support for defining 'BlockchainTime' instances
module Ouroboros.Consensus.BlockchainTime.WallClock.Util (
    -- * Tracing
    TraceBlockchainTimeEvent(..)
    -- * Exceptions
  , SystemClockMovedBackException(..)
    -- * Support for defining 'BlockchainTime' instances
  , waitUntilSystemStart
  ) where

import           Control.Exception (Exception)
import           Control.Monad
import           Control.Tracer
import           Data.Time (NominalDiffTime, UTCTime, diffUTCTime)

import           Cardano.Slotting.Slot

import           Ouroboros.Consensus.BlockchainTime.WallClock.Types
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Time

{-------------------------------------------------------------------------------
  Tracing
-------------------------------------------------------------------------------}

-- | Time related tracing
data TraceBlockchainTimeEvent =
    -- | The start time of the blockchain time is in the future
    --
    -- We have to block (for 'NominalDiffTime') until that time comes.
    TraceStartTimeInTheFuture SystemStart NominalDiffTime

    -- | Current slot is not yet known
    --
    -- This happens when the tip of our current chain is so far in the past that
    -- we cannot translate the current wallclock to a slot number, typically
    -- during syncing. Until the current slot number is known, we cannot
    -- produce blocks. Seeing this message during syncing therefore is
    -- normal and to be expected.
    --
    -- We record the current time (the time we tried to translate to a 'SlotNo')
    -- as well as the 'PastHorizonException', which provides detail on the
    -- bounds between which we /can/ do conversions. The distance between the
    -- current time and the upper bound should rapidly decrease with consecutive
    -- 'TraceCurrentSlotUnknown' messages during syncing.
  | TraceCurrentSlotUnknown UTCTime HardFork.PastHorizonException
  deriving (Show)

{-------------------------------------------------------------------------------
  Exceptions
-------------------------------------------------------------------------------}

data SystemClockMovedBackException =
    -- | The system clock got moved back so far that the slot number decreased
    --
    -- We record the time at which we discovered the clock change, the slot
    -- number before the clock change, and the slot number after the change.
    SystemClockMovedBack UTCTime SlotNo SlotNo
  deriving (Show)

instance Exception SystemClockMovedBackException

{-------------------------------------------------------------------------------
  Wait for system start
-------------------------------------------------------------------------------}

-- | Wait until system start if necessary
waitUntilSystemStart :: MonadDelay m
                     => Tracer m TraceBlockchainTimeEvent
                     -> SystemTime m
                     -> m ()
waitUntilSystemStart tracer SystemTime{..} = do
    now <- systemTimeCurrent
    when (getSystemStart systemTimeStart > now) $ do
      let delay = getSystemStart systemTimeStart `diffUTCTime` now
      traceWith tracer $ TraceStartTimeInTheFuture systemTimeStart delay
      threadDelay (nominalDelay delay)
