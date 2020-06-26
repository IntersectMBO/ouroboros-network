{-# LANGUAGE RecordWildCards #-}

-- | Support for defining 'BlockchainTime' instances
module Ouroboros.Consensus.BlockchainTime.WallClock.Util (
    -- * Tracing
    TraceBlockchainTimeEvent(..)
    -- * Exceptions
  , SystemClockMovedBackException(..)
  ) where

import           Control.Exception (Exception)
import           Data.Time (NominalDiffTime, UTCTime)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime.WallClock.Types
                     (SystemStart)
import           Ouroboros.Consensus.HardFork.History (PastHorizonException)

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
  | TraceCurrentSlotUnknown UTCTime PastHorizonException
  deriving (Show)

{-------------------------------------------------------------------------------
  Exceptions
-------------------------------------------------------------------------------}

data SystemClockMovedBackException =
    -- | The system clock got moved back so far that the slot number decreased
    --
    -- We record the the slot number before and after the change.
    SystemClockMovedBack SlotNo SlotNo
  deriving (Show)

instance Exception SystemClockMovedBackException
