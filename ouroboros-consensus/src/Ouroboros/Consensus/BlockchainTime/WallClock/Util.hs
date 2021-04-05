{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE RecordWildCards #-}

-- | Support for defining 'BlockchainTime' instances
module Ouroboros.Consensus.BlockchainTime.WallClock.Util (
    -- * Tracing
    TraceBlockchainTimeEvent (..)
    -- * Exceptions
  , SystemClockMovedBackException (..)
  ) where

import           Control.Exception (Exception)
import           Data.Time (NominalDiffTime)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime.WallClock.Types
                     (SystemStart)
import           Ouroboros.Consensus.HardFork.History (PastHorizonException)

{-------------------------------------------------------------------------------
  Tracing
-------------------------------------------------------------------------------}

-- | Time related tracing
--
-- The @t@ parameter can be instantiated by the time, e.g., @UTCTime@ or
-- @RelativeTime@.
data TraceBlockchainTimeEvent t =
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
  | TraceCurrentSlotUnknown t PastHorizonException

    -- | The system clock moved back an acceptable time span, e.g., because of
    -- an NTP sync.
    --
    -- The system clock moved back such that the new current slot would be
    -- smaller than the previous one. If this is within the configured limit, we
    -- trace this warning but *do not change the current slot*. The current slot
    -- never decreases, but the current slot may stay the same longer than
    -- expected.
    --
    -- When the system clock moved back more than the configured limit, we shut
    -- down with a fatal exception.
  | TraceSystemClockMovedBack t t
  deriving (Show, Functor)

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
