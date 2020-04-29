{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Supporting definitions for BlockchanTime instances that use the wallclock
module Ouroboros.Consensus.BlockchainTime.WallClock (
    -- * System start
    SystemStart(..)
    -- * Slot length
  , SlotLength -- Opaque
  , getSlotLength
  , mkSlotLength
    -- ** Conversions
  , slotLengthFromSec
  , slotLengthToSec
  , slotLengthFromMillisec
  , slotLengthToMillisec
    -- * Tracing
  , TraceBlockchainTimeEvent(..)
    -- * Exceptions
  , SystemClockMovedBackException(..)
  ) where

import           Control.Exception (Exception)
import           Data.Fixed
import           Data.Time (NominalDiffTime, UTCTime)
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks, UseIsNormalForm (..))
import           Cardano.Slotting.Slot

{-------------------------------------------------------------------------------
  System start
-------------------------------------------------------------------------------}

-- | System start
--
-- Slots are counted from the system start.
newtype SystemStart = SystemStart { getSystemStart :: UTCTime }
  deriving (Eq, Show)
  deriving NoUnexpectedThunks via UseIsNormalForm SystemStart

{-------------------------------------------------------------------------------
  SlotLength
-------------------------------------------------------------------------------}

-- | Slot length
newtype SlotLength = SlotLength { getSlotLength :: NominalDiffTime }
  deriving (Show, Eq, Generic, NoUnexpectedThunks)

-- | Constructor for 'SlotLength'
mkSlotLength :: NominalDiffTime -> SlotLength
mkSlotLength = SlotLength

slotLengthFromSec :: Integer -> SlotLength
slotLengthFromSec = slotLengthFromMillisec . (* 1000)

slotLengthToSec :: SlotLength -> Integer
slotLengthToSec = (`div` 1000) . slotLengthToMillisec

slotLengthFromMillisec :: Integer -> SlotLength
slotLengthFromMillisec = mkSlotLength . conv
  where
    -- Explicit type annotation here means that /if/ we change the precision,
    -- we are forced to reconsider this code.
    conv :: Integer -> NominalDiffTime
    conv = (realToFrac :: Pico -> NominalDiffTime)
         . (/ 1000)
         . (fromInteger :: Integer -> Pico)

slotLengthToMillisec :: SlotLength -> Integer
slotLengthToMillisec = conv . getSlotLength
  where
    -- Explicit type annotation here means that /if/ we change the precision,
    -- we are forced to reconsider this code.
    conv :: NominalDiffTime -> Integer
    conv = truncate
         . (* 1000)
         . (realToFrac :: NominalDiffTime -> Pico)

{-------------------------------------------------------------------------------
  Tracing
-------------------------------------------------------------------------------}

-- | Time related tracing
data TraceBlockchainTimeEvent
  = TraceStartTimeInTheFuture SystemStart NominalDiffTime
    -- ^ The start time of the blockchain time is in the future. We have to
    -- block (for 'NominalDiffTime') until that time comes.
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
