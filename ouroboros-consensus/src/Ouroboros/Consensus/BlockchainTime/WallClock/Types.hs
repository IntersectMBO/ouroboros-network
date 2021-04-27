{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ouroboros.Consensus.BlockchainTime.WallClock.Types (
    -- * System time
    SystemStart (..)
    -- * Relative time
  , RelativeTime (..)
  , addRelTime
  , diffRelTime
  , fromRelativeTime
  , toRelativeTime
    -- * Get current time (as 'RelativeTime')
  , SystemTime (..)
    -- * Slot length
  , getSlotLength
  , mkSlotLength
    -- ** Conversions
  , slotLengthFromMillisec
  , slotLengthFromSec
  , slotLengthToMillisec
  , slotLengthToSec
    -- ** opaque
  , SlotLength
  ) where

import           Data.Time.Clock (NominalDiffTime)
import           NoThunks.Class (NoThunks, OnlyCheckWhnfNamed (..))

import           Cardano.Slotting.Time

addRelTime :: NominalDiffTime -> RelativeTime -> RelativeTime
addRelTime = addRelativeTime

diffRelTime :: RelativeTime -> RelativeTime -> NominalDiffTime
diffRelTime = diffRelativeTime

{-------------------------------------------------------------------------------
  Get current time (as RelativeTime)
-------------------------------------------------------------------------------}

-- | System time
--
-- Slots are counted from the system start.
data SystemTime m = SystemTime {
      -- | Get current time (as a 'RelativeTime')
      --
      -- For real deployment, this will take the current 'UTCTime' and then
      -- subtract the 'SystemStart' (see 'defaultSystemTime'). Tests don't
      -- bother with a 'UTCTime' and just work entirely in 'RelativeTime'.
      systemTimeCurrent :: m RelativeTime

      -- | Wait for 'SystemStart'
      --
      -- For the real deployment, this waits for the current 'UTCTime'
      -- to reach 'SystemStart'. In tests this does nothing.
    , systemTimeWait    :: m ()
    }
  deriving NoThunks via OnlyCheckWhnfNamed "SystemTime" (SystemTime m)
