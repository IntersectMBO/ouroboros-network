{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ouroboros.Consensus.BlockchainTime.WallClock.Types (
    -- * System time
    SystemStart(..)
    -- * Relative time
  , RelativeTime(..)
  , addRelTime
  , diffRelTime
  , toRelativeTime
    -- * Get current time (as 'RelativeTime')
  , SystemTime(..)
    -- * Slot length
  , SlotLength -- Opaque
  , getSlotLength
  , mkSlotLength
    -- ** Conversions
  , slotLengthFromSec
  , slotLengthToSec
  , slotLengthFromMillisec
  , slotLengthToMillisec
  ) where

import           Codec.Serialise
import           Control.Exception (assert)
import           Data.Fixed
import           Data.Time (NominalDiffTime, UTCTime, diffUTCTime)
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks, OnlyCheckIsWHNF (..),
                     UseIsNormalForm (..))

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
  Relative time
-------------------------------------------------------------------------------}

-- | 'RelativeTime' is time relative to the 'SystemStart'
newtype RelativeTime = RelativeTime { getRelativeTime :: NominalDiffTime }
  deriving stock   (Show, Eq, Ord, Generic)
  deriving newtype (NoUnexpectedThunks)

addRelTime :: NominalDiffTime -> RelativeTime -> RelativeTime
addRelTime delta (RelativeTime t) = RelativeTime (t + delta)

diffRelTime :: RelativeTime -> RelativeTime -> NominalDiffTime
diffRelTime (RelativeTime t) (RelativeTime t') = t - t'

toRelativeTime :: SystemStart -> UTCTime -> RelativeTime
toRelativeTime (SystemStart t) t' = assert (t' >= t) $
                                      RelativeTime (diffUTCTime t' t)

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
  deriving NoUnexpectedThunks via OnlyCheckIsWHNF "SystemTime" (SystemTime m)

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
  Serialisation
-------------------------------------------------------------------------------}

instance Serialise RelativeTime where
  encode = encode . toSeconds . getRelativeTime
    where
      toSeconds :: NominalDiffTime -> Pico
      toSeconds = realToFrac

  decode = (RelativeTime . fromSeconds) <$> decode
    where
      fromSeconds :: Pico -> NominalDiffTime
      fromSeconds = realToFrac
