{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
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

import           Codec.Serialise
import           Control.Exception (assert)
import           Data.Fixed
import           Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import           GHC.Generics (Generic)
import           NoThunks.Class (InspectHeap (..), NoThunks,
                     OnlyCheckWhnfNamed (..))
import           Quiet

{-------------------------------------------------------------------------------
  System start
-------------------------------------------------------------------------------}

-- | System start
--
-- Slots are counted from the system start.
newtype SystemStart = SystemStart { getSystemStart :: UTCTime }
  deriving (Eq, Generic)
  deriving NoThunks via InspectHeap SystemStart
  deriving Show via Quiet SystemStart

{-------------------------------------------------------------------------------
  Relative time
-------------------------------------------------------------------------------}

-- | 'RelativeTime' is time relative to the 'SystemStart'
newtype RelativeTime = RelativeTime { getRelativeTime :: NominalDiffTime }
  deriving stock   (Eq, Ord, Generic)
  deriving newtype (NoThunks)
  deriving Show via Quiet RelativeTime

addRelTime :: NominalDiffTime -> RelativeTime -> RelativeTime
addRelTime delta (RelativeTime t) = RelativeTime (t + delta)

diffRelTime :: RelativeTime -> RelativeTime -> NominalDiffTime
diffRelTime (RelativeTime t) (RelativeTime t') = t - t'

toRelativeTime :: SystemStart -> UTCTime -> RelativeTime
toRelativeTime (SystemStart t) t' = assert (t' >= t) $
                                      RelativeTime (diffUTCTime t' t)

fromRelativeTime :: SystemStart -> RelativeTime -> UTCTime
fromRelativeTime (SystemStart t) (RelativeTime t') = addUTCTime t' t

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

{-------------------------------------------------------------------------------
  SlotLength
-------------------------------------------------------------------------------}

-- | Slot length
newtype SlotLength = SlotLength { getSlotLength :: NominalDiffTime }
  deriving (Eq, Generic, NoThunks)
  deriving Show via Quiet SlotLength

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
  encode = encode . toPico . getRelativeTime
    where
      toPico :: NominalDiffTime -> Pico
      toPico = realToFrac

  decode = (RelativeTime . fromPico) <$> decode
    where
      fromPico :: Pico -> NominalDiffTime
      fromPico = realToFrac

instance Serialise SlotLength where
  encode = encode . slotLengthToMillisec
  decode = slotLengthFromMillisec <$> decode
