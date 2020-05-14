{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ouroboros.Consensus.BlockchainTime.WallClock.Types (
    -- * System time
    SystemStart(..)
  , SystemTime(..)
  , defaultSystemTime
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

import           Data.Fixed
import           Data.Time (NominalDiffTime, UTCTime)
import           GHC.Generics (Generic)

import           Cardano.Prelude (CanonicalExamples, NoUnexpectedThunks,
                     OnlyCheckIsWHNF (..), UseIsNormalForm (..))
import           Cardano.Prelude.CanonicalExamples.Orphans ()

import           Control.Monad.Class.MonadTime (MonadTime (..))

{-------------------------------------------------------------------------------
  System start
-------------------------------------------------------------------------------}

-- | System start
--
-- Slots are counted from the system start.
newtype SystemStart = SystemStart { getSystemStart :: UTCTime }
  deriving (Eq, Show)
  deriving NoUnexpectedThunks via UseIsNormalForm SystemStart

-- | System time
--
-- Slots are counted from the system start.
data SystemTime m = SystemTime {
      systemTimeStart   :: !SystemStart
    , systemTimeCurrent :: !(m UTCTime)
    }
  deriving NoUnexpectedThunks via OnlyCheckIsWHNF "SystemTime" (SystemTime m)

defaultSystemTime :: MonadTime m => SystemStart -> SystemTime m
defaultSystemTime start = SystemTime start getCurrentTime

{-------------------------------------------------------------------------------
  SlotLength
-------------------------------------------------------------------------------}

-- | Slot length
newtype SlotLength = SlotLength { getSlotLength :: NominalDiffTime }
  deriving (Show, Eq, Generic, NoUnexpectedThunks, CanonicalExamples)

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
