{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- TODO: Temporary:
{-# OPTIONS -Wwarn #-}

module Ouroboros.Consensus.Storage.ImmutableDB.ChunkInfo (
    ChunkInfo -- Opaque
  , simpleChunkInfo
    -- * Derived info
  , ChunkSize -- Opaque
  , getChunkSize
    -- * Layout
  , RelativeSlot(..)
  , maxRelativeSlot
    -- * Emulate EpochInfo interface (TODO: temporary)
  , epochInfoFirst
  , epochInfoEpoch
  ) where

import           Data.Functor.Identity
import           Data.Word
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)
import           Cardano.Slotting.Slot

import           Cardano.Slotting.EpochInfo (EpochInfo)
import qualified Cardano.Slotting.EpochInfo as EI

-- | Number of blocks per file in the immutable DB
--
-- This is an internal type, we export some convenience constructors only.
newtype ChunkInfo = ChunkInfo (EpochInfo Identity)
  deriving (NoUnexpectedThunks, Show)

-- | Single uniform chunk size
simpleChunkInfo :: Word64 -> ChunkInfo
simpleChunkInfo = ChunkInfo . EI.fixedSizeEpochInfo . EpochSize

{-------------------------------------------------------------------------------
  Derived info

  TODO: These 'EpochNo's are really 'ChunkNo's.
-------------------------------------------------------------------------------}

-- | Size of a chunk
--
-- This is an opaque type! The reason is that the interpretation of the
-- chunk size is a bit confusing: the number of available slots within a single
-- chunk is the chunksize /plus one/, since we always make space for an EBB.
newtype ChunkSize = ChunkSize Word64
  deriving (Show)

getChunkSize :: ChunkInfo -> EpochNo -> ChunkSize
getChunkSize (ChunkInfo ei) = ChunkSize . unEpochSize
                            . runIdentity . EI.epochInfoSize ei

{-------------------------------------------------------------------------------
  Layout

  This is used only internally by the immutable DB, and is not relevant for
  clients. We nonetheless define it here so that we can keep 'ChunkInfo' and
  related types entirely opaque outside of this one module.
-------------------------------------------------------------------------------}

-- | A /relative/ slot within an 'EpochNo'.
newtype RelativeSlot = RelativeSlot { unRelativeSlot :: Word64 }
  deriving stock   (Eq, Ord, Show, Generic)
  deriving newtype (Num, Enum, NoUnexpectedThunks)

-- | Return the last relative slot within the given chunk
--
-- Relative slot 0 is reserved for the EBB and regular relative slots start at
-- 1, so the last relative slot is equal to the chunk size.
maxRelativeSlot :: ChunkSize -> RelativeSlot
maxRelativeSlot (ChunkSize sz) = RelativeSlot sz

{-------------------------------------------------------------------------------
  TODO: Temporary:
  Emulate the EpochInfo interface
-------------------------------------------------------------------------------}

epochInfoFirst :: ChunkInfo -> EpochNo -> SlotNo
epochInfoFirst (ChunkInfo ei) = runIdentity . EI.epochInfoFirst ei

epochInfoEpoch :: ChunkInfo -> SlotNo -> EpochNo
epochInfoEpoch (ChunkInfo ei) = runIdentity . EI.epochInfoEpoch ei
