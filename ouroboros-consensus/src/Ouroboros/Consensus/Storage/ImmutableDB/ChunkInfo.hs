-- TODO: Temporary:
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- TODO: Temporary:
{-# OPTIONS -Wwarn #-}

module Ouroboros.Consensus.Storage.ImmutableDB.ChunkInfo (
    ChunkInfo -- Opaque
  , simpleChunkInfo
    -- * Emulate EpochInfo interface (TODO: temporary)
  , epochInfoSize
  , epochInfoFirst
  , epochInfoEpoch
  ) where

import           Data.Functor.Identity
import           Data.Word

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
  TODO: Temporary:
  Emulate the EpochInfo interface
-------------------------------------------------------------------------------}

epochInfoSize :: ChunkInfo -> EpochNo -> EpochSize
epochInfoSize (ChunkInfo ei) = runIdentity . EI.epochInfoSize ei

epochInfoFirst :: ChunkInfo -> EpochNo -> SlotNo
epochInfoFirst (ChunkInfo ei) = runIdentity . EI.epochInfoFirst ei

epochInfoEpoch :: ChunkInfo -> SlotNo -> EpochNo
epochInfoEpoch (ChunkInfo ei) = runIdentity . EI.epochInfoEpoch ei
