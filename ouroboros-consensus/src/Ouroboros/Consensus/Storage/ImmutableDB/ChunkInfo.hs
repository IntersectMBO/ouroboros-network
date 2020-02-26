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

  TODO: Next step: make pure.
-------------------------------------------------------------------------------}

epochInfoSize :: Monad m => ChunkInfo -> EpochNo -> m EpochSize
epochInfoSize (ChunkInfo ei) = hoist . EI.epochInfoSize ei

epochInfoFirst :: Monad m => ChunkInfo -> EpochNo -> m SlotNo
epochInfoFirst (ChunkInfo ei) = hoist . EI.epochInfoFirst ei

epochInfoEpoch :: Monad m => ChunkInfo -> SlotNo -> m EpochNo
epochInfoEpoch (ChunkInfo ei) = hoist . EI.epochInfoEpoch ei

hoist :: Monad m => Identity a -> m a
hoist (Identity a) = return a
