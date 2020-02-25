module Ouroboros.Consensus.Storage.ImmutableDB.ChunkSize (
    ChunkSize(..)
  , slotForEBB
  ) where

import           Data.Word

import           Cardano.Slotting.Slot

-- | Chunk size
--
-- The immutable DB stores blocks in files containing 'getChunkSize' slots
-- (slots, not blocks).
--
-- The chunk size can be chosen more or less arbitrarily, /unless/ there are
-- EBBs present. If there are EBBs present, the chunk size /must/ be chosen
-- such that the EBB is the first block in each chunk; that is, such that
-- 'slotForBoundaryBlock' is correct.
newtype ChunkSize = ChunkSize { getChunkSize :: Word64 }
  deriving (Show)

-- | The slot number for the EBB of the given epoch
slotForEBB :: ChunkSize -> EpochNo -> SlotNo
slotForEBB (ChunkSize sz) (EpochNo epoch) = SlotNo (sz * epoch)
