module Ouroboros.Consensus.Storage.ImmutableDB.Chunks (
    module X
  ) where


import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Layout as X

-- Only export public API from the Internal module
--
-- In particular, the following types will remain opaque:
--
-- * 'ChunkInfo'
-- * 'ChunkSize'
-- * 'ChunkNo'
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal as X
                     (ChunkInfo, ChunkNo, ChunkSize, chunksBetween,
                     compareRelativeSlot, countChunks, firstChunkNo,
                     getChunkSize, mkRelativeSlot, nextChunkNo, prevChunkNo,
                     simpleChunkInfo, singleChunkInfo)
