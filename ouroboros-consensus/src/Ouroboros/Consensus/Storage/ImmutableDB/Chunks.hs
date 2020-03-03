module Ouroboros.Consensus.Storage.ImmutableDB.Chunks (
    module X
  ) where

-- Only export public API from the Internal module
--
-- In particular, the following types will remain opaque:
--
-- * 'ChunkInfo'
-- * 'ChunkSize'
-- * 'ChunkNo'
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal as X
                     (ChunkInfo, ChunkNo, ChunkSize, chunksBetween,
                     countChunks, firstChunkNo, getChunkSize, nextChunkNo,
                     prevChunkNo, simpleChunkInfo)
