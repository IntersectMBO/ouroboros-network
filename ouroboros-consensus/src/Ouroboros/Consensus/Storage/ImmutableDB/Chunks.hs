module Ouroboros.Consensus.Storage.ImmutableDB.Chunks (
    module X
  ) where

-- Only export public API from the Internal module
--
-- In particular, the following types will remain opaque:
--
-- * 'ChunkInfo'
-- * 'ChunkSize'
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal as X
                     (ChunkInfo, ChunkSize, getChunkSize, simpleChunkInfo)
