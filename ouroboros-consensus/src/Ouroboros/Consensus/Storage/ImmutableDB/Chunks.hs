module Ouroboros.Consensus.Storage.ImmutableDB.Chunks (
    module X
  ) where

-- Only export public API from the Internal module
--
-- In particular, keep 'ChunkConfig' itself opaque.
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal as X
                     (ChunkInfo, epochInfoEpoch, epochInfoFirst, epochInfoSize,
                     simpleChunkInfo)
