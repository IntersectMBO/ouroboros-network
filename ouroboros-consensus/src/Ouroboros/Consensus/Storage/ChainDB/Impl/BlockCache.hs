{-# LANGUAGE FlexibleContexts #-}
-- | Cache blocks in memory
--
-- Intended for qualified import.
--
-- > import           Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache (BlockCache)
-- > import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache as BlockCache
module Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache (
    cacheBlock
  , empty
  , lookup
  , singleton
    -- * opaque
  , BlockCache
  ) where

import           Prelude hiding (lookup)

import           Data.Map (Map)
import qualified Data.Map as Map

import           Ouroboros.Consensus.Block

newtype BlockCache blk = BlockCache (Map (HeaderHash blk) blk)

empty :: BlockCache blk
empty = BlockCache Map.empty

singleton :: HasHeader blk => blk -> BlockCache blk
singleton blk = cacheBlock blk empty

cacheBlock :: HasHeader blk => blk -> BlockCache blk -> BlockCache blk
cacheBlock blk (BlockCache cache) = BlockCache (Map.insert (blockHash blk) blk cache)

lookup :: HasHeader blk => HeaderHash blk -> BlockCache blk -> Maybe blk
lookup hash (BlockCache cache) = Map.lookup hash cache
