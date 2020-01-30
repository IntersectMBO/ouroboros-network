{-# LANGUAGE FlexibleContexts #-}
-- | Cache blocks in memory
--
-- Intended for qualified import.
--
-- > import           Ouroboros.Storage.ChainDB.Impl.BlockCache (BlockCache)
-- > import qualified Ouroboros.Storage.ChainDB.Impl.BlockCache as BlockCache
module Ouroboros.Storage.ChainDB.Impl.BlockCache
  ( BlockCache -- opaque
  , empty
  , singleton
  , cacheBlock
  , lookup
  , toHeaderOrBlock
  ) where

import           Prelude hiding (lookup)

import           Data.Map (Map)
import qualified Data.Map as Map

import           Ouroboros.Network.Block (HasHeader (..), HeaderHash)

import           Ouroboros.Consensus.Block (Header, headerHash)


newtype BlockCache blk = BlockCache (Map (HeaderHash blk) blk)

empty :: BlockCache blk
empty = BlockCache Map.empty

singleton :: HasHeader blk => blk -> BlockCache blk
singleton blk = cacheBlock blk empty

cacheBlock :: HasHeader blk => blk -> BlockCache blk -> BlockCache blk
cacheBlock blk (BlockCache cache) = BlockCache (Map.insert (blockHash blk) blk cache)

lookup :: HasHeader blk => HeaderHash blk -> BlockCache blk -> Maybe blk
lookup hash (BlockCache cache) = Map.lookup hash cache

toHeaderOrBlock
  :: (HasHeader blk, HasHeader (Header blk))
  => Header blk -> BlockCache blk -> Either (Header blk) blk
toHeaderOrBlock hdr blockCache
    | Just blk <- lookup (headerHash hdr) blockCache
    = Right blk
    | otherwise
    = Left hdr
