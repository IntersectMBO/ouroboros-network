module Test.Ouroboros.Network.Diffusion.Node.ChainDB
  ( ChainDB (..)
  , SelectChain (..)
  , newChainDB
  , addBlock
  , getBlockPointSet
  ) where

import           Control.Concurrent.Class.MonadSTM (MonadSTM (..))
import           Data.Coerce (coerce)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Ouroboros.Network.AnchoredFragment (Point)
import           Ouroboros.Network.Block (ChainHash (..), HasFullHeader,
                     HasHeader, blockHash, blockPoint, blockPrevHash)
import           Ouroboros.Network.Mock.Chain (Chain (..), selectChain)
import qualified Ouroboros.Network.Mock.Chain as Chain

-- | ChainDB is an in memory store of all fetched (downloaded) blocks.
--
-- This type holds an index mapping previous hashes to their blocks (i.e. if a
-- block "A" has prevHash "H" then the entry "H -> [A]" exists in the map) and
-- the current version of the longest chain.
--
-- Used to simulate real world ChainDB, it offers the invariant that
-- 'cdbLongestChainVar' is always the longest known chain of downloaded blocks.
-- Whenever a node generates a new block it gets added here, and whenever it gets
-- a block via block fetch it gets added here as well. Everytime 'addBlock' is
-- called the possibly new longest chain gets computed, since the API is atomic
-- we can guarantee that in each moment ChainDB has the current longest chain.
--
-- This type is used in diffusion simulation.
--
data ChainDB block m = ChainDB { cdbIndexVar :: TVar m (Map (ChainHash block) [block]),
                                 cdbLongestChainVar :: TVar m (Chain block)
                               }

-- | Constructs a new ChainDB, the index has only 1 value which is the
-- 'GenesisHash' but this hash does not map to any block.
--
newChainDB :: MonadSTM m => m (ChainDB block m)
newChainDB = do
  indexVar <- newTVarIO (Map.singleton GenesisHash [])
  longestChain <- newTVarIO Genesis
  return (ChainDB indexVar longestChain)

-- | Adds a block to ChainDB.
--
-- This function also recomputes the longest chain with the new block
-- information.
--
addBlock :: (MonadSTM m, HasFullHeader block)
         => block -> ChainDB block m -> STM m ()
addBlock block chainDB@(ChainDB indexVar lchainVar) = do
  modifyTVar' indexVar $ \index ->
    case Map.lookup (blockPrevHash block) index of
      Nothing -> Map.insertWith (++) GenesisHash [block] index
      Just _  -> Map.insertWith (++) (blockPrevHash block) [block] index
  longestChain <- getLongestChain chainDB
  writeTVar lchainVar longestChain

-- | Constructs the block Point set of all downloaded blocks
--
getBlockPointSet :: (MonadSTM m, HasHeader block)
                 => ChainDB block m -> STM m (Set (Point block))
getBlockPointSet (ChainDB indexVar _) = do
  index <- readTVar indexVar
  return (foldMap (Set.fromList . map blockPoint) index)

-- | Computes the longest chain from Genesis
--
getLongestChain :: (HasHeader block, MonadSTM m)
                => ChainDB block m
                -> STM m (Chain block)
getLongestChain (ChainDB indexVar _) = do
    index <- readTVar indexVar
    return (go Nothing Genesis index)
  where
    go :: HasHeader block
       => Maybe block
       -> Chain block
       -> Map (ChainHash block) [block]
       -> Chain block
    go mbblock chain m =
      let hash = maybe GenesisHash (BlockHash . blockHash) mbblock
       in case Map.lookup hash m of
            Nothing -> maybe Genesis (`Chain.addBlock` chain) mbblock
            Just blocks ->
              let longestChain = getSelectedChain
                               $ foldMap (\b -> SelectChain
                                              $ go (Just b) chain m)
                                         blocks
               in maybe longestChain (`Chain.addBlock` longestChain) mbblock

-- | Chain selection as a 'Monoid'.
--
newtype SelectChain block = SelectChain { getSelectedChain :: Chain block }

instance HasHeader block => Semigroup (SelectChain block) where
    (<>) = (coerce :: (     Chain block ->       Chain block ->       Chain block)
                   -> SelectChain block -> SelectChain block -> SelectChain block)
           selectChain

instance HasHeader block => Monoid (SelectChain block) where
    mempty = SelectChain Genesis
