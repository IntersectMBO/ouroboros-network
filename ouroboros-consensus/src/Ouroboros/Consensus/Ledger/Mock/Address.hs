module Ouroboros.Consensus.Ledger.Mock.Address (
    Addr
  , AddrDist
  , mkAddrDist
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Ouroboros.Consensus.NodeId (NodeId (..))

-- | Mock address
type Addr = String

-- | Mapping from addresses to node IDs
--
-- This is needed in order to assign stake to nodes.
type AddrDist = Map Addr NodeId

-- | Construct address to node ID mapping
mkAddrDist :: Int -- ^ Number of nodes
           -> AddrDist
mkAddrDist numCoreNodes =
    Map.fromList $ zip [[addr]   | addr <- ['a'..]]
                       [CoreId n | n    <- [0  .. numCoreNodes - 1]]
