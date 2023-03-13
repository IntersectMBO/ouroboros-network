{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ouroboros.Consensus.Mock.Ledger.Address (
    Addr
  , AddrDist
  , mkAddrDist
  ) where

import           Codec.Serialise (Serialise)
import           Control.DeepSeq (NFData)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.String
import           NoThunks.Class (NoThunks)
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId (NodeId (..))
import           Ouroboros.Consensus.Util.Condense

-- | Mock address
newtype Addr = Addr String
  deriving (
      Show
    , Eq
    , Ord
    , IsString
    , Serialise
    , NFData
    , NoThunks
    )

instance Condense Addr where
  condense (Addr addr) = addr

-- | Mapping from addresses to node IDs
--
-- This is needed in order to assign stake to nodes.
type AddrDist = Map Addr NodeId

-- | Construct address to node ID mapping
mkAddrDist :: NumCoreNodes -> AddrDist
mkAddrDist numCoreNodes =
    Map.fromList $ zip [ fromString [addr] | addr <- ['a'..] ]
                       [ CoreId nid
                       | nid <- enumCoreNodes numCoreNodes
                       ]
