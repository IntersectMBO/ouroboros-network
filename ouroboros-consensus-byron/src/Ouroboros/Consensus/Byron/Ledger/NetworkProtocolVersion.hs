{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Byron.Ledger.NetworkProtocolVersion (
    ByronNetworkProtocolVersion(..)
  ) where

import           Ouroboros.Network.NodeToClient (NodeToClientVersion (..))

import           Ouroboros.Consensus.Node.NetworkProtocolVersion

import           Ouroboros.Consensus.Byron.Ledger.Block

data ByronNetworkProtocolVersion =
    -- | We send headers without a size hint
    ByronNetworkProtocolVersion1

    -- | Use local query protocol
  | ByronNetworkProtocolVersion2

    -- | We send headers /with/ a size hint
  | ByronNetworkProtocolVersion3
  deriving (Show, Eq, Ord, Enum, Bounded)

instance HasNetworkProtocolVersion ByronBlock where
  type NetworkProtocolVersion ByronBlock = ByronNetworkProtocolVersion

  supportedNetworkProtocolVersions _ = [ ByronNetworkProtocolVersion1
                                       , ByronNetworkProtocolVersion2
                                       ]
  mostRecentNetworkProtocolVersion _ = ByronNetworkProtocolVersion2

  nodeToNodeProtocolVersion _ ByronNetworkProtocolVersion1 = NodeToNodeV_1
  nodeToNodeProtocolVersion _ ByronNetworkProtocolVersion2 = NodeToNodeV_1
  nodeToNodeProtocolVersion _ ByronNetworkProtocolVersion3 = error "version 2 not yet supported"

  nodeToClientProtocolVersion _ ByronNetworkProtocolVersion1 = NodeToClientV_1
  nodeToClientProtocolVersion _ ByronNetworkProtocolVersion2 = NodeToClientV_2
  nodeToClientProtocolVersion _ ByronNetworkProtocolVersion3 = error "version 2 not yet supported"
