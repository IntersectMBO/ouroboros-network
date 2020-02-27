{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ouroboros.Consensus.Byron.Ledger.NetworkProtocolVersion (
    ByronNetworkProtocolVersion(..)
  ) where

import           Ouroboros.Network.NodeToClient (NodeToClientVersion (..))

import           Ouroboros.Consensus.Node.NetworkProtocolVersion

import           Ouroboros.Consensus.Byron.Ledger.Block

data ByronNetworkProtocolVersion =
    -- | We send headers without a size hint
    ByronNetworkProtocolVersion1

    -- | We send headers /with/ a size hint
  | ByronNetworkProtocolVersion2
  deriving (Show, Eq, Ord, Enum, Bounded)

instance HasNetworkProtocolVersion ByronBlock where
  type NetworkProtocolVersion ByronBlock = ByronNetworkProtocolVersion

  supportedNetworkProtocolVersions _ = [ByronNetworkProtocolVersion1]
  mostRecentNetworkProtocolVersion _ = ByronNetworkProtocolVersion1

  nodeToNodeProtocolVersion _ ByronNetworkProtocolVersion1 = NodeToNodeV_1
  nodeToNodeProtocolVersion _ ByronNetworkProtocolVersion2 = error "version 2 not yet supported"

  nodeToClientProtocolVersion _ ByronNetworkProtocolVersion1 = NodeToClientV_1
  nodeToClientProtocolVersion _ ByronNetworkProtocolVersion2 = error "version 2 not yet supported"
