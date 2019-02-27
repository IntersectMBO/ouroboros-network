
-- | This is the starting point for a module that will bring together the
-- overall node to client protocol, as a collection of mini-protocols.
--
module Ouroboros.Network.NodeToClient (
  NodeToClientProtocols(..)
  ) where

import Ouroboros.Network.Mux.Types (ProtocolEnum(..))


-- | An index type used with the mux to enumerate all the mini-protocols that
-- make up the overall node-to-client protocol.
--
data NodeToClientProtocols = ChainSyncWithBlocks
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | These are the actual wire format protocol numbers.
--
-- These are chosen to not overlap with the node to node protocol numbers.
-- This is not essential for correctness, but is helpful to allow a single
-- shared implementation of tools that can analyse both protocols, e.g.
-- wireshark plugins.
--
instance ProtocolEnum NodeToClientProtocols where

  fromProtocolEnum ChainSyncWithBlocks = 5

  toProtocolEnum 5 = Just ChainSyncWithBlocks
  toProtocolEnum _ = Nothing

