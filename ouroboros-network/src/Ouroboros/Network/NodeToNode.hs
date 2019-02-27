
-- | This is the starting point for a module that will bring together the
-- overall node to node protocol, as a collection of mini-protocols.
--
module Ouroboros.Network.NodeToNode (
  NodeToNodeProtocols(..)
  ) where

import Ouroboros.Network.Mux.Types (ProtocolEnum(..))


-- | An index type used with the mux to enumerate all the mini-protocols that
-- make up the overall node-to-node protocol.
--
data NodeToNodeProtocols = ChainSyncWithHeaders
                         | BlockFetch
                         | TxSubmission
  deriving (Eq, Ord, Enum, Bounded, Show)

-- These protocol numbers end up in the wire format so it is vital that they
-- are stable, even as they are upgraded. So we use custom Enum instances here.
-- This allows us to retire old versions and add new, which may leave some
-- holes in the numbering space.

-- | These are the actual wire format protocol numbers.
--
-- The application specific protocol numbers start from 2 because of the two
-- mux built-in protocols.
--
-- These are chosen to not overlap with the node to client protocol numbers.
-- This is not essential for correctness, but is helpful to allow a single
-- shared implementation of tools that can analyse both protocols, e.g.
-- wireshark plugins.
--
instance ProtocolEnum NodeToNodeProtocols where

  fromProtocolEnum ChainSyncWithHeaders = 2
  fromProtocolEnum BlockFetch           = 3
  fromProtocolEnum TxSubmission         = 4

  toProtocolEnum 2 = Just ChainSyncWithHeaders
  toProtocolEnum 3 = Just BlockFetch
  toProtocolEnum 4 = Just TxSubmission
  toProtocolEnum _ = Nothing

