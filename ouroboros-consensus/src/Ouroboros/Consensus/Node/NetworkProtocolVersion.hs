{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies      #-}

module Ouroboros.Consensus.Node.NetworkProtocolVersion
  ( HasNetworkProtocolVersion(..)
  , SerialisationVersion(..)
    -- * Convenience re-exports
  , NodeToClientVersion(..)
  , NodeToNodeVersion(..)
  ) where

import           Data.Proxy

import           Ouroboros.Network.NodeToClient (NodeToClientVersion (..))
import           Ouroboros.Network.NodeToNode (NodeToNodeVersion (..))

{-------------------------------------------------------------------------------
  Protocol versioning
-------------------------------------------------------------------------------}

-- | Protocol versioning
--
-- The network layer supports protocol versioning. Here we define a single
-- top-level "node protocol version" that must map to the network layer's
-- versioning.
class HasNetworkProtocolVersion blk where
  type NetworkProtocolVersion blk :: *

  -- | Enumerate all supported versions
  supportedNetworkProtocolVersions
    :: Proxy blk -> [NetworkProtocolVersion blk]

  -- | The most recent version
  mostRecentNetworkProtocolVersion
    :: Proxy blk -> NetworkProtocolVersion blk

  -- | The node-to-node version corresponding to this top-level protocol version
  nodeToNodeProtocolVersion
    :: Proxy blk -> NetworkProtocolVersion blk -> NodeToNodeVersion

  -- | The node-to-node version corresponding to this top-level protocol version
  nodeToClientProtocolVersion
    :: Proxy blk -> NetworkProtocolVersion blk -> NodeToClientVersion

  -- Defaults

  type NetworkProtocolVersion blk = ()

  default supportedNetworkProtocolVersions
    :: NetworkProtocolVersion blk ~ ()
    => Proxy blk -> [NetworkProtocolVersion blk]
  supportedNetworkProtocolVersions _ = [()]

  default mostRecentNetworkProtocolVersion
    :: NetworkProtocolVersion blk ~ ()
    => Proxy blk -> NetworkProtocolVersion blk
  mostRecentNetworkProtocolVersion _ = ()

  default nodeToNodeProtocolVersion
    :: NetworkProtocolVersion blk ~ ()
    => Proxy blk -> NetworkProtocolVersion blk -> NodeToNodeVersion
  nodeToNodeProtocolVersion _ () = NodeToNodeV_1

  default nodeToClientProtocolVersion
    :: NetworkProtocolVersion blk ~ ()
    => Proxy blk -> NetworkProtocolVersion blk -> NodeToClientVersion
  nodeToClientProtocolVersion _ () = NodeToClientV_1

data SerialisationVersion a =
    -- | On-disk version
    --
    -- The on-disk version is decided by the consensus storage layer, and is
    -- independent from any network protocol versionining.
    SerialisedToDisk

    -- | Serialized form we send across the network
    --
    -- This is subject to network protocol versioning.
  | SentAcrossNetwork a
  deriving (Show, Eq)
