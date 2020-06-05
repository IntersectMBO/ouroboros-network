{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}

module Ouroboros.Consensus.Node.NetworkProtocolVersion
  ( HasNetworkProtocolVersion(..)
  ) where

import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Proxy

import qualified Ouroboros.Network.NodeToClient as N
import qualified Ouroboros.Network.NodeToNode as N

{-------------------------------------------------------------------------------
  Protocol versioning
-------------------------------------------------------------------------------}

-- | Protocol versioning
class ( Show (NodeToNodeVersion   blk)
      , Show (NodeToClientVersion blk)
      , Eq   (NodeToNodeVersion   blk)
      , Eq   (NodeToClientVersion blk)
      ) => HasNetworkProtocolVersion blk where
  type NodeToNodeVersion   blk :: *
  type NodeToClientVersion blk :: *

  -- | Enumerate all supported node-to-node versions
  supportedNodeToNodeVersions
    :: Proxy blk -> NonEmpty (NodeToNodeVersion blk)

  -- | Enumerate all supported node-to-client versions
  supportedNodeToClientVersions
    :: Proxy blk -> NonEmpty (NodeToClientVersion blk)

  -- | The most recent node-to-node version
  mostRecentNodeToNodeVersion
    :: Proxy blk -> NodeToNodeVersion blk

  -- | The most recent node-to-client version
  mostRecentNodeToClientVersion
    :: Proxy blk -> NodeToClientVersion blk

  -- | Translate to network-layer type
  nodeToNodeProtocolVersion
    :: Proxy blk -> NodeToNodeVersion blk -> N.NodeToNodeVersion

  -- | Translate to network-layer type
  nodeToClientProtocolVersion
    :: Proxy blk -> NodeToClientVersion blk -> N.NodeToClientVersion

  -- Defaults

  type NodeToNodeVersion   blk = ()
  type NodeToClientVersion blk = ()

  default supportedNodeToNodeVersions
    :: NodeToNodeVersion blk ~ ()
    => Proxy blk -> NonEmpty (NodeToNodeVersion blk)
  supportedNodeToNodeVersions _ = () :| []

  default supportedNodeToClientVersions
    :: NodeToClientVersion blk ~ ()
    => Proxy blk -> NonEmpty (NodeToClientVersion blk)
  supportedNodeToClientVersions _ = () :| []

  default mostRecentNodeToNodeVersion
    :: NodeToNodeVersion blk ~ ()
    => Proxy blk -> NodeToNodeVersion blk
  mostRecentNodeToNodeVersion _ = ()

  default mostRecentNodeToClientVersion
    :: NodeToClientVersion blk ~ ()
    => Proxy blk -> NodeToClientVersion blk
  mostRecentNodeToClientVersion _ = ()

  default nodeToNodeProtocolVersion
    :: NodeToNodeVersion blk ~ ()
    => Proxy blk -> NodeToNodeVersion blk -> N.NodeToNodeVersion
  nodeToNodeProtocolVersion _ () = N.NodeToNodeV_1

  default nodeToClientProtocolVersion
    :: NodeToClientVersion blk ~ ()
    => Proxy blk -> NodeToClientVersion blk -> N.NodeToClientVersion
  nodeToClientProtocolVersion _ () = N.NodeToClientV_1
