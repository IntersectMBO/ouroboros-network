{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}

module Ouroboros.Consensus.Node.NetworkProtocolVersion
  ( HasNetworkProtocolVersion(..)
  , TranslateNetworkProtocolVersion(..)
    -- * Re-exports
  , NodeToNodeVersion(..)
  , NodeToClientVersion(..)
  ) where

import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Proxy

import           Ouroboros.Network.NodeToClient
import           Ouroboros.Network.NodeToNode

{-------------------------------------------------------------------------------
  Protocol versioning
-------------------------------------------------------------------------------}

-- | Protocol versioning
class ( Show (BlockNodeToNodeVersion   blk)
      , Show (BlockNodeToClientVersion blk)
      , Eq   (BlockNodeToNodeVersion   blk)
      , Eq   (BlockNodeToClientVersion blk)
      ) => HasNetworkProtocolVersion blk where
  type BlockNodeToNodeVersion   blk :: *
  type BlockNodeToClientVersion blk :: *

  -- Defaults

  type BlockNodeToNodeVersion   blk = ()
  type BlockNodeToClientVersion blk = ()

class HasNetworkProtocolVersion blk => TranslateNetworkProtocolVersion blk where
  -- | Enumerate all supported node-to-node versions
  supportedNodeToNodeVersions
    :: Proxy blk -> NonEmpty (BlockNodeToNodeVersion blk)

  -- | Enumerate all supported node-to-client versions
  supportedNodeToClientVersions
    :: Proxy blk -> NonEmpty (BlockNodeToClientVersion blk)

  -- | The most recent support node-to-node version
  --
  -- This is only used in the tests, where we are running the protocol with
  -- the version we expect to be ran in practice.
  mostRecentSupportedNodeToNode
    :: Proxy blk -> BlockNodeToNodeVersion blk

  -- | The most recent supported node-to-client version
  --
  -- This is only used in the tests, where we are running the protocol with
  -- the version we expect to be ran in practice.
  mostRecentSupportedNodeToClient
    :: Proxy blk -> BlockNodeToClientVersion blk

  -- | Translate to network-layer type
  nodeToNodeProtocolVersion
    :: Proxy blk -> BlockNodeToNodeVersion blk -> NodeToNodeVersion

  -- | Translate to network-layer type
  nodeToClientProtocolVersion
    :: Proxy blk -> BlockNodeToClientVersion blk -> NodeToClientVersion

  -- Defaults

  default supportedNodeToNodeVersions
    :: BlockNodeToNodeVersion blk ~ ()
    => Proxy blk -> NonEmpty (BlockNodeToNodeVersion blk)
  supportedNodeToNodeVersions _ = () :| []

  default supportedNodeToClientVersions
    :: BlockNodeToClientVersion blk ~ ()
    => Proxy blk -> NonEmpty (BlockNodeToClientVersion blk)
  supportedNodeToClientVersions _ = () :| []

  default mostRecentSupportedNodeToNode
    :: BlockNodeToNodeVersion blk ~ ()
    => Proxy blk -> BlockNodeToNodeVersion blk
  mostRecentSupportedNodeToNode _ = ()

  default mostRecentSupportedNodeToClient
    :: BlockNodeToClientVersion blk ~ ()
    => Proxy blk -> BlockNodeToClientVersion blk
  mostRecentSupportedNodeToClient _ = ()
