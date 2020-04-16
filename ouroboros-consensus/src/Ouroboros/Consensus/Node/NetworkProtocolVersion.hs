{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

module Ouroboros.Consensus.Node.NetworkProtocolVersion
  ( HasNetworkProtocolVersion(..)
  , SerialisationVersion(..)
  , SerialisationAcrossNetwork(..)
  , serialisedNodeToNode
  , serialisedNodeToClient
  , isNodeToNodeVersion
  , isNodeToClientVersion
  , castSerialisationVersion
  , castSerialisationAcrossNetwork
  ) where

import           Data.Proxy
import           Data.List.NonEmpty (NonEmpty (..))

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

data SerialisationVersion blk =
    -- | On-disk version
    --
    -- The on-disk version is decided by the consensus storage layer, and is
    -- independent from any network protocol versionining.
    SerialisedToDisk

    -- | Stuff sent across the network
  | SerialisedAcrossNetwork (SerialisationAcrossNetwork blk)

data SerialisationAcrossNetwork blk =
    -- | Serialised form we use for node-to-node communication
    --
    -- This is subject to network protocol versioning.
    SerialisedNodeToNode (NodeToNodeVersion blk)

    -- | Serialised form we use for node-to-client communication
    --
    -- This is subject to network protocol versioning.
  | SerialisedNodeToClient (NodeToClientVersion blk)

serialisedNodeToNode :: NodeToNodeVersion blk -> SerialisationVersion blk
serialisedNodeToNode = SerialisedAcrossNetwork . SerialisedNodeToNode

serialisedNodeToClient :: NodeToClientVersion blk -> SerialisationVersion blk
serialisedNodeToClient = SerialisedAcrossNetwork . SerialisedNodeToClient

isNodeToNodeVersion :: SerialisationVersion blk -> Bool
isNodeToNodeVersion = \case
    SerialisedToDisk                                   -> False
    SerialisedAcrossNetwork (SerialisedNodeToNode _)   -> True
    SerialisedAcrossNetwork (SerialisedNodeToClient _) -> False

isNodeToClientVersion :: SerialisationVersion blk -> Bool
isNodeToClientVersion = \case
    SerialisedToDisk                                   -> False
    SerialisedAcrossNetwork (SerialisedNodeToNode _)   -> False
    SerialisedAcrossNetwork (SerialisedNodeToClient _) -> True

deriving instance HasNetworkProtocolVersion blk => Show (SerialisationVersion       blk)
deriving instance HasNetworkProtocolVersion blk => Eq   (SerialisationVersion       blk)
deriving instance HasNetworkProtocolVersion blk => Show (SerialisationAcrossNetwork blk)
deriving instance HasNetworkProtocolVersion blk => Eq   (SerialisationAcrossNetwork blk)

castSerialisationVersion
  :: ( NodeToNodeVersion blk   ~ NodeToNodeVersion   blk'
     , NodeToClientVersion blk ~ NodeToClientVersion blk'
     )
  => SerialisationVersion blk
  -> SerialisationVersion blk'
castSerialisationVersion = \case
    SerialisedToDisk ->
      SerialisedToDisk
    SerialisedAcrossNetwork v ->
      SerialisedAcrossNetwork (castSerialisationAcrossNetwork v)

castSerialisationAcrossNetwork
  :: ( NodeToNodeVersion blk   ~ NodeToNodeVersion   blk'
     , NodeToClientVersion blk ~ NodeToClientVersion blk'
     )
  => SerialisationAcrossNetwork blk
  -> SerialisationAcrossNetwork blk'
castSerialisationAcrossNetwork = \case
    SerialisedNodeToNode   v -> SerialisedNodeToNode   v
    SerialisedNodeToClient v -> SerialisedNodeToClient v
