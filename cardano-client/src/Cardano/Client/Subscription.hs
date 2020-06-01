{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Client.Subscription (
  subscribe
  ) where

import           Data.Proxy
import           Data.Void (Void)
import qualified Data.ByteString.Lazy as BSL
import           Ouroboros.Consensus.Block (getCodecConfig)
import           Ouroboros.Consensus.Config (TopLevelConfig, configBlock)
import           Ouroboros.Consensus.Network.NodeToClient (clientCodecs, ClientCodecs)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion (
                    nodeToClientProtocolVersion , supportedNodeToClientVersions)
import           Ouroboros.Consensus.Node.Run (RunNode)
import           Ouroboros.Network.Mux (MuxMode (..), OuroborosApplication)
import           Ouroboros.Network.NodeToClient (ClientSubscriptionParams (..),
                    ConnectionId, LocalAddress,
                    NodeToClientProtocols (..),
                    NetworkClientSubcriptionTracers,
                    NodeToClientVersionData (..),
                    ncSubscriptionWorker,
                    newNetworkMutableState,
                    versionedNodeToClientProtocols)

import           Ouroboros.Consensus.Node.NetworkProtocolVersion (NodeToClientVersion)
import           Ouroboros.Network.Protocol.Handshake.Version (DictVersion, Versions, foldMapVersions)
import qualified Ouroboros.Network.Snocket as Snocket

import qualified Ouroboros.Network.NodeToClient (NodeToClientVersion)
import           Control.Monad.Class.MonadST (MonadST)
import           Prelude
import           Ouroboros.Consensus.Config.SupportsNode (getNetworkMagic)

subscribe ::
  ( RunNode blk , MonadST m )
  => Snocket.LocalSnocket
  -> TopLevelConfig blk
  -> NetworkClientSubcriptionTracers
  -> ClientSubscriptionParams ()
  -> (NodeToClientVersion blk
      -> ClientCodecs blk m
      -> ConnectionId LocalAddress
      -> NodeToClientProtocols 'InitiatorMode BSL.ByteString IO x y)
  -> IO Void
subscribe
  sn
  topLevelConfig
  tracers
  subscriptionParams
  protocols
  = do
    networkState <- newNetworkMutableState
    ncSubscriptionWorker
        sn
        tracers
        networkState
        subscriptionParams
        (versionedProtocols (Proxy :: Proxy blk) topLevelConfig protocols)

versionedProtocols ::
  (RunNode blk, MonadST m)
  => Proxy blk
  -> TopLevelConfig blk
  -> (NodeToClientVersion blk
      -> ClientCodecs blk m
      -> ConnectionId LocalAddress
      -> NodeToClientProtocols appType bytes IO a b)
  -> Versions
       Ouroboros.Network.NodeToClient.NodeToClientVersion
       DictVersion
       (OuroborosApplication appType LocalAddress bytes IO a b)
versionedProtocols blkProxy topLevelConfig p
  = foldMapVersions applyVersion $ supportedNodeToClientVersions blkProxy
  where
    blockConfig = configBlock topLevelConfig
    applyVersion v =
      versionedNodeToClientProtocols
        (nodeToClientProtocolVersion blkProxy v)
        (NodeToClientVersionData { networkMagic = getNetworkMagic blockConfig })
        (p v $ clientCodecs (getCodecConfig blockConfig) v)
