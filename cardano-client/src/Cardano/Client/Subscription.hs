{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Cardano.Client.Subscription (
    subscribe
  , MuxMode (..)
  , ClientCodecs
  , ConnectionId
  , LocalAddress
  , NodeToClientProtocols (..)
  , NodeToClientVersion
  , MuxPeer (..)
  , MuxTrace
  , RunMiniProtocol (..)
  , WithMuxBearer
  , cChainSyncCodec
  , cStateQueryCodec
  , cTxSubmissionCodec
  ) where

import           Control.Monad.Class.MonadST (MonadST)
import qualified Data.ByteString.Lazy as BSL
import           Data.Proxy
import           Data.Void (Void)

import           Network.Mux.Trace (MuxTrace, WithMuxBearer)

import           Ouroboros.Network.Mux (MuxMode (..), MuxPeer (..),
                     OuroborosApplication, RunMiniProtocol (..))
import           Ouroboros.Network.NodeToClient (ClientSubscriptionParams (..),
                     ConnectionId, LocalAddress,
                     NetworkClientSubcriptionTracers,
                     NodeToClientProtocols (..), NodeToClientVersionData (..),
                     ncSubscriptionWorker, newNetworkMutableState,
                     versionedNodeToClientProtocols)
import qualified Ouroboros.Network.NodeToClient (NodeToClientVersion)
import           Ouroboros.Network.Protocol.Handshake.Version (DictVersion,
                     Versions, foldMapVersions)
import qualified Ouroboros.Network.Snocket as Snocket

import           Ouroboros.Consensus.Block (getCodecConfig)
import           Ouroboros.Consensus.Config (TopLevelConfig, configBlock)
import           Ouroboros.Consensus.Config.SupportsNode (getNetworkMagic)
import           Ouroboros.Consensus.Network.NodeToClient (ClientCodecs,
                     cChainSyncCodec, cStateQueryCodec, cTxSubmissionCodec,
                     clientCodecs)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
                     (NodeToClientVersion, nodeToClientProtocolVersion,
                     supportedNodeToClientVersions)
import           Ouroboros.Consensus.Node.Run (RunNode)

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
