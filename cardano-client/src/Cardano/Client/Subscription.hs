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
  , BlockNodeToClientVersion
  , MuxPeer (..)
  , MuxTrace
  , RunMiniProtocol (..)
  , WithMuxBearer
  , RunOrStop (..)
  , cChainSyncCodec
  , cStateQueryCodec
  , cTxSubmissionCodec
  ) where

import           Control.Monad.Class.MonadST (MonadST)
import           Control.Monad.Class.MonadSTM
import qualified Data.ByteString.Lazy as BSL
import           Data.Proxy
import           Data.Void (Void)

import           Network.Mux.Trace (MuxTrace, WithMuxBearer)

import           Ouroboros.Network.Mux (MuxMode (..), MuxPeer (..),
                     OuroborosApplication, RunMiniProtocol (..),
                     RunOrStop (..))
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

import           Ouroboros.Consensus.Config (TopLevelConfig, configBlock,
                     configCodec)
import           Ouroboros.Consensus.Config.SupportsNode (getNetworkMagic)
import           Ouroboros.Consensus.Network.NodeToClient (ClientCodecs,
                     cChainSyncCodec, cStateQueryCodec, cTxSubmissionCodec,
                     clientCodecs)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
                     (BlockNodeToClientVersion, nodeToClientProtocolVersion,
                     supportedNodeToClientVersions)
import           Ouroboros.Consensus.Node.Run (RunNode)

subscribe ::
     RunNode blk
  => Snocket.LocalSnocket
  -> TopLevelConfig blk
  -> NetworkClientSubcriptionTracers
  -> ClientSubscriptionParams ()
  -> (BlockNodeToClientVersion blk
      -> ClientCodecs blk IO
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
        (versionedProtocols (Proxy :: Proxy blk) topLevelConfig
          (\version codecs connectionId _ -> protocols version codecs connectionId))


versionedProtocols ::
     ( MonadST m
     , RunNode blk
     )
  => Proxy blk
  -> TopLevelConfig blk
  -> (BlockNodeToClientVersion blk
      -> ClientCodecs blk m
      -> ConnectionId LocalAddress
      -> STM m RunOrStop
      -> NodeToClientProtocols appType bytes m a b)
  -- ^ callback which recieves codecs, connection id and STM action which can be
  -- checked if the networking runtime system requests the protocols to stop.
  --
  -- TODO: the `RunOrStop` might not be needed for `node-to-client`, hence it's
  -- not exposed in `subscribe`.  We should provide
  -- `OuroborosClientApplication`, which does not include it.
  -> Versions
       Ouroboros.Network.NodeToClient.NodeToClientVersion
       DictVersion
       (OuroborosApplication appType LocalAddress bytes m a b)
versionedProtocols blkProxy topLevelConfig p
  = foldMapVersions applyVersion $ supportedNodeToClientVersions blkProxy
  where
    blockConfig = configBlock topLevelConfig
    applyVersion v =
      versionedNodeToClientProtocols
        (nodeToClientProtocolVersion blkProxy v)
        (NodeToClientVersionData { networkMagic = getNetworkMagic blockConfig })
        (p v $ clientCodecs (configCodec topLevelConfig) v)
