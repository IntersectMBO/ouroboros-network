{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Implement ChainSync and BlockFetch servers on top of just the immutable DB.
module Cardano.Tools.ImmDBServer.MiniProtocols (immDBServer) where

import           Control.Monad (forever)
import           Control.Tracer
import qualified Data.ByteString.Lazy as BL
import           Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import           Data.Void (Void)

import           Ouroboros.Network.Block (Serialised (..), Tip (..))
import           Ouroboros.Network.Driver (runPeer)
import           Ouroboros.Network.KeepAlive (keepAliveServer)
import           Ouroboros.Network.Magic (NetworkMagic)
import           Ouroboros.Network.Mux (MiniProtocol (..), MuxMode (..),
                     MuxPeer (..), OuroborosApplication (..),
                     RunMiniProtocol (..))
import           Ouroboros.Network.NodeToNode (NodeToNodeVersionData (..),
                     Versions (..))
import qualified Ouroboros.Network.NodeToNode as N2N
import           Ouroboros.Network.Protocol.BlockFetch.Server
import           Ouroboros.Network.Protocol.BlockFetch.Type (ChainRange (..))
import           Ouroboros.Network.Protocol.ChainSync.Server
import           Ouroboros.Network.Protocol.Handshake.Version (Version (..))
import           Ouroboros.Network.Protocol.KeepAlive.Server
                     (keepAliveServerPeer)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Network.NodeToNode
                     (Codecs (cBlockFetchCodecSerialised))
import qualified Ouroboros.Consensus.Network.NodeToNode as Consensus.N2N
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run (SerialiseNodeToNodeConstraints)
import           Ouroboros.Consensus.Storage.ChainDB.API (getSerialisedHeader)
import           Ouroboros.Consensus.Storage.Common
import           Ouroboros.Consensus.Storage.ImmutableDB.API (ImmutableDB,
                     Iterator)
import qualified Ouroboros.Consensus.Storage.ImmutableDB.API as ImmutableDB
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry

immDBServer ::
     forall m blk addr.
     ( IOLike m
     , HasHeader blk
     , ShowProxy blk
     , SerialiseNodeToNodeConstraints blk
     , SupportedNetworkProtocolVersion blk
     )
  => CodecConfig blk
  -> ImmutableDB m blk
  -> NetworkMagic
  -> Versions NodeToNodeVersion NodeToNodeVersionData
       (OuroborosApplication 'ResponderMode addr BL.ByteString m Void ())
immDBServer codecCfg immDB networkMagic = do
    forAllVersions application
  where
    forAllVersions ::
         (NodeToNodeVersion -> BlockNodeToNodeVersion blk -> r)
      -> Versions NodeToNodeVersion NodeToNodeVersionData r
    forAllVersions mkR =
          Versions
        $ Map.mapWithKey mkVersion
        $ supportedNodeToNodeVersions (Proxy @blk)
      where
        mkVersion version blockVersion = Version {..}
          where
            versionApplication _ = mkR version blockVersion
            versionData = NodeToNodeVersionData {..}
              where
                diffusionMode = N2N.InitiatorOnlyDiffusionMode

    application ::
         NodeToNodeVersion
      -> BlockNodeToNodeVersion blk
      -> OuroborosApplication 'ResponderMode addr BL.ByteString m Void ()
    application version blockVersion =
        OuroborosApplication \_connId _controlMessageSTM -> miniprotocols
      where
        miniprotocols =
            [ mkMiniProtocol
                N2N.keepAliveMiniProtocolNum
                N2N.keepAliveProtocolLimits
                keepAliveProt
            , mkMiniProtocol
                N2N.chainSyncMiniProtocolNum
                N2N.chainSyncProtocolLimits
                chainSyncProt
            , mkMiniProtocol
                N2N.blockFetchMiniProtocolNum
                N2N.blockFetchProtocolLimits
                blockFetchProt
            , mkMiniProtocol
                N2N.txSubmissionMiniProtocolNum
                N2N.txSubmissionProtocolLimits
                txSubmissionProt
            ]
          where
            Consensus.N2N.Codecs {..} =
              Consensus.N2N.defaultCodecs codecCfg blockVersion version

            keepAliveProt  =
                MuxPeer nullTracer cKeepAliveCodec
              $ keepAliveServerPeer keepAliveServer
            chainSyncProt  =
                MuxPeerRaw \channel ->
                withRegistry
              $ runPeer nullTracer cChainSyncCodecSerialised channel
              . chainSyncServerPeer
              . chainSyncServer immDB getSerialisedHeader
            blockFetchProt =
                MuxPeerRaw \channel ->
                withRegistry
              $ runPeer nullTracer cBlockFetchCodecSerialised channel
              . blockFetchServerPeer
              . blockFetchServer immDB (Serialised <$> GetRawBlock)
            txSubmissionProt =
                MuxPeerRaw \_channel -> forever $ threadDelay 10


        mkMiniProtocol miniProtocolNum limits proto = MiniProtocol {..}
          where
            miniProtocolLimits = limits N2N.defaultMiniProtocolParameters
            miniProtocolRun    = ResponderProtocolOnly proto

chainSyncServer ::
     forall m blk a. (IOLike m, HasHeader blk)
  => ImmutableDB m blk
  -> BlockComponent blk a
  -> ResourceRegistry m
  -> ChainSyncServer a (Point blk) (Tip blk) m ()
chainSyncServer immDB blockComponent registry = ChainSyncServer $ do
    iterator <- ImmutableDB.streamAll immDB registry blockComponent
    runChainSyncServer $ idle iterator Nothing
  where
    idle ::
         Iterator m blk a
      -> Maybe (Point blk) -- just-negotiated intersection, if any
      -> ChainSyncServer a (Point blk) (Tip blk) m ()
    idle iterator mIntersectionPt =
        ChainSyncServer $ pure ServerStIdle {
            recvMsgRequestNext   = handleRequestNext'
          , recvMsgFindIntersect = handleFindIntersect iterator
          , recvMsgDoneClient    = pure ()
          }
      where
        handleRequestNext' = case mIntersectionPt of
          Just pt -> do
            tip <- getImmutableTip
            pure $ Left $ SendMsgRollBackward pt tip $ idle iterator Nothing
          Nothing -> handleRequestNext iterator

    handleRequestNext ::
         Iterator m blk a
      -> m (Either (ServerStNext a (Point blk) (Tip blk) m ())
                (m (ServerStNext a (Point blk) (Tip blk) m ())))
    handleRequestNext iterator =
       ImmutableDB.iteratorNext iterator >>= \case
         ImmutableDB.IteratorExhausted ->
           throwIO ReachedImmutableTip
         ImmutableDB.IteratorResult a  -> Left <$> do
           tip <- getImmutableTip
           pure $ SendMsgRollForward a tip $ idle iterator Nothing

    handleFindIntersect ::
         Iterator m blk a
      -> [Point blk]
      -> m (ServerStIntersect a (Point blk) (Tip blk) m ())
    handleFindIntersect iterator = go
      where
        go (pt : pts) =
          ImmutableDB.streamAfterPoint immDB registry blockComponent pt >>= \case
            Left _          -> go pts
            Right iterator' -> do
              ImmutableDB.iteratorClose iterator
              tip <- getImmutableTip
              pure $ SendMsgIntersectFound pt tip $ idle iterator' (Just pt)
        go [] = do
          tip <- getImmutableTip
          pure $ SendMsgIntersectNotFound tip $ idle iterator Nothing

    getImmutableTip :: m (Tip blk)
    getImmutableTip = atomically (ImmutableDB.getTip immDB) <&> \case
        Origin                         -> TipGenesis
        NotOrigin ImmutableDB.Tip {..} -> Tip tipSlotNo tipHash tipBlockNo

blockFetchServer ::
     forall m blk a. (IOLike m)
  => ImmutableDB m blk
  -> BlockComponent blk a
  -> ResourceRegistry m
  -> BlockFetchServer a (Point blk) m ()
blockFetchServer immDB blockComponent registry = idle
  where
    idle :: BlockFetchServer a (Point blk) m ()
    idle = flip BlockFetchServer () \(ChainRange from to) -> do
        streamTo   <- StreamFromInclusive <$> toRealPoint from
        streamFrom <- StreamToInclusive <$> toRealPoint to
        ImmutableDB.stream immDB registry blockComponent streamTo streamFrom <&> \case
          Left _         -> SendMsgNoBlocks $ pure idle
          Right iterator -> SendMsgStartBatch $ sendBlocks iterator
      where
        toRealPoint :: Point blk -> m (RealPoint blk)
        toRealPoint pt = case pointToWithOriginRealPoint pt of
          Origin           -> throwIO TriedToFetchGenesis
          NotOrigin realPt -> pure realPt

    sendBlocks :: Iterator m blk a -> m (BlockFetchSendBlocks a (Point blk) m ())
    sendBlocks iterator = ImmutableDB.iteratorNext iterator <&> \case
        ImmutableDB.IteratorExhausted -> SendMsgBatchDone (pure idle)
        ImmutableDB.IteratorResult a  -> SendMsgBlock a $ sendBlocks iterator

data ImmDBServerException =
    ReachedImmutableTip
  | TriedToFetchGenesis
  deriving stock (Show)
  deriving anyclass (Exception)
