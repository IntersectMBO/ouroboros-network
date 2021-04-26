{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

-- | Intended for qualified import
module Ouroboros.Consensus.Network.NodeToNode (
    -- * Handlers
    Handlers (..)
  , mkHandlers
    -- * Codecs
  , Codecs (..)
  , defaultCodecs
  , identityCodecs
    -- * Tracers
  , Tracers
  , Tracers' (..)
  , nullTracers
  , showTracers
    -- * Applications
  , Apps (..)
  , ClientApp
  , ServerApp
  , mkApps
    -- ** Projections
  , initiator
  , responder
    -- * Re-exports
  , ChainSyncTimeout (..)
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Control.Monad (forever)
import           Control.Monad.Class.MonadTime (MonadTime)
import           Control.Monad.Class.MonadTimer (MonadTimer)
import           Control.Tracer
import           Data.ByteString.Lazy (ByteString)
import           Data.Map.Strict (Map)
import           Data.Void (Void)

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import           Ouroboros.Network.Block (Serialised (..), decodePoint,
                     decodeTip, encodePoint, encodeTip)
import           Ouroboros.Network.BlockFetch
import           Ouroboros.Network.BlockFetch.Client (BlockFetchClient,
                     blockFetchClient)
import           Ouroboros.Network.Channel
import           Ouroboros.Network.Codec
import           Ouroboros.Network.DeltaQ
import           Ouroboros.Network.Driver
import           Ouroboros.Network.KeepAlive
import           Ouroboros.Network.Mux
import           Ouroboros.Network.NodeToNode
import           Ouroboros.Network.Protocol.BlockFetch.Codec
import           Ouroboros.Network.Protocol.BlockFetch.Server (BlockFetchServer,
                     blockFetchServerPeer)
import           Ouroboros.Network.Protocol.BlockFetch.Type (BlockFetch (..))
import           Ouroboros.Network.Protocol.ChainSync.ClientPipelined
import           Ouroboros.Network.Protocol.ChainSync.Codec
import           Ouroboros.Network.Protocol.ChainSync.PipelineDecision
import           Ouroboros.Network.Protocol.ChainSync.Server
import           Ouroboros.Network.Protocol.ChainSync.Type
import           Ouroboros.Network.Protocol.KeepAlive.Client
import           Ouroboros.Network.Protocol.KeepAlive.Codec
import           Ouroboros.Network.Protocol.KeepAlive.Server
import           Ouroboros.Network.Protocol.KeepAlive.Type
import qualified Ouroboros.Network.Protocol.Trans.Hello.Util as Hello
import           Ouroboros.Network.Protocol.TxSubmission.Client
import           Ouroboros.Network.Protocol.TxSubmission.Codec
import           Ouroboros.Network.Protocol.TxSubmission.Server
import           Ouroboros.Network.Protocol.TxSubmission.Type
import           Ouroboros.Network.Protocol.TxSubmission2.Codec
import           Ouroboros.Network.Protocol.TxSubmission2.Type
import           Ouroboros.Network.TxSubmission.Inbound
import           Ouroboros.Network.TxSubmission.Outbound

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.MiniProtocol.BlockFetch.Server
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Server
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Node.Serialisation
import qualified Ouroboros.Consensus.Node.Tracers as Node
import           Ouroboros.Consensus.NodeKernel
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import           Ouroboros.Consensus.Util (ShowProxy)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Ouroboros.Consensus.Storage.Serialisation (SerialisedHeader)

{-------------------------------------------------------------------------------
  Handlers
-------------------------------------------------------------------------------}

-- | Protocol handlers for node-to-node (remote) communication
data Handlers m peer blk = Handlers {
      hChainSyncClient
        :: peer
        -> NodeToNodeVersion
        -> ControlMessageSTM m
        -> StrictTVar m (AnchoredFragment (Header blk))
        -> ChainSyncClientPipelined (Header blk) (Point blk) (Tip blk) m ChainSyncClientResult
        -- TODO: we should consider either bundling these context parameters
        -- into a record, or extending the protocol handler representation
        -- to support bracket-style initialisation so that we could have the
        -- closure include these and not need to be explicit about them here.

    , hChainSyncServer
        :: NodeToNodeVersion
        -> ChainDB.Follower m blk (ChainDB.WithPoint blk (SerialisedHeader blk))
        -> ChainSyncServer (SerialisedHeader blk) (Point blk) (Tip blk) m ()

    -- TODO block fetch client does not have GADT view of the handlers.
    , hBlockFetchClient
        :: NodeToNodeVersion
        -> ControlMessageSTM m
        -> BlockFetchClient (Header blk) blk m ()

    , hBlockFetchServer
        :: NodeToNodeVersion
        -> ResourceRegistry m
        -> BlockFetchServer (Serialised blk) (Point blk) m ()

    , hTxSubmissionClient
        :: NodeToNodeVersion
        -> ControlMessageSTM m
        -> peer
        -> TxSubmissionClient (GenTxId blk) (GenTx blk) m ()

    , hTxSubmissionServer
        :: NodeToNodeVersion
        -> peer
        -> TxSubmissionServerPipelined (GenTxId blk) (GenTx blk) m ()

    , hKeepAliveClient
        :: NodeToNodeVersion
        -> ControlMessageSTM m
        -> peer
        -> StrictTVar m (Map peer PeerGSV)
        -> KeepAliveInterval
        -> KeepAliveClient m ()

    , hKeepAliveServer
        :: NodeToNodeVersion
        -> peer
        -> KeepAliveServer m ()
    }

mkHandlers
  :: forall m blk remotePeer localPeer.
     ( IOLike m
     , MonadTime m
     , MonadTimer m
     , LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     , LedgerSupportsProtocol blk
     , Ord remotePeer
     )
  => NodeKernelArgs m remotePeer localPeer blk
  -> NodeKernel     m remotePeer localPeer blk
  -> Handlers       m remotePeer           blk
mkHandlers
      NodeKernelArgs {keepAliveRng, miniProtocolParameters}
      NodeKernel {getChainDB, getMempool, getTopLevelConfig, getTracers = tracers} =
    Handlers {
        hChainSyncClient = \peer ->
          chainSyncClient
            (pipelineDecisionLowHighMark
              (chainSyncPipeliningLowMark  miniProtocolParameters)
              (chainSyncPipeliningHighMark miniProtocolParameters))
            (contramap (TraceLabelPeer peer) (Node.chainSyncClientTracer tracers))
            getTopLevelConfig
            (defaultChainDbView getChainDB)
      , hChainSyncServer = \_version ->
          chainSyncHeadersServer
            (Node.chainSyncServerHeaderTracer tracers)
            getChainDB
      , hBlockFetchClient =
          blockFetchClient
      , hBlockFetchServer = \version ->
          blockFetchServer
            (Node.blockFetchServerTracer tracers)
            getChainDB
            version
      , hTxSubmissionClient = \version controlMessageSTM peer ->
          txSubmissionOutbound
            (contramap (TraceLabelPeer peer) (Node.txOutboundTracer tracers))
            (txSubmissionMaxUnacked miniProtocolParameters)
            (getMempoolReader getMempool)
            version
            controlMessageSTM
      , hTxSubmissionServer = \version peer ->
          txSubmissionInbound
            (contramap (TraceLabelPeer peer) (Node.txInboundTracer tracers))
            (txSubmissionMaxUnacked miniProtocolParameters)
            (getMempoolReader getMempool)
            (getMempoolWriter getMempool)
            version
      , hKeepAliveClient = \_version -> keepAliveClient (Node.keepAliveClientTracer tracers) keepAliveRng
      , hKeepAliveServer = \_version _peer -> keepAliveServer
      }

{-------------------------------------------------------------------------------
  Codecs
-------------------------------------------------------------------------------}

-- | Node-to-node protocol codecs needed to run 'Handlers'.
data Codecs blk e m bCS bSCS bBF bSBF bTX bTX2 bKA = Codecs {
      cChainSyncCodec            :: Codec (ChainSync (Header blk) (Point blk) (Tip blk))           e m bCS
    , cChainSyncCodecSerialised  :: Codec (ChainSync (SerialisedHeader blk) (Point blk) (Tip blk)) e m bSCS
    , cBlockFetchCodec           :: Codec (BlockFetch blk (Point blk))                             e m bBF
    , cBlockFetchCodecSerialised :: Codec (BlockFetch (Serialised blk) (Point blk))                e m bSBF
    , cTxSubmissionCodec         :: Codec (TxSubmission (GenTxId blk) (GenTx blk))                 e m bTX
    , cTxSubmission2Codec        :: Codec (TxSubmission2 (GenTxId blk) (GenTx blk))                e m bTX2
    , cKeepAliveCodec            :: Codec KeepAlive                                                e m bKA
    }

-- | Protocol codecs for the node-to-node protocols
defaultCodecs :: forall m blk. (IOLike m, SerialiseNodeToNodeConstraints blk,
                                ShowProxy (GenTxId blk), ShowProxy (GenTx blk))
              => CodecConfig       blk
              -> BlockNodeToNodeVersion blk
              -> NodeToNodeVersion
              -> Codecs blk DeserialiseFailure m
                   ByteString ByteString ByteString ByteString ByteString ByteString ByteString
defaultCodecs ccfg version nodeToNodeVersion = Codecs {
      cChainSyncCodec =
        codecChainSync
          enc
          dec
          (encodePoint (encodeRawHash p))
          (decodePoint (decodeRawHash p))
          (encodeTip   (encodeRawHash p))
          (decodeTip   (decodeRawHash p))

    , cChainSyncCodecSerialised =
        codecChainSync
          enc
          dec
          (encodePoint (encodeRawHash p))
          (decodePoint (decodeRawHash p))
          (encodeTip   (encodeRawHash p))
          (decodeTip   (decodeRawHash p))

    , cBlockFetchCodec =
        codecBlockFetch
          enc
          dec
          (encodePoint (encodeRawHash p))
          (decodePoint (decodeRawHash p))

    , cBlockFetchCodecSerialised =
        codecBlockFetch
          enc
          dec
          (encodePoint (encodeRawHash p))
          (decodePoint (decodeRawHash p))

    , cTxSubmissionCodec =
        codecTxSubmission
          enc
          dec
          enc
          dec

    , cTxSubmission2Codec =
        codecTxSubmission2
          enc
          dec
          enc
          dec

    , cKeepAliveCodec =
        if nodeToNodeVersion <= NodeToNodeV_6
          then codecKeepAlive
          else codecKeepAlive_v2
    }
  where
    p :: Proxy blk
    p = Proxy

    enc :: SerialiseNodeToNode blk a => a -> Encoding
    enc = encodeNodeToNode ccfg version

    dec :: SerialiseNodeToNode blk a => forall s. Decoder s a
    dec = decodeNodeToNode ccfg version

-- | Identity codecs used in tests.
identityCodecs :: Monad m
               => Codecs blk CodecFailure m
                    (AnyMessage (ChainSync (Header blk) (Point blk) (Tip blk)))
                    (AnyMessage (ChainSync (SerialisedHeader blk) (Point blk) (Tip blk)))
                    (AnyMessage (BlockFetch blk (Point blk)))
                    (AnyMessage (BlockFetch (Serialised blk) (Point blk)))
                    (AnyMessage (TxSubmission (GenTxId blk) (GenTx blk)))
                    (AnyMessage (TxSubmission2 (GenTxId blk) (GenTx blk)))
                    (AnyMessage KeepAlive)
identityCodecs = Codecs {
      cChainSyncCodec            = codecChainSyncId
    , cChainSyncCodecSerialised  = codecChainSyncId
    , cBlockFetchCodec           = codecBlockFetchId
    , cBlockFetchCodecSerialised = codecBlockFetchId
    , cTxSubmissionCodec         = codecTxSubmissionId
    , cTxSubmission2Codec        = codecTxSubmission2Id
    , cKeepAliveCodec            = codecKeepAliveId
    }

{-------------------------------------------------------------------------------
  Tracers
-------------------------------------------------------------------------------}

-- | A record of 'Tracer's for the different protocols.
type Tracers m peer blk e =
     Tracers'  peer blk e (Tracer m)

data Tracers' peer blk e f = Tracers {
      tChainSyncTracer            :: f (TraceLabelPeer peer (TraceSendRecv (ChainSync (Header blk) (Point blk) (Tip blk))))
    , tChainSyncSerialisedTracer  :: f (TraceLabelPeer peer (TraceSendRecv (ChainSync (SerialisedHeader blk) (Point blk) (Tip blk))))
    , tBlockFetchTracer           :: f (TraceLabelPeer peer (TraceSendRecv (BlockFetch blk (Point blk))))
    , tBlockFetchSerialisedTracer :: f (TraceLabelPeer peer (TraceSendRecv (BlockFetch (Serialised blk) (Point blk))))
    , tTxSubmissionTracer         :: f (TraceLabelPeer peer (TraceSendRecv (TxSubmission (GenTxId blk) (GenTx blk))))
    , tTxSubmission2Tracer        :: f (TraceLabelPeer peer (TraceSendRecv (TxSubmission2 (GenTxId blk) (GenTx blk))))
    }

instance (forall a. Semigroup (f a)) => Semigroup (Tracers' peer blk e f) where
  l <> r = Tracers {
        tChainSyncTracer            = f tChainSyncTracer
      , tChainSyncSerialisedTracer  = f tChainSyncSerialisedTracer
      , tBlockFetchTracer           = f tBlockFetchTracer
      , tBlockFetchSerialisedTracer = f tBlockFetchSerialisedTracer
      , tTxSubmissionTracer         = f tTxSubmissionTracer
      , tTxSubmission2Tracer        = f tTxSubmission2Tracer
      }
    where
      f :: forall a. Semigroup a
        => (Tracers' peer blk e f -> a)
        -> a
      f prj = prj l <> prj r

-- | Use a 'nullTracer' for each protocol.
nullTracers :: Monad m => Tracers m peer blk e
nullTracers = Tracers {
      tChainSyncTracer            = nullTracer
    , tChainSyncSerialisedTracer  = nullTracer
    , tBlockFetchTracer           = nullTracer
    , tBlockFetchSerialisedTracer = nullTracer
    , tTxSubmissionTracer         = nullTracer
    , tTxSubmission2Tracer        = nullTracer
    }

showTracers :: ( Show blk
               , Show peer
               , Show (Header blk)
               , Show (GenTx blk)
               , Show (GenTxId blk)
               , HasHeader blk
               , HasNestedContent Header blk
               )
            => Tracer m String -> Tracers m peer blk e
showTracers tr = Tracers {
      tChainSyncTracer            = showTracing tr
    , tChainSyncSerialisedTracer  = showTracing tr
    , tBlockFetchTracer           = showTracing tr
    , tBlockFetchSerialisedTracer = showTracing tr
    , tTxSubmissionTracer         = showTracing tr
    , tTxSubmission2Tracer        = showTracing tr
    }

{-------------------------------------------------------------------------------
  Applications
-------------------------------------------------------------------------------}

-- | A node-to-node application
type ClientApp m peer bytes a =
     NodeToNodeVersion
  -> ControlMessageSTM m
  -> peer
  -> Channel m bytes
  -> m (a, Maybe bytes)

type ServerApp m peer bytes a =
     NodeToNodeVersion
  -> peer
  -> Channel m bytes
  -> m (a, Maybe bytes)

-- | Applications for the node-to-node protocols
--
-- See 'Network.Mux.Types.MuxApplication'
data Apps m peer bCS bBF bTX bTX2 bKA a = Apps {
      -- | Start a chain sync client that communicates with the given upstream
      -- node.
      aChainSyncClient     :: ClientApp m peer bCS a

      -- | Start a chain sync server.
    , aChainSyncServer     :: ServerApp m peer bCS a

      -- | Start a block fetch client that communicates with the given
      -- upstream node.
    , aBlockFetchClient    :: ClientApp m peer bBF a

      -- | Start a block fetch server.
    , aBlockFetchServer    :: ServerApp m peer bBF a

      -- | Start a transaction submission client that communicates with the
      -- given upstream node.
    , aTxSubmissionClient  :: ClientApp m peer bTX a

      -- | Start a transaction submission server.
    , aTxSubmissionServer  :: ServerApp m peer bTX a

      -- | Start a transaction submission v2 client that communicates with the
      -- given upstream node.
    , aTxSubmission2Client :: ClientApp m peer bTX2 a

      -- | Start a transaction submission v2 server.
    , aTxSubmission2Server :: ServerApp m peer bTX2 a

      -- | Start a keep-alive client.
    , aKeepAliveClient     :: ClientApp m peer bKA a

      -- | Start a keep-alive server.
    , aKeepAliveServer     :: ServerApp m peer bKA a
    }

-- | Construct the 'NetworkApplication' for the node-to-node protocols
mkApps
  :: forall m remotePeer localPeer blk e bCS bBF bTX bTX2 bKA.
     ( IOLike m
     , MonadTimer m
     , Ord remotePeer
     , Exception e
     , LedgerSupportsProtocol blk
     , ShowProxy blk
     , ShowProxy (Header blk)
     , ShowProxy (TxId (GenTx blk))
     , ShowProxy (GenTx blk)
     )
  => NodeKernel m remotePeer localPeer blk -- ^ Needed for bracketing only
  -> Tracers m remotePeer blk e
  -> (NodeToNodeVersion -> Codecs blk e m bCS bCS bBF bBF bTX bTX2 bKA)
  -> m ChainSyncTimeout
  -> Handlers m remotePeer blk
  -> Apps m remotePeer bCS bBF bTX bTX2 bKA ()
mkApps kernel Tracers {..} mkCodecs genChainSyncTimeout Handlers {..} =
    Apps {..}
  where
    aChainSyncClient
      :: NodeToNodeVersion
      -> ControlMessageSTM m
      -> remotePeer
      -> Channel m bCS
      -> m ((), Maybe bCS)
    aChainSyncClient version controlMessageSTM them channel = do
      labelThisThread "ChainSyncClient"
      -- Note that it is crucial that we sync with the fetch client "outside"
      -- of registering the state for the sync client. This is needed to
      -- maintain a state invariant required by the block fetch logic: that for
      -- each candidate chain there is a corresponding block fetch client that
      -- can be used to fetch blocks for that chain.
      bracketSyncWithFetchClient
        (getFetchClientRegistry kernel) them $
        bracketChainSyncClient
            (contramap (TraceLabelPeer them) (Node.chainSyncClientTracer (getTracers kernel)))
            (defaultChainDbView (getChainDB kernel))
            (getNodeCandidates kernel)
            them $ \varCandidate -> do
              chainSyncTimeout <- genChainSyncTimeout
              (_, trailing) <-
                runPipelinedPeerWithLimits
                  (contramap (TraceLabelPeer them) tChainSyncTracer)
                  (cChainSyncCodec (mkCodecs version))
                  (byteLimitsChainSync (const 0)) -- TODO: Real Bytelimits, see #1727
                  (timeLimitsChainSync chainSyncTimeout)
                  channel
                  $ chainSyncClientPeerPipelined
                  $ hChainSyncClient them version controlMessageSTM varCandidate
              return ((), trailing)

    aChainSyncServer
      :: NodeToNodeVersion
      -> remotePeer
      -> Channel m bCS
      -> m ((), Maybe bCS)
    aChainSyncServer version them channel = do
      labelThisThread "ChainSyncServer"
      chainSyncTimeout <- genChainSyncTimeout
      bracketWithPrivateRegistry
        (chainSyncHeaderServerFollower (getChainDB kernel))
        ChainDB.followerClose
        $ \flr ->
          runPeerWithLimits
            (contramap (TraceLabelPeer them) tChainSyncSerialisedTracer)
            (cChainSyncCodecSerialised (mkCodecs version))
            (byteLimitsChainSync (const 0)) -- TODO: Real Bytelimits, see #1727
            (timeLimitsChainSync chainSyncTimeout)
            channel
            $ chainSyncServerPeer
            $ hChainSyncServer version flr

    aBlockFetchClient
      :: NodeToNodeVersion
      -> ControlMessageSTM m
      -> remotePeer
      -> Channel m bBF
      -> m ((), Maybe bBF)
    aBlockFetchClient version controlMessageSTM them channel = do
      labelThisThread "BlockFetchClient"
      bracketFetchClient (getFetchClientRegistry kernel) them $ \clientCtx ->
        runPipelinedPeerWithLimits
          (contramap (TraceLabelPeer them) tBlockFetchTracer)
          (cBlockFetchCodec (mkCodecs version))
          (byteLimitsBlockFetch (const 0)) -- TODO: Real Bytelimits, see #1727
          timeLimitsBlockFetch
          channel
          $ hBlockFetchClient version controlMessageSTM clientCtx

    aBlockFetchServer
      :: NodeToNodeVersion
      -> remotePeer
      -> Channel m bBF
      -> m ((), Maybe bBF)
    aBlockFetchServer version them channel = do
      labelThisThread "BlockFetchServer"
      withRegistry $ \registry ->
        runPeerWithLimits
          (contramap (TraceLabelPeer them) tBlockFetchSerialisedTracer)
          (cBlockFetchCodecSerialised (mkCodecs version))
          (byteLimitsBlockFetch (const 0)) -- TODO: Real Bytelimits, see #1727
          timeLimitsBlockFetch
          channel
          $ blockFetchServerPeer
          $ hBlockFetchServer version registry

    aTxSubmissionClient
      :: NodeToNodeVersion
      -> ControlMessageSTM m
      -> remotePeer
      -> Channel m bTX
      -> m ((), Maybe bTX)
    aTxSubmissionClient version controlMessageSTM them channel = do
      labelThisThread "TxSubmissionClient"
      runPeerWithLimits
        (contramap (TraceLabelPeer them) tTxSubmissionTracer)
        (cTxSubmissionCodec (mkCodecs version))
        (byteLimitsTxSubmission (const 0)) -- TODO: Real Bytelimits, see #1727
        timeLimitsTxSubmission
        channel
        (txSubmissionClientPeer (hTxSubmissionClient version controlMessageSTM them))

    aTxSubmissionServer
      :: NodeToNodeVersion
      -> remotePeer
      -> Channel m bTX
      -> m ((), Maybe bTX)
    aTxSubmissionServer version them channel = do
      labelThisThread "TxSubmissionServer"
      runPipelinedPeerWithLimits
        (contramap (TraceLabelPeer them) tTxSubmissionTracer)
        (cTxSubmissionCodec (mkCodecs version))
        (byteLimitsTxSubmission (const 0)) -- TODO: Real Bytelimits, see #1727
        timeLimitsTxSubmission
        channel
        (txSubmissionServerPeerPipelined (hTxSubmissionServer version them))

    aTxSubmission2Client
      :: NodeToNodeVersion
      -> ControlMessageSTM m
      -> remotePeer
      -> Channel m bTX2
      -> m ((), Maybe bTX2)
    aTxSubmission2Client version controlMessageSTM them channel = do
      labelThisThread "TxSubmissionClient"
      runPeerWithLimits
        (contramap (TraceLabelPeer them) tTxSubmission2Tracer)
        (cTxSubmission2Codec (mkCodecs version))
        (byteLimitsTxSubmission2 (const 0)) -- TODO: Real Bytelimits, see #1727
        timeLimitsTxSubmission2
        channel
        (Hello.wrapClientPeer (txSubmissionClientPeer (hTxSubmissionClient version controlMessageSTM them)))

    aTxSubmission2Server
      :: NodeToNodeVersion
      -> remotePeer
      -> Channel m bTX2
      -> m ((), Maybe bTX2)
    aTxSubmission2Server version them channel = do
      labelThisThread "TxSubmissionServer"
      runPipelinedPeerWithLimits
        (contramap (TraceLabelPeer them) tTxSubmission2Tracer)
        (cTxSubmission2Codec (mkCodecs version))
        (byteLimitsTxSubmission2 (const 0)) -- TODO: Real Bytelimits, see #1727
        timeLimitsTxSubmission2
        channel
        (Hello.wrapServerPeerPipelined (txSubmissionServerPeerPipelined (hTxSubmissionServer version them)))

    aKeepAliveClient
      :: NodeToNodeVersion
      -> ControlMessageSTM m
      -> remotePeer
      -> Channel m bKA
      -> m ((), Maybe bKA)
    aKeepAliveClient version _controlMessageSTM them channel = do
      labelThisThread "KeepAliveClient"
      let kacApp = case version of
                     -- Version 1 doesn't support keep alive protocol but Blockfetch
                     -- still requires a PeerGSV per peer.
                     NodeToNodeV_1 -> \_ -> forever (threadDelay 1000) >> return ((), Nothing)
                     NodeToNodeV_2 -> \_ -> forever (threadDelay 1000) >> return ((), Nothing)
                     _             -> \dqCtx -> do
                       runPeerWithLimits
                         nullTracer
                         (cKeepAliveCodec (mkCodecs version))
                         (byteLimitsKeepAlive (const 0)) -- TODO: Real Bytelimits, see #1727
                         timeLimitsKeepAlive
                         channel
                         $ keepAliveClientPeer
                         $ hKeepAliveClient version (continueForever (Proxy :: Proxy m)) them dqCtx
                             (KeepAliveInterval 10)

      bracketKeepAliveClient (getFetchClientRegistry kernel) them kacApp

    aKeepAliveServer
      :: NodeToNodeVersion
      -> remotePeer
      -> Channel m bKA
      -> m ((), Maybe bKA)
    aKeepAliveServer version _them channel = do
      labelThisThread "KeepAliveServer"
      runPeerWithLimits
        nullTracer
        (cKeepAliveCodec (mkCodecs version))
        (byteLimitsKeepAlive (const 0)) -- TODO: Real Bytelimits, see #1727
        timeLimitsKeepAlive
        channel
        $ keepAliveServerPeer
        $ keepAliveServer

{-------------------------------------------------------------------------------
  Projections from 'Apps'
-------------------------------------------------------------------------------}

-- | A projection from 'NetworkApplication' to a client-side
-- 'OuroborosApplication' for the node-to-node protocols.
--
-- Implementation note: network currently doesn't enable protocols conditional
-- on the protocol version, but it eventually may; this is why @_version@ is
-- currently unused.
initiator
  :: MiniProtocolParameters
  -> NodeToNodeVersion
  -> Apps m (ConnectionId peer) b b b b b a
  -> OuroborosApplication 'InitiatorMode peer b m a Void
initiator miniProtocolParameters version Apps {..} =
    nodeToNodeProtocols
      miniProtocolParameters
      -- TODO: currently consensus is using 'ConnectionId' for its 'peer' type.
      -- This is currently ok, as we might accept multiple connections from the
      -- same ip address, however this will change when we will switch to
      -- p2p-governor & connection-manager.  Then consenus can use peer's ip
      -- address & port number, rather than 'ConnectionId' (which is
      -- a quadruple uniquely determinaing a connection).
      (\them controlMessageSTM -> NodeToNodeProtocols {
          chainSyncProtocol =
            (InitiatorProtocolOnly (MuxPeerRaw (aChainSyncClient version controlMessageSTM them))),
          blockFetchProtocol =
            (InitiatorProtocolOnly (MuxPeerRaw (aBlockFetchClient version controlMessageSTM them))),
          txSubmissionProtocol =
            if version >= NodeToNodeV_6
              then InitiatorProtocolOnly (MuxPeerRaw (aTxSubmission2Client version controlMessageSTM them))
              else InitiatorProtocolOnly (MuxPeerRaw (aTxSubmissionClient  version controlMessageSTM them)),
          keepAliveProtocol =
            (InitiatorProtocolOnly (MuxPeerRaw (aKeepAliveClient version controlMessageSTM them)))
        })
      version

-- | A projection from 'NetworkApplication' to a server-side
-- 'OuroborosApplication' for the node-to-node protocols.
--
-- See 'initiatorNetworkApplication' for rationale for the @_version@ arg.
responder
  :: MiniProtocolParameters
  -> NodeToNodeVersion
  -> Apps m (ConnectionId peer) b b b b b a
  -> OuroborosApplication 'ResponderMode peer b m Void a
responder miniProtocolParameters version Apps {..} =
    nodeToNodeProtocols
      miniProtocolParameters
      (\them _controlMessageSTM -> NodeToNodeProtocols {
          chainSyncProtocol =
            (ResponderProtocolOnly (MuxPeerRaw (aChainSyncServer version them))),
          blockFetchProtocol =
            (ResponderProtocolOnly (MuxPeerRaw (aBlockFetchServer version them))),
          txSubmissionProtocol =
            if version >= NodeToNodeV_6
              then ResponderProtocolOnly (MuxPeerRaw (aTxSubmission2Server version them))
              else ResponderProtocolOnly (MuxPeerRaw (aTxSubmissionServer  version them)),
          keepAliveProtocol =
            (ResponderProtocolOnly (MuxPeerRaw (aKeepAliveServer version them)))
        })
      version
