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
  , mkApps
    -- ** Projections
  , initiator
  , responder
  ) where

import           Codec.Serialise (Serialise)
import           Control.Monad (void)
import           Control.Monad.Class.MonadTimer (MonadTimer)
import           Control.Tracer
import           Data.ByteString.Lazy (ByteString)
import           Data.Proxy (Proxy (..))
import           Data.Void (Void)

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment (..))
import           Ouroboros.Network.Block
import           Ouroboros.Network.BlockFetch
import           Ouroboros.Network.BlockFetch.Client (
                   BlockFetchClient, blockFetchClient)
import           Ouroboros.Network.Channel
import           Ouroboros.Network.Codec
import           Ouroboros.Network.Driver
import           Ouroboros.Network.Mux
import           Ouroboros.Network.NodeToNode hiding (NodeToNodeVersion(..))
import qualified Ouroboros.Network.NodeToNode as N (NodeToNodeVersion(..))
import           Ouroboros.Network.Protocol.BlockFetch.Codec
import           Ouroboros.Network.Protocol.BlockFetch.Server (
                   BlockFetchServer, blockFetchServerPeer)
import           Ouroboros.Network.Protocol.BlockFetch.Type (BlockFetch (..))
import           Ouroboros.Network.Protocol.ChainSync.ClientPipelined
import           Ouroboros.Network.Protocol.ChainSync.Codec
import           Ouroboros.Network.Protocol.ChainSync.PipelineDecision
import           Ouroboros.Network.Protocol.ChainSync.Server
import           Ouroboros.Network.Protocol.ChainSync.Type
import           Ouroboros.Network.Protocol.TxSubmission.Client
import           Ouroboros.Network.Protocol.TxSubmission.Codec
import           Ouroboros.Network.Protocol.TxSubmission.Server
import           Ouroboros.Network.Protocol.TxSubmission.Type
import           Ouroboros.Network.TxSubmission.Inbound
import           Ouroboros.Network.TxSubmission.Outbound

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.MiniProtocol.BlockFetch.Server
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Server
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run
import qualified Ouroboros.Consensus.Node.Tracers as Node
import           Ouroboros.Consensus.NodeKernel
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.ResourceRegistry

{-------------------------------------------------------------------------------
  Handlers
-------------------------------------------------------------------------------}

-- | Protocol handlers for node-to-node (remote) communication
data Handlers m peer blk = Handlers {
      hChainSyncClient
        :: StrictTVar m (AnchoredFragment (Header blk))
        -> ChainSyncClientPipelined (Header blk) (Tip blk) m Void
        -- TODO: we should consider either bundling these context parameters
        -- into a record, or extending the protocol handler representation
        -- to support bracket-style initialisation so that we could have the
        -- closure include these and not need to be explicit about them here.

    , hChainSyncServer
        :: ResourceRegistry m
        -> ChainSyncServer (Serialised (Header blk)) (Tip blk) m ()

    -- TODO block fetch client does not have GADT view of the handlers.
    , hBlockFetchClient
        :: BlockFetchClient (Header blk) blk m ()

    , hBlockFetchServer
        :: ResourceRegistry m
        -> BlockFetchServer (Serialised blk) m ()

    , hTxSubmissionClient
        :: peer
        -> TxSubmissionClient (GenTxId blk) (GenTx blk) m ()

    , hTxSubmissionServer
        :: peer
        -> TxSubmissionServerPipelined (GenTxId blk) (GenTx blk) m ()
    }

mkHandlers
  :: forall m blk remotePeer localPeer.
     ( IOLike m
     , ApplyTx blk
     , HasTxId (GenTx blk)
     , LedgerSupportsProtocol blk
     , Serialise (HeaderHash blk)
     )
  => NodeArgs   m remotePeer localPeer blk
  -> NodeKernel m remotePeer localPeer blk
  -> Handlers   m remotePeer           blk
mkHandlers
      NodeArgs {btime, maxClockSkew, miniProtocolParameters}
      NodeKernel {getChainDB, getMempool, getTopLevelConfig, getTracers = tracers} =
    Handlers {
        hChainSyncClient =
          chainSyncClient
            (pipelineDecisionLowHighMark
              (chainSyncPipeliningLowMark  miniProtocolParameters)
              (chainSyncPipeliningHighMark miniProtocolParameters))
            (Node.chainSyncClientTracer tracers)
            getTopLevelConfig
            btime
            maxClockSkew
            (defaultChainDbView getChainDB)
      , hChainSyncServer =
          chainSyncHeadersServer
            (Node.chainSyncServerHeaderTracer tracers)
            getChainDB
      , hBlockFetchClient = blockFetchClient
      , hBlockFetchServer =
          blockFetchServer
            (Node.blockFetchServerTracer tracers)
            getChainDB
      , hTxSubmissionClient = \peer ->
          txSubmissionOutbound
            (contramap (TraceLabelPeer peer) (Node.txOutboundTracer tracers))
            (txSubmissionMaxUnacked miniProtocolParameters)
            (getMempoolReader getMempool)
      , hTxSubmissionServer = \peer ->
          txSubmissionInbound
            (contramap (TraceLabelPeer peer) (Node.txInboundTracer tracers))
            (txSubmissionMaxUnacked miniProtocolParameters)
            (getMempoolReader getMempool)
            (getMempoolWriter getMempool)
      }

{-------------------------------------------------------------------------------
  Codecs
-------------------------------------------------------------------------------}

-- | Node-to-node protocol codecs needed to run 'Handlers'.
data Codecs blk e m bCS bSCS bBF bSBF bTX = Codecs {
      cChainSyncCodec            :: Codec (ChainSync (Header blk) (Tip blk))              e m bCS
    , cChainSyncCodecSerialised  :: Codec (ChainSync (Serialised (Header blk)) (Tip blk)) e m bSCS
    , cBlockFetchCodec           :: Codec (BlockFetch blk)                                e m bBF
    , cBlockFetchCodecSerialised :: Codec (BlockFetch (Serialised blk))                   e m bSBF
    , cTxSubmissionCodec         :: Codec (TxSubmission (GenTxId blk) (GenTx blk))        e m bTX
    }

-- | Protocol codecs for the node-to-node protocols
defaultCodecs :: forall m blk. (IOLike m, RunNode blk)
              => BlockConfig       blk
              -> NodeToNodeVersion blk
              -> Codecs blk DeserialiseFailure m
                   ByteString ByteString ByteString ByteString ByteString
defaultCodecs cfg version = Codecs {
      cChainSyncCodec =
        codecChainSync
          (wrapCBORinCBOR   (nodeEncodeHeader cfg (serialisedNodeToNode version)))
          (unwrapCBORinCBOR (nodeDecodeHeader cfg (serialisedNodeToNode version)))
          (encodePoint (nodeEncodeHeaderHash (Proxy @blk)))
          (decodePoint (nodeDecodeHeaderHash (Proxy @blk)))
          (encodeTip   (nodeEncodeHeaderHash (Proxy @blk)))
          (decodeTip   (nodeDecodeHeaderHash (Proxy @blk)))

    , cChainSyncCodecSerialised =
        codecChainSyncSerialised'
          (nodeEncodeWrappedHeader cfg (SerialisedNodeToNode version))
          (nodeDecodeWrappedHeader cfg (SerialisedNodeToNode version))
          (encodePoint (nodeEncodeHeaderHash (Proxy @blk)))
          (decodePoint (nodeDecodeHeaderHash (Proxy @blk)))
          (encodeTip   (nodeEncodeHeaderHash (Proxy @blk)))
          (decodeTip   (nodeDecodeHeaderHash (Proxy @blk)))

    , cBlockFetchCodec =
        codecBlockFetch
          (wrapCBORinCBOR   (nodeEncodeBlock cfg))
          (unwrapCBORinCBOR (nodeDecodeBlock cfg))
          (nodeEncodeHeaderHash (Proxy @blk))
          (nodeDecodeHeaderHash (Proxy @blk))

    , cBlockFetchCodecSerialised =
        codecBlockFetchSerialised
          (nodeEncodeHeaderHash (Proxy @blk))
          (nodeDecodeHeaderHash (Proxy @blk))

    , cTxSubmissionCodec =
        codecTxSubmission
          nodeEncodeGenTxId
          nodeDecodeGenTxId
          nodeEncodeGenTx
          nodeDecodeGenTx
    }

-- | Identity codecs used in tests.
identityCodecs :: Monad m
               => Codecs blk CodecFailure m
                    (AnyMessage (ChainSync (Header blk) (Tip blk)))
                    (AnyMessage (ChainSync (Serialised (Header blk)) (Tip blk)))
                    (AnyMessage (BlockFetch blk))
                    (AnyMessage (BlockFetch (Serialised blk)))
                    (AnyMessage (TxSubmission (GenTxId blk) (GenTx blk)))
identityCodecs = Codecs {
      cChainSyncCodec            = codecChainSyncId
    , cChainSyncCodecSerialised  = codecChainSyncId
    , cBlockFetchCodec           = codecBlockFetchId
    , cBlockFetchCodecSerialised = codecBlockFetchId
    , cTxSubmissionCodec         = codecTxSubmissionId
    }

{-------------------------------------------------------------------------------
  Tracers
-------------------------------------------------------------------------------}

-- | A record of 'Tracer's for the different protocols.
type Tracers m peer blk e =
     Tracers'  peer blk e (Tracer m)

data Tracers' peer blk e f = Tracers {
      tChainSyncTracer            :: f (TraceLabelPeer peer (TraceSendRecv (ChainSync (Header blk) (Tip blk))))
    , tChainSyncSerialisedTracer  :: f (TraceLabelPeer peer (TraceSendRecv (ChainSync (Serialised (Header blk)) (Tip blk))))
    , tBlockFetchTracer           :: f (TraceLabelPeer peer (TraceSendRecv (BlockFetch blk)))
    , tBlockFetchSerialisedTracer :: f (TraceLabelPeer peer (TraceSendRecv (BlockFetch (Serialised blk))))
    , tTxSubmissionTracer         :: f (TraceLabelPeer peer (TraceSendRecv (TxSubmission (GenTxId blk) (GenTx blk))))
    }

instance (forall a. Semigroup (f a)) => Semigroup (Tracers' peer blk e f) where
  l <> r = Tracers {
        tChainSyncTracer            = f tChainSyncTracer
      , tChainSyncSerialisedTracer  = f tChainSyncSerialisedTracer
      , tBlockFetchTracer           = f tBlockFetchTracer
      , tBlockFetchSerialisedTracer = f tBlockFetchSerialisedTracer
      , tTxSubmissionTracer         = f tTxSubmissionTracer
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
    }

showTracers :: ( Show blk
               , Show peer
               , Show (Header blk)
               , Show (GenTx blk)
               , Show (GenTxId blk)
               , HasHeader blk
               )
            => Tracer m String -> Tracers m peer blk e
showTracers tr = Tracers {
      tChainSyncTracer            = showTracing tr
    , tChainSyncSerialisedTracer  = showTracing tr
    , tBlockFetchTracer           = showTracing tr
    , tBlockFetchSerialisedTracer = showTracing tr
    , tTxSubmissionTracer         = showTracing tr
    }

{-------------------------------------------------------------------------------
  Applications
-------------------------------------------------------------------------------}

-- | Applications for the node-to-node protocols
--
-- See 'Network.Mux.Types.MuxApplication'
data Apps m peer bCS bBF bTX a = Apps {
      -- | Start a chain sync client that communicates with the given upstream
      -- node.
      aChainSyncClient    :: peer -> Channel m bCS -> m a

      -- | Start a chain sync server.
    , aChainSyncServer    :: peer -> Channel m bCS -> m a

      -- | Start a block fetch client that communicates with the given
      -- upstream node.
    , aBlockFetchClient   :: peer -> Channel m bBF -> m a

      -- | Start a block fetch server.
    , aBlockFetchServer   :: peer -> Channel m bBF -> m a

      -- | Start a transaction submission client that communicates with the
      -- given upstream node.
    , aTxSubmissionClient :: peer -> Channel m bTX -> m a

      -- | Start a transaction submission server.
    , aTxSubmissionServer :: peer -> Channel m bTX -> m a
    }

-- | Construct the 'NetworkApplication' for the node-to-node protocols
mkApps
  :: forall m remotePeer localPeer blk e bCS bBF bTX.
     ( IOLike m
     , MonadTimer m
     , Ord remotePeer
     , Exception e
     , LedgerSupportsProtocol blk
     )
  => NodeKernel m remotePeer localPeer blk -- ^ Needed for bracketing only
  -> Tracers m remotePeer blk e
  -> Codecs blk e m bCS bCS bBF bBF bTX
  -> Maybe DiffTime -- Workaround for #1882, test cases that can't cope with timeouts
  -> Handlers m remotePeer blk
  -> Apps m remotePeer bCS bBF bTX ()
mkApps kernel Tracers {..} Codecs {..} chainSyncTimeout Handlers {..} =
    Apps {..}
  where
    aChainSyncClient
      :: remotePeer
      -> Channel m bCS
      -> m ()
    aChainSyncClient them channel = do
      labelThisThread "ChainSyncClient"
      void $
        -- Note that it is crucial that we sync with the fetch client "outside"
        -- of registering the state for the sync client. This is needed to
        -- maintain a state invariant required by the block fetch logic: that for
        -- each candidate chain there is a corresponding block fetch client that
        -- can be used to fetch blocks for that chain.
        bracketSyncWithFetchClient
          (getFetchClientRegistry kernel) them $
        bracketChainSyncClient
            (Node.chainSyncClientTracer (getTracers kernel))
            (defaultChainDbView (getChainDB kernel))
            (getNodeCandidates kernel)
            them $ \varCandidate->
          runPipelinedPeerWithLimits
            (contramap (TraceLabelPeer them) tChainSyncTracer)
            cChainSyncCodec
            (byteLimitsChainSync (const 0)) -- TODO: Real Bytelimits, see #1727
            (timeLimitsChainSync chainSyncTimeout)
            channel
            $ chainSyncClientPeerPipelined
            $ hChainSyncClient varCandidate

    aChainSyncServer
      :: remotePeer
      -> Channel m bCS
      -> m ()
    aChainSyncServer them channel = do
      labelThisThread "ChainSyncServer"
      withRegistry $ \registry ->
        runPeerWithLimits
          (contramap (TraceLabelPeer them) tChainSyncSerialisedTracer)
          cChainSyncCodecSerialised
          (byteLimitsChainSync (const 0)) -- TODO: Real Bytelimits, see #1727
          (timeLimitsChainSync chainSyncTimeout)
          channel
          $ chainSyncServerPeer
          $ hChainSyncServer registry

    aBlockFetchClient
      :: remotePeer
      -> Channel m bBF
      -> m ()
    aBlockFetchClient them channel = do
      labelThisThread "BlockFetchClient"
      bracketFetchClient (getFetchClientRegistry kernel) them $ \clientCtx ->
        runPipelinedPeerWithLimits
          (contramap (TraceLabelPeer them) tBlockFetchTracer)
          cBlockFetchCodec
          (byteLimitsBlockFetch (const 0)) -- TODO: Real Bytelimits, see #1727
          timeLimitsBlockFetch
          channel
          $ hBlockFetchClient clientCtx

    aBlockFetchServer
      :: remotePeer
      -> Channel m bBF
      -> m ()
    aBlockFetchServer them channel = do
      labelThisThread "BlockFetchServer"
      withRegistry $ \registry ->
        runPeerWithLimits
          (contramap (TraceLabelPeer them) tBlockFetchSerialisedTracer)
          cBlockFetchCodecSerialised
          (byteLimitsBlockFetch (const 0)) -- TODO: Real Bytelimits, see #1727
          timeLimitsBlockFetch
          channel
          $ blockFetchServerPeer
          $ hBlockFetchServer registry

    aTxSubmissionClient
      :: remotePeer
      -> Channel m bTX
      -> m ()
    aTxSubmissionClient them channel = do
      labelThisThread "TxSubmissionClient"
      runPeerWithLimits
        (contramap (TraceLabelPeer them) tTxSubmissionTracer)
        cTxSubmissionCodec
        (byteLimitsTxSubmission (const 0)) -- TODO: Real Bytelimits, see #1727
        timeLimitsTxSubmission
        channel
        (txSubmissionClientPeer (hTxSubmissionClient them))

    aTxSubmissionServer
      :: remotePeer
      -> Channel m bTX
      -> m ()
    aTxSubmissionServer them channel = do
      labelThisThread "TxSubmissionServer"
      runPipelinedPeerWithLimits
        (contramap (TraceLabelPeer them) tTxSubmissionTracer)
        cTxSubmissionCodec
        (byteLimitsTxSubmission (const 0)) -- TODO: Real Bytelimits, see #1727
        timeLimitsTxSubmission
        channel
        (txSubmissionServerPeerPipelined (hTxSubmissionServer them))

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
  -> N.NodeToNodeVersion
  -> Apps m peer b b b a
  -> peer
  -> OuroborosApplication 'InitiatorApp b m a Void
initiator miniProtocolParameters _version Apps {..} them =
    nodeToNodeProtocols
      miniProtocolParameters
      NodeToNodeProtocols {
          chainSyncProtocol =
            (InitiatorProtocolOnly (MuxPeerRaw (aChainSyncClient them))),
          blockFetchProtocol =
            (InitiatorProtocolOnly (MuxPeerRaw (aBlockFetchClient them))),
          txSubmissionProtocol =
            (InitiatorProtocolOnly (MuxPeerRaw (aTxSubmissionClient them)))
        }

-- | A projection from 'NetworkApplication' to a server-side
-- 'OuroborosApplication' for the node-to-node protocols.
--
-- See 'initiatorNetworkApplication' for rationale for the @_version@ arg.
responder
  :: MiniProtocolParameters
  -> N.NodeToNodeVersion
  -> Apps m peer b b b a
  -> peer
  -> OuroborosApplication 'ResponderApp b m Void a
responder miniProtocolParameters _version Apps {..} them =
    nodeToNodeProtocols
      miniProtocolParameters
      NodeToNodeProtocols {
          chainSyncProtocol =
            (ResponderProtocolOnly (MuxPeerRaw (aChainSyncServer them))),
          blockFetchProtocol =
            (ResponderProtocolOnly (MuxPeerRaw (aBlockFetchServer them))),
          txSubmissionProtocol =
            (ResponderProtocolOnly (MuxPeerRaw (aTxSubmissionServer them)))
        }
