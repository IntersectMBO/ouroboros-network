{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Werror=missing-fields -Wno-unticked-promoted-constructors #-}

module Ouroboros.Consensus.NodeNetwork (
    ProtocolHandlers (..)
  , protocolHandlers
  , ProtocolCodecs (..)
  , protocolCodecs
  , protocolCodecsId
  , NodeToClientCodecs (..)
  , nodeToClientCodecs
  , ProtocolTracers
  , ProtocolTracers' (..)
  , nullProtocolTracers
  , showProtocolTracers
  , NetworkApplication(..)
  , consensusNetworkApps
  , initiatorNetworkApplication
  , responderNetworkApplication
  , localResponderNetworkApplication
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
import           Ouroboros.Network.BlockFetch.Client (BlockFetchClient,
                     blockFetchClient)
import           Ouroboros.Network.Channel
import           Ouroboros.Network.Codec
import           Ouroboros.Network.Driver
import           Ouroboros.Network.Mux
import           Ouroboros.Network.NodeToClient
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
import           Ouroboros.Network.Protocol.LocalStateQuery.Codec
import           Ouroboros.Network.Protocol.LocalStateQuery.Server
import           Ouroboros.Network.Protocol.LocalStateQuery.Type
import           Ouroboros.Network.Protocol.LocalTxSubmission.Codec
import           Ouroboros.Network.Protocol.LocalTxSubmission.Server
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type
import           Ouroboros.Network.Protocol.TxSubmission.Client
import           Ouroboros.Network.Protocol.TxSubmission.Codec
import           Ouroboros.Network.Protocol.TxSubmission.Server
import           Ouroboros.Network.Protocol.TxSubmission.Type
import           Ouroboros.Network.TxSubmission.Inbound
import           Ouroboros.Network.TxSubmission.Outbound

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.MiniProtocol.BlockFetch.Server
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Server
import           Ouroboros.Consensus.MiniProtocol.LocalStateQuery.Server
import           Ouroboros.Consensus.MiniProtocol.LocalTxSubmission.Server
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Node.Tracers
import           Ouroboros.Consensus.NodeKernel
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Ouroboros.Consensus.Storage.ChainDB.API (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB

-- | The collection of all the mini-protocol handlers provided by the consensus
-- layer.
--
-- These are used to construct the server (responder) and client (initiator)
-- sides of the node-to-node protocol bundle, and the node-to-client protocol
-- bundle.
--
data ProtocolHandlers m peer blk = ProtocolHandlers {
      phChainSyncClient
        :: StrictTVar m (AnchoredFragment (Header blk))
        -> ChainSyncClientPipelined (Header blk) (Tip blk) m Void
        -- TODO: we should consider either bundling these context parameters
        -- into a record, or extending the protocol handler representation
        -- to support bracket-style initialisation so that we could have the
        -- closure include these and not need to be explicit about them here.

    , phChainSyncServer
        :: ResourceRegistry m
        -> ChainSyncServer (Serialised (Header blk)) (Tip blk) m ()

    -- TODO block fetch client does not have GADT view of the handlers.
    , phBlockFetchClient
        :: BlockFetchClient (Header blk) blk m ()

    , phBlockFetchServer
        :: ResourceRegistry m
        -> BlockFetchServer (Serialised blk) m ()

    , phTxSubmissionClient
        :: peer
        -> TxSubmissionClient (GenTxId blk) (GenTx blk) m ()

    , phTxSubmissionServer
        :: peer
        -> TxSubmissionServerPipelined (GenTxId blk) (GenTx blk) m ()

     -- Handlers for the local node-to-client bundle of mini-protocols

    , phLocalChainSyncServer
        :: ResourceRegistry m
        -> ChainSyncServer (Serialised blk) (Tip blk) m ()

    , phLocalTxSubmissionServer
        :: LocalTxSubmissionServer (GenTx blk) (ApplyTxErr blk) m ()

    , phLocalStateQueryServer
        :: LocalStateQueryServer blk (Query blk) m ()
    }

protocolHandlers
    :: forall m blk peer.
       ( IOLike m
       , ApplyTx blk
       , HasTxId (GenTx blk)
       , LedgerSupportsProtocol blk
       , QueryLedger blk
       , Serialise (HeaderHash blk)
       )
    => NodeArgs   m peer blk  --TODO eliminate, merge relevant into NodeKernel
    -> NodeKernel m peer blk
    -> ProtocolHandlers m peer blk
protocolHandlers NodeArgs {btime, cfg, maxClockSkew, tracers, miniProtocolParameters}
                 NodeKernel {getChainDB, getMempool, getTopLevelConfig} =
    --TODO: bundle needed NodeArgs into the NodeKernel
    -- so we do not have to pass it separately
    --TODO: need to review the use of the peer id in the tracers.
    -- Curently it is not available here to use.
    ProtocolHandlers {
      phChainSyncClient =
        chainSyncClient
          (pipelineDecisionLowHighMark (chainSyncPipeliningLowMark  miniProtocolParameters)
                                       (chainSyncPipeliningHighMark miniProtocolParameters))
          (chainSyncClientTracer tracers)
          getTopLevelConfig
          btime
          maxClockSkew
          (chainDbView getChainDB)
    , phChainSyncServer =
        chainSyncHeadersServer
          (chainSyncServerHeaderTracer tracers)
          getChainDB
    , phBlockFetchClient = blockFetchClient
    , phBlockFetchServer =
        blockFetchServer
          (blockFetchServerTracer tracers)
          getChainDB
    , phTxSubmissionClient = \peer ->
        txSubmissionOutbound
          (contramap (TraceLabelPeer peer) (txOutboundTracer tracers))
          (txSubmissionMaxUnacked miniProtocolParameters)
          (getMempoolReader getMempool)
    , phTxSubmissionServer = \peer ->
        txSubmissionInbound
          (contramap (TraceLabelPeer peer) (txInboundTracer tracers))
          (txSubmissionMaxUnacked miniProtocolParameters)
          (getMempoolReader getMempool)
          (getMempoolWriter getMempool)

    , phLocalChainSyncServer =
        chainSyncBlocksServer
          (chainSyncServerBlockTracer tracers)
          getChainDB
    , phLocalTxSubmissionServer =
        localTxSubmissionServer
          (localTxSubmissionServerTracer tracers)
          getMempool
    , phLocalStateQueryServer =
        localStateQueryServer
          (configLedger cfg)
          (ChainDB.newLedgerCursor getChainDB)
    }


-- | Protocol codecs needed to run 'ProtocolHandlers'.
--
data ProtocolCodecs blk failure m
                    bytesCS bytesSCS bytesBF bytesSBF bytesTX
                    bytesLCS bytesLTX bytesLSQ = ProtocolCodecs {
    pcChainSyncCodec            :: Codec (ChainSync (Header blk) (Tip blk))
                                         failure m bytesCS
  , pcChainSyncCodecSerialised  :: Codec (ChainSync (Serialised (Header blk)) (Tip blk))
                                         failure m bytesSCS
  , pcBlockFetchCodec           :: Codec (BlockFetch blk)
                                         failure m bytesBF
  , pcBlockFetchCodecSerialised :: Codec (BlockFetch (Serialised blk))
                                         failure m bytesSBF
  , pcTxSubmissionCodec         :: Codec (TxSubmission (GenTxId blk) (GenTx blk))
                                         failure m bytesTX
  , pcLocalChainSyncCodec       :: Codec (ChainSync (Serialised blk) (Tip blk))
                                         failure m bytesLCS
  , pcLocalTxSubmissionCodec    :: Codec (LocalTxSubmission (GenTx blk) (ApplyTxErr blk))
                                         failure m bytesLTX
  , pcLocalStateQueryCodec      :: Codec (LocalStateQuery blk (Query blk))
                                         failure m bytesLSQ
  }

-- | The real codecs
--
protocolCodecs :: forall m blk. (IOLike m, RunNode blk)
               => TopLevelConfig blk
               -> NetworkProtocolVersion blk
               -> ProtocolCodecs blk DeserialiseFailure m
                    ByteString ByteString ByteString ByteString ByteString
                    ByteString ByteString ByteString
protocolCodecs cfg version = ProtocolCodecs {
      pcChainSyncCodec =
        codecChainSync
          (wrapCBORinCBOR   (nodeEncodeHeader cfg (SentAcrossNetwork version)))
          (unwrapCBORinCBOR (nodeDecodeHeader cfg (SentAcrossNetwork version)))
          (encodePoint (nodeEncodeHeaderHash (Proxy @blk)))
          (decodePoint (nodeDecodeHeaderHash (Proxy @blk)))
          (encodeTip   (nodeEncodeHeaderHash (Proxy @blk)))
          (decodeTip   (nodeDecodeHeaderHash (Proxy @blk)))

    , pcChainSyncCodecSerialised =
        codecChainSyncSerialised'
          (nodeEncodeWrappedHeader cfg version)
          (nodeDecodeWrappedHeader cfg version)
          (encodePoint (nodeEncodeHeaderHash (Proxy @blk)))
          (decodePoint (nodeDecodeHeaderHash (Proxy @blk)))
          (encodeTip   (nodeEncodeHeaderHash (Proxy @blk)))
          (decodeTip   (nodeDecodeHeaderHash (Proxy @blk)))

    , pcBlockFetchCodec =
        codecBlockFetch
          (wrapCBORinCBOR   (nodeEncodeBlock cfg))
          (unwrapCBORinCBOR (nodeDecodeBlock cfg))
          (nodeEncodeHeaderHash (Proxy @blk))
          (nodeDecodeHeaderHash (Proxy @blk))

    , pcBlockFetchCodecSerialised =
        codecBlockFetchSerialised
          (nodeEncodeHeaderHash (Proxy @blk))
          (nodeDecodeHeaderHash (Proxy @blk))

    , pcTxSubmissionCodec =
        codecTxSubmission
          nodeEncodeGenTxId
          nodeDecodeGenTxId
          nodeEncodeGenTx
          nodeDecodeGenTx

    , pcLocalChainSyncCodec    = nodeToClientChainSyncCodec ntcCodecs

    , pcLocalTxSubmissionCodec = nodeToClientTxSubmissionCodec ntcCodecs

    , pcLocalStateQueryCodec   = nodeToClientStateQueryCodec ntcCodecs
    }
    where
       ntcCodecs = nodeToClientCodecs NodeToClientV_1

data NodeToClientCodecs blk failure m bytesLCS bytesLTX bytesLSQ = NodeToClientCodecs {
    nodeToClientChainSyncCodec    :: Codec (ChainSync (Serialised blk) (Tip blk))
                                           failure m bytesLCS
  , nodeToClientTxSubmissionCodec :: Codec (LocalTxSubmission (GenTx blk) (ApplyTxErr blk))
                                           failure m bytesLTX
  , nodeToClientStateQueryCodec   :: Codec (LocalStateQuery blk (Query blk))
                                           failure m bytesLSQ
  }

nodeToClientCodecs
    :: forall m blk. (IOLike m, RunNode blk)
    => NodeToClientVersion
    -> NodeToClientCodecs blk DeserialiseFailure m ByteString ByteString ByteString
nodeToClientCodecs NodeToClientV_1 = NodeToClientCodecs {..}
  where
    nodeToClientChainSyncCodec =
        codecChainSyncSerialised
          (encodePoint (nodeEncodeHeaderHash (Proxy @blk)))
          (decodePoint (nodeDecodeHeaderHash (Proxy @blk)))
          (encodeTip   (nodeEncodeHeaderHash (Proxy @blk)))
          (decodeTip   (nodeDecodeHeaderHash (Proxy @blk)))

    nodeToClientTxSubmissionCodec =
        codecLocalTxSubmission
          nodeEncodeGenTx
          nodeDecodeGenTx
          (nodeEncodeApplyTxError (Proxy @blk))
          (nodeDecodeApplyTxError (Proxy @blk))

    nodeToClientStateQueryCodec =
        codecLocalStateQuery
          (encodePoint (nodeEncodeHeaderHash (Proxy @blk)))
          (decodePoint (nodeDecodeHeaderHash (Proxy @blk)))
          nodeEncodeQuery
          nodeDecodeQuery
          nodeEncodeResult
          nodeDecodeResult

-- | Id codecs used in tests.
--
protocolCodecsId :: (Monad m, QueryLedger blk)
                 => ProtocolCodecs blk CodecFailure m
                      (AnyMessage (ChainSync (Header blk) (Tip blk)))
                      (AnyMessage (ChainSync (Serialised (Header blk)) (Tip blk)))
                      (AnyMessage (BlockFetch blk))
                      (AnyMessage (BlockFetch (Serialised blk)))
                      (AnyMessage (TxSubmission (GenTxId blk) (GenTx blk)))
                      (AnyMessage (ChainSync (Serialised blk) (Tip blk)))
                      (AnyMessage (LocalTxSubmission (GenTx blk) (ApplyTxErr blk)))
                      (AnyMessage (LocalStateQuery blk (Query blk)))
protocolCodecsId = ProtocolCodecs {
      pcChainSyncCodec            = codecChainSyncId
    , pcChainSyncCodecSerialised  = codecChainSyncId
    , pcBlockFetchCodec           = codecBlockFetchId
    , pcBlockFetchCodecSerialised = codecBlockFetchId
    , pcTxSubmissionCodec         = codecTxSubmissionId
    , pcLocalChainSyncCodec       = codecChainSyncId
    , pcLocalTxSubmissionCodec    = codecLocalTxSubmissionId
    , pcLocalStateQueryCodec      = codecLocalStateQueryId eqQuery
    }

-- | A record of 'Tracer's for the different protocols.
type ProtocolTracers m peer localPeer blk failure = ProtocolTracers' peer localPeer blk failure (Tracer m)

data ProtocolTracers' peer localPeer blk failure f = ProtocolTracers {
    ptChainSyncTracer            :: f (TraceLabelPeer peer (TraceSendRecv (ChainSync (Header blk) (Tip blk))))
  , ptChainSyncSerialisedTracer  :: f (TraceLabelPeer peer (TraceSendRecv (ChainSync (Serialised (Header blk)) (Tip blk))))
  , ptBlockFetchTracer           :: f (TraceLabelPeer peer (TraceSendRecv (BlockFetch blk)))
  , ptBlockFetchSerialisedTracer :: f (TraceLabelPeer peer (TraceSendRecv (BlockFetch (Serialised blk))))
  , ptTxSubmissionTracer         :: f (TraceLabelPeer peer (TraceSendRecv (TxSubmission (GenTxId blk) (GenTx blk))))
  , ptLocalChainSyncTracer       :: f (TraceLabelPeer localPeer (TraceSendRecv (ChainSync (Serialised blk) (Tip blk))))
  , ptLocalTxSubmissionTracer    :: f (TraceLabelPeer localPeer (TraceSendRecv (LocalTxSubmission (GenTx blk) (ApplyTxErr blk))))
  , ptLocalStateQueryTracer      :: f (TraceLabelPeer localPeer (TraceSendRecv (LocalStateQuery blk (Query blk))))
  }

instance (forall a. Semigroup (f a)) => Semigroup (ProtocolTracers' peer localPeer blk failure f) where
  l <> r = ProtocolTracers {
      ptChainSyncTracer            = f ptChainSyncTracer
    , ptChainSyncSerialisedTracer  = f ptChainSyncSerialisedTracer
    , ptBlockFetchTracer           = f ptBlockFetchTracer
    , ptBlockFetchSerialisedTracer = f ptBlockFetchSerialisedTracer
    , ptTxSubmissionTracer         = f ptTxSubmissionTracer
    , ptLocalChainSyncTracer       = f ptLocalChainSyncTracer
    , ptLocalTxSubmissionTracer    = f ptLocalTxSubmissionTracer
    , ptLocalStateQueryTracer      = f ptLocalStateQueryTracer
    }
    where
      f :: forall a. Semigroup a
        => (ProtocolTracers' peer localPeer blk failure f -> a)
        -> a
      f prj = prj l <> prj r

-- | Use a 'nullTracer' for each protocol.
nullProtocolTracers :: Monad m => ProtocolTracers m peer localPeer blk failure
nullProtocolTracers = ProtocolTracers {
    ptChainSyncTracer            = nullTracer
  , ptChainSyncSerialisedTracer  = nullTracer
  , ptBlockFetchTracer           = nullTracer
  , ptBlockFetchSerialisedTracer = nullTracer
  , ptTxSubmissionTracer         = nullTracer
  , ptLocalChainSyncTracer       = nullTracer
  , ptLocalTxSubmissionTracer    = nullTracer
  , ptLocalStateQueryTracer      = nullTracer
  }

showProtocolTracers :: ( Show blk
                       , Show peer
                       , Show localPeer
                       , Show (Header blk)
                       , Show (GenTx blk)
                       , Show (GenTxId blk)
                       , Show (ApplyTxErr blk)
                       , ShowQuery (Query blk)
                       , HasHeader blk
                       )
                    => Tracer m String -> ProtocolTracers m peer localPeer blk failure
showProtocolTracers tr = ProtocolTracers {
    ptChainSyncTracer            = showTracing tr
  , ptChainSyncSerialisedTracer  = showTracing tr
  , ptBlockFetchTracer           = showTracing tr
  , ptBlockFetchSerialisedTracer = showTracing tr
  , ptTxSubmissionTracer         = showTracing tr
  , ptLocalChainSyncTracer       = showTracing tr
  , ptLocalTxSubmissionTracer    = showTracing tr
  , ptLocalStateQueryTracer      = showTracing tr
  }

-- | Consensus provides a chains sync, block fetch applications.  This data
-- type can be mapped to 'MuxApplication' under the assumption that 'bytesCS'
-- and 'bytesBF' coincide.  Using different bytes for different application is
-- useful for running different encoding on each channel in tests (identity
-- codecs).
--
data NetworkApplication m peer localPeer
                        bytesCS bytesBF bytesTX
                        bytesLCS bytesLTX bytesLSQ a = NetworkApplication {
      -- | Start a chain sync client that communicates with the given upstream
      -- node.
      naChainSyncClient         :: peer -> Channel m bytesCS -> m a

      -- | Start a chain sync server.
    , naChainSyncServer         :: peer -> Channel m bytesCS -> m a

      -- | Start a block fetch client that communicates with the given
      -- upstream node.
    , naBlockFetchClient        :: peer -> Channel m bytesBF -> m a

      -- | Start a block fetch server.
    , naBlockFetchServer        :: peer -> Channel m bytesBF -> m a

      -- | Start a transaction submission client that communicates with the
      -- given upstream node.
    , naTxSubmissionClient      :: peer -> Channel m bytesTX -> m a

      -- | Start a transaction submission server.
    , naTxSubmissionServer      :: peer -> Channel m bytesTX -> m a

      -- | Start a local chain sync server.
    , naLocalChainSyncServer    :: localPeer -> Channel m bytesLCS -> m a

      -- | Start a local transaction submission server.
    , naLocalTxSubmissionServer :: localPeer -> Channel m bytesLTX -> m a

      -- | Start a local state query server.
    , naLocalStateQueryServer   :: localPeer -> Channel m bytesLSQ -> m a
    }


-- | A projection from 'NetworkApplication' to a client-side
-- 'OuroborosApplication' for the node-to-node protocols.
--
initiatorNetworkApplication
  :: MiniProtocolParameters
  -> NetworkApplication m peer localPeer bytes bytes bytes bytes bytes bytes a
  -> peer
  -> OuroborosApplication InitiatorApp bytes m a Void
initiatorNetworkApplication miniProtocolParameters NetworkApplication {..} them =
    nodeToNodeProtocols
      miniProtocolParameters
      NodeToNodeProtocols {
          chainSyncProtocol =
            (InitiatorProtocolOnly (MuxPeerRaw (naChainSyncClient them))),
          blockFetchProtocol =
            (InitiatorProtocolOnly (MuxPeerRaw (naBlockFetchClient them))),
          txSubmissionProtocol =
            (InitiatorProtocolOnly (MuxPeerRaw (naTxSubmissionClient them)))
        }

-- | A projection from 'NetworkApplication' to a server-side
-- 'OuroborosApplication' for the node-to-node protocols.
--
responderNetworkApplication
  :: MiniProtocolParameters
  -> NetworkApplication m peer localPeer bytes bytes bytes bytes bytes bytes a
  -> peer
  -> OuroborosApplication ResponderApp bytes m Void a
responderNetworkApplication miniProtocolParameters NetworkApplication {..} them =
    nodeToNodeProtocols
      miniProtocolParameters
      NodeToNodeProtocols {
          chainSyncProtocol =
            (ResponderProtocolOnly (MuxPeerRaw (naChainSyncServer them))),
          blockFetchProtocol =
            (ResponderProtocolOnly (MuxPeerRaw (naBlockFetchServer them))),
          txSubmissionProtocol =
            (ResponderProtocolOnly (MuxPeerRaw (naTxSubmissionServer them)))
        }

-- | A projection from 'NetworkApplication' to a server-side
-- 'OuroborosApplication' for the node-to-client protocols.
--
localResponderNetworkApplication
  :: NetworkApplication m peer localPeer bytes bytes bytes bytes bytes bytes a
  -> localPeer
  -> OuroborosApplication ResponderApp bytes m Void a
localResponderNetworkApplication NetworkApplication {..} peer =
    nodeToClientProtocols
      NodeToClientProtocols {
          localChainSyncProtocol =
            (ResponderProtocolOnly (MuxPeerRaw (naLocalChainSyncServer peer))),
          localTxSubmissionProtocol =
            (ResponderProtocolOnly (MuxPeerRaw (naLocalTxSubmissionServer peer)))
        }


-- | Example function which creates consensus mux applications, this is useful
-- for testing but also untill we have only a single version in
-- 'NodeToNodeVersions'.
--
consensusNetworkApps
    :: forall m peer localPeer blk failure bytesCS bytesBF bytesTX bytesLCS bytesLTX bytesLSQ.
       ( IOLike m
       , MonadTimer m
       , Ord peer
       , Exception failure
       , LedgerSupportsProtocol blk
       )
    => NodeKernel m peer blk
    -> ProtocolTracers m peer localPeer blk failure
    -> ProtocolCodecs blk failure m bytesCS bytesCS bytesBF bytesBF bytesTX bytesLCS bytesLTX bytesLSQ
    -> Maybe DiffTime -- Workaround for #1882, test cases that can't cope with timeouts
    -> ProtocolHandlers m peer blk
    -> NetworkApplication m peer localPeer bytesCS bytesBF bytesTX bytesLCS bytesLTX bytesLSQ ()
consensusNetworkApps kernel ProtocolTracers {..} ProtocolCodecs {..} chainSyncTimeout ProtocolHandlers {..} =
    NetworkApplication {
      naChainSyncClient,
      naChainSyncServer,
      naBlockFetchClient,
      naBlockFetchServer,
      naTxSubmissionClient,
      naTxSubmissionServer,
      naLocalChainSyncServer,
      naLocalTxSubmissionServer,
      naLocalStateQueryServer
    }
  where
    naChainSyncClient
      :: peer
      -> Channel m bytesCS
      -> m ()
    naChainSyncClient them channel = do
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
            (chainSyncClientTracer (getTracers kernel))
            (chainDbView (getChainDB kernel))
            (getNodeCandidates kernel)
            them $ \varCandidate->
          runPipelinedPeerWithLimits
            (contramap (TraceLabelPeer them) ptChainSyncTracer)
            pcChainSyncCodec
            (byteLimitsChainSync (const 0)) -- TODO: Real Bytelimits, see #1727
            (timeLimitsChainSync chainSyncTimeout)
            channel
            $ chainSyncClientPeerPipelined
            $ phChainSyncClient varCandidate

    naChainSyncServer
      :: peer
      -> Channel m bytesCS
      -> m ()
    naChainSyncServer them channel = do
      labelThisThread "ChainSyncServer"
      withRegistry $ \registry ->
        runPeerWithLimits
          (contramap (TraceLabelPeer them) ptChainSyncSerialisedTracer)
          pcChainSyncCodecSerialised
          (byteLimitsChainSync (const 0)) -- TODO: Real Bytelimits, see #1727
          (timeLimitsChainSync chainSyncTimeout)
          channel
          $ chainSyncServerPeer
          $ phChainSyncServer registry

    naBlockFetchClient
      :: peer
      -> Channel m bytesBF
      -> m ()
    naBlockFetchClient them channel = do
      labelThisThread "BlockFetchClient"
      bracketFetchClient (getFetchClientRegistry kernel) them $ \clientCtx ->
        runPipelinedPeerWithLimits
          (contramap (TraceLabelPeer them) ptBlockFetchTracer)
          pcBlockFetchCodec
          (byteLimitsBlockFetch (const 0)) -- TODO: Real Bytelimits, see #1727
          timeLimitsBlockFetch
          channel
          $ phBlockFetchClient clientCtx

    naBlockFetchServer
      :: peer
      -> Channel m bytesBF
      -> m ()
    naBlockFetchServer them channel = do
      labelThisThread "BlockFetchServer"
      withRegistry $ \registry ->
        runPeerWithLimits
          (contramap (TraceLabelPeer them) ptBlockFetchSerialisedTracer)
          pcBlockFetchCodecSerialised
          (byteLimitsBlockFetch (const 0)) -- TODO: Real Bytelimits, see #1727
          timeLimitsBlockFetch
          channel
          $ blockFetchServerPeer
          $ phBlockFetchServer registry

    naTxSubmissionClient
      :: peer
      -> Channel m bytesTX
      -> m ()
    naTxSubmissionClient them channel = do
      labelThisThread "TxSubmissionClient"
      runPeerWithLimits
        (contramap (TraceLabelPeer them) ptTxSubmissionTracer)
        pcTxSubmissionCodec
        (byteLimitsTxSubmission (const 0)) -- TODO: Real Bytelimits, see #1727
        timeLimitsTxSubmission
        channel
        (txSubmissionClientPeer (phTxSubmissionClient them))

    naTxSubmissionServer
      :: peer
      -> Channel m bytesTX
      -> m ()
    naTxSubmissionServer them channel = do
      labelThisThread "TxSubmissionServer"
      runPipelinedPeerWithLimits
        (contramap (TraceLabelPeer them) ptTxSubmissionTracer)
        pcTxSubmissionCodec
        (byteLimitsTxSubmission (const 0)) -- TODO: Real Bytelimits, see #1727
        timeLimitsTxSubmission
        channel
        (txSubmissionServerPeerPipelined (phTxSubmissionServer them))

    naLocalChainSyncServer
      :: localPeer
      -> Channel m bytesLCS
      -> m ()
    naLocalChainSyncServer them channel = do
      labelThisThread "LocalChainSyncServer"
      withRegistry $ \registry ->
        runPeer
          (contramap (TraceLabelPeer them) ptLocalChainSyncTracer)
          pcLocalChainSyncCodec
          channel
          $ chainSyncServerPeer
          $ phLocalChainSyncServer registry

    naLocalTxSubmissionServer
      :: localPeer
      -> Channel m bytesLTX
      -> m ()
    naLocalTxSubmissionServer them channel = do
      labelThisThread "LocalTxSubmissionServer"
      runPeer
        (contramap (TraceLabelPeer them) ptLocalTxSubmissionTracer)
        pcLocalTxSubmissionCodec
        channel
        (localTxSubmissionServerPeer (pure phLocalTxSubmissionServer))

    naLocalStateQueryServer
      :: localPeer
      -> Channel m bytesLSQ
      -> m ()
    naLocalStateQueryServer them channel = do
      labelThisThread "LocalStateQueryServer"
      runPeer
        (contramap (TraceLabelPeer them) ptLocalStateQueryTracer)
        pcLocalStateQueryCodec
        channel
        (localStateQueryServerPeer phLocalStateQueryServer)

chainDbView :: (IOLike m, HasHeader (Header blk))
            => ChainDB m blk -> ChainDbView m blk
chainDbView chainDB = ChainDbView
  { getCurrentChain   = ChainDB.getCurrentChain   chainDB
  , getCurrentLedger  = ChainDB.getCurrentLedger  chainDB
  , getOurTip         = ChainDB.getCurrentTip     chainDB
  , getIsInvalidBlock = ChainDB.getIsInvalidBlock chainDB
  }
