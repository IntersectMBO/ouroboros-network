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

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
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
import           Ouroboros.Network.Protocol.TxSubmission.Client
import           Ouroboros.Network.Protocol.TxSubmission.Codec
import           Ouroboros.Network.Protocol.TxSubmission.Server
import           Ouroboros.Network.Protocol.TxSubmission.Type
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
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Ouroboros.Consensus.Storage.ChainDB.Serialisation
                     (ReconstructNestedCtxt, SerialisedHeader)

{-------------------------------------------------------------------------------
  Handlers
-------------------------------------------------------------------------------}

-- | Protocol handlers for node-to-node (remote) communication
data Handlers m peer blk = Handlers {
      hChainSyncClient
        :: BlockNodeToNodeVersion blk
        -> StrictTVar m (AnchoredFragment (Header blk))
        -> ChainSyncClientPipelined (Header blk) (Tip blk) m Void
        -- TODO: we should consider either bundling these context parameters
        -- into a record, or extending the protocol handler representation
        -- to support bracket-style initialisation so that we could have the
        -- closure include these and not need to be explicit about them here.

    , hChainSyncServer
        :: BlockNodeToNodeVersion blk
        -> ResourceRegistry m
        -> ChainSyncServer (SerialisedHeader blk) (Tip blk) m ()

    -- TODO block fetch client does not have GADT view of the handlers.
    , hBlockFetchClient
        :: BlockNodeToNodeVersion blk
        -> BlockFetchClient (Header blk) blk m ()

    , hBlockFetchServer
        :: BlockNodeToNodeVersion blk
        -> ResourceRegistry m
        -> BlockFetchServer (Serialised blk) m ()

    , hTxSubmissionClient
        :: BlockNodeToNodeVersion blk
        -> peer
        -> TxSubmissionClient (GenTxId blk) (GenTx blk) m ()

    , hTxSubmissionServer
        :: BlockNodeToNodeVersion blk
        -> peer
        -> TxSubmissionServerPipelined (GenTxId blk) (GenTx blk) m ()
    }

mkHandlers
  :: forall m blk remotePeer localPeer.
     ( IOLike m
     , LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     , LedgerSupportsProtocol blk
     , Serialise (HeaderHash blk)
     , ReconstructNestedCtxt Header blk
     , TranslateNetworkProtocolVersion blk
     )
  => NodeArgs   m remotePeer localPeer blk
  -> NodeKernel m remotePeer localPeer blk
  -> Handlers   m remotePeer           blk
mkHandlers
      NodeArgs {miniProtocolParameters}
      NodeKernel {getChainDB, getMempool, getTopLevelConfig, getTracers = tracers} =
    Handlers {
        hChainSyncClient =
          chainSyncClient
            (pipelineDecisionLowHighMark
              (chainSyncPipeliningLowMark  miniProtocolParameters)
              (chainSyncPipeliningHighMark miniProtocolParameters))
            (Node.chainSyncClientTracer tracers)
            getTopLevelConfig
            (defaultChainDbView getChainDB)
      , hChainSyncServer =
          chainSyncHeadersServer
            (Node.chainSyncServerHeaderTracer tracers)
            getChainDB
      , hBlockFetchClient = \version ->
          blockFetchClient
            (nodeToNodeProtocolVersion (Proxy :: Proxy blk) version)
      , hBlockFetchServer = \version ->
          blockFetchServer
            (Node.blockFetchServerTracer tracers)
            getChainDB
            version
      , hTxSubmissionClient = \version peer ->
          txSubmissionOutbound
            (contramap (TraceLabelPeer peer) (Node.txOutboundTracer tracers))
            (txSubmissionMaxUnacked miniProtocolParameters)
            (getMempoolReader getMempool)
            (nodeToNodeProtocolVersion (Proxy :: Proxy blk) version)
      , hTxSubmissionServer = \version peer ->
          txSubmissionInbound
            (contramap (TraceLabelPeer peer) (Node.txInboundTracer tracers))
            (txSubmissionMaxUnacked miniProtocolParameters)
            (getMempoolReader getMempool)
            (getMempoolWriter getMempool)
            (nodeToNodeProtocolVersion (Proxy :: Proxy blk) version)
      }

{-------------------------------------------------------------------------------
  Codecs
-------------------------------------------------------------------------------}

-- | Node-to-node protocol codecs needed to run 'Handlers'.
data Codecs blk e m bCS bSCS bBF bSBF bTX = Codecs {
      cChainSyncCodec            :: Codec (ChainSync (Header blk) (Tip blk))           e m bCS
    , cChainSyncCodecSerialised  :: Codec (ChainSync (SerialisedHeader blk) (Tip blk)) e m bSCS
    , cBlockFetchCodec           :: Codec (BlockFetch blk)                             e m bBF
    , cBlockFetchCodecSerialised :: Codec (BlockFetch (Serialised blk))                e m bSBF
    , cTxSubmissionCodec         :: Codec (TxSubmission (GenTxId blk) (GenTx blk))     e m bTX
    }

-- | Protocol codecs for the node-to-node protocols
defaultCodecs :: forall m blk. (IOLike m, SerialiseNodeToNodeConstraints blk)
              => CodecConfig       blk
              -> BlockNodeToNodeVersion blk
              -> Codecs blk DeserialiseFailure m
                   ByteString ByteString ByteString ByteString ByteString
defaultCodecs ccfg version = Codecs {
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
          (encodeRawHash p)
          (decodeRawHash p)

    , cBlockFetchCodecSerialised =
        codecBlockFetch
          enc
          dec
          (encodeRawHash p)
          (decodeRawHash p)

    , cTxSubmissionCodec =
        codecTxSubmission
          enc
          dec
          enc
          dec
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
                    (AnyMessage (ChainSync (Header blk) (Tip blk)))
                    (AnyMessage (ChainSync (SerialisedHeader blk) (Tip blk)))
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
    , tChainSyncSerialisedTracer  :: f (TraceLabelPeer peer (TraceSendRecv (ChainSync (SerialisedHeader blk) (Tip blk))))
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
               , HasNestedContent Header blk
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
data Apps m peer blk bCS bBF bTX a = Apps {
      -- | Start a chain sync client that communicates with the given upstream
      -- node.
      aChainSyncClient    :: BlockNodeToNodeVersion blk -> peer -> Channel m bCS -> m a

      -- | Start a chain sync server.
    , aChainSyncServer    :: BlockNodeToNodeVersion blk -> peer -> Channel m bCS -> m a

      -- | Start a block fetch client that communicates with the given
      -- upstream node.
    , aBlockFetchClient   :: BlockNodeToNodeVersion blk -> peer -> Channel m bBF -> m a

      -- | Start a block fetch server.
    , aBlockFetchServer   :: BlockNodeToNodeVersion blk -> peer -> Channel m bBF -> m a

      -- | Start a transaction submission client that communicates with the
      -- given upstream node.
    , aTxSubmissionClient :: BlockNodeToNodeVersion blk -> peer -> Channel m bTX -> m a

      -- | Start a transaction submission server.
    , aTxSubmissionServer :: BlockNodeToNodeVersion blk -> peer -> Channel m bTX -> m a
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
  -> Apps m remotePeer blk bCS bBF bTX ()
mkApps kernel Tracers {..} Codecs {..} chainSyncTimeout Handlers {..} =
    Apps {..}
  where
    aChainSyncClient
      :: BlockNodeToNodeVersion blk
      -> remotePeer
      -> Channel m bCS
      -> m ()
    aChainSyncClient version them channel = do
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
            $ hChainSyncClient version varCandidate

    aChainSyncServer
      :: BlockNodeToNodeVersion blk
      -> remotePeer
      -> Channel m bCS
      -> m ()
    aChainSyncServer version them channel = do
      labelThisThread "ChainSyncServer"
      withRegistry $ \registry ->
        runPeerWithLimits
          (contramap (TraceLabelPeer them) tChainSyncSerialisedTracer)
          cChainSyncCodecSerialised
          (byteLimitsChainSync (const 0)) -- TODO: Real Bytelimits, see #1727
          (timeLimitsChainSync chainSyncTimeout)
          channel
          $ chainSyncServerPeer
          $ hChainSyncServer version registry

    aBlockFetchClient
      :: BlockNodeToNodeVersion blk
      -> remotePeer
      -> Channel m bBF
      -> m ()
    aBlockFetchClient version them channel = do
      labelThisThread "BlockFetchClient"
      bracketFetchClient (getFetchClientRegistry kernel) them $ \clientCtx ->
        runPipelinedPeerWithLimits
          (contramap (TraceLabelPeer them) tBlockFetchTracer)
          cBlockFetchCodec
          (byteLimitsBlockFetch (const 0)) -- TODO: Real Bytelimits, see #1727
          timeLimitsBlockFetch
          channel
          $ hBlockFetchClient version clientCtx

    aBlockFetchServer
      :: BlockNodeToNodeVersion blk
      -> remotePeer
      -> Channel m bBF
      -> m ()
    aBlockFetchServer version them channel = do
      labelThisThread "BlockFetchServer"
      withRegistry $ \registry ->
        runPeerWithLimits
          (contramap (TraceLabelPeer them) tBlockFetchSerialisedTracer)
          cBlockFetchCodecSerialised
          (byteLimitsBlockFetch (const 0)) -- TODO: Real Bytelimits, see #1727
          timeLimitsBlockFetch
          channel
          $ blockFetchServerPeer
          $ hBlockFetchServer version registry

    aTxSubmissionClient
      :: BlockNodeToNodeVersion blk
      -> remotePeer
      -> Channel m bTX
      -> m ()
    aTxSubmissionClient version them channel = do
      labelThisThread "TxSubmissionClient"
      runPeerWithLimits
        (contramap (TraceLabelPeer them) tTxSubmissionTracer)
        cTxSubmissionCodec
        (byteLimitsTxSubmission (const 0)) -- TODO: Real Bytelimits, see #1727
        timeLimitsTxSubmission
        channel
        (txSubmissionClientPeer (hTxSubmissionClient version them))

    aTxSubmissionServer
      :: BlockNodeToNodeVersion blk
      -> remotePeer
      -> Channel m bTX
      -> m ()
    aTxSubmissionServer version them channel = do
      labelThisThread "TxSubmissionServer"
      runPipelinedPeerWithLimits
        (contramap (TraceLabelPeer them) tTxSubmissionTracer)
        cTxSubmissionCodec
        (byteLimitsTxSubmission (const 0)) -- TODO: Real Bytelimits, see #1727
        timeLimitsTxSubmission
        channel
        (txSubmissionServerPeerPipelined (hTxSubmissionServer version them))

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
  -> BlockNodeToNodeVersion blk
  -> Apps m (ConnectionId peer) blk b b b a
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
      (\them _shouldStopSTM -> NodeToNodeProtocols {
          chainSyncProtocol =
            (InitiatorProtocolOnly (MuxPeerRaw (aChainSyncClient version them))),
          blockFetchProtocol =
            (InitiatorProtocolOnly (MuxPeerRaw (aBlockFetchClient version them))),
          txSubmissionProtocol =
            (InitiatorProtocolOnly (MuxPeerRaw (aTxSubmissionClient version them)))
        })

-- | A projection from 'NetworkApplication' to a server-side
-- 'OuroborosApplication' for the node-to-node protocols.
--
-- See 'initiatorNetworkApplication' for rationale for the @_version@ arg.
responder
  :: MiniProtocolParameters
  -> BlockNodeToNodeVersion blk
  -> Apps m (ConnectionId peer) blk b b b a
  -> OuroborosApplication 'ResponderMode peer b m Void a
responder miniProtocolParameters version Apps {..} =
    nodeToNodeProtocols
      miniProtocolParameters
      (\them _shouldStopSTM -> NodeToNodeProtocols {
          chainSyncProtocol =
            (ResponderProtocolOnly (MuxPeerRaw (aChainSyncServer version them))),
          blockFetchProtocol =
            (ResponderProtocolOnly (MuxPeerRaw (aBlockFetchServer version them))),
          txSubmissionProtocol =
            (ResponderProtocolOnly (MuxPeerRaw (aTxSubmissionServer version them)))
        })
