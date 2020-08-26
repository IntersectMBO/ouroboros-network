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
    -- * Re-exports
  , ChainSyncTimeout (..)
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Codec.Serialise (Serialise)
import           Control.Monad (forever)
import           Control.Monad.Class.MonadTimer (MonadTimer)
import           Control.Tracer
import           Data.ByteString.Lazy (ByteString)
import           Data.Map.Strict (Map)
import           Data.Void (Void)

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment (..))
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
import           Ouroboros.Network.Protocol.TxSubmission.Client
import           Ouroboros.Network.Protocol.TxSubmission.Codec
import           Ouroboros.Network.Protocol.TxSubmission.Server
import           Ouroboros.Network.Protocol.TxSubmission.Type
import           Ouroboros.Network.TxSubmission.Inbound
import           Ouroboros.Network.TxSubmission.Outbound
import           Ouroboros.Network.Util.ShowProxy

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
        :: NodeToNodeVersion
        -> StrictTVar m (AnchoredFragment (Header blk))
        -> ChainSyncClientPipelined (Header blk) (Tip blk) m ChainSyncClientResult
        -- TODO: we should consider either bundling these context parameters
        -- into a record, or extending the protocol handler representation
        -- to support bracket-style initialisation so that we could have the
        -- closure include these and not need to be explicit about them here.

    , hChainSyncServer
        :: NodeToNodeVersion
        -> ResourceRegistry m
        -> ChainSyncServer (SerialisedHeader blk) (Tip blk) m ()

    -- TODO block fetch client does not have GADT view of the handlers.
    , hBlockFetchClient
        :: NodeToNodeVersion
        -> BlockFetchClient (Header blk) blk m ()

    , hBlockFetchServer
        :: NodeToNodeVersion
        -> ResourceRegistry m
        -> BlockFetchServer (Serialised blk) m ()

    , hTxSubmissionClient
        :: NodeToNodeVersion
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
     , MonadTimer m
     , LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     , LedgerSupportsProtocol blk
     , Serialise (HeaderHash blk)
     , ReconstructNestedCtxt Header blk
     , Ord remotePeer
     )
  => NodeArgs   m remotePeer localPeer blk
  -> NodeKernel m remotePeer localPeer blk
  -> Handlers   m remotePeer           blk
mkHandlers
      NodeArgs {keepAliveRng, miniProtocolParameters}
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
      , hBlockFetchClient =
          blockFetchClient
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
            version
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
data Codecs blk e m bCS bSCS bBF bSBF bTX bKA = Codecs {
      cChainSyncCodec            :: Codec (ChainSync (Header blk) (Tip blk))           e m bCS
    , cChainSyncCodecSerialised  :: Codec (ChainSync (SerialisedHeader blk) (Tip blk)) e m bSCS
    , cBlockFetchCodec           :: Codec (BlockFetch blk)                             e m bBF
    , cBlockFetchCodecSerialised :: Codec (BlockFetch (Serialised blk))                e m bSBF
    , cTxSubmissionCodec         :: Codec (TxSubmission (GenTxId blk) (GenTx blk))     e m bTX
    , cKeepAliveCodec            :: Codec KeepAlive                                    e m bKA
    }

-- | Protocol codecs for the node-to-node protocols
defaultCodecs :: forall m blk. (IOLike m, SerialiseNodeToNodeConstraints blk)
              => CodecConfig       blk
              -> BlockNodeToNodeVersion blk
              -> Codecs blk DeserialiseFailure m
                   ByteString ByteString ByteString ByteString ByteString ByteString
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

    , cKeepAliveCodec =
        codecKeepAlive
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
                    (AnyMessage KeepAlive)
identityCodecs = Codecs {
      cChainSyncCodec            = codecChainSyncId
    , cChainSyncCodecSerialised  = codecChainSyncId
    , cBlockFetchCodec           = codecBlockFetchId
    , cBlockFetchCodecSerialised = codecBlockFetchId
    , cTxSubmissionCodec         = codecTxSubmissionId
    , cKeepAliveCodec            = codecKeepAliveId
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
data Apps m peer bCS bBF bTX bKA a = Apps {
      -- | Start a chain sync client that communicates with the given upstream
      -- node.
      aChainSyncClient    :: NodeToNodeVersion -> peer -> Channel m bCS -> m (a, Maybe bCS)

      -- | Start a chain sync server.
    , aChainSyncServer    :: NodeToNodeVersion -> peer -> Channel m bCS -> m (a, Maybe bCS)

      -- | Start a block fetch client that communicates with the given
      -- upstream node.
    , aBlockFetchClient   :: NodeToNodeVersion -> peer -> Channel m bBF -> m (a, Maybe bBF)

      -- | Start a block fetch server.
    , aBlockFetchServer   :: NodeToNodeVersion -> peer -> Channel m bBF -> m (a, Maybe bBF)

      -- | Start a transaction submission client that communicates with the
      -- given upstream node.
    , aTxSubmissionClient :: NodeToNodeVersion -> peer -> Channel m bTX -> m (a, Maybe bTX)

      -- | Start a transaction submission server.
    , aTxSubmissionServer :: NodeToNodeVersion -> peer -> Channel m bTX -> m (a, Maybe bTX)

      -- | Start a keep-alive client.
    , aKeepAliveClient :: NodeToNodeVersion -> peer -> Channel m bKA -> m (a, Maybe bKA)

      -- | Start a keep-alive server.
    , aKeepAliveServer :: NodeToNodeVersion -> peer -> Channel m bKA -> m (a, Maybe bKA)
    }

-- | Construct the 'NetworkApplication' for the node-to-node protocols
mkApps
  :: forall m remotePeer localPeer blk e bCS bBF bTX bKA.
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
  -> Codecs blk e m bCS bCS bBF bBF bTX bKA
  -> m ChainSyncTimeout
  -> Handlers m remotePeer blk
  -> Apps m remotePeer bCS bBF bTX bKA ()
mkApps kernel Tracers {..} Codecs {..} genChainSyncTimeout Handlers {..} =
    Apps {..}
  where
    aChainSyncClient
      :: NodeToNodeVersion
      -> remotePeer
      -> Channel m bCS
      -> m ((), Maybe bCS)
    aChainSyncClient version them channel = do
      labelThisThread "ChainSyncClient"
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
            them $ \varCandidate -> do
              chainSyncTimeout <- genChainSyncTimeout
              (_, trailing) <-
                runPipelinedPeerWithLimits
                  (contramap (TraceLabelPeer them) tChainSyncTracer)
                  cChainSyncCodec
                  (byteLimitsChainSync (const 0)) -- TODO: Real Bytelimits, see #1727
                  (timeLimitsChainSync chainSyncTimeout)
                  channel
                  $ chainSyncClientPeerPipelined
                  $ hChainSyncClient version varCandidate
              return ((), trailing)

    aChainSyncServer
      :: NodeToNodeVersion
      -> remotePeer
      -> Channel m bCS
      -> m ((), Maybe bCS)
    aChainSyncServer version them channel = do
      labelThisThread "ChainSyncServer"
      withRegistry $ \registry -> do
        chainSyncTimeout <- genChainSyncTimeout
        runPeerWithLimits
          (contramap (TraceLabelPeer them) tChainSyncSerialisedTracer)
          cChainSyncCodecSerialised
          (byteLimitsChainSync (const 0)) -- TODO: Real Bytelimits, see #1727
          (timeLimitsChainSync chainSyncTimeout)
          channel
          $ chainSyncServerPeer
          $ hChainSyncServer version registry

    aBlockFetchClient
      :: NodeToNodeVersion
      -> remotePeer
      -> Channel m bBF
      -> m ((), Maybe bBF)
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
      :: NodeToNodeVersion
      -> remotePeer
      -> Channel m bBF
      -> m ((), Maybe bBF)
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
      :: NodeToNodeVersion
      -> remotePeer
      -> Channel m bTX
      -> m ((), Maybe bTX)
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
      :: NodeToNodeVersion
      -> remotePeer
      -> Channel m bTX
      -> m ((), Maybe bTX)
    aTxSubmissionServer version them channel = do
      labelThisThread "TxSubmissionServer"
      runPipelinedPeerWithLimits
        (contramap (TraceLabelPeer them) tTxSubmissionTracer)
        cTxSubmissionCodec
        (byteLimitsTxSubmission (const 0)) -- TODO: Real Bytelimits, see #1727
        timeLimitsTxSubmission
        channel
        (txSubmissionServerPeerPipelined (hTxSubmissionServer version them))

    aKeepAliveClient
      :: NodeToNodeVersion
      -> remotePeer
      -> Channel m bKA
      -> m ((), Maybe bKA)
    aKeepAliveClient version them channel = do
      labelThisThread "KeepAliveClient"
      let kacApp = case version of
                     -- Version 1 doesn't support keep alive protocol but Blockfetch
                     -- still requires a PeerGSV per peer.
                     NodeToNodeV_1 -> \_ -> forever (threadDelay 1000) >> return ((), Nothing)
                     NodeToNodeV_2 -> \_ -> forever (threadDelay 1000) >> return ((), Nothing)
                     _             -> \dqCtx -> do
                       runPeerWithLimits
                         nullTracer
                         cKeepAliveCodec
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
    aKeepAliveServer _version _them channel = do
      labelThisThread "KeepAliveServer"
      runPeerWithLimits
        nullTracer
        cKeepAliveCodec
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
  -> Apps m (ConnectionId peer) b b b b a
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
            (InitiatorProtocolOnly (MuxPeerRaw (aTxSubmissionClient version them))),
          keepAliveProtocol =
            (InitiatorProtocolOnly (MuxPeerRaw (aKeepAliveClient version them)))
        })
      version

-- | A projection from 'NetworkApplication' to a server-side
-- 'OuroborosApplication' for the node-to-node protocols.
--
-- See 'initiatorNetworkApplication' for rationale for the @_version@ arg.
responder
  :: MiniProtocolParameters
  -> NodeToNodeVersion
  -> Apps m (ConnectionId peer) b b b b a
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
            (ResponderProtocolOnly (MuxPeerRaw (aTxSubmissionServer version them))),
          keepAliveProtocol =
            (ResponderProtocolOnly (MuxPeerRaw (aKeepAliveServer version them)))
        })
      version
