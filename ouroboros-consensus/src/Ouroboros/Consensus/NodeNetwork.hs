{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Wredundant-constraints -Werror=missing-fields #-}

module Ouroboros.Consensus.NodeNetwork (
    ProtocolHandlers (..)
  , protocolHandlers
  , ProtocolCodecs (..)
  , protocolCodecsId
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

import           Control.Monad (void)
import           Data.Void (Void)

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Tracer

import           Network.Mux.Interface
import           Network.TypedProtocol.Channel
import           Network.TypedProtocol.Codec.Cbor
import           Network.TypedProtocol.Driver
import           Ouroboros.Network.NodeToClient
import           Ouroboros.Network.NodeToNode

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment (..))
import           Ouroboros.Network.Block
import           Ouroboros.Network.BlockFetch
import           Ouroboros.Network.BlockFetch.Client (BlockFetchClient,
                     blockFetchClient)
import           Ouroboros.Network.Mux
import           Ouroboros.Network.Protocol.BlockFetch.Codec (codecBlockFetchId)
import           Ouroboros.Network.Protocol.BlockFetch.Server (BlockFetchServer,
                     blockFetchServerPeer)
import           Ouroboros.Network.Protocol.BlockFetch.Type (BlockFetch (..))
import           Ouroboros.Network.Protocol.ChainSync.Client
import           Ouroboros.Network.Protocol.ChainSync.Codec (codecChainSyncId)
import           Ouroboros.Network.Protocol.ChainSync.Server
import           Ouroboros.Network.Protocol.ChainSync.Type
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
import           Ouroboros.Consensus.BlockFetchServer
import           Ouroboros.Consensus.ChainSyncClient
import           Ouroboros.Consensus.ChainSyncServer
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Node.Tracers
import           Ouroboros.Consensus.TxSubmission
import           Ouroboros.Consensus.Util.Orphans ()

import qualified Ouroboros.Storage.ChainDB.API as ChainDB

import           Ouroboros.Consensus.Node


-- | The collection of all the mini-protocol handlers provided by the consensus
-- layer.
--
-- These are used to construct the server (responder) and client (initiator)
-- sides of the node-to-node protocol bundle, and the node-to-client protocol
-- bundle.
--
data ProtocolHandlers m peer blk = ProtocolHandlers {
      phChainSyncClient
        :: TVar m (CandidateState blk)
        -> AnchoredFragment (Header blk)
        -> ChainSyncClient (Header blk) (Point blk) m Void
        -- TODO: we should consider either bundling these context paramaters
        -- into a record, or extending the protocol handler representation
        -- to support bracket-style initialisation so that we could have the
        -- closure include these and not need to be explicit about them here.

    , phChainSyncServer
        :: ChainSyncServer (Header blk) (Point blk) m ()

    -- TODO block fetch client does not have GADT view of the handlers.
    , phBlockFetchClient
        :: BlockFetchClient (Header blk) blk m ()

    , phBlockFetchServer
        :: BlockFetchServer blk m ()

    , phTxSubmissionClient
        :: TxSubmissionClient (GenTxId blk) (GenTx blk) m ()

    , phTxSubmissionServer
        :: TxSubmissionServerPipelined (GenTxId blk) (GenTx blk) m ()

     -- Handlers for the local node-to-client bundle of mini-protocols

    , phLocalChainSyncServer
        :: ChainSyncServer blk (Point blk) m ()

    , phLocalTxSubmissionServer
        :: LocalTxSubmissionServer (GenTx blk) String m ()
    }

protocolHandlers
    :: forall m blk peer.
       ( MonadSTM   m
       , MonadThrow (STM m)
       , MonadTime  m
       , MonadThrow m
       , ApplyTx blk
       , ProtocolLedgerView blk
       , Show (ApplyTxErr blk)  --TODO: consider using condense
       )
    => NodeParams m peer blk  --TODO eliminate, merge relevant into NodeKernel
    -> NodeKernel m peer blk
    -> ProtocolHandlers m peer blk
protocolHandlers NodeParams {btime, maxClockSkew, tracers, maxUnackTxs}
                 NodeKernel {getChainDB, getMempool, getNodeConfig} =
    --TODO: bundle needed NodeParams into the NodeKernel
    -- so we do not have to pass it separately
    --TODO: need to review the use of the peer id in the tracers.
    -- Curently it is not available here to use.
    ProtocolHandlers {
      phChainSyncClient =
        chainSyncClient
          (chainSyncClientTracer tracers)
          getNodeConfig
          btime
          maxClockSkew
          (ChainDB.getCurrentLedger getChainDB)
    , phChainSyncServer =
        chainSyncHeadersServer
          (chainSyncServerTracer tracers)
          getChainDB
    , phBlockFetchClient = blockFetchClient
    , phBlockFetchServer =
        blockFetchServer
          (blockFetchServerTracer tracers)
          getChainDB
    , phTxSubmissionClient =
        txSubmissionOutbound
          (txOutboundTracer tracers)
          maxUnackTxs
          (getMempoolReader getMempool)
    , phTxSubmissionServer =
        txSubmissionInbound
          (txInboundTracer tracers)
          maxUnackTxs
          (getMempoolWriter getMempool)

    , phLocalChainSyncServer =
        chainSyncBlocksServer
          (chainSyncServerTracer tracers)
          getChainDB
    , phLocalTxSubmissionServer =
        localTxSubmissionServer
          (localTxSubmissionServerTracer tracers)
          getMempool
    }


-- | Protocol codecs needed to run 'ProtocolHandlers'.
--
data ProtocolCodecs blk failure m
                    bytesCS bytesBF bytesTX
                    bytesLCS bytesLTX = ProtocolCodecs {
    pcChainSyncCodec         :: Codec (ChainSync (Header blk) (Point blk))
                                      failure m bytesCS
  , pcBlockFetchCodec        :: Codec (BlockFetch blk)
                                      failure m bytesBF
  , pcTxSubmissionCodec      :: Codec (TxSubmission (GenTxId blk) (GenTx blk))
                                      failure m bytesTX
  , pcLocalChainSyncCodec    :: Codec (ChainSync blk (Point blk))
                                      failure m bytesLCS
  , pcLocalTxSubmissionCodec :: Codec (LocalTxSubmission (GenTx blk) String)
                                      failure m bytesLTX
  }

-- | Id codecs used in tests.
--
protocolCodecsId :: Monad m
                 => ProtocolCodecs blk CodecFailure m
                      (AnyMessage (ChainSync (Header blk) (Point blk)))
                      (AnyMessage (BlockFetch blk))
                      (AnyMessage (TxSubmission (GenTxId blk) (GenTx blk)))
                      (AnyMessage (ChainSync blk (Point blk)))
                      (AnyMessage (LocalTxSubmission (GenTx blk) String))
protocolCodecsId = ProtocolCodecs {
      pcChainSyncCodec         = codecChainSyncId
    , pcBlockFetchCodec        = codecBlockFetchId
    , pcTxSubmissionCodec      = codecTxSubmissionId
    , pcLocalChainSyncCodec    = codecChainSyncId
    , pcLocalTxSubmissionCodec = codecLocalTxSubmissionId
    }

-- | A record of 'Tracer's for the different protocols.
type ProtocolTracers m peer blk failure = ProtocolTracers' peer blk failure (Tracer m)

data ProtocolTracers' peer blk failure f = ProtocolTracers {
    ptChainSyncTracer         :: f (TraceSendRecv (ChainSync (Header blk) (Point blk))     peer failure)
  , ptBlockFetchTracer        :: f (TraceSendRecv (BlockFetch blk)                         peer failure)
  , ptTxSubmissionTracer      :: f (TraceSendRecv (TxSubmission (GenTxId blk) (GenTx blk)) peer failure)
  , ptLocalChainSyncTracer    :: f (TraceSendRecv (ChainSync blk (Point blk))              peer failure)
  , ptLocalTxSubmissionTracer :: f (TraceSendRecv (LocalTxSubmission (GenTx blk) String)   peer failure)
  }

-- | Use a 'nullTracer' for each protocol.
nullProtocolTracers :: Monad m => ProtocolTracers m peer blk failure
nullProtocolTracers = ProtocolTracers {
    ptChainSyncTracer         = nullTracer
  , ptBlockFetchTracer        = nullTracer
  , ptTxSubmissionTracer      = nullTracer
  , ptLocalChainSyncTracer    = nullTracer
  , ptLocalTxSubmissionTracer = nullTracer
  }

showProtocolTracers :: ( StandardHash blk
                       , Show blk
                       , Show (Header blk)
                       , Show peer
                       , Show failure
                       , Show (GenTx blk)
                       , Show (GenTxId blk)
                       )
                    => Tracer m String -> ProtocolTracers m peer blk failure
showProtocolTracers tr = ProtocolTracers {
    ptChainSyncTracer         = showTracing tr
  , ptBlockFetchTracer        = showTracing tr
  , ptTxSubmissionTracer      = showTracing tr
  , ptLocalChainSyncTracer    = showTracing tr
  , ptLocalTxSubmissionTracer = showTracing tr
  }

-- | Consensus provides a chains sync, block fetch applications.  This data
-- type can be mapped to 'MuxApplication' under the assumption that 'bytesCS'
-- and 'bytesBF' coincide.  Using different bytes for different application is
-- useful for running different encoding on each channel in tests (identity
-- codecs).
--
data NetworkApplication m peer
                        bytesCS bytesBF bytesTX
                        bytesLCS bytesLTX a = NetworkApplication {
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
    , naLocalChainSyncServer    :: peer -> Channel m bytesLCS -> m a

      -- | Start a local transaction submission server.
    , naLocalTxSubmissionServer :: peer -> Channel m bytesLTX -> m a
    }


-- | A projection from 'NetworkApplication' to a client-side 'MuxApplication'
-- for the 'NodeToNodeProtocols'.
--
initiatorNetworkApplication
  :: NetworkApplication m peer bytes bytes bytes bytes bytes a
  -> OuroborosApplication 'InitiatorApp peer NodeToNodeProtocols m bytes a Void
initiatorNetworkApplication NetworkApplication {..} =
    OuroborosInitiatorApplication $ \them ptcl -> case ptcl of
      ChainSyncWithHeadersPtcl -> naChainSyncClient them
      BlockFetchPtcl           -> naBlockFetchClient them
      TxSubmissionPtcl         -> naTxSubmissionClient them

-- | A projection from 'NetworkApplication' to a server-side 'MuxApplication'
-- for the 'NodeToNodeProtocols'.
--
responderNetworkApplication
  :: NetworkApplication m peer bytes bytes bytes bytes bytes a
  -> AnyResponderApp peer NodeToNodeProtocols m bytes
responderNetworkApplication NetworkApplication {..} =
    AnyResponderApp $ OuroborosResponderApplication $ \them ptcl -> case ptcl of
      ChainSyncWithHeadersPtcl -> naChainSyncServer them
      BlockFetchPtcl           -> naBlockFetchServer them
      TxSubmissionPtcl         -> naTxSubmissionServer them

-- | A projection from 'NetworkApplication' to a server-side 'MuxApplication'
-- for the 'NodeToClientProtocols'.
--
localResponderNetworkApplication
  :: NetworkApplication m peer bytes bytes bytes bytes bytes a
  -> AnyResponderApp peer NodeToClientProtocols m bytes
localResponderNetworkApplication NetworkApplication {..} =
    AnyResponderApp $ OuroborosResponderApplication $ \peer  ptcl -> case ptcl of
      ChainSyncWithBlocksPtcl -> naLocalChainSyncServer peer
      LocalTxSubmissionPtcl   -> naLocalTxSubmissionServer peer


-- | Example function which creates consensus mux applications, this is useful
-- for testing but also untill we have only a single version in
-- 'NodeToNodeVersions'.
--
consensusNetworkApps
    :: forall m peer blk failure bytesCS bytesBF bytesTX bytesLCS bytesLTX.
       ( MonadAsync m
       , MonadFork m
       , MonadCatch m
       , Ord peer
       , Exception failure
       )
    => NodeKernel m peer blk
    -> ProtocolTracers m peer blk failure
    -> ProtocolCodecs blk failure m bytesCS bytesBF bytesTX bytesLCS bytesLTX
    -> ProtocolHandlers m peer blk
    -> NetworkApplication m peer bytesCS bytesBF bytesTX bytesLCS bytesLTX ()
consensusNetworkApps kernel ProtocolTracers {..} ProtocolCodecs {..} ProtocolHandlers {..} =
    NetworkApplication {
      naChainSyncClient,
      naChainSyncServer,
      naBlockFetchClient,
      naBlockFetchServer,
      naTxSubmissionClient,
      naTxSubmissionServer,
      naLocalChainSyncServer,
      naLocalTxSubmissionServer
    }
  where
    naChainSyncClient
      :: peer
      -> Channel m bytesCS
      -> m ()
    naChainSyncClient them channel =
      void $
      -- Note that it is crucial that we sync with the fetch client "outside"
      -- of registering the state for the sync client. This is needed to
      -- maintain a state invariant required by the block fetch logic: that for
      -- each candidate chain there is a corresponding block fetch client that
      -- can be used to fetch blocks for that chain.
      bracketSyncWithFetchClient
        (getFetchClientRegistry kernel) them $
      bracketChainSyncClient
          (ChainDB.getCurrentChain  (getChainDB kernel))
          (ChainDB.getCurrentLedger (getChainDB kernel))
          (getNodeCandidates kernel)
          them $ \varCandidate curChain ->
        runPeer
          ptChainSyncTracer
          pcChainSyncCodec
          them
          channel
          $ chainSyncClientPeer
          $ phChainSyncClient varCandidate curChain

    naChainSyncServer
      :: peer
      -> Channel m bytesCS
      -> m ()
    naChainSyncServer them channel =
      runPeer
        ptChainSyncTracer
        pcChainSyncCodec
        them
        channel
        $ chainSyncServerPeer
        $ phChainSyncServer

    naBlockFetchClient
      :: peer
      -> Channel m bytesBF
      -> m ()
    naBlockFetchClient them channel =
      bracketFetchClient (getFetchClientRegistry kernel) them $ \clientCtx ->
        runPipelinedPeer
          ptBlockFetchTracer
          pcBlockFetchCodec
          them
          channel
          $ phBlockFetchClient clientCtx

    naBlockFetchServer
      :: peer
      -> Channel m bytesBF
      -> m ()
    naBlockFetchServer them channel =
      runPeer
        ptBlockFetchTracer
        pcBlockFetchCodec
        them
        channel
        $ blockFetchServerPeer
        $ phBlockFetchServer

    naTxSubmissionClient
      :: peer
      -> Channel m bytesTX
      -> m ()
    naTxSubmissionClient them channel =
      runPeer
        ptTxSubmissionTracer
        pcTxSubmissionCodec
        them
        channel
        (txSubmissionClientPeer phTxSubmissionClient)

    naTxSubmissionServer
      :: peer
      -> Channel m bytesTX
      -> m ()
    naTxSubmissionServer them channel =
      runPipelinedPeer
        ptTxSubmissionTracer
        pcTxSubmissionCodec
        them
        channel
        (txSubmissionServerPeerPipelined phTxSubmissionServer)

    naLocalChainSyncServer
      :: peer
      -> Channel m bytesLCS
      -> m ()
    naLocalChainSyncServer them channel =
      runPeer
        ptLocalChainSyncTracer
        pcLocalChainSyncCodec
        them
        channel
        (chainSyncServerPeer phLocalChainSyncServer)

    naLocalTxSubmissionServer
      :: peer
      -> Channel m bytesLTX
      -> m ()
    naLocalTxSubmissionServer them channel =
      runPeer
        ptLocalTxSubmissionTracer
        pcLocalTxSubmissionCodec
        them
        channel
        (localTxSubmissionServerPeer (pure phLocalTxSubmissionServer))
