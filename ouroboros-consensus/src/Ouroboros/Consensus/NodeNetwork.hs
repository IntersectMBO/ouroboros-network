{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wredundant-constraints -Werror=missing-fields #-}

module Ouroboros.Consensus.NodeNetwork (
    ProtocolHandlers (..)
  , protocolHandlers
  , ProtocolCodecs (..)
  , protocolCodecsId
  , NetworkApplication(..)
  , consensusNetworkApps
  , muxInitiatorNetworkApplication
  , muxResponderNetworkApplication
  ) where

import           Control.Monad (void)
import           Data.Void (Void)

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Tracer

import           Network.TypedProtocol.Channel
import           Network.TypedProtocol.Driver
import           Ouroboros.Network.Codec
import           Ouroboros.Network.Mux.Interface
import           Ouroboros.Network.NodeToNode

import           Ouroboros.Network.Block
import           Ouroboros.Network.BlockFetch
import           Ouroboros.Network.BlockFetch.Client (BlockFetchClient, blockFetchClient)
import           Ouroboros.Network.Protocol.BlockFetch.Server (BlockFetchServer, blockFetchServerPeer)
import           Ouroboros.Network.Protocol.BlockFetch.Type (BlockFetch (..))
import           Ouroboros.Network.Protocol.BlockFetch.Codec (codecBlockFetchId)
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment (..))
import           Ouroboros.Network.Protocol.ChainSync.Client
import           Ouroboros.Network.Protocol.ChainSync.Server
import           Ouroboros.Network.Protocol.ChainSync.Type
import           Ouroboros.Network.Protocol.ChainSync.Codec (codecChainSyncId)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type
import           Ouroboros.Network.Protocol.LocalTxSubmission.Server
import           Ouroboros.Network.Protocol.LocalTxSubmission.Codec

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockFetchServer
import           Ouroboros.Consensus.ChainSyncClient
import           Ouroboros.Consensus.ChainSyncServer
import           Ouroboros.Consensus.TxSubmission
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Util.Condense
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
       , ProtocolLedgerView blk
       , Condense (Header blk)
       , Condense (ChainHash blk)
       , Condense peer
       , Show (ApplyTxErr blk)  --TODO: consider using condense
       )
    => NodeParams m peer blk  --TODO eliminate, merge relevant into NodeKernel
    -> NodeKernel m peer blk
    -> ProtocolHandlers m peer blk
protocolHandlers NodeParams {btime, maxClockSkew, tracer}
                 NodeKernel {getChainDB, getMempool, getNodeConfig} =
    --TODO: bundle needed NodeParams into the NodeKernel
    -- so we do not have to pass it separately
    --TODO: need to review the use of the peer id in the tracers.
    -- Curently it is not available here to use.
    ProtocolHandlers {
      phChainSyncClient =
        chainSyncClient
          (tracePrefix "CSClient" (Nothing :: Maybe peer) tracer)
          getNodeConfig
          btime
          maxClockSkew
          (ChainDB.getCurrentLedger getChainDB)
    , phChainSyncServer =
        chainSyncHeadersServer
          (tracePrefix "CSServer" (Nothing :: Maybe peer) tracer)
          getChainDB
    , phBlockFetchClient = blockFetchClient
    , phBlockFetchServer =
        blockFetchServer
          (tracePrefix "BFServer" (Nothing :: Maybe peer) tracer)
          getChainDB
    , phLocalChainSyncServer =
        chainSyncBlocksServer
          (tracePrefix "CSLocalServer" (Nothing :: Maybe peer) tracer)
          getChainDB
    , phLocalTxSubmissionServer =
        localTxSubmissionServer
          (tracePrefix "TxSubmitServer" (Nothing :: Maybe peer) tracer)
          getMempool
    }


-- | Protocol codecs needed to run 'ProtocolHandlers'.
--
data ProtocolCodecs blk failure m bytesCS bytesBF = ProtocolCodecs {
    pcChainSyncCodec  :: Codec (ChainSync (Header blk) (Point blk)) failure m bytesCS
  , pcBlockFetchCodec :: Codec (BlockFetch blk) failure m bytesBF
  }

-- | Id codecs used in tests.
--
protocolCodecsId :: Monad m
                 => ProtocolCodecs blk CodecFailure m
                      (AnyMessage (ChainSync (Header blk) (Point blk)))
                      (AnyMessage (BlockFetch blk))
protocolCodecsId = ProtocolCodecs {
      pcChainSyncCodec = codecChainSyncId
    , pcBlockFetchCodec = codecBlockFetchId
    }

-- | Consensus provides a chains sync, block fetch applications.  This data
-- type can be mapped to 'MuxApplication' under the assumption that 'bytesCS'
-- and 'bytesBF' coincide.  Using different bytes for different application is
-- useful for running different encoding on each channel in tests (identity
-- codecs).
--
data NetworkApplication m peer bytesCS bytesBF a = NetworkApplication {
      -- | Start a chain sync client that communicates with the given upstream
      -- node.
      naChainSyncClient  :: peer -> Channel m bytesCS -> m a

      -- | Start a chain sync server.
    , naChainSyncServer  :: Channel m bytesCS -> m a

      -- | Start a block fetch client that communicates with the given
      -- upstream node.
    , naBlockFetchClient :: peer -> Channel m bytesBF -> m a

      -- | Start a block fetch server server.
    , naBlockFetchServer :: Channel m bytesBF -> m a
    }


-- | A projection from 'NetworkApplication' to 'MuxApplication'.
--
muxInitiatorNetworkApplication
  :: peer
  -> NetworkApplication m peer bytes bytes a
  -> MuxApplication InitiatorApp NodeToNodeProtocols m bytes a Void
muxInitiatorNetworkApplication them NetworkApplication {..} =
    MuxInitiatorApplication $ \ptcl -> case ptcl of
      ChainSyncWithHeadersPtcl -> naChainSyncClient them
      BlockFetchPtcl           -> naBlockFetchClient them

-- | A project from 'NetworkApplication' to 'MuxApplication'.
--
muxResponderNetworkApplication
  :: NetworkApplication m peer bytes bytes a
  -> AnyMuxResponderApp NodeToNodeProtocols m bytes
muxResponderNetworkApplication NetworkApplication {..} =
    AnyMuxResponderApp $ MuxResponderApplication $ \ptcl -> case ptcl of
      ChainSyncWithHeadersPtcl -> naChainSyncServer
      BlockFetchPtcl           -> naBlockFetchServer


-- | Example function which creates consensus mux applications, this is useful
-- for testing but also untill we have only a single version in
-- 'NodeToNodeVersions'.
--
consensusNetworkApps
    :: forall m peer blk failure bytesCS bytesBF.
       ( MonadAsync m
       , MonadFork m
       , MonadCatch m
       , Ord peer
       , Exception failure
       )
    => Tracer m (TraceSendRecv (ChainSync (Header blk) (Point blk)))
    -> Tracer m (TraceSendRecv (BlockFetch blk))
    -> NodeKernel m peer blk
    -> ProtocolCodecs blk failure m bytesCS bytesBF
    -> ProtocolHandlers m peer blk
    -> NetworkApplication m peer bytesCS bytesBF ()

consensusNetworkApps traceChainSync traceBlockFetch kernel
                     ProtocolCodecs {..} ProtocolHandlers {..} =
    NetworkApplication {
      naChainSyncClient = naChainSyncClient,
      naChainSyncServer,
      naBlockFetchClient = naBlockFetchClient,
      naBlockFetchServer
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
          traceChainSync
          pcChainSyncCodec
          channel
          $ chainSyncClientPeer
          $ phChainSyncClient varCandidate curChain

    naChainSyncServer
      :: Channel m bytesCS
      -> m ()
    naChainSyncServer channel =
      runPeer
        traceChainSync
        pcChainSyncCodec
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
          traceBlockFetch
          pcBlockFetchCodec
          channel
          $ phBlockFetchClient clientCtx

    naBlockFetchServer
      :: Channel m bytesBF
      -> m ()
    naBlockFetchServer channel =
      runPeer
        traceBlockFetch
        pcBlockFetchCodec
        channel
        $ blockFetchServerPeer
        $ phBlockFetchServer
