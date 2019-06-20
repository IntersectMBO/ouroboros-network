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
import           Ouroboros.Network.Protocol.ChainSync.Client
import           Ouroboros.Network.Protocol.ChainSync.Server
import           Ouroboros.Network.Protocol.ChainSync.Type
import           Ouroboros.Network.Protocol.ChainSync.Codec (codecChainSyncId)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockFetchServer
import           Ouroboros.Consensus.ChainSyncClient
import           Ouroboros.Consensus.ChainSyncServer
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()

import qualified Ouroboros.Storage.ChainDB.API as ChainDB

import           Ouroboros.Consensus.Node


-- | Protocol handlers as provided by the consensus.
--
data ProtocolHandlers m peer blk = ProtocolHandlers {
      phChainSyncClient
        :: peer
        -> ChainSyncClient (Header blk) (Point blk) m Void

    , phChainSyncServer
        :: ChainSyncServer (Header blk) (Point blk) m ()

    -- TODO block fetch client does not have GADT view of the handlers.
    , phBlockFetchClient
        :: BlockFetchClient (Header blk) blk m ()

    , phBlockFetchServer
        :: BlockFetchServer blk m ()
    }

protocolHandlers
    :: forall m blk peer.
       ( MonadSTM   m
       , MonadThrow (STM m)
       , MonadTime  m
       , MonadThrow m
       , ProtocolLedgerView blk
       , Ord peer
       , Condense (Header blk)
       , Condense (ChainHash blk)
       , Condense peer
       )
    => NodeParams m peer blk
    -> NodeKernel m peer blk
    -> ProtocolHandlers m peer blk
protocolHandlers NodeParams {..} kernel = ProtocolHandlers {
      phChainSyncClient = \them ->
        chainSyncClient
          (tracePrefix "CSClient" (Just them) tracer)
          cfg
          btime
          maxClockSkew
          (ChainDB.getCurrentChain chainDB)
          (ChainDB.getCurrentLedger chainDB)
          (getNodeCandidates kernel)
          them
    , phChainSyncServer =
        chainSyncServer
          (tracePrefix "CSServer" (Nothing :: Maybe peer) tracer)
          chainDB
    , phBlockFetchClient = blockFetchClient
    , phBlockFetchServer =
        blockFetchServer
          (tracePrefix "BFServer" (Nothing :: Maybe peer) tracer)
          chainDB
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
      void $ runPeer
        traceChainSync
        pcChainSyncCodec
        channel
        $ chainSyncClientPeer
        $ phChainSyncClient them

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
