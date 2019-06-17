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
    NetworkApplication(..)
  , consensusNetworkApps
  , muxInitiatorNetworkApplication
  , muxResponderNetworkApplication
  , SharedState (..)
  , runSharedState
  ) where

import           Control.Monad (void)
import           Data.Void (Void)
import qualified Data.Map.Strict as Map

import           Control.Monad.Class.MonadAsync
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
import           Ouroboros.Network.BlockFetch.Client (blockFetchClient)
import           Ouroboros.Network.Protocol.BlockFetch.Server (blockFetchServerPeer)
import           Ouroboros.Network.Protocol.BlockFetch.Type (BlockFetch)
import           Ouroboros.Network.Protocol.ChainSync.Client
import           Ouroboros.Network.Protocol.ChainSync.Server
import           Ouroboros.Network.Protocol.ChainSync.Type

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockFetchServer
import           Ouroboros.Consensus.ChainSyncClient
import           Ouroboros.Consensus.ChainSyncServer
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()

import qualified Ouroboros.Storage.ChainDB.API as ChainDB

import           Ouroboros.Consensus.Node


data SharedState m s a = SharedState (m s) (s -> a)

instance Functor m => Functor (SharedState m s) where
    fmap f (SharedState ms g) = SharedState ms (f . g)

-- A left biased semigroup instance.
--
-- TODO: needing this instance is another sign that we should either fix
-- consensus client applications or generalise mux.  It is lawful, but it
-- brings the arbitrary choice of being left biased.
--
instance Semigroup a => Semigroup (SharedState m s a) where
    SharedState ms f <> SharedState _ g = SharedState ms (f <> g)

runSharedState :: Monad m => SharedState m s a -> m a
runSharedState (SharedState ms f) = ms >>= pure . f


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
       , MonadCatch m
       , MonadThrow (STM m)
       , MonadTime  m
       , ProtocolLedgerView blk
       , Condense (Header blk)
       , Condense (ChainHash blk)
       , Ord peer
       , Condense peer
       , Exception failure
       )
    => Tracer m (TraceSendRecv (ChainSync (Header blk) (Point blk)))
    -> Tracer m (TraceSendRecv (BlockFetch blk))
    -> Codec (ChainSync (Header blk) (Point blk)) failure m bytesCS
    -> Codec (BlockFetch blk) failure m bytesBF
    -> NodeParams m peer blk
    -> NodeKernel m peer blk
    -- TODO: SharedState is created before version negotiation starts
    -- (which allocates resources early) and is consumed after mux started,
    -- this is not ideal.  Currently mux layer does not support custom monadic
    -- expression before it starts; we either need to fix the client side of
    -- block fetch & chain sync protocols or generalise mux.  SharedState here
    -- makes additional problem: to build version dictionary
    -- from singletons we need to use the left biased semigroup instance of
    -- 'SharedState'.
    -> SharedState m (TMVar m ())
        (NetworkApplication m peer bytesCS bytesBF ())

consensusNetworkApps traceChainSync traceBlockFetch codecChainSync codecBlockFetch NodeParams {..} kernel =
    SharedState @m newEmptyTMVarM
      $ \clientRegistered ->
          NetworkApplication {
              naChainSyncClient = naChainSyncClient clientRegistered,
              naChainSyncServer,
              naBlockFetchClient = naBlockFetchClient clientRegistered,
              naBlockFetchServer
            }
  where
    naChainSyncClient
      :: TMVar m ()
      -> peer
      -> Channel m bytesCS
      -> m ()
    naChainSyncClient clientRegistered them channel = do
      -- The block fetch logic thread in the background wants there to be a
      -- block fetch client thread for each chain sync candidate it sees. So
      -- start the chain sync client after the block fetch thread was
      -- registered to make sure it never sees a chain sync candidate without
      -- a corresponding block fetch client thread.
      atomically $ takeTMVar clientRegistered

      void $ runPeer
        traceChainSync
        codecChainSync
        channel
        $ chainSyncClientPeer
        $ chainSyncClient
            (tracePrefix "CSClient" (Just them) tracer)
            cfg
            btime
            maxClockSkew
            (ChainDB.getCurrentChain chainDB)
            (ChainDB.getCurrentLedger chainDB)
            (getNodeCandidates kernel)
            them

    naChainSyncServer
      :: Channel m bytesCS
      -> m ()
    naChainSyncServer channel =
      runPeer
        traceChainSync
        codecChainSync
        channel
        $ chainSyncServerPeer
        $ chainSyncServer
          (tracePrefix "CSServer" (Nothing :: Maybe peer) tracer)
          chainDB

    prunePeerSTM
      :: peer
      -> STM m ()
    prunePeerSTM peer =
      modifyTVar' (getNodeCandidates kernel) (Map.delete peer)

    naBlockFetchClient
      :: TMVar m ()
      -> peer
      -> Channel m bytesBF
      -> m ()
    naBlockFetchClient clientRegistered them channel =
      bracketFetchClient (getFetchClientRegistry kernel) prunePeerSTM them $ \clientCtx -> do
        atomically $ putTMVar clientRegistered ()

        runPipelinedPeer
          traceBlockFetch
          codecBlockFetch
          channel
          $ blockFetchClient clientCtx

    naBlockFetchServer
      :: Channel m bytesBF
      -> m ()
    naBlockFetchServer channel =
      runPeer
        traceBlockFetch
        codecBlockFetch
        channel
        $ blockFetchServerPeer
        $ blockFetchServer
          (tracePrefix "BFServer" (Nothing :: Maybe peer) tracer)
          chainDB
