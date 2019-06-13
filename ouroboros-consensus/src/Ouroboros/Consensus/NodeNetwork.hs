{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wredundant-constraints -Werror=missing-fields #-}

module Ouroboros.Consensus.NodeNetwork (
    NetworkProvides(..)
  , NodeComms(..)
  , initNetworkLayer
  ) where

import           Control.Monad (void)
import           Data.Functor.Contravariant (contramap)

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork (MonadFork)
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Tracer

import           Network.TypedProtocol.Driver
import           Ouroboros.Network.Channel as Network
import           Ouroboros.Network.Codec

import           Ouroboros.Network.Block
import           Ouroboros.Network.BlockFetch
import           Ouroboros.Network.BlockFetch.Client (BlockFetchClient,
                     blockFetchClient)
import           Ouroboros.Network.Protocol.BlockFetch.Server
                     (BlockFetchServer (..), blockFetchServerPeer)
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
import           Ouroboros.Consensus.Util.ThreadRegistry

import qualified Ouroboros.Storage.ChainDB.API as ChainDB

import           Ouroboros.Consensus.Node


{-------------------------------------------------------------------------------
  Network layer
-------------------------------------------------------------------------------}

-- TODO something nicer than eCS, eBF, bytesCS, and bytesBF. Mux them over one
-- channel.
data NetworkProvides m up blk = NetworkProvides {
      -- | Notify network layer of new upstream node
      --
      -- NOTE: Eventually it will be the responsibility of the network layer
      -- itself to register and deregister peers.
      addUpstream   :: forall eCS eBF bytesCS bytesBF.
                       (Exception eCS, Exception eBF)
                    => up
                    -> NodeComms m (ChainSync (Header blk) (Point blk)) eCS bytesCS
                       -- Communication for the Chain Sync protocol
                    -> NodeComms m (BlockFetch blk)                     eBF bytesBF
                       -- Communication for the Block Fetch protocol
                    -> m ()

      -- | Notify network layer of a new downstream node
      --
      -- NOTE: Eventually it will be the responsibility of the network layer
      -- itself to register and deregister peers.p
    , addDownstream :: forall eCS eBF bytesCS bytesBF.
                       (Exception eCS, Exception eBF)
                    => NodeComms m (ChainSync (Header blk) (Point blk)) eCS bytesCS
                       -- Communication for the Chain Sync protocol
                    -> NodeComms m (BlockFetch blk)                     eBF bytesBF
                       -- Communication for the Block Fetch protocol
                    -> m ()
    }


-- | Required by the network layer to initiate comms to a new node
data NodeComms m ps e bytes = NodeComms {
      -- | Codec used for the protocol
      ncCodec    :: Codec ps e m bytes

      -- | Construct a channel to the node
      --
      -- This is in CPS style to allow for resource allocation.
    , ncWithChan :: forall a. (Channel m bytes -> m a) -> m a
    }


initNetworkLayer
    :: forall m up blk.
       ( MonadSTM   m
       , MonadAsync m
       , MonadFork  m
       , MonadCatch m
       , MonadMask  m
       , MonadThrow (STM m)
       , MonadTime  m
       , Ord up
       , ProtocolLedgerView blk
       , TraceConstraints up blk
       )
    => NodeParams m up blk
    -> NodeKernel m up blk
    -> NetworkProvides m up blk
initNetworkLayer NodeParams {..} NodeKernel{..} = NetworkProvides {..}
  where
    addDownstream :: (Exception eCS, Exception eBF)
                  => NodeComms m (ChainSync (Header blk) (Point blk)) eCS bytesCS
                  -> NodeComms m (BlockFetch blk)                     eBF bytesBF
                  -> m ()
    addDownstream ncCS ncBF = do
      -- TODO use subregistry here?
      let NodeComms csCodec csWithChan = ncCS
          NodeComms bfCodec bfWithChan = ncBF
      void $ forkLinked threadRegistry $ bfWithChan $ \chan ->
        runPeer nullTracer bfCodec chan $
          blockFetchServerPeer nrBlockFetchServer
      void $ forkLinked threadRegistry $ csWithChan $ \chan ->
        runPeer nullTracer csCodec chan $
          chainSyncServerPeer nrChainSyncServer

    addUpstream :: (Exception eCS, Exception eBF)
                => up
                -> NodeComms m (ChainSync (Header blk) (Point blk)) eCS bytesCS
                -> NodeComms m (BlockFetch blk)                     eBF bytesBF
                -> m ()
    addUpstream up ncCS ncBF = do
      -- TODO use subregistry here?
      let NodeComms csCodec csWithChan = ncCS
          NodeComms bfCodec bfWithChan = ncBF

      clientRegistered <- newEmptyTMVarM

      void $ forkLinked threadRegistry $ bfWithChan $ \chan ->
        bracketFetchClient getFetchClientRegistry up $ \clientCtx -> do
          atomically $ putTMVar clientRegistered ()
          runPipelinedPeer nullTracer bfCodec chan $
            nrBlockFetchClient clientCtx

      -- The block fetch logic thread in the background wants there to be a
      -- block fetch client thread for each chain sync candidate it sees. So
      -- start the chain sync client after the block fetch thread was
      -- registered to make sure it never sees a chain sync candidate without
      -- a corresponding block fetch client thread.
      atomically $ takeTMVar clientRegistered

      void $ forkNonTerminating threadRegistry $ csWithChan $ \chan ->
        runPeer nullTracer csCodec chan $
          chainSyncClientPeer (nrChainSyncClient up)

    nrChainSyncClient :: up -> Consensus ChainSyncClient blk m
    nrChainSyncClient up = chainSyncClient
      (tracePrefix "CSClient" (Just up))
      cfg
      btime
      maxClockSkew
      (ChainDB.getCurrentChain chainDB)
      (ChainDB.getCurrentLedger chainDB)
      getNodeCandidates
      up

    nrChainSyncServer :: ChainSyncServer (Header blk) (Point blk) m ()
    nrChainSyncServer =
      chainSyncServer (tracePrefix "CSServer" Nothing) chainDB

    nrBlockFetchClient :: BlockFetchClient (Header blk) blk m ()
    nrBlockFetchClient = blockFetchClient
      -- Note the tracer for the changes in the fetch client state
      -- is passed to the blockFetchLogic.
      -- The message level tracer is passed to runPipelinedPeer.

    nrBlockFetchServer :: BlockFetchServer blk m ()
    nrBlockFetchServer =
      blockFetchServer (tracePrefix "BFServer" Nothing) chainDB
