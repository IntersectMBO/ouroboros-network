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
    NetworkApplication(..)
  , consensusNetworkApps
  , SharedState (..)
  , runSharedState
  ) where

import           Control.Monad (void)
import           Data.Void (Void)

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Tracer

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

runSharedState :: Monad m => SharedState m s a -> m a
runSharedState (SharedState ms f) = ms >>= pure . f

-- | Network applications that are the consensus layer will run over
-- a 'NetworkInterface'.
--
data NetworkApplication m versions up blk bytes a b = NetworkApplication {
      -- | Mux application which runs with upstream peers.
      --
      naMuxInitiatorApp :: up
                        -> SharedState m (TMVar m ())
                            (versions
                              (MuxApplication
                                InitiatorApp
                                NodeToNodeProtocols
                                m bytes a Void))

      -- | Mux application which serves downstream peers.
      --
      -- TODO: use 'AnyMuxResponderApp', but it requires better connection
      -- management on the networking side.  This way we avoid simultaneously
      -- opened connections for now.
      --
    , naMuxResponderApp :: versions
                            (MuxApplication
                              ResponderApp
                              NodeToNodeProtocols
                              m bytes Void b)
    }

-- | Example function which creates consensus mux applications, this is useful
-- for testing but also untill we have only a single version in
-- 'NodeToNodeVersions'.
--
consensusNetworkApps
    :: forall m versions up blk failure bytes.
       ( MonadAsync m
       , MonadCatch m
       , MonadThrow (STM m)
       , MonadTime  m
       , ProtocolLedgerView blk
       , Condense (Header blk)
       , Condense (ChainHash blk)
       , Ord up
       , Condense up
       , Exception failure
       )
    => (forall a. a -> versions a)
    -> Tracer m (TraceSendRecv (ChainSync (Header blk) (Point blk)))
    -> Tracer m (TraceSendRecv (BlockFetch blk))
    -> Codec (ChainSync (Header blk) (Point blk)) failure m bytes
    -> Codec (BlockFetch blk) failure m bytes
    -> NodeParams m up blk
    -> NodeKernel m up blk
    -> NetworkApplication m versions up blk bytes () ()
consensusNetworkApps singletonV traceChainSync traceBlockFetch codecChainSync codecBlockFetch NodeParams {..} kernel =
    NetworkApplication {
        naMuxInitiatorApp = initiatorApp,
        naMuxResponderApp = singletonV (simpleMuxResponderApplication responderApp)
      }
  where
    initiatorApp :: up
                 -> SharedState m (TMVar m ())
                      (versions
                        (MuxApplication
                          InitiatorApp
                          NodeToNodeProtocols
                          m bytes () Void))

    initiatorApp up = SharedState newEmptyTMVarM $ \clientRegistered ->
      singletonV $ MuxInitiatorApplication $
        \ptcl channel -> case ptcl of
          ChainSyncWithHeadersPtcl -> do
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
                  (tracePrefix "CSClient" (Just up) tracer)
                  cfg
                  btime
                  maxClockSkew
                  (ChainDB.getCurrentChain chainDB)
                  (ChainDB.getCurrentLedger chainDB)
                  (getNodeCandidates kernel)
                  up

          BlockFetchPtcl ->
            bracketFetchClient (getFetchClientRegistry kernel) up $ \clientCtx -> do

              atomically $ putTMVar clientRegistered ()

              runPipelinedPeer
                traceBlockFetch
                codecBlockFetch
                channel
                (blockFetchClient clientCtx)


    responderApp :: NodeToNodeProtocols
                 -> MuxPeer failure m bytes ()

    responderApp ChainSyncWithHeadersPtcl =
      MuxPeer
        traceChainSync
        codecChainSync
        (chainSyncServerPeer
          $ chainSyncServer
            (tracePrefix "CSServer" (Nothing :: Maybe up) tracer)
            chainDB)

    responderApp BlockFetchPtcl =
      MuxPeer
        traceBlockFetch
        codecBlockFetch
        (blockFetchServerPeer
          $ blockFetchServer
            (tracePrefix "BFServer" (Nothing :: Maybe up) tracer)
            chainDB)
