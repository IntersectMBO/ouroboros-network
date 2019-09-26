{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

{-# OPTIONS_GHC -Wno-orphans            #-}
module Test.Mux
  ( tests)
  where

import           Codec.Serialise (Serialise (..))

import           Control.Monad.IOSim
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Tracer

import           Test.ChainGenerators (TestBlockChainAndUpdates (..))
import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Network.TypedProtocol.Core

import           Ouroboros.Network.Block (BlockNo)
import           Ouroboros.Network.MockChain.Chain (Chain, ChainUpdate, Point)
import qualified Ouroboros.Network.MockChain.Chain as Chain
import qualified Ouroboros.Network.MockChain.ProducerState as CPS
import qualified Ouroboros.Network.Protocol.ChainSync.Type     as ChainSync
import qualified Ouroboros.Network.Protocol.ChainSync.Client   as ChainSync
import qualified Ouroboros.Network.Protocol.ChainSync.Codec    as ChainSync
import qualified Ouroboros.Network.Protocol.ChainSync.Examples as ChainSync
import qualified Ouroboros.Network.Protocol.ChainSync.Server   as ChainSync

import qualified Network.Mux.Types as Mx
import qualified Network.Mux.Bearer.Queues as Mx
import qualified Ouroboros.Network.Mux as Mx


tests :: TestTree
tests =
    testGroup "Mux"
  [ testProperty "ChainSync Demo (IO)"  prop_mux_demo_io
  , testProperty "ChainSync Demo (Sim)" prop_mux_demo_sim
  ]

activeTracer :: forall m a. (MonadSay m, Show a) => Tracer m a
activeTracer = nullTracer
--activeTracer = showTracing sayTracer

_sayTracer :: MonadSay m => Tracer m String
_sayTracer = Tracer say


data TestProtocols = ChainSyncPr
  deriving (Eq, Ord, Enum, Bounded, Show)

instance Mx.MiniProtocolLimits TestProtocols where
    maximumMessageSize _  = 0xffff
    maximumIngressQueue _ = 0xffff

instance Mx.ProtocolEnum TestProtocols where
  fromProtocolEnum ChainSyncPr = 2

  toProtocolEnum 2 = Just ChainSyncPr
  toProtocolEnum _ = Nothing


demo :: forall m block.
        ( MonadAsync m
        , MonadCatch m
        , MonadMask m
        , MonadSay m
        , MonadST m
        , MonadSTM m
        , MonadThrow (STM m)
        , MonadTime m
        , MonadTimer m
        , Chain.HasHeader block
        , Serialise (Chain.HeaderHash block)
        , Serialise block
        , Eq block
        , Show block
        , Eq (Async m ()) )
     => Chain block -> [ChainUpdate block block] -> DiffTime -> m Property
demo chain0 updates delay = do
    client_w <- atomically $ newTBQueue 10
    client_r <- atomically $ newTBQueue 10
    let sduLen = 1280
    let server_w = client_r
        server_r = client_w
    producerVar <- atomically $ newTVar (CPS.initChainProducerState chain0)
    consumerVar <- atomically $ newTVar chain0
    done <- atomically newEmptyTMVar

    let Just expectedChain = Chain.applyChainUpdates updates chain0
        target = Chain.headPoint expectedChain

        consumerPeer :: Peer (ChainSync.ChainSync block (Point block, BlockNo)) AsClient ChainSync.StIdle m ()
        consumerPeer = ChainSync.chainSyncClientPeer
                          (ChainSync.chainSyncClientExample consumerVar
                          (consumerClient done target consumerVar))
        consumerApp = Mx.simpleInitiatorApplication
                        (\ChainSyncPr ->
                        Mx.MuxPeer
                        nullTracer
                        (ChainSync.codecChainSync
                        encode (fmap const decode)
                        encode             decode
                        encode             decode)
                        (consumerPeer))

        producerPeer :: Peer (ChainSync.ChainSync block (Point block, BlockNo)) AsServer ChainSync.StIdle m ()
        producerPeer = ChainSync.chainSyncServerPeer (ChainSync.chainSyncServerExample () producerVar)
        producerApp = Mx.simpleResponderApplication
                        (\ChainSyncPr ->
                        Mx.MuxPeer
                        nullTracer
                        (ChainSync.codecChainSync
                        encode (fmap const decode)
                        encode             decode
                        encode             decode)
                        producerPeer)

    clientAsync <- async $ Mx.runMuxWithQueues activeTracer "consumer" (Mx.toApplication consumerApp) client_w client_r sduLen Nothing
    serverAsync <- async $ Mx.runMuxWithQueues activeTracer "producer" (Mx.toApplication producerApp) server_w server_r sduLen Nothing

    updateAid <- async $ sequence_
        [ do
            threadDelay delay -- X milliseconds, just to provide interest
            atomically $ do
              p <- readTVar producerVar
              let Just p' = CPS.applyChainUpdate update p
              writeTVar producerVar p'
        | update <- updates
        ]

    wait updateAid
    r <- waitBoth clientAsync serverAsync
    case r of
         (_, Just _) -> return $ property False
         _           -> do
           ret <- atomically $ takeTMVar done
           return $ property ret

  where
    checkTip target consumerVar = atomically $ do
      chain <- readTVar consumerVar
      return (Chain.headPoint chain == target)

    -- A simple chain-sync client which runs until it recieves an update to
    -- a given point (either as a roll forward or as a roll backward).
    consumerClient :: StrictTMVar m Bool
                   -> Point block
                   -> StrictTVar m (Chain block)
                   -> ChainSync.Client block (Point block, BlockNo) m ()
    consumerClient done target chain =
      ChainSync.Client
        { ChainSync.rollforward = \_ -> checkTip target chain >>= \b ->
            if b then do
                    atomically $ putTMVar done True
                    pure $ Left ()
                 else
                    pure $ Right $ consumerClient done target chain
        , ChainSync.rollbackward = \_ _ -> checkTip target chain >>= \b ->
            if b then do
                    atomically $ putTMVar done True
                    pure $ Left ()
                 else
                    pure $ Right $ consumerClient done target chain
        , ChainSync.points = \_ -> pure $ consumerClient done target chain
        }

prop_mux_demo_io :: TestBlockChainAndUpdates -> Property
prop_mux_demo_io (TestBlockChainAndUpdates chain updates) =
    ioProperty $ demo chain updates 10e-4

prop_mux_demo_sim :: TestBlockChainAndUpdates -> Property
prop_mux_demo_sim (TestBlockChainAndUpdates chain updates) =
    case runSimStrictShutdown $ demo chain updates 10e-3 of
         Left  _ -> property  False
         Right r -> r
