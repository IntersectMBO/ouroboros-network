{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-orphans            #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Test.Ouroboros.Network.Mux (tests) where

import Codec.Serialise (Serialise (..))
import Data.Functor (void)
import Data.Monoid.Synchronisation (FirstToFinish (..))

import Control.Applicative (Alternative)
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Monad.IOSim
import Control.Tracer

import Test.ChainGenerators (TestBlockChainAndUpdates (..))
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Network.TypedProtocol.Core
import Network.TypedProtocol.Peer.Client (Client)
import Network.TypedProtocol.Peer.Server (Server)

import Ouroboros.Network.Block (Tip (..), decodeTip, encodeTip)
import Ouroboros.Network.Context
import Ouroboros.Network.Mock.Chain (Chain, ChainUpdate, Point)
import Ouroboros.Network.Mock.Chain qualified as Chain
import Ouroboros.Network.Mock.ProducerState qualified as CPS
import Ouroboros.Network.Protocol.ChainSync.Client qualified as ChainSync
import Ouroboros.Network.Protocol.ChainSync.Codec qualified as ChainSync
import Ouroboros.Network.Protocol.ChainSync.Examples qualified as ChainSync
import Ouroboros.Network.Protocol.ChainSync.Server qualified as ChainSync
import Ouroboros.Network.Protocol.ChainSync.Type qualified as ChainSync
import Ouroboros.Network.Util.ShowProxy

import Network.Mux qualified as Mx
import Network.Mux.Bearer qualified as Mx
import Network.Mux.Bearer.Queues qualified as Mx
import Ouroboros.Network.Mux as Mx


tests :: TestTree
tests =
    testGroup "Ouroboros.Network.Mux"
  [ testProperty "ChainSync Demo (IO)"  prop_mux_demo_io
  , testProperty "ChainSync Demo (Sim)" prop_mux_demo_sim
  ]

activeTracer :: forall m a. (MonadSay m, Show a) => Tracer m a
activeTracer = nullTracer
--activeTracer = showTracing sayTracer

_sayTracer :: MonadSay m => Tracer m String
_sayTracer = Tracer say


testProtocols :: RunMiniProtocolWithMinimalCtx appType addr bytes m a b
              -> OuroborosApplicationWithMinimalCtx appType addr bytes m a b
testProtocols chainSync =
    OuroborosApplication [
      MiniProtocol {
        miniProtocolNum    = MiniProtocolNum 2,
        miniProtocolStart  = StartOnDemand,
        miniProtocolLimits = MiniProtocolLimits {
                               maximumIngressQueue = 0xffff
                             },
        miniProtocolRun    = chainSync
      }
    ]


demo :: forall m block.
        ( Alternative (STM m)
        , MonadAsync m
        , MonadDelay m
        , MonadCatch m
        , MonadFork m
        , MonadLabelledSTM m
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
        , ShowProxy block
        , Eq (Async m ()) )
     => Chain block -> [ChainUpdate block block] -> DiffTime -> m Property
demo chain0 updates delay = do
    client_w <- atomically $ newTBQueue 10
    client_r <- atomically $ newTBQueue 10
    let server_w = client_r
        server_r = client_w
    producerVar <- newTVarIO (CPS.initChainProducerState chain0)
    consumerVar <- newTVarIO chain0
    done <- newEmptyTMVarIO

    let Just expectedChain = Chain.applyChainUpdates updates chain0
        target = Chain.headPoint expectedChain

        consumerApp = testProtocols chainSyncInitator

        chainSyncInitator =
          InitiatorProtocolOnly $
          mkMiniProtocolCbFromPeer $ \_ctx ->
            ( nullTracer
            , ChainSync.codecChainSync
                 encode             decode
                 encode             decode
                (encodeTip encode) (decodeTip decode)
            , consumerPeer
            )

        consumerPeer :: Client (ChainSync.ChainSync block (Point block) (Tip block))
                               'NonPipelined ChainSync.StIdle m ()
        consumerPeer = ChainSync.chainSyncClientPeer
                          (ChainSync.chainSyncClientExample consumerVar
                          (consumerClient done target consumerVar))

        producerApp = testProtocols chainSyncResponder

        chainSyncResponder =
          ResponderProtocolOnly $ mkMiniProtocolCbFromPeer $ \_ctx ->
            ( nullTracer
            , ChainSync.codecChainSync
                 encode             decode
                 encode             decode
                (encodeTip encode) (decodeTip decode)
            , producerPeer
            )

        producerPeer :: Server (ChainSync.ChainSync block (Point block) (Tip block))
                               'NonPipelined ChainSync.StIdle m ()
        producerPeer = ChainSync.chainSyncServerPeer (ChainSync.chainSyncServerExample () producerVar id)

    clientBearer <- Mx.getBearer Mx.makeQueueChannelBearer
                      (-1)
                      Mx.QueueChannel { Mx.writeQueue = client_w,
                                        Mx.readQueue = client_r
                                      }
                       Nothing
    serverBearer <- Mx.getBearer Mx.makeQueueChannelBearer
                       (-1)
                       Mx.QueueChannel { Mx.writeQueue = server_w,
                                         Mx.readQueue = server_r
                                       }
                       Nothing

    clientAsync <- async $ do
      clientMux <- Mx.new (toMiniProtocolInfos (\_ _ -> Nothing) consumerApp)
      let initCtx = MinimalInitiatorContext (ConnectionId "consumer" "producer")
      resOps <- sequence
        [ Mx.runMiniProtocol
            clientMux
            miniProtocolNum
            miniProtocolDir
            Mx.StartEagerly
            (\a -> do
              r <- action a
              return (r, Nothing)
            )
        | MiniProtocol{miniProtocolNum, miniProtocolRun}
            <- getOuroborosApplication consumerApp
        , (miniProtocolDir, action) <-
            case miniProtocolRun of
              InitiatorProtocolOnly initiator ->
                [(Mx.InitiatorDirectionOnly, void . runMiniProtocolCb initiator initCtx)]
        ]
      withAsync (Mx.run (Mx.Tracers activeTracer activeTracer activeTracer) clientMux clientBearer) $ \aid -> do
        _ <- atomically $ runFirstToFinish $ foldMap FirstToFinish resOps
        Mx.stop clientMux
        wait aid

    serverAsync <- async $ do
      serverMux <- Mx.new (toMiniProtocolInfos (\_ _ -> Nothing) producerApp)
      let respCtx = ResponderContext (ConnectionId "producer" "consumer")
      resOps <- sequence
        [ Mx.runMiniProtocol
            serverMux
            miniProtocolNum
            miniProtocolDir
            Mx.StartEagerly
            (\a -> do
              r <- action a
              return (r, Nothing)
            )
        | MiniProtocol{miniProtocolNum, miniProtocolRun}
            <- getOuroborosApplication producerApp
        , (miniProtocolDir, action) <-
            case miniProtocolRun of
              ResponderProtocolOnly responder ->
                [(Mx.ResponderDirectionOnly, void . runMiniProtocolCb responder respCtx)]
        ]
      withAsync (Mx.run (Mx.Tracers activeTracer activeTracer activeTracer) serverMux serverBearer) $ \aid -> do
        _ <- atomically $ runFirstToFinish $ foldMap FirstToFinish resOps
        Mx.stop serverMux
        wait aid

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
    _ <- waitBoth clientAsync serverAsync
    -- TODO: use new mechanism to collect mini-protocol result:
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
                   -> ChainSync.Client block (Point block) (Tip block) m ()
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
        , ChainSync.points = \_ -> pure $ Right $ consumerClient done target chain
        }

prop_mux_demo_io :: TestBlockChainAndUpdates -> Property
prop_mux_demo_io (TestBlockChainAndUpdates chain updates) =
    ioProperty $ demo chain updates 10e-4

prop_mux_demo_sim :: TestBlockChainAndUpdates -> Property
prop_mux_demo_sim (TestBlockChainAndUpdates chain updates) =
    case runSimStrictShutdown $ demo chain updates 10e-3 of
         Left  _ -> property  False
         Right r -> r
