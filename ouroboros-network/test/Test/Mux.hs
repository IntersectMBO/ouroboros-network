{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wno-orphans            #-}
module Test.Mux (tests) where

import           Codec.Serialise (Serialise (..))

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Monad.IOSim
import           Control.Tracer

import           Test.ChainGenerators (TestBlockChainAndUpdates (..))
import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Network.TypedProtocol.Core

import           Ouroboros.Network.Block (Tip (..), decodeTip, encodeTip)
import           Ouroboros.Network.ConnectionId
import           Ouroboros.Network.MockChain.Chain (Chain, ChainUpdate, Point)
import qualified Ouroboros.Network.MockChain.Chain as Chain
import qualified Ouroboros.Network.MockChain.ProducerState as CPS
import qualified Ouroboros.Network.Protocol.ChainSync.Client as ChainSync
import qualified Ouroboros.Network.Protocol.ChainSync.Codec as ChainSync
import qualified Ouroboros.Network.Protocol.ChainSync.Examples as ChainSync
import qualified Ouroboros.Network.Protocol.ChainSync.Server as ChainSync
import qualified Ouroboros.Network.Protocol.ChainSync.Type as ChainSync
import           Ouroboros.Network.Util.ShowProxy

import qualified Network.Mux.Bearer.Queues as Mx
import qualified Network.Mux.Compat as Mx (muxStart)
import qualified Network.Mux.Types as Mx
import           Ouroboros.Network.Mux as Mx


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


testProtocols :: RunMiniProtocol appType bytes m a b
              -> OuroborosApplication appType addr bytes m a b
testProtocols chainSync =
    OuroborosApplication $ \_connectionId _shouldStopSTM -> [
      MiniProtocol {
        miniProtocolNum    = MiniProtocolNum 2,
        miniProtocolLimits = MiniProtocolLimits {
                               maximumIngressQueue = 0xffff
                             },
        miniProtocolRun    = chainSync
      }
    ]


demo :: forall m block.
        ( MonadAsync m
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
    let sduLen = Mx.SDUSize 1280
    let server_w = client_r
        server_r = client_w
    producerVar <- atomically $ newTVar (CPS.initChainProducerState chain0)
    consumerVar <- atomically $ newTVar chain0
    done <- atomically newEmptyTMVar

    let Just expectedChain = Chain.applyChainUpdates updates chain0
        target = Chain.headPoint expectedChain

        consumerApp = testProtocols chainSyncInitator

        chainSyncInitator =
          InitiatorProtocolOnly $
          MuxPeer
            nullTracer
            (ChainSync.codecChainSync
               encode             decode
               encode             decode
               (encodeTip encode) (decodeTip decode))
            consumerPeer

        consumerPeer :: Peer (ChainSync.ChainSync block (Point block) (Tip block))
                             AsClient ChainSync.StIdle m ()
        consumerPeer = ChainSync.chainSyncClientPeer
                          (ChainSync.chainSyncClientExample consumerVar
                          (consumerClient done target consumerVar))

        producerApp = testProtocols chainSyncResponder

        chainSyncResponder =
          ResponderProtocolOnly $
          MuxPeer
            nullTracer
            (ChainSync.codecChainSync
               encode             decode
               encode             decode
               (encodeTip encode) (decodeTip decode))
            producerPeer

        producerPeer :: Peer (ChainSync.ChainSync block (Point block) (Tip block))
                        AsServer ChainSync.StIdle m ()
        producerPeer = ChainSync.chainSyncServerPeer (ChainSync.chainSyncServerExample () producerVar)

    let clientBearer = Mx.queuesAsMuxBearer activeTracer client_w client_r sduLen
        serverBearer = Mx.queuesAsMuxBearer activeTracer server_w server_r sduLen

    clientAsync <- async $
      Mx.muxStart
        activeTracer
        (Mx.toApplication
          (ConnectionId "client" "server")
          (continueForever (Proxy :: Proxy m))
          consumerApp)
        clientBearer
    serverAsync <- async $
      Mx.muxStart
        activeTracer
        (Mx.toApplication
          (ConnectionId "server" "client")
          (continueForever (Proxy :: Proxy m))
          producerApp)
        serverBearer

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
