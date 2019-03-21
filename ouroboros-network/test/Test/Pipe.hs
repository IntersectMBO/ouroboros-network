{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans     #-}

module Test.Pipe (tests) where

import           Codec.Serialise (Serialise)
import           Control.Monad
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTimer
import           Test.ChainGenerators (TestBlockChainAndUpdates (..))
import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Network.TypedProtocol.Core

import           Ouroboros.Network.Chain (Chain, ChainUpdate, Point)
import qualified Ouroboros.Network.Chain as Chain
import qualified Ouroboros.Network.ChainProducerState as CPS
import qualified Ouroboros.Network.Mux as Mx
import qualified Ouroboros.Network.Mux.Interface as Mx
import           Ouroboros.Network.Pipe
import           Ouroboros.Network.Protocol.ChainSync.Type     as ChainSync
import           Ouroboros.Network.Protocol.ChainSync.Client   as ChainSync
import           Ouroboros.Network.Protocol.ChainSync.Codec    as ChainSync
import           Ouroboros.Network.Protocol.ChainSync.Examples as ChainSync
import           Ouroboros.Network.Protocol.ChainSync.Server   as ChainSync
import qualified Test.Mux as Mxt

--
-- The list of all tests
--

tests :: TestTree
tests =
  testGroup "Pipe"
  [ testProperty "pipe sync demo"        prop_pipe_demo
  ]

--
-- Properties
--

prop_pipe_demo :: TestBlockChainAndUpdates -> Property
prop_pipe_demo (TestBlockChainAndUpdates chain updates) =
    ioProperty $ demo chain updates


-- | The enumeration of all mini-protocols in our demo protocol.
data DemoProtocols = ChainSync
  deriving (Eq, Ord, Enum, Bounded, Show)

instance Mx.ProtocolEnum DemoProtocols where
  fromProtocolEnum ChainSync = 2

  toProtocolEnum 2 = Just ChainSync
  toProtocolEnum _ = Nothing

instance Mx.MiniProtocolLimits DemoProtocols where
  maximumMessageSize ChainSync  = 2000000
  maximumIngressQueue ChainSync = 2000000

-- | A demonstration that we can run the simple chain consumer protocol
-- over a pipe with full message serialisation, framing etc.
--
demo :: forall block .
        (Chain.HasHeader block, Serialise block, Eq block )
     => Chain block -> [ChainUpdate block] -> IO Bool
demo chain0 updates = do


    producerVar <- atomically $ newTVar (CPS.initChainProducerState chain0)
    consumerVar <- atomically $ newTVar chain0
    done <- atomically newEmptyTMVar

    (consumerPipeCtx, producerPipeCtx) <- createConnectedPipeCtx

    let Just expectedChain = Chain.applyChainUpdates updates chain0
        target = Chain.headPoint expectedChain

        consumerPeer :: Peer (ChainSync.ChainSync block (Point block)) AsClient ChainSync.StIdle IO ()
        consumerPeer = ChainSync.chainSyncClientPeer
                          (ChainSync.chainSyncClientExample consumerVar
                          (consumerClient done target consumerVar))
        consumerPeers Mxt.ReqResp1 = Mx.OnlyClient ChainSync.codecChainSync consumerPeer

        producerPeer :: Peer (ChainSync.ChainSync block (Point block)) AsServer ChainSync.StIdle IO ()
        producerPeer = ChainSync.chainSyncServerPeer (ChainSync.chainSyncServerExample () producerVar)
        producerPeers Mxt.ReqResp1 = Mx.OnlyServer ChainSync.codecChainSync producerPeer

    runNetworkNodeWithPipe [Mxt.version0] (\_ -> Just producerPeers) Mx.StyleServer producerPipeCtx
    runNetworkNodeWithPipe [Mxt.version0] (\_ -> Just consumerPeers) Mx.StyleClient consumerPipeCtx

    void $ fork $ sequence_
        [ do threadDelay 10000 -- just to provide interest
             atomically $ do
                 p <- readTVar producerVar
                 let Just p' = CPS.applyChainUpdate update p
                 writeTVar producerVar p'
             | update <- updates
        ]

    atomically $ takeTMVar done

  where
    checkTip target consumerVar = atomically $ do
      chain <- readTVar consumerVar
      return (Chain.headPoint chain == target)

    -- A simple chain-sync client which runs until it recieves an update to
    -- a given point (either as a roll forward or as a roll backward).
    consumerClient :: TMVar IO Bool
                   -> Point block
                   -> TVar IO (Chain block)
                   -> ChainSync.Client block IO ()
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
