{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans     #-}

module Test.Pipe (tests) where

import           Codec.Serialise (Serialise (..))
import           Control.Monad
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTimer
import           Data.Int
import           System.Info (os)
import           System.Process (createPipe)
import           Test.ChainGenerators (TestBlockChainAndUpdates (..))
import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Control.Tracer (nullTracer)

import           Ouroboros.Network.Chain (Chain, ChainUpdate, Point)
import qualified Ouroboros.Network.Chain as Chain
import qualified Ouroboros.Network.ChainProducerState as CPS
import qualified Ouroboros.Network.Mux as Mx
import qualified Ouroboros.Network.Mux.Interface as Mx
import           Ouroboros.Network.Pipe
import           Ouroboros.Network.Protocol.ChainSync.Client as ChainSync
import           Ouroboros.Network.Protocol.ChainSync.Codec as ChainSync
import           Ouroboros.Network.Protocol.ChainSync.Examples as ChainSync
import           Ouroboros.Network.Protocol.ChainSync.Server as ChainSync
import qualified Test.Mux as Mxt

--
-- The list of all tests
--

tests :: TestTree
tests =
    {-
     - Anonymous pipe test cases fails for an unknown reason
     - when compiled without "-threaded" on Windows. The Socket test
     - suite deadlocks when compiled with "-threaded" on windows due to
     - https://gitlab.haskell.org/ghc/ghc/issues/14503.
     -
     - We require working sockets not anoynymous pipes on Windows so
     - this test group is disabled for now.
     -}
    if os == "mingw32"
        then testGroup "Pipe" []
        else testGroup "Pipe"
                 [ testProperty "pipe sync demo"        prop_pipe_demo
                 ]

--
-- Properties
--

prop_pipe_demo :: TestBlockChainAndUpdates -> Property
prop_pipe_demo (TestBlockChainAndUpdates chain updates) =
    ioProperty $ demo chain updates

defaultMiniProtocolLimit :: Int64
defaultMiniProtocolLimit = 3000000

-- | The enumeration of all mini-protocols in our demo protocol.
data DemoProtocols = ChainSync
  deriving (Eq, Ord, Enum, Bounded, Show)

instance Mx.ProtocolEnum DemoProtocols where
  fromProtocolEnum ChainSync = 2

  toProtocolEnum 2 = Just ChainSync
  toProtocolEnum _ = Nothing

instance Mx.MiniProtocolLimits DemoProtocols where
  maximumMessageSize ChainSync  = defaultMiniProtocolLimit
  maximumIngressQueue ChainSync = defaultMiniProtocolLimit

-- | A demonstration that we can run the simple chain consumer protocol
-- over a pipe with full message serialisation, framing etc.
--
demo :: forall block .
        (Chain.HasHeader block, Serialise (Chain.HeaderHash block), Serialise block, Eq block )
     => Chain block -> [ChainUpdate block] -> IO Bool
demo chain0 updates = do

    (hndRead1, hndWrite1) <- createPipe
    (hndRead2, hndWrite2) <- createPipe

    producerVar <- atomically $ newTVar (CPS.initChainProducerState chain0)
    consumerVar <- atomically $ newTVar chain0
    done <- atomically newEmptyTMVar

    let Just expectedChain = Chain.applyChainUpdates updates chain0
        target = Chain.headPoint expectedChain

        consumerApp :: Mx.MuxApplication Mx.InitiatorApp Mxt.TestProtocols1 IO
        consumerApp = Mx.simpleMuxInitiatorApplication $
          \Mxt.ChainSync1 ->
            Mx.MuxPeer nullTracer
                       (ChainSync.codecChainSync encode decode encode decode)
                       (ChainSync.chainSyncClientPeer
                          (ChainSync.chainSyncClientExample consumerVar
                            (consumerClient done target consumerVar)))

        server :: ChainSyncServer block (Point block) IO ()
        server = ChainSync.chainSyncServerExample () producerVar

        producerApp :: Mx.MuxApplication Mx.ResponderApp Mxt.TestProtocols1 IO
        producerApp = Mx.simpleMuxResponderApplication $
          \Mxt.ChainSync1 ->
            Mx.MuxPeer nullTracer
                       (ChainSync.codecChainSync encode decode encode decode)
                       (ChainSync.chainSyncServerPeer server)

    _ <- async $ runNetworkNodeWithPipe producerApp hndRead1 hndWrite2
    _ <- async $ runNetworkNodeWithPipe consumerApp hndRead2 hndWrite1

    void $ fork $ sequence_
        [ do threadDelay 10e-4 -- 1 milliseconds, just to provide interest
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
