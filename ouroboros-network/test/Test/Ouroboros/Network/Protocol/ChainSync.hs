{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Ouroboros.Network.Protocol.ChainSync where

import Control.Monad (unless, void)
import Control.Monad.ST.Lazy (runST)
import Data.ByteString (ByteString)
import System.Process (createPipe)

import Codec.CBOR.Encoding (Encoding)

import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadProbe

import Control.Monad.IOSim (SimM)

import Protocol.Core (connect)
import Protocol.Codec
import Protocol.Channel
import Protocol.Driver (useCodecWithDuplex)

import           Ouroboros.Network.Chain (Point)
import qualified Ouroboros.Network.Chain as Chain
import Ouroboros.Network.Pipe (pipeDuplex)
import qualified Ouroboros.Network.ChainProducerState as ChainProducerState

import Ouroboros.Network.Protocol.ChainSync.Client
import Ouroboros.Network.Protocol.ChainSync.Server
import Ouroboros.Network.Protocol.ChainSync.Direct
import Ouroboros.Network.Protocol.ChainSync.Codec.Cbor
import qualified Ouroboros.Network.Protocol.ChainSync.Examples as ChainSyncExamples

import Ouroboros.Network.Testing.ConcreteBlock (Block (..))
import Test.ChainProducerState (ChainProducerStateForkTest (..))
import Test.Ouroboros.Network.Testing.Utils (runExperiment, tmvarChannels)

import Test.QuickCheck hiding (Result)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests = testGroup "Ouroboros.Network.Protocol.ChainSyncProtocol"
  [ testProperty "direct ST" propChainSyncDirectST
  , testProperty "direct IO" propChainSyncDirectIO
  , testProperty "connect ST" propChainSyncConnectST
  , testProperty "connect IO" propChainSyncConnectIO
  , testProperty "demo ST" propChainSyncDemoST
  , testProperty "demo IO" propChainSyncDemoIO
  , testProperty "pipe demo" propChainSyncPipe
  ]

-- | Testing @'Client'@ which stops at a given point.
--
testClient
  :: MonadSTM m
  => TVar m Bool
  -> Point Block
  -> ChainSyncExamples.Client Block m ()
testClient doneVar tip =
  ChainSyncExamples.Client { 
      ChainSyncExamples.rollbackward = \point _ ->
        if point == tip
          then do
            atomically $ writeTVar doneVar True
            return $ Left ()
          else return $ Right (testClient doneVar tip),
      ChainSyncExamples.rollforward = \block ->
        if Chain.blockPoint block == tip
          then do
            atomically $ writeTVar doneVar True
            return $ Left ()
          else return $ Right (testClient doneVar tip),
      ChainSyncExamples.points = \_ -> return (testClient doneVar tip)
    }

-- | An experiment in which the client has a fork of the server chain.  The
-- experiment finishes successfully if the client receives the server's chain.
--
chainSyncForkExperiment
  :: forall m.
     ( MonadST m
     , MonadSTM m
     , MonadProbe m
     )
  => (forall a b. ChainSyncServer Block (Point Block) m a
      -> ChainSyncClient Block (Point Block) m b
      -> m ())
  -> ChainProducerStateForkTest
  -> Probe m Property
  -> m ()
chainSyncForkExperiment run (ChainProducerStateForkTest cps chain) probe = do
  let pchain = ChainProducerState.producerChain cps
  cpsVar   <- atomically $ newTVar cps
  chainVar <- atomically $ newTVar chain
  doneVar  <- atomically $ newTVar False
  let server = ChainSyncExamples.chainSyncServerExample
        (error "chainSyncServerExample: lazy in the result type")
        cpsVar
      client = ChainSyncExamples.chainSyncClientExample chainVar (testClient doneVar (Chain.headPoint pchain))
  _ <- run server client

  cchain <- atomically $ readTVar chainVar
  probeOutput probe (pchain === cchain)

propChainSyncDirectST :: ChainProducerStateForkTest -> Property
propChainSyncDirectST cps = runST $ runExperiment $ chainSyncForkExperiment ((fmap . fmap) void direct) cps

propChainSyncDirectIO :: ChainProducerStateForkTest -> Property
propChainSyncDirectIO cps = ioProperty $ runExperiment $ chainSyncForkExperiment ((fmap . fmap) void direct) cps

propChainSyncConnectST :: ChainProducerStateForkTest -> Property
propChainSyncConnectST cps = runST $ runExperiment $ chainSyncForkExperiment
  (\ser cli ->
      void $ connect (chainSyncServerPeer ser) (chainSyncClientPeer cli)
  ) cps 

propChainSyncConnectIO :: ChainProducerStateForkTest -> Property
propChainSyncConnectIO cps = ioProperty $ runExperiment $ chainSyncForkExperiment
  (\ser cli ->
      void $  connect (chainSyncServerPeer ser) (chainSyncClientPeer cli)
  ) cps 

chainSyncDemo
  :: forall m.
     ( MonadST m
     , MonadSTM m
     , MonadProbe m
     )
  => Duplex m m Encoding ByteString
  -> Duplex m m Encoding ByteString
  -> ChainProducerStateForkTest
  -> Probe m Property
  -> m ()
chainSyncDemo clientChan serverChan (ChainProducerStateForkTest cps chain) probe = withLiftST @m $ \liftST -> do
  let pchain = ChainProducerState.producerChain cps
  cpsVar   <- atomically $ newTVar cps
  chainVar <- atomically $ newTVar chain
  doneVar  <- atomically $ newTVar False

  let server = ChainSyncExamples.chainSyncServerExample
        (error "chainSyncServerExample: lazy in the result type")
        cpsVar

      client = ChainSyncExamples.chainSyncClientExample chainVar (testClient doneVar (Chain.headPoint pchain))

      codec = hoistCodec liftST codecChainSync

  fork (void $ useCodecWithDuplex serverChan codec (chainSyncServerPeer server))
  fork (void $ useCodecWithDuplex clientChan codec (chainSyncClientPeer client))

  atomically $ do
    done <- readTVar doneVar
    unless done retry

  cchain <- atomically $ readTVar chainVar
  probeOutput probe (pchain === cchain)

propChainSyncDemoST
  :: ChainProducerStateForkTest
  -> Property
propChainSyncDemoST cps =
  runST $ runExperiment $ \probe -> do
    (clientChan, serverChan) <- tmvarChannels
    chainSyncDemo @(SimM _) clientChan serverChan cps probe

propChainSyncDemoIO
  :: ChainProducerStateForkTest
  -> Property
propChainSyncDemoIO cps =
  ioProperty $ runExperiment $ \probe -> do
    (clientChan, serverChan) <- tmvarChannels
    chainSyncDemo clientChan serverChan cps probe

propChainSyncPipe
  :: ChainProducerStateForkTest
  -> Property
propChainSyncPipe cps =
  ioProperty $ runExperiment $ \probe -> do
    (serRead, cliWrite) <- createPipe
    (cliRead, serWrite) <- createPipe
    let clientChan = pipeDuplex cliRead cliWrite
        serverChan = pipeDuplex serRead serWrite
    chainSyncDemo clientChan serverChan cps probe
