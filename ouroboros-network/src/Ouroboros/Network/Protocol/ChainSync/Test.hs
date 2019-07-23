{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Network.Protocol.ChainSync.Test (tests) where

import qualified Codec.Serialise as S
import           Control.Monad (unless, void)
import qualified Control.Monad.ST as ST
import           Data.ByteString.Lazy (ByteString)

import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Tracer (nullTracer)

import           Control.Monad.IOSim (runSimOrThrow)

import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Channel
import           Network.TypedProtocol.Driver
import           Network.TypedProtocol.Proofs (connect)

import           Ouroboros.Network.Channel

import           Ouroboros.Network.MockChain.Chain (Point)
import qualified Ouroboros.Network.MockChain.Chain as Chain
import qualified Ouroboros.Network.MockChain.ProducerState as ChainProducerState

import           Ouroboros.Network.Protocol.ChainSync.Client
import           Ouroboros.Network.Protocol.ChainSync.Codec
import           Ouroboros.Network.Protocol.ChainSync.Direct
import qualified Ouroboros.Network.Protocol.ChainSync.Examples as ChainSyncExamples
import           Ouroboros.Network.Protocol.ChainSync.Server
import           Ouroboros.Network.Protocol.ChainSync.Type

import           Ouroboros.Network.Testing.ConcreteBlock (Block (..),
                     BlockHeader (..))
import           Test.ChainGenerators ()
import           Test.ChainProducerState (ChainProducerStateForkTest (..))
import           Test.Ouroboros.Network.Testing.Utils (splits2, splits3)

import           Test.QuickCheck hiding (Result)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests = testGroup "Ouroboros.Network.Protocol.ChainSyncProtocol"
  [ testProperty "direct ST" propChainSyncDirectST
  , testProperty "direct IO" propChainSyncDirectIO
  , testProperty "connect ST" propChainSyncConnectST
  , testProperty "connect IO" propChainSyncConnectIO
  , testProperty "codec"          prop_codec_ChainSync
  , testProperty "codec 2-splits" prop_codec_splits2_ChainSync
  , testProperty "codec 3-splits" $ withMaxSuccess 30 prop_codec_splits3_ChainSync
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
     )
  => (forall a b. ChainSyncServer Block (Point Block) m a
      -> ChainSyncClient Block (Point Block) m b
      -> m ())
  -> ChainProducerStateForkTest
  -> m Property
chainSyncForkExperiment run (ChainProducerStateForkTest cps chain) = do
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
  return (pchain === cchain)

propChainSyncDirectST :: ChainProducerStateForkTest -> Property
propChainSyncDirectST cps =
    runSimOrThrow $
      chainSyncForkExperiment ((fmap . fmap) void direct) cps

propChainSyncDirectIO :: ChainProducerStateForkTest -> Property
propChainSyncDirectIO cps =
    ioProperty $
      chainSyncForkExperiment ((fmap . fmap) void direct) cps

propChainSyncConnectST :: ChainProducerStateForkTest -> Property
propChainSyncConnectST cps =
    runSimOrThrow $
      chainSyncForkExperiment
        (\ser cli ->
            void $ connect (chainSyncClientPeer cli) (chainSyncServerPeer ser)
        ) cps

propChainSyncConnectIO :: ChainProducerStateForkTest -> Property
propChainSyncConnectIO cps =
    ioProperty $
      chainSyncForkExperiment
        (\ser cli ->
            void $  connect (chainSyncClientPeer cli) (chainSyncServerPeer ser)
        ) cps

instance Arbitrary (AnyMessageAndAgency (ChainSync BlockHeader (Point BlockHeader))) where
  arbitrary = oneof
    [ return $ AnyMessageAndAgency (ClientAgency TokIdle) MsgRequestNext
    , return $ AnyMessageAndAgency (ServerAgency (TokNext TokCanAwait)) MsgAwaitReply

    , AnyMessageAndAgency (ServerAgency (TokNext TokCanAwait))
        <$> (MsgRollForward <$> arbitrary
                            <*> arbitrary)

    , AnyMessageAndAgency (ServerAgency (TokNext TokMustReply))
        <$> (MsgRollForward <$> arbitrary
                            <*> arbitrary)

    , AnyMessageAndAgency (ServerAgency (TokNext TokCanAwait))
        <$> (MsgRollBackward <$> arbitrary
                             <*> arbitrary)

    , AnyMessageAndAgency (ServerAgency (TokNext TokMustReply))
        <$> (MsgRollBackward <$> arbitrary
                             <*> arbitrary)

    , AnyMessageAndAgency (ClientAgency TokIdle) . MsgFindIntersect
        <$> arbitrary

    , AnyMessageAndAgency (ServerAgency TokIntersect)
        <$> (MsgIntersectImproved <$> arbitrary
                                  <*> arbitrary)

    , AnyMessageAndAgency (ServerAgency TokIntersect) . MsgIntersectUnchanged
        <$> arbitrary

    , return $ AnyMessageAndAgency (ClientAgency TokIdle) MsgDone
    ]

instance (Show header, Show point) => Show (AnyMessageAndAgency (ChainSync header point)) where
  show (AnyMessageAndAgency _ msg) = show msg

instance (Eq header, Eq point) => Eq (AnyMessage (ChainSync header point)) where
  AnyMessage MsgRequestNext               == AnyMessage MsgRequestNext               = True
  AnyMessage MsgAwaitReply                == AnyMessage MsgAwaitReply                = True
  AnyMessage (MsgRollForward h1 p1)       == AnyMessage (MsgRollForward h2 p2)       = h1 == h2 && p1 == p2
  AnyMessage (MsgRollBackward p1 h1)      == AnyMessage (MsgRollBackward p2 h2)      = p1 == p2 && h1 == h2
  AnyMessage (MsgFindIntersect ps1)       == AnyMessage (MsgFindIntersect ps2)       = ps1 == ps2
  AnyMessage (MsgIntersectImproved p1 h1) == AnyMessage (MsgIntersectImproved p2 h2) = p1 == p2 && h1 == h2
  AnyMessage (MsgIntersectUnchanged h1)   == AnyMessage (MsgIntersectUnchanged h2)   = h1 == h2
  AnyMessage MsgDone                      == AnyMessage MsgDone                      = True
  _                                       == _                                       = False

prop_codec_ChainSync
  :: AnyMessageAndAgency (ChainSync BlockHeader (Point BlockHeader))
  -> Bool
prop_codec_ChainSync msg =
    ST.runST $ prop_codecM (codecChainSync S.encode S.decode S.encode S.decode) msg

prop_codec_splits2_ChainSync
  :: AnyMessageAndAgency (ChainSync BlockHeader (Point BlockHeader))
  -> Bool
prop_codec_splits2_ChainSync msg =
    ST.runST $ prop_codec_splitsM
      splits2
      (codecChainSync S.encode S.decode S.encode S.decode)
      msg

prop_codec_splits3_ChainSync
  :: AnyMessageAndAgency (ChainSync BlockHeader (Point BlockHeader))
  -> Bool
prop_codec_splits3_ChainSync msg =
    ST.runST $ prop_codec_splitsM
      splits3
      (codecChainSync S.encode S.decode S.encode S.decode)
      msg

chainSyncDemo
  :: forall m.
     ( MonadST m
     , MonadSTM m
     , MonadFork m
     , MonadThrow m
     )
  => Channel m ByteString
  -> Channel m ByteString
  -> ChainProducerStateForkTest
  -> m Property
chainSyncDemo clientChan serverChan (ChainProducerStateForkTest cps chain) = do
  let pchain = ChainProducerState.producerChain cps
  cpsVar   <- atomically $ newTVar cps
  chainVar <- atomically $ newTVar chain
  doneVar  <- atomically $ newTVar False

  let server :: ChainSyncServer Block (Point Block) m a
      server = ChainSyncExamples.chainSyncServerExample
        (error "chainSyncServerExample: lazy in the result type")
        cpsVar

      client = ChainSyncExamples.chainSyncClientExample chainVar (testClient doneVar (Chain.headPoint pchain))

  void $ fork (void $ runPeer nullTracer (codecChainSync S.encode S.decode S.encode S.decode) "server" serverChan (chainSyncServerPeer server))
  void $ fork (void $ runPeer nullTracer (codecChainSync S.encode S.decode S.encode S.decode) "client" clientChan (chainSyncClientPeer client))

  atomically $ do
    done <- readTVar doneVar
    unless done retry

  cchain <- atomically $ readTVar chainVar
  return (pchain === cchain)

propChainSyncDemoST
  :: ChainProducerStateForkTest
  -> Property
propChainSyncDemoST cps =
  runSimOrThrow $ do
    (clientChan, serverChan) <- createConnectedChannels
    chainSyncDemo clientChan serverChan cps

propChainSyncDemoIO
  :: ChainProducerStateForkTest
  -> Property
propChainSyncDemoIO cps =
  ioProperty $ do
    (clientChan, serverChan) <- createConnectedChannels
    chainSyncDemo clientChan serverChan cps

propChainSyncPipe
  :: ChainProducerStateForkTest
  -> Property
propChainSyncPipe cps =
  ioProperty $ do
    (clientChan, serverChan) <- createPipeConnectedChannels
    chainSyncDemo clientChan serverChan cps
