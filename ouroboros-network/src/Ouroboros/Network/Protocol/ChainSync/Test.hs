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

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Tracer (nullTracer)

import           Control.Monad.IOSim (runSimOrThrow)

import           Network.TypedProtocol.Channel
import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Driver
import           Network.TypedProtocol.Proofs (connect, connectPipelined)

import           Ouroboros.Network.Channel

import           Ouroboros.Network.Block (BlockNo, StandardHash)
import           Ouroboros.Network.MockChain.Chain (Chain, Point)
import qualified Ouroboros.Network.MockChain.Chain as Chain
import qualified Ouroboros.Network.MockChain.ProducerState as ChainProducerState

import           Ouroboros.Network.Protocol.ChainSync.Client
import           Ouroboros.Network.Protocol.ChainSync.ClientPipelined
import           Ouroboros.Network.Protocol.ChainSync.Codec
import           Ouroboros.Network.Protocol.ChainSync.Direct
import           Ouroboros.Network.Protocol.ChainSync.DirectPipelined
import           Ouroboros.Network.Protocol.ChainSync.Examples (Client,
                     ExampleTip (..))
import qualified Ouroboros.Network.Protocol.ChainSync.Examples as ChainSyncExamples
import qualified Ouroboros.Network.Protocol.ChainSync.ExamplesPipelined as ChainSyncExamples
import           Ouroboros.Network.Protocol.ChainSync.Server
import           Ouroboros.Network.Protocol.ChainSync.Type

import           Ouroboros.Network.Testing.ConcreteBlock (Block (..),
                     BlockHeader (..))
import           Test.ChainGenerators ()
import           Test.ChainProducerState (ChainProducerStateForkTest (..))
import           Test.Ouroboros.Network.Testing.Utils (prop_codec_cborM,
                     splits2, splits3)

import           Test.QuickCheck hiding (Result)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests = testGroup "Ouroboros.Network.Protocol.ChainSyncProtocol"
  [ testProperty "direct ST" propChainSyncDirectST
  , testProperty "direct IO" propChainSyncDirectIO
  , testProperty "connect ST" propChainSyncConnectST
  , testProperty "connect IO" propChainSyncConnectIO
  , testProperty "directPipelinedMax ST" propChainSyncPipeliendMaxDirectST
  , testProperty "directPipelinedMax IO" propChainSyncPipeliendMaxDirectIO
  , testProperty "directPipelinedMin ST" propChainSyncPipeliendMinDirectST
  , testProperty "directPipelinedMin IO" propChainSyncPipeliendMinDirectIO
  , testProperty "connectPipelinedMax ST" propChainSyncPipelinedMaxConnectST
  , testProperty "connectPipelinedMin ST" propChainSyncPipelinedMinConnectST
  , testProperty "connectPipelinedMax IO" propChainSyncPipelinedMaxConnectIO
  , testProperty "connectPipelinedMin IO" propChainSyncPipelinedMinConnectIO
  , testProperty "codec"          prop_codec_ChainSync
  , testProperty "codec 2-splits" prop_codec_splits2_ChainSync
  , testProperty "codec 3-splits" $ withMaxSuccess 30 prop_codec_splits3_ChainSync
  , testProperty "codec cbor"     prop_codec_cbor
  , testProperty "demo ST" propChainSyncDemoST
  , testProperty "demo IO" propChainSyncDemoIO
  , testProperty "demoPipelinedMax ST"     propChainSyncDemoPipelinedMaxST
  , testProperty "demoPipelinedMax IO"     propChainSyncDemoPipelinedMaxIO
  , testProperty "demoPipelinedMin ST"     propChainSyncDemoPipelinedMinST
  , testProperty "demoPipelinedMin IO"     propChainSyncDemoPipelinedMinIO
  , testProperty "demoPipelinedLowHigh ST" propChainSyncDemoPipelinedLowHighST
  , testProperty "demoPipelinedLowHigh IO" propChainSyncDemoPipelinedLowHighIO
  , testProperty "demoPipelinedMin IO (buffered)"
                                       propChainSyncDemoPipelinedMinBufferedIO
  , testProperty "demo IO" propChainSyncDemoIO
  , testProperty "pipe demo" propChainSyncPipe
  ]

-- | Testing @'Client'@ which stops at a given point.
--
testClient
  :: MonadSTM m
  => StrictTVar m Bool
  -> Point Block
  -> ChainSyncExamples.Client Block blockInfo m ()
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
  => (forall a b. ChainSyncServer Block (ExampleTip Block) m a
      -> ChainSyncClient Block (ExampleTip Block) m b
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


--
-- Properties of pipelined client
--

-- | An experiment in which the client has a fork of the server chain.  The
-- experiment finishes successfully if the client receives the server's chain.
--
chainSyncPipelinedForkExperiment
  :: forall m.
     ( MonadST m
     , MonadSTM m
     )
  => (forall a b. ChainSyncServer Block (ExampleTip Block) m a
      -> ChainSyncClientPipelined Block (ExampleTip Block) m b
      -> m ())
  -> (forall a. StrictTVar m (Chain Block)
      -> Client Block (ExampleTip Block) m a
      -> ChainSyncClientPipelined Block (ExampleTip Block) m a)
  -> ChainProducerStateForkTest
  -> m Bool
chainSyncPipelinedForkExperiment run mkClient (ChainProducerStateForkTest cps chain) = do
  let pchain = ChainProducerState.producerChain cps
  cpsVar   <- atomically $ newTVar cps
  chainVar <- atomically $ newTVar chain
  doneVar  <- atomically $ newTVar False
  let server = ChainSyncExamples.chainSyncServerExample
        (error "chainSyncServerExample: lazy in the result type")
        cpsVar
      client :: ChainSyncClientPipelined Block (ExampleTip Block) m ()
      client = mkClient chainVar (testClient doneVar (Chain.headPoint pchain))
  _ <- run server client

  cchain <- atomically $ readTVar chainVar
  return (pchain == cchain)

--
-- Piplined direct tests
--

propChainSyncPipeliendMaxDirectST :: ChainProducerStateForkTest
                                  -> Positive Int
                                  -> Bool
propChainSyncPipeliendMaxDirectST cps (Positive omax) =
    runSimOrThrow $
      chainSyncPipelinedForkExperiment
        ((fmap . fmap) void directPipelined)
        (ChainSyncExamples.chainSyncClientPipelinedMax omax)
        cps

propChainSyncPipeliendMaxDirectIO :: ChainProducerStateForkTest
                                  -> Positive Int
                                  -> Property
propChainSyncPipeliendMaxDirectIO cps (Positive omax) =
    ioProperty $
      chainSyncPipelinedForkExperiment
        ((fmap . fmap) void directPipelined)
        (ChainSyncExamples.chainSyncClientPipelinedMax omax)
        cps

propChainSyncPipeliendMinDirectST :: ChainProducerStateForkTest
                                  -> Positive Int
                                  -> Bool
propChainSyncPipeliendMinDirectST cps (Positive omax) =
    runSimOrThrow $
      chainSyncPipelinedForkExperiment
        ((fmap . fmap) void directPipelined)
        (ChainSyncExamples.chainSyncClientPipelinedMin omax)
        cps

propChainSyncPipeliendMinDirectIO :: ChainProducerStateForkTest
                                  -> Positive Int
                                  -> Property
propChainSyncPipeliendMinDirectIO cps (Positive omax) =
    ioProperty $
      chainSyncPipelinedForkExperiment
        ((fmap . fmap) void directPipelined)
        (ChainSyncExamples.chainSyncClientPipelinedMin omax)
        cps

--
-- Piplined connect tests
--

propChainSyncPipelinedMaxConnectST :: ChainProducerStateForkTest
                                   -> [Bool]
                                   -> Positive Int
                                   -> Bool
propChainSyncPipelinedMaxConnectST cps choices (Positive omax) =
    runSimOrThrow $
      chainSyncPipelinedForkExperiment
        (\ser cli ->
            void $ connectPipelined
              choices
              (chainSyncClientPeerPipelined cli)
              (chainSyncServerPeer ser)
        )
        (ChainSyncExamples.chainSyncClientPipelinedMax omax)
        cps


propChainSyncPipelinedMinConnectST :: ChainProducerStateForkTest
                                   -> [Bool]
                                   -> Positive Int
                                   -> Bool
propChainSyncPipelinedMinConnectST cps choices (Positive omax) =
    runSimOrThrow $
      chainSyncPipelinedForkExperiment
        (\ser cli ->
            void $ connectPipelined
              choices
              (chainSyncClientPeerPipelined cli)
              (chainSyncServerPeer ser)
        )
        (ChainSyncExamples.chainSyncClientPipelinedMin omax)
        cps

propChainSyncPipelinedMaxConnectIO :: ChainProducerStateForkTest
                                   -> [Bool]
                                   -> Positive Int
                                   -> Property
propChainSyncPipelinedMaxConnectIO cps choices (Positive omax) =
    ioProperty $
      chainSyncPipelinedForkExperiment
        (\ser cli ->
            void $ connectPipelined
              choices
              (chainSyncClientPeerPipelined cli)
              (chainSyncServerPeer ser)
        )
        (ChainSyncExamples.chainSyncClientPipelinedMax omax)
        cps

propChainSyncPipelinedMinConnectIO :: ChainProducerStateForkTest
                                   -> [Bool]
                                   -> Positive Int
                                   -> Property
propChainSyncPipelinedMinConnectIO cps choices (Positive omax) =
    ioProperty $
      chainSyncPipelinedForkExperiment
        (\ser cli ->
            void $ connectPipelined
              choices
              (chainSyncClientPeerPipelined cli)
              (chainSyncServerPeer ser)
        )
        (ChainSyncExamples.chainSyncClientPipelinedMin omax)
        cps

instance Arbitrary (AnyMessageAndAgency (ChainSync BlockHeader (Point BlockHeader, BlockNo))) where
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
        <$> (MsgIntersectFound <$> arbitrary
                               <*> arbitrary)

    , AnyMessageAndAgency (ServerAgency TokIntersect)
        <$> (MsgIntersectNotFound <$> arbitrary)

    , return $ AnyMessageAndAgency (ClientAgency TokIdle) MsgDone
    ]

instance (StandardHash header, Show header, Show tip) => Show (AnyMessageAndAgency (ChainSync header tip)) where
  show (AnyMessageAndAgency _ msg) = show msg

instance ( StandardHash header
         , Eq header
         , Eq tip
         ) => Eq (AnyMessage (ChainSync header tip)) where
  AnyMessage MsgRequestNext              == AnyMessage MsgRequestNext              = True
  AnyMessage MsgAwaitReply               == AnyMessage MsgAwaitReply               = True
  AnyMessage (MsgRollForward h1 tip1)    == AnyMessage (MsgRollForward h2 tip2)    = h1 == h2 && tip1 == tip2
  AnyMessage (MsgRollBackward p1 tip1)   == AnyMessage (MsgRollBackward p2 tip2)   = p1 == p2 && tip1 == tip2
  AnyMessage (MsgFindIntersect ps1)      == AnyMessage (MsgFindIntersect ps2)      = ps1 == ps2
  AnyMessage (MsgIntersectFound p1 tip1) == AnyMessage (MsgIntersectFound p2 tip2) = p1 == p2 && tip1 == tip2
  AnyMessage (MsgIntersectNotFound tip1) == AnyMessage (MsgIntersectNotFound tip2) = tip1 == tip2
  AnyMessage MsgDone                     == AnyMessage MsgDone                     = True
  _                                      == _                                      = False

codec :: ( MonadST m
         , S.Serialise block
         , S.Serialise (Chain.HeaderHash block)
         , S.Serialise tip
         )
      => Codec (ChainSync block tip)
               S.DeserialiseFailure
               m ByteString
codec = codecChainSync S.encode (fmap const S.decode)
                       S.encode             S.decode
                       S.encode             S.decode

prop_codec_ChainSync
  :: AnyMessageAndAgency (ChainSync BlockHeader (Point BlockHeader, BlockNo))
  -> Bool
prop_codec_ChainSync msg =
    ST.runST $ prop_codecM codec msg

prop_codec_splits2_ChainSync
  :: AnyMessageAndAgency (ChainSync BlockHeader (Point BlockHeader, BlockNo))
  -> Bool
prop_codec_splits2_ChainSync msg =
    ST.runST $ prop_codec_splitsM
      splits2
      codec
      msg

prop_codec_splits3_ChainSync
  :: AnyMessageAndAgency (ChainSync BlockHeader (Point BlockHeader, BlockNo))
  -> Bool
prop_codec_splits3_ChainSync msg =
    ST.runST $ prop_codec_splitsM
      splits3
      codec
      msg
prop_codec_cbor
  :: AnyMessageAndAgency (ChainSync BlockHeader (Point BlockHeader, BlockNo))
  -> Bool
prop_codec_cbor msg =
    ST.runST (prop_codec_cborM codec msg)

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

  let server :: ChainSyncServer Block (ExampleTip Block) m a
      server = ChainSyncExamples.chainSyncServerExample
        (error "chainSyncServerExample: lazy in the result type")
        cpsVar

      client :: ChainSyncClient Block (ExampleTip Block) m ()
      client = ChainSyncExamples.chainSyncClientExample chainVar (testClient doneVar (Chain.headPoint pchain))

  void $ fork (void $ runPeer nullTracer codec "server" serverChan (chainSyncServerPeer server))
  void $ fork (void $ runPeer nullTracer codec "client" clientChan (chainSyncClientPeer client))

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

--
-- Piplined demo
--

chainSyncDemoPipelined
  :: forall m.
     ( MonadST    m
     , MonadSTM   m
     , MonadFork  m
     , MonadAsync m
     , MonadThrow m
     , MonadSay   m
     )
  => Channel m ByteString
  -> Channel m ByteString
  -> (forall a. StrictTVar m (Chain Block)
      -> Client Block (ExampleTip Block) m a
      -> ChainSyncClientPipelined Block (ExampleTip Block) m a)
  -> ChainProducerStateForkTest
  -> m Property
chainSyncDemoPipelined clientChan serverChan mkClient (ChainProducerStateForkTest cps chain) = do
  let pchain = ChainProducerState.producerChain cps
  cpsVar   <- atomically $ newTVar cps
  chainVar <- atomically $ newTVar chain
  doneVar  <- atomically $ newTVar False

  let server :: ChainSyncServer Block (ExampleTip Block) m a
      server = ChainSyncExamples.chainSyncServerExample
        (error "chainSyncServerExample: lazy in the result type")
        cpsVar

      client :: ChainSyncClientPipelined Block (ExampleTip Block) m ()
      client = mkClient chainVar (testClient doneVar (Chain.headPoint pchain))

  void $ fork (void $ runPeer nullTracer codec "server" serverChan (chainSyncServerPeer server))
  void $ fork (void $ runPipelinedPeer nullTracer codec "client" clientChan (chainSyncClientPeerPipelined client))

  atomically $ do
    done <- readTVar doneVar
    unless done retry

  cchain <- atomically $ readTVar chainVar
  return (pchain === cchain)

propChainSyncDemoPipelinedMaxST
  :: ChainProducerStateForkTest
  -> Positive Int
  -> Property
propChainSyncDemoPipelinedMaxST cps (Positive omax) =
  runSimOrThrow $ do
    (clientChan, serverChan) <- createPipelineTestChannels (fromIntegral omax)
    chainSyncDemoPipelined
      clientChan serverChan
      (ChainSyncExamples.chainSyncClientPipelinedMax omax)
      cps

propChainSyncDemoPipelinedMaxIO
  :: ChainProducerStateForkTest
  -> Positive Int
  -> Property
propChainSyncDemoPipelinedMaxIO cps (Positive omax) =
  ioProperty $ do
    (clientChan, serverChan) <- createPipelineTestChannels (fromIntegral omax)
    chainSyncDemoPipelined
      clientChan serverChan
      (ChainSyncExamples.chainSyncClientPipelinedMax omax)
      cps

propChainSyncDemoPipelinedMinST
  :: ChainProducerStateForkTest
  -> Positive Int
  -> Property
propChainSyncDemoPipelinedMinST cps (Positive omax) =
  runSimOrThrow $ do
    (clientChan, serverChan) <- createPipelineTestChannels (fromIntegral omax)
    chainSyncDemoPipelined
      clientChan serverChan
      (ChainSyncExamples.chainSyncClientPipelinedMin omax)
      cps

propChainSyncDemoPipelinedMinIO
  :: ChainProducerStateForkTest
  -> Positive Int
  -> Property
propChainSyncDemoPipelinedMinIO cps (Positive omax) =
  ioProperty $ do
    (clientChan, serverChan) <- createPipelineTestChannels (fromIntegral omax)
    chainSyncDemoPipelined
      clientChan serverChan
      (ChainSyncExamples.chainSyncClientPipelinedMin omax)
      cps

propChainSyncDemoPipelinedLowHighST
  :: ChainProducerStateForkTest
  -> Positive Int
  -> Positive Int
  -> Property
propChainSyncDemoPipelinedLowHighST cps (Positive x) (Positive y) =
    runSimOrThrow $ do
      (clientChan, serverChan) <- createPipelineTestChannels (fromIntegral highMark)
      chainSyncDemoPipelined
        clientChan serverChan
        (ChainSyncExamples.chainSyncClientPipelinedLowHigh lowMark highMark)
        cps
  where
    lowMark = min x y
    highMark = max x y

propChainSyncDemoPipelinedLowHighIO
  :: ChainProducerStateForkTest
  -> Positive Int
  -> Positive Int
  -> Property
propChainSyncDemoPipelinedLowHighIO cps (Positive x) (Positive y) =
    ioProperty $ do
      (clientChan, serverChan) <- createPipelineTestChannels (fromIntegral highMark)
      chainSyncDemoPipelined
        clientChan serverChan
        (ChainSyncExamples.chainSyncClientPipelinedLowHigh lowMark highMark)
        cps
  where
    lowMark = min x y
    highMark = max x y

propChainSyncDemoPipelinedMinBufferedIO
  :: ChainProducerStateForkTest
  -> Positive Int
  -> Positive Int
  -> Property
propChainSyncDemoPipelinedMinBufferedIO cps (Positive n) (Positive m) =
    ioProperty $ do
      (clientChan, serverChan) <- createConnectedBoundedChannels (fromIntegral omin)
      chainSyncDemoPipelined
        clientChan serverChan
        (ChainSyncExamples.chainSyncClientPipelinedMin omax)
        cps
  where
    omin = min n m
    omax = max n m
