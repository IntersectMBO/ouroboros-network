{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import           Ouroboros.Network.Block (Serialised (..), StandardHash,
                     Tip (..), WithBlockSize (..), castPoint, decodeTip,
                     decodeWithBlockSize, encodeTip, encodeWithBlockSize)
import           Ouroboros.Network.MockChain.Chain (Chain, Point)
import qualified Ouroboros.Network.MockChain.Chain as Chain
import qualified Ouroboros.Network.MockChain.ProducerState as ChainProducerState

import           Ouroboros.Network.Protocol.ChainSync.Client
import           Ouroboros.Network.Protocol.ChainSync.ClientPipelined
import           Ouroboros.Network.Protocol.ChainSync.Codec
import           Ouroboros.Network.Protocol.ChainSync.Direct
import           Ouroboros.Network.Protocol.ChainSync.DirectPipelined
import           Ouroboros.Network.Protocol.ChainSync.Examples (Client)
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
  , testProperty "codecSerialised"            prop_codec_ChainSyncSerialised
  , testProperty "codecSerialised 2-splits"   prop_codec_splits2_ChainSyncSerialised
  , testProperty "codecSerialised 3-splits" $ withMaxSuccess 30
                                              prop_codec_splits3_ChainSyncSerialised
  , testProperty "codecSerialised cbor"       prop_codec_cbor_ChainSyncSerialised
  , testProperty "codec/codecSerialised bin compat"  prop_codec_binary_compat_ChainSync_ChainSyncSerialised
  , testProperty "codecSerialised/codec bin compat"  prop_codec_binary_compat_ChainSyncSerialised_ChainSync
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
  => (forall a b. ChainSyncServer Block (Tip Block) m a
      -> ChainSyncClient Block (Tip Block) m b
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
  => (forall a b. ChainSyncServer Block (Tip Block) m a
      -> ChainSyncClientPipelined Block (Tip Block) m b
      -> m ())
  -> (forall a. StrictTVar m (Chain Block)
      -> Client Block (Tip Block) m a
      -> ChainSyncClientPipelined Block (Tip Block) m a)
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
      client :: ChainSyncClientPipelined Block (Tip Block) m ()
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

genChainSync :: Gen (Point header)
             -> Gen header
             -> Gen tip
             -> Gen (AnyMessageAndAgency (ChainSync header tip))
genChainSync genPoint genHeader genTip = oneof
    [ return $ AnyMessageAndAgency (ClientAgency TokIdle) MsgRequestNext
    , return $ AnyMessageAndAgency (ServerAgency (TokNext TokCanAwait)) MsgAwaitReply

    , AnyMessageAndAgency (ServerAgency (TokNext TokCanAwait))
        <$> (MsgRollForward <$> genHeader
                            <*> genTip)

    , AnyMessageAndAgency (ServerAgency (TokNext TokMustReply))
        <$> (MsgRollForward <$> genHeader
                            <*> genTip)

    , AnyMessageAndAgency (ServerAgency (TokNext TokCanAwait))
        <$> (MsgRollBackward <$> genPoint
                             <*> genTip)

    , AnyMessageAndAgency (ServerAgency (TokNext TokMustReply))
        <$> (MsgRollBackward <$> genPoint
                             <*> genTip)

    , AnyMessageAndAgency (ClientAgency TokIdle) . MsgFindIntersect
        <$> listOf genPoint

    , AnyMessageAndAgency (ServerAgency TokIntersect)
        <$> (MsgIntersectFound <$> genPoint
                               <*> genTip)

    , AnyMessageAndAgency (ServerAgency TokIntersect)
        <$> (MsgIntersectNotFound <$> genTip)

    , return $ AnyMessageAndAgency (ClientAgency TokIdle) MsgDone
    ]

genWithBlockSize :: Gen a -> Gen (WithBlockSize a)
genWithBlockSize genA = WithBlockSize <$> choose (1, 100) <*> genA

instance Arbitrary a => Arbitrary (WithBlockSize a) where
  arbitrary = genWithBlockSize arbitrary

instance Arbitrary (AnyMessageAndAgency (ChainSync (WithBlockSize BlockHeader) (Tip BlockHeader))) where
  arbitrary = genChainSync (castPoint <$> genPoint) arbitrary genTip
    where
      genTip = Tip <$> arbitrary <*> arbitrary

      genPoint :: Gen (Point BlockHeader)
      genPoint = arbitrary

instance Arbitrary (AnyMessageAndAgency (ChainSync (WithBlockSize (Serialised BlockHeader)) (Tip BlockHeader))) where
  arbitrary = genChainSync (castPoint <$> genPoint)
                           (genWithBlockSize (serialiseBlock <$> arbitrary))
                           genTip
    where
      genTip = Tip <$> arbitrary <*> arbitrary

      genPoint :: Gen (Point BlockHeader)
      genPoint = arbitrary

      serialiseBlock :: BlockHeader -> Serialised BlockHeader
      serialiseBlock = Serialised . S.serialise

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
         , S.Serialise (Chain.HeaderHash tip)
         )
      => Codec (ChainSync (WithBlockSize block) (Tip tip))
               S.DeserialiseFailure
               m ByteString
codec = codecChainSync encodeWithBlockSize  decodeWithBlockSize
                       S.encode (fmap const S.decode)
                       S.encode             S.decode
                       (encodeTip S.encode) (decodeTip S.decode)

prop_codec_ChainSync
  :: AnyMessageAndAgency (ChainSync (WithBlockSize BlockHeader) (Tip BlockHeader))
  -> Bool
prop_codec_ChainSync msg =
    ST.runST $ prop_codecM codec msg

prop_codec_splits2_ChainSync
  :: AnyMessageAndAgency (ChainSync (WithBlockSize BlockHeader) (Tip BlockHeader))
  -> Bool
prop_codec_splits2_ChainSync msg =
    ST.runST $ prop_codec_splitsM
      splits2
      codec
      msg

prop_codec_splits3_ChainSync
  :: AnyMessageAndAgency (ChainSync (WithBlockSize BlockHeader) (Tip BlockHeader))
  -> Bool
prop_codec_splits3_ChainSync msg =
    ST.runST $ prop_codec_splitsM
      splits3
      codec
      msg
prop_codec_cbor
  :: AnyMessageAndAgency (ChainSync (WithBlockSize BlockHeader) (Tip BlockHeader))
  -> Bool
prop_codec_cbor msg =
    ST.runST (prop_codec_cborM codec msg)

codecSerialised
  :: ( MonadST m
     , S.Serialise (Chain.HeaderHash block)
     , S.Serialise (Chain.HeaderHash tip)
     )
  => Codec (ChainSync (WithBlockSize (Serialised block)) (Tip tip))
           S.DeserialiseFailure
           m ByteString
codecSerialised = codecChainSyncSerialised
    encodeWithBlockSize  decodeWithBlockSize
    S.encode             S.decode
    (encodeTip S.encode) (decodeTip S.decode)

prop_codec_ChainSyncSerialised
  :: AnyMessageAndAgency (ChainSync (WithBlockSize (Serialised BlockHeader)) (Tip BlockHeader))
  -> Bool
prop_codec_ChainSyncSerialised msg =
    ST.runST $ prop_codecM codecSerialised msg

prop_codec_splits2_ChainSyncSerialised
  :: AnyMessageAndAgency (ChainSync (WithBlockSize (Serialised BlockHeader)) (Tip BlockHeader))
  -> Bool
prop_codec_splits2_ChainSyncSerialised msg =
    ST.runST $ prop_codec_splitsM
      splits2
      codecSerialised
      msg

prop_codec_splits3_ChainSyncSerialised
  :: AnyMessageAndAgency (ChainSync (WithBlockSize (Serialised BlockHeader)) (Tip BlockHeader))
  -> Bool
prop_codec_splits3_ChainSyncSerialised msg =
    ST.runST $ prop_codec_splitsM
      splits3
      codecSerialised
      msg

prop_codec_cbor_ChainSyncSerialised
  :: AnyMessageAndAgency (ChainSync (WithBlockSize (Serialised BlockHeader)) (Tip BlockHeader))
  -> Bool
prop_codec_cbor_ChainSyncSerialised msg =
    ST.runST (prop_codec_cborM codecSerialised msg)

prop_codec_binary_compat_ChainSync_ChainSyncSerialised
  :: AnyMessageAndAgency (ChainSync (WithBlockSize BlockHeader) (Tip BlockHeader))
  -> Bool
prop_codec_binary_compat_ChainSync_ChainSyncSerialised msg =
    ST.runST (prop_codec_binary_compatM codec codecSerialised stokEq msg)
  where
    stokEq
      :: forall pr (stA :: ChainSync (WithBlockSize BlockHeader) (Tip BlockHeader)).
         PeerHasAgency pr stA
      -> SamePeerHasAgency pr (ChainSync (WithBlockSize (Serialised BlockHeader)) (Tip BlockHeader))
    stokEq (ClientAgency ca) = case ca of
      TokIdle -> SamePeerHasAgency $ ClientAgency TokIdle
    stokEq (ServerAgency sa) = case sa of
      TokNext k    -> SamePeerHasAgency $ ServerAgency (TokNext k)
      TokIntersect -> SamePeerHasAgency $ ServerAgency TokIntersect

prop_codec_binary_compat_ChainSyncSerialised_ChainSync
  :: AnyMessageAndAgency (ChainSync (WithBlockSize (Serialised BlockHeader)) (Tip BlockHeader))
  -> Bool
prop_codec_binary_compat_ChainSyncSerialised_ChainSync msg =
    ST.runST (prop_codec_binary_compatM codecSerialised codec stokEq msg)
  where
    stokEq
      :: forall pr (stA :: ChainSync (WithBlockSize (Serialised BlockHeader)) (Tip BlockHeader)).
         PeerHasAgency pr stA
      -> SamePeerHasAgency pr (ChainSync (WithBlockSize BlockHeader) (Tip BlockHeader))
    stokEq (ClientAgency ca) = case ca of
      TokIdle -> SamePeerHasAgency $ ClientAgency TokIdle
    stokEq (ServerAgency sa) = case sa of
      TokNext k    -> SamePeerHasAgency $ ServerAgency (TokNext k)
      TokIntersect -> SamePeerHasAgency $ ServerAgency TokIntersect

codecBlock :: MonadST m
           => Codec (ChainSync Block (Tip Block))
                    S.DeserialiseFailure
                    m ByteString
codecBlock = codecChainSync id                   id
                            S.encode (fmap const S.decode)
                            S.encode             S.decode
                            (encodeTip S.encode) (decodeTip S.decode)

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

  let server :: ChainSyncServer Block (Tip Block) m a
      server = ChainSyncExamples.chainSyncServerExample
        (error "chainSyncServerExample: lazy in the result type")
        cpsVar

      client :: ChainSyncClient Block (Tip Block) m ()
      client = ChainSyncExamples.chainSyncClientExample chainVar (testClient doneVar (Chain.headPoint pchain))

  void $ fork (void $ runPeer nullTracer codecBlock "server" serverChan (chainSyncServerPeer server))
  void $ fork (void $ runPeer nullTracer codecBlock "client" clientChan (chainSyncClientPeer client))

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
      -> Client Block (Tip Block) m a
      -> ChainSyncClientPipelined Block (Tip Block) m a)
  -> ChainProducerStateForkTest
  -> m Property
chainSyncDemoPipelined clientChan serverChan mkClient (ChainProducerStateForkTest cps chain) = do
  let pchain = ChainProducerState.producerChain cps
  cpsVar   <- atomically $ newTVar cps
  chainVar <- atomically $ newTVar chain
  doneVar  <- atomically $ newTVar False

  let server :: ChainSyncServer Block (Tip Block) m a
      server = ChainSyncExamples.chainSyncServerExample
        (error "chainSyncServerExample: lazy in the result type")
        cpsVar

      client :: ChainSyncClientPipelined Block (Tip Block) m ()
      client = mkClient chainVar (testClient doneVar (Chain.headPoint pchain))

  void $ fork (void $ runPeer nullTracer codecBlock "server" serverChan (chainSyncServerPeer server))
  void $ fork (void $ runPipelinedPeer nullTracer codecBlock "client" clientChan (chainSyncClientPeerPipelined client))

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
      (clientChan, serverChan) <- createConnectedBufferedChannels (fromIntegral omin)
      chainSyncDemoPipelined
        clientChan serverChan
        (ChainSyncExamples.chainSyncClientPipelinedMin omax)
        cps
  where
    omin = min n m
    omax = max n m
