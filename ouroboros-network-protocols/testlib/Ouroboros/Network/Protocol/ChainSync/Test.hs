{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Network.Protocol.ChainSync.Test (tests) where

import qualified Codec.Serialise as S
import           Control.Monad (unless, void)
import qualified Control.Monad.ST as ST
import           Data.ByteString.Lazy (ByteString)
import           Data.Word

import           Control.Applicative (Alternative)
import           Control.Concurrent.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadThrow
import           Control.Tracer (nullTracer)

import           Control.Monad.IOSim (runSimOrThrow)

import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Proofs (connect)

import           Ouroboros.Network.Channel
import           Ouroboros.Network.Driver

import           Ouroboros.Network.Block (BlockNo, Serialised (..),
                     StandardHash, Tip (..), decodeTip, encodeTip,
                     pattern BlockPoint, pattern GenesisPoint, unwrapCBORinCBOR,
                     wrapCBORinCBOR)
import           Ouroboros.Network.Mock.Chain (Chain, Point)
import qualified Ouroboros.Network.Mock.Chain as Chain
import           Ouroboros.Network.Mock.ConcreteBlock (Block, BlockHeader (..))
import qualified Ouroboros.Network.Mock.ProducerState as ChainProducerState

import           Ouroboros.Network.Protocol.ChainSync.Client
import           Ouroboros.Network.Protocol.ChainSync.ClientPipelined
import           Ouroboros.Network.Protocol.ChainSync.Codec
import           Ouroboros.Network.Protocol.ChainSync.Examples (Client)
import qualified Ouroboros.Network.Protocol.ChainSync.Examples as ChainSyncExamples
import qualified Ouroboros.Network.Protocol.ChainSync.ExamplesPipelined as ChainSyncExamples
import           Ouroboros.Network.Protocol.ChainSync.Server
import           Ouroboros.Network.Protocol.ChainSync.Type

import           Test.ChainGenerators ()
import           Test.ChainProducerState (ChainProducerStateForkTest (..))
import           Test.Ouroboros.Network.Testing.Utils (prop_codec_cborM,
                     prop_codec_valid_cbor_encoding, splits2, splits3)

import           Test.QuickCheck hiding (Result)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests = testGroup "Ouroboros.Network.Protocol"
  [ testGroup "ChainSync"
    [ testProperty "connect ST" propChainSyncConnectST
    , testProperty "connect IO" propChainSyncConnectIO
    , testProperty "connectPipelinedMax ST" propChainSyncPipelinedMaxConnectST
    , testProperty "connectPipelinedMin ST" propChainSyncPipelinedMinConnectST
    , testProperty "connectPipelinedMax IO" propChainSyncPipelinedMaxConnectIO
    , testProperty "connectPipelinedMin IO" propChainSyncPipelinedMinConnectIO
    , testProperty "codec"            prop_codec_ChainSync
    , testProperty "codec 2-splits"   prop_codec_splits2_ChainSync
    , testProperty "codec 3-splits"   $ withMaxSuccess 30 prop_codec_splits3_ChainSync
    , testProperty "codec cbor"       prop_codec_cbor
    , testProperty "codec valid cbor" prop_codec_valid_cbor
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
  ]

-- | Testing @'Client'@ which stops at a given point.
--
testClient
  :: MonadSTM m
  => StrictTVar m Bool
  -> Point Block
  -> ChainSyncExamples.Client Block (Point Block) blockInfo m ()
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
      ChainSyncExamples.points = \_ -> return (Right $ testClient doneVar tip)
    }

-- | An experiment in which the client has a fork of the server chain.  The
-- experiment finishes successfully if the client receives the server's chain.
--
chainSyncForkExperiment
  :: forall m.
     ( MonadST m
     , MonadSTM m
     )
  => (forall a b.
         ChainSyncServer Block (Point Block) (Tip Block) m a
      -> ChainSyncClient Block (Point Block) (Tip Block) m b
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
        id
      client = ChainSyncExamples.chainSyncClientExample chainVar (testClient doneVar (Chain.headPoint pchain))
  _ <- run server client

  cchain <- atomically $ readTVar chainVar
  return (pchain === cchain)


propChainSyncConnectST :: ChainProducerStateForkTest -> Property
propChainSyncConnectST cps =
    runSimOrThrow $
      chainSyncForkExperiment
        (\ser cli ->
            void $ connect [] [] (chainSyncClientPeer cli) (chainSyncServerPeer ser)
        ) cps

propChainSyncConnectIO :: ChainProducerStateForkTest -> Property
propChainSyncConnectIO cps =
    ioProperty $
      chainSyncForkExperiment
        (\ser cli ->
            void $  connect [] [] (chainSyncClientPeer cli) (chainSyncServerPeer ser)
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
  => (forall a b. ChainSyncServer Block (Point Block) (Tip Block) m a
      -> ChainSyncClientPipelined Block (Point Block) (Tip Block) m b
      -> m ())
  -> (forall a. StrictTVar m (Chain Block)
      -> Client Block (Point Block) (Tip Block) m a
      -> ChainSyncClientPipelined Block (Point Block) (Tip Block) m a)
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
        id
      client :: ChainSyncClientPipelined Block (Point Block) (Tip Block) m ()
      client = mkClient chainVar (testClient doneVar (Chain.headPoint pchain))
  _ <- run server client

  cchain <- atomically $ readTVar chainVar
  return (pchain == cchain)

--
-- Pipelined direct tests
--

--
-- Pipelined connect tests
--

propChainSyncPipelinedMaxConnectST :: ChainProducerStateForkTest
                                   -> [Bool]
                                   -> Positive Word32
                                   -> Bool
propChainSyncPipelinedMaxConnectST cps choices (Positive omax) =
    runSimOrThrow $
      chainSyncPipelinedForkExperiment
        (\ser cli ->
            void $ connect
              choices []
              (chainSyncClientPeerPipelined cli)
              (chainSyncServerPeer ser)
        )
        (ChainSyncExamples.chainSyncClientPipelinedMax omax)
        cps


propChainSyncPipelinedMinConnectST :: ChainProducerStateForkTest
                                   -> [Bool]
                                   -> Positive Word32
                                   -> Bool
propChainSyncPipelinedMinConnectST cps choices (Positive omax) =
    runSimOrThrow $
      chainSyncPipelinedForkExperiment
        (\ser cli ->
            void $ connect
              choices []
              (chainSyncClientPeerPipelined cli)
              (chainSyncServerPeer ser)
        )
        (ChainSyncExamples.chainSyncClientPipelinedMin omax)
        cps

propChainSyncPipelinedMaxConnectIO :: ChainProducerStateForkTest
                                   -> [Bool]
                                   -> Positive Word32
                                   -> Property
propChainSyncPipelinedMaxConnectIO cps choices (Positive omax) =
    ioProperty $
      chainSyncPipelinedForkExperiment
        (\ser cli ->
            void $ connect
              choices []
              (chainSyncClientPeerPipelined cli)
              (chainSyncServerPeer ser)
        )
        (ChainSyncExamples.chainSyncClientPipelinedMax omax)
        cps

propChainSyncPipelinedMinConnectIO :: ChainProducerStateForkTest
                                   -> [Bool]
                                   -> Positive Word32
                                   -> Property
propChainSyncPipelinedMinConnectIO cps choices (Positive omax) =
    ioProperty $
      chainSyncPipelinedForkExperiment
        (\ser cli ->
            void $ connect
              choices []
              (chainSyncClientPeerPipelined cli)
              (chainSyncServerPeer ser)
        )
        (ChainSyncExamples.chainSyncClientPipelinedMin omax)
        cps


instance (Arbitrary header, Arbitrary point, Arbitrary tip)
      => Arbitrary (AnyMessage (ChainSync header point tip)) where
  arbitrary = oneof
    [ return $ AnyMessage MsgRequestNext
    , return $ AnyMessage MsgAwaitReply

    , AnyMessage
        <$> (MsgRollForward <$> arbitrary
                            <*> arbitrary
              :: Gen (Message (ChainSync header point tip) (StNext StCanAwait) StIdle))

    , AnyMessage
        <$> (MsgRollForward <$> arbitrary
                            <*> arbitrary
              :: Gen (Message (ChainSync header point tip) (StNext StMustReply) StIdle))

    , AnyMessage
        <$> (MsgRollBackward <$> arbitrary
                             <*> arbitrary
              :: Gen (Message (ChainSync header point tip) (StNext StCanAwait) StIdle))

    , AnyMessage
        <$> (MsgRollBackward <$> arbitrary
                             <*> arbitrary
              :: Gen (Message (ChainSync header point tip) (StNext StMustReply) StIdle))

    , AnyMessage . MsgFindIntersect
        <$> listOf arbitrary

    , AnyMessage
        <$> (MsgIntersectFound <$> arbitrary
                               <*> arbitrary)

    , AnyMessage
        <$> (MsgIntersectNotFound <$> arbitrary)

    , return $ AnyMessage MsgDone
    ]

  shrink (AnyMessage MsgRequestNext) = []
  shrink (AnyMessage MsgAwaitReply) = []
  -- shrink (AnyMessage (MsgRollForward header tip)) = []
  shrink (AnyMessage (MsgRollForward header  tip :: Message (ChainSync header point tip) st st')) =
    -- NOTE: if `mkMsg` is inlined GHC-9.6.2 complains that there are multiple
    -- incoherent instances in the scope to resolve `StateTokenI`.
    let mkMsg :: header -> tip -> Message (ChainSync header point tip) st st'
        mkMsg = MsgRollForward
    in
        [ AnyMessage (mkMsg header' tip)
        | header' <- shrink header
        ]
     ++ [ AnyMessage (mkMsg header tip')
        | tip' <- shrink tip
        ]
  shrink (AnyMessage (MsgRollBackward header tip :: Message (ChainSync header point tip) st st')) =
    -- NOTE: if `mkMsg` is inlined GHC-9.6.2 complains that there are multiple
    -- incoherent instances in the scope to resolve `StateTokenI`.
    let mkMsg :: point -> tip -> Message (ChainSync header point tip) st st'
        mkMsg = MsgRollBackward
    in
       [ AnyMessage (mkMsg header' tip)
       | header' <- shrink header
       ]
    ++ [ AnyMessage (mkMsg header tip')
       | tip' <- shrink tip
       ]
  shrink (AnyMessage (MsgFindIntersect points)) =
       [ AnyMessage (MsgFindIntersect points')
       | points' <- shrink points
       ]
  shrink (AnyMessage (MsgIntersectFound point tip)) =
       [ AnyMessage (MsgIntersectFound point' tip)
       | point' <- shrink point
       ]
    ++ [ AnyMessage (MsgIntersectFound point tip')
       | tip' <- shrink tip
       ]
  shrink (AnyMessage (MsgIntersectNotFound tip)) =
       [ AnyMessage (MsgIntersectNotFound tip')
       | tip' <- shrink tip
       ]
  shrink (AnyMessage MsgDone) = []


-- type aliases to keep sizes down
type ChainSync_BlockHeader =
     ChainSync BlockHeader (Point BlockHeader) (Tip BlockHeader)

type ChainSync_Serialised_BlockHeader =
     ChainSync (Serialised BlockHeader) (Point BlockHeader) (Tip BlockHeader)

instance Arbitrary (Tip BlockHeader) where
  arbitrary = f <$> arbitrary <*> arbitrary
    where
      f :: Point BlockHeader ->  BlockNo -> Tip BlockHeader
      f GenesisPoint _     = TipGenesis
      f (BlockPoint s h) b = Tip s h b

  shrink TipGenesis = []
  shrink (Tip slotNo hash blockNo) =
       [ Tip slotNo' hash blockNo
       | slotNo' <- shrink slotNo
       ]
    ++ [ Tip slotNo hash blockNo'
       | blockNo' <- shrink blockNo
       ]

instance Arbitrary (Serialised BlockHeader) where
  arbitrary = serialiseBlock <$> arbitrary
    where
      serialiseBlock :: BlockHeader -> Serialised BlockHeader
      serialiseBlock = Serialised . S.serialise

instance ( StandardHash header
         , Eq header
         , Eq point
         , Eq tip
         ) => Eq (AnyMessage (ChainSync header point tip)) where
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
         )
      => Codec (ChainSync block (Point block) (Tip block))
               S.DeserialiseFailure
               m ByteString
codec = codecChainSync S.encode             S.decode
                       S.encode             S.decode
                       (encodeTip S.encode) (decodeTip S.decode)

codecWrapped :: ( MonadST m
                , S.Serialise block
                , S.Serialise (Chain.HeaderHash block)
                )
             => Codec (ChainSync block (Point block) (Tip block))
                      S.DeserialiseFailure
                      m ByteString
codecWrapped =
    codecChainSync (wrapCBORinCBOR S.encode) (unwrapCBORinCBOR (const <$> S.decode))
                   S.encode                  S.decode
                   (encodeTip S.encode)      (decodeTip S.decode)

prop_codec_ChainSync
  :: AnyMessage ChainSync_BlockHeader
  -> Bool
prop_codec_ChainSync msg =
    ST.runST $ prop_codecM codec msg

prop_codec_splits2_ChainSync
  :: AnyMessage ChainSync_BlockHeader
  -> Bool
prop_codec_splits2_ChainSync msg =
    ST.runST $ prop_codec_splitsM
      splits2
      codec
      msg

prop_codec_splits3_ChainSync
  :: AnyMessage ChainSync_BlockHeader
  -> Bool
prop_codec_splits3_ChainSync msg =
    ST.runST $ prop_codec_splitsM
      splits3
      codec
      msg

prop_codec_cbor
  :: AnyMessage ChainSync_BlockHeader
  -> Bool
prop_codec_cbor msg =
    ST.runST (prop_codec_cborM codec msg)

prop_codec_valid_cbor
  :: AnyMessage ChainSync_BlockHeader
  -> Property
prop_codec_valid_cbor = prop_codec_valid_cbor_encoding codec

codecSerialised
  :: ( MonadST m
     , S.Serialise (Chain.HeaderHash block)
     )
  => Codec (ChainSync (Serialised block) (Point block) (Tip block))
           S.DeserialiseFailure
           m ByteString
codecSerialised = codecChainSync
    S.encode             S.decode
    S.encode             S.decode
    (encodeTip S.encode) (decodeTip S.decode)

prop_codec_ChainSyncSerialised
  :: AnyMessage ChainSync_Serialised_BlockHeader
  -> Bool
prop_codec_ChainSyncSerialised msg =
    ST.runST $ prop_codecM codecSerialised msg

prop_codec_splits2_ChainSyncSerialised
  :: AnyMessage ChainSync_Serialised_BlockHeader
  -> Bool
prop_codec_splits2_ChainSyncSerialised msg =
    ST.runST $ prop_codec_splitsM
      splits2
      codecSerialised
      msg

prop_codec_splits3_ChainSyncSerialised
  :: AnyMessage ChainSync_Serialised_BlockHeader
  -> Bool
prop_codec_splits3_ChainSyncSerialised msg =
    ST.runST $ prop_codec_splitsM
      splits3
      codecSerialised
      msg

prop_codec_cbor_ChainSyncSerialised
  :: AnyMessage ChainSync_Serialised_BlockHeader
  -> Bool
prop_codec_cbor_ChainSyncSerialised msg =
    ST.runST (prop_codec_cborM codecSerialised msg)

prop_codec_binary_compat_ChainSync_ChainSyncSerialised
  :: AnyMessage ChainSync_BlockHeader
  -> Bool
prop_codec_binary_compat_ChainSync_ChainSyncSerialised msg =
    ST.runST (prop_codec_binary_compatM codecWrapped codecSerialised stokEq msg)
  where
    stokEq
      :: forall (stA :: ChainSync_BlockHeader).
         ActiveState stA
      => StateToken stA
      -> SomeState ChainSync_Serialised_BlockHeader
    stokEq SingIdle =
      SomeState SingIdle
    stokEq (SingNext SingCanAwait) =
      SomeState (SingNext SingCanAwait)
    stokEq (SingNext SingMustReply) =
      SomeState (SingNext SingMustReply)
    stokEq SingIntersect =
      SomeState SingIntersect
    stokEq a@SingDone = notActiveState a

prop_codec_binary_compat_ChainSyncSerialised_ChainSync
  :: AnyMessage ChainSync_Serialised_BlockHeader
  -> Bool
prop_codec_binary_compat_ChainSyncSerialised_ChainSync msg =
    ST.runST (prop_codec_binary_compatM codecSerialised codecWrapped stokEq msg)
  where
    stokEq
      :: forall (stA :: ChainSync_Serialised_BlockHeader).
         ActiveState stA
      => StateToken stA
      -> SomeState ChainSync_BlockHeader
    stokEq SingIdle =
      SomeState SingIdle
    stokEq (SingNext SingCanAwait) =
      SomeState (SingNext SingCanAwait)
    stokEq (SingNext SingMustReply) =
      SomeState (SingNext SingMustReply)
    stokEq SingIntersect =
      SomeState SingIntersect
    stokEq a@SingDone = notActiveState a

chainSyncDemo
  :: forall m.
     ( Alternative (STM m)
     , MonadAsync m
     , MonadMask m
     , MonadST m
     , MonadLabelledSTM m
     , MonadFork m
     , MonadThrow m
     , MonadThrow (STM m)
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

  let server :: ChainSyncServer Block (Point Block) (Tip Block) m a
      server = ChainSyncExamples.chainSyncServerExample
        (error "chainSyncServerExample: lazy in the result type")
        cpsVar
        id

      client :: ChainSyncClient Block (Point Block) (Tip Block) m ()
      client = ChainSyncExamples.chainSyncClientExample chainVar (testClient doneVar (Chain.headPoint pchain))

  void $ forkIO (void $ runPeer nullTracer codec serverChan (chainSyncServerPeer server))
  void $ forkIO (void $ runPeer nullTracer codec clientChan (chainSyncClientPeer client))

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
-- Pipelined demo
--

chainSyncDemoPipelined
  :: forall m.
     ( Alternative (STM m)
     , MonadMask  m
     , MonadST    m
     , MonadLabelledSTM m
     , MonadFork  m
     , MonadAsync m
     , MonadThrow m
     , MonadThrow (STM m)
     , MonadSay   m
     )
  => Channel m ByteString
  -> Channel m ByteString
  -> (forall a. StrictTVar m (Chain Block)
      -> Client                   Block (Point Block) (Tip Block) m a
      -> ChainSyncClientPipelined Block (Point Block) (Tip Block) m a)
  -> ChainProducerStateForkTest
  -> m Property
chainSyncDemoPipelined clientChan serverChan mkClient (ChainProducerStateForkTest cps chain) = do
  let pchain = ChainProducerState.producerChain cps
  cpsVar   <- atomically $ newTVar cps
  chainVar <- atomically $ newTVar chain
  doneVar  <- atomically $ newTVar False

  let server :: ChainSyncServer Block (Point Block) (Tip Block) m a
      server = ChainSyncExamples.chainSyncServerExample
        (error "chainSyncServerExample: lazy in the result type")
        cpsVar
        id

      client :: ChainSyncClientPipelined Block (Point Block) (Tip Block) m ()
      client = mkClient chainVar (testClient doneVar (Chain.headPoint pchain))

  void $ forkIO (void $ runPeer nullTracer codec serverChan (chainSyncServerPeer server))
  void $ forkIO (void $ runPeer nullTracer codec clientChan (chainSyncClientPeerPipelined client))

  atomically $ do
    done <- readTVar doneVar
    unless done retry

  cchain <- atomically $ readTVar chainVar
  return (pchain === cchain)

propChainSyncDemoPipelinedMaxST
  :: ChainProducerStateForkTest
  -> Positive Word32
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
  -> Positive Word32
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
  -> Positive Word32
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
  -> Positive Word32
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
  -> Positive Word32
  -> Positive Word32
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
  -> Positive Word32
  -> Positive Word32
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
  -> Positive Word32
  -> Positive Word32
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
