{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE NumericUnderscores       #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- orphaned arbitrary instances
{-# OPTIONS_GHC -Wno-orphans #-}

module Network.TypedProtocol.PingPong.Tests
  ( tests
  , splits2
  , splits3
  , splits2BS
  , splits3BS
  ) where


import           Network.TypedProtocol.Channel
import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Driver.Simple
import           Network.TypedProtocol.Proofs

import           Network.TypedProtocol.PingPong.Client
import           Network.TypedProtocol.PingPong.Codec
import qualified Network.TypedProtocol.PingPong.Codec.CBOR as CBOR
import           Network.TypedProtocol.PingPong.Examples
import           Network.TypedProtocol.PingPong.Server
import           Network.TypedProtocol.PingPong.Type

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTest
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTimer
import           Control.Monad.IOSim
import           Control.Monad.ST (runST)
import           Control.Tracer (Tracer (..), nullTracer)

import           Data.Bifunctor (bimap)
import           Data.Functor.Identity (Identity (..))
import           Data.List (inits, tails)

import qualified Data.ByteString.Lazy as LBS
#if !defined(mingw32_HOST_OS)
import qualified Network.Socket as Socket
import           System.Directory (removeFile)
import           System.IO
import qualified System.Posix.Files as Posix
#endif

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)


--
-- The list of all properties
--

tests :: TestTree
tests = testGroup "Network.TypedProtocol.PingPong"
  [ testProperty "direct"                prop_direct
  , testProperty "directPipelined 1"     prop_directPipelined1
  , testProperty "directPipelined 2"     prop_directPipelined2
  , testProperty "connect"               prop_connect
  , testProperty "connect_pipelined 1"   prop_connect_pipelined1
  , testProperty "connect_pipelined 2"   prop_connect_pipelined2
  , testProperty "connect_pipelined 3"   prop_connect_pipelined3
  , testProperty "connect_pipelined 4"   prop_connect_pipelined4
  , testProperty "connect_pipelined 5"   prop_connect_pipelined5
  , testProperty "connect_pipelined stm" prop_connect_pipelined_stm
  , testProperty "channel ST"            prop_channel_ST
  , testProperty "channel IO"            prop_channel_IO
  , testProperty "channel STM ST"        prop_channel_stm_ST
  , testProperty "channel STM IO"        prop_channel_stm_IO
#if !defined(mingw32_HOST_OS)
  , testProperty "namedPipePipelined"    prop_namedPipePipelined_IO
  , testProperty "socketPipelined"       prop_socketPipelined_IO
#endif
  , testGroup "Codec"
    [ testProperty "codec"               prop_codec_PingPong
    , testProperty "codec 2-splits"      prop_codec_splits2_PingPong
    , testProperty "codec 3-splits"      prop_codec_splits3_PingPong
    , testGroup "CBOR"
      [ testProperty "codec"             prop_codec_cbor_PingPong
      , testProperty "codec 2-splits"    prop_codec_cbor_splits2_PingPong
      , testProperty "codec 3-splits"  $ withMaxSuccess 30 prop_codec_cbor_splits3_PingPong
      ]
    ]
  ]


--
-- Properties going directly, not via Peer.
--

-- | The 'PingPongClient m' and 'PingPongServer m' types are complementary.
-- The former can be used to feed the latter directly, in the same thread.
-- That's demonstrated here by constructing 'direct'.
--
direct :: Monad m
       => PingPongClient m a
       -> PingPongServer m b
       -> m (a, b)

direct (SendMsgDone clientResult) PingPongServer{recvMsgDone} =
    pure (clientResult, recvMsgDone)

direct (SendMsgPing kPong) PingPongServer{recvMsgPing} = do
    server' <- recvMsgPing
    client' <- kPong
    direct client' server'

type SingQueue = SingQueueF SingTr
newtype SingTr st st' = SingT (SingTrans (Tr st st'))

directPipelined :: Monad m
                => PingPongClientPipelined m a
                -> PingPongServer          m b
                -> m (a, b)
directPipelined (PingPongClientPipelined client0) server0 =
    go SingEmptyF client0 server0
  where
    go :: Monad m
       => SingQueue q
       -> PingPongClientIdle q m a
       -> PingPongServer       m b
       -> m (a, b)
    go SingEmptyF (SendMsgDonePipelined clientResult) PingPongServer{recvMsgDone} =
      pure (clientResult, recvMsgDone)

    go q (SendMsgPingPipelined client') PingPongServer{recvMsgPing} = do
      server' <- recvMsgPing
      let singTr :: SingTr StBusy StIdle
          singTr = SingT SingTr
      go (q |> singTr) client' server'

    go (SingConsF _ q) (CollectPipelined _ k) server = do
      client' <- k
      go q client' server


-- | Run a simple ping\/pong client and server, without going via the 'Peer'
-- representation at all.
--
prop_direct :: NonNegative Int -> Bool
prop_direct (NonNegative n) =
    runIdentity
      (direct (pingPongClientCount n)
               pingPongServerCount)
 ==
    ((), n)


-- | Run a ping\/pong server and pipelined client, without going via the 'Peer'
-- representation.
--
-- This uses a client that forces maximum pipeling. It shows that irrespective
-- of the envronment's choices, the interleaving we get is all requests
-- followed by all responses
--
prop_directPipelined1 :: NonNegative Int -> Bool
prop_directPipelined1 (NonNegative n) =
    runIdentity
      (directPipelined
        (pingPongClientPipelinedMax n)
         pingPongServerCount)
 ==
    (reqResps, n)
  where
    reqResps = map Left [0..n-1] ++ map Right [0..n-1]


-- | Run a ping\/pong server and pipelined client, without going via the 'Peer'
-- representation.
--
-- This uses a client that collects eagerly. It shows that when the environment
-- chooses minimum pipelining, then the interleaving we get is the in-order
-- non-pipelined interleaving of each request followed by its response.
--
prop_directPipelined2 :: NonNegative Int -> Bool
prop_directPipelined2 (NonNegative n) =
    runIdentity
      (directPipelined
        (pingPongClientPipelinedMin n)
         pingPongServerCount)
 ==
    (reqResps, n)
  where
    reqResps = concat [ [Left n', Right n'] | n' <- [0..n-1] ]


--
-- Properties using connect, without pipelining.
--

-- | Run a simple ping\/pong client and server, going via the 'Peer'
-- representation, but without going via a channel.
--
prop_connect :: NonNegative Int -> Bool
prop_connect (NonNegative n) =
  case runIdentity
         (connectNonPipelined
           (pingPongClientPeer (pingPongClientCount n))
           (pingPongServerPeer  pingPongServerCount))

    of ((), n', TerminalStates SingDone
                               ReflNobodyAgency
                               SingDone
                               ReflNobodyAgency) ->
        n == n'


--
-- Properties using connect, with pipelining.
--

-- | Run a pipelined ping\/pong client with a normal server. The client
-- should return the interleaving of messages it sent and received. This
-- will be used to exercise various interleavings in properties below.
--
connect_pipelined :: (forall s. PingPongClientPipelined (IOSim s) [Either Int Int])
                  -> [Bool]
                  -> (Int, [Either Int Int])
connect_pipelined client cs =
  case runSimOrThrow
         (connect cs []
            (pingPongClientPeerPipelined client)
            (pingPongServerPeer pingPongServerCount))

    of (reqResps, n, TerminalStates SingDone
                                    ReflNobodyAgency
                                    SingDone
                                    ReflNobodyAgency) ->
         (n, reqResps)


connect_pipelined_stm :: (forall s. PingPongClientPipelined (IOSim s) [Either Int Int])
                      -> [Bool]
                      -> (Int, [Either Int Int])
connect_pipelined_stm client cs =
  case runSimOrThrow
         (connect cs []
            (pingPongClientPeerPipelinedSTM client)
            (pingPongServerPeer pingPongServerCount))

    of (reqResps, n, TerminalStates SingDone
                                    ReflNobodyAgency
                                    SingDone
                                    ReflNobodyAgency) ->
         (n, reqResps)


-- | Using a client that forces maximum pipeling, show that irrespective of
-- the envronment's choices, the interleaving we get is all requests followed
-- by all responses.
--
prop_connect_pipelined1 :: [Bool] -> NonNegative Int -> Bool
prop_connect_pipelined1 choices (NonNegative n) =
    connect_pipelined (pingPongClientPipelinedMax n) choices
 ==
    (n, reqResps)
  where
    reqResps = map Left [0..n-1] ++ map Right [0..n-1]


-- | Using a client that collects eagerly, show that when the environment
-- chooses maximum pipelining, then the interleaving we get is all requests
-- followed by all responses.
--
prop_connect_pipelined2 :: NonNegative Int -> Bool
prop_connect_pipelined2 (NonNegative n) =
    connect_pipelined (pingPongClientPipelinedMin n) choices
 ==
    (n, reqResps)
  where
    choices  = repeat True
    reqResps = map Left [0..n-1] ++ map Right [0..n-1]


-- | Using a client that collects eagerly, show that when the environment
-- chooses minimum pipelining, then the interleaving we get is the in-order
-- non-pipelined interleaving of each request followed by its response.
--
prop_connect_pipelined3 :: NonNegative Int -> Bool
prop_connect_pipelined3 (NonNegative n) =
    connect_pipelined (pingPongClientPipelinedMin n) choices
 ==
    (n, reqResps)
  where
    choices  = repeat False
    reqResps = concat [ [Left n', Right n'] | n' <- [0..n-1] ]


-- | Using a client that collects eagerly, but otherwise is always willing
-- to send new messages, show that when the environment chooses arbitrary
-- pipelining, then we get complex interleavings given by the reference
-- specification 'pipelineInterleaving'.
--
prop_connect_pipelined4 :: [Bool] -> NonNegative Int -> Bool
prop_connect_pipelined4 choices (NonNegative n) =
    connect_pipelined (pingPongClientPipelinedMin n) choices
 ==
    (n, reqResps)
  where
    reqResps = pipelineInterleaving maxBound choices [0..n-1] [0..n-1]


-- | Using a client that collects eagerly, and is willing to send new messages
-- up to a fixed limit of outstanding messages, show that when the environment
-- chooses arbitrary pipelining, then we get complex interleavings given by
-- the reference specification 'pipelineInterleaving', for that limit of
-- outstanding messages.
--
prop_connect_pipelined5 :: [Bool] -> Positive Int -> NonNegative Int -> Bool
prop_connect_pipelined5 choices (Positive omax) (NonNegative n) =
    connect_pipelined (pingPongClientPipelinedLimited omax n) choices
 ==
    (n, reqResps)
  where
    reqResps = pipelineInterleaving omax choices [0..n-1] [0..n-1]


-- | Using a client that forces maximum pipeling, show that irrespective of
-- the envronment's choices, the interleaving we get is all requests followed
-- by all responses.
--
prop_connect_pipelined_stm :: [Bool] -> NonNegative Int -> Bool
prop_connect_pipelined_stm choices (NonNegative n) =
    connect_pipelined_stm (pingPongClientPipelinedMax n) choices
 ==
    (n, reqResps)
  where
    reqResps = map Left [0..n-1] ++ map Right [0..n-1]

--
-- Properties using channels, codecs and drivers.
--

-- | Run a non-pipelined client and server over a channel using a codec.
--
prop_channel :: ( MonadLabelledSTM m, MonadTraceSTM m, MonadAsync m
                , MonadCatch m, MonadMask m, MonadThrow (STM m))
             => NonNegative Int
             -> m Bool
prop_channel (NonNegative n) = do
    ((), n') <- runConnectedPeers createConnectedChannels
                                  nullTracer
                                  codecPingPong client server
    return (n' == n)
  where
    client = pingPongClientPeer (pingPongClientCount n)
    server = pingPongServerPeer  pingPongServerCount


prop_channel_IO :: NonNegative Int -> Property
prop_channel_IO n =
    ioProperty (prop_channel n)

prop_channel_ST :: NonNegative Int -> Bool
prop_channel_ST n =
    runSimOrThrow (prop_channel n)


prop_channel_stm :: ( MonadLabelledSTM m, MonadAsync m, MonadCatch m, MonadMask m
                    , MonadTraceSTM m, MonadTest m, MonadThrow (STM m), MonadTimer m )
             => DiffTime
             -> DiffTime
             -> NonNegative Int
             -> Tracer m (Role, TraceSendRecv PingPong)
             -> m Bool
prop_channel_stm a b (NonNegative n) tr = do
    exploreRaces
    (_, r) <- runConnectedPeers (bimap (delayChannel a)
                                       (delayChannel b)
                                  <$> createConnectedBufferedChannelsUnbounded)
                                tr
                                codecPingPong client server
    return (r == n)
  where
    client = pingPongClientPeerPipelinedSTM (pingPongClientPipelinedMin n)
    server = pingPongServerPeer  pingPongServerCount


prop_channel_stm_ST :: NonNegative Int
                    -- delay in simulated seconds
                    -> NonNegative Int
                    -- delay in simulated seconds
                    -> NonNegative Int
                    -> Property
prop_channel_stm_ST (NonNegative a) (NonNegative b) n =
    exploreSimTrace id sim $ \_ trace ->
        counterexample (ppTrace trace)
      $ case traceResult True trace of
          Left e  -> counterexample (show e) False
          Right r -> property r
  where
    sim :: IOSim s Bool
    sim = prop_channel_stm (fromIntegral a)
                           (fromIntegral b)
                           n
                           (Tracer (say . show))

prop_channel_stm_IO :: NonNegative Int
                    -- delay in micro seconds
                    -> NonNegative Int
                    -- delay in micro seconds
                    -> NonNegative Int
                    -> Property
prop_channel_stm_IO (NonNegative a) (NonNegative b) n =
    ioProperty (prop_channel_stm (fromIntegral a / 1_000_000)
                                 (fromIntegral b / 1_000_000)
                                 n nullTracer)


#if !defined(mingw32_HOST_OS)
prop_namedPipePipelined_IO :: NonNegative Int
                           -> Property
prop_namedPipePipelined_IO (NonNegative n) = ioProperty $ do
    let client = pingPongClientPeer (pingPongClientCount n)
        server = pingPongServerPeer  pingPongServerCount

    let cliPath = "client.sock"
        srvPath = "server.sock"
        mode = Posix.ownerModes

    Posix.createNamedPipe cliPath mode
    Posix.createNamedPipe srvPath mode

    bracket   (openFile cliPath ReadWriteMode)
              (\_ -> removeFile cliPath)
            $ \cliHandle ->
      bracket (openFile srvPath ReadWriteMode)
              (\_ -> removeFile srvPath)
           $ \srvHandle -> do
              ((), n') <- runConnectedPeers (return ( handlesAsChannel cliHandle srvHandle
                                                    , handlesAsChannel srvHandle cliHandle
                                                    ))
                                            nullTracer
                                            CBOR.codecPingPong client server
              return (n' == n)
#endif


#if !defined(mingw32_HOST_OS)
prop_socketPipelined_IO :: NonNegative Int
                        -> Property
prop_socketPipelined_IO (NonNegative n) = ioProperty $ do
    ai : _ <- Socket.getAddrInfo Nothing (Just "127.0.0.1") Nothing
    bracket
      ((,) <$> Socket.openSocket ai
           <*> Socket.openSocket ai)
      ( \ (sock, sock') -> Socket.close sock
                        >> Socket.close sock')
      $ \ (sock, sock') -> do
          Socket.bind sock (Socket.addrAddress ai)
          addr <- Socket.getSocketName sock
          Socket.listen sock 1
          Socket.connect sock' addr
          bracket (fst <$> Socket.accept sock) Socket.close
                $ \sock'' -> do
            let client = pingPongClientPeer (pingPongClientCount n)
                server = pingPongServerPeer  pingPongServerCount

            ((), n') <- runConnectedPeers (return ( socketAsChannel sock'
                                                  , socketAsChannel sock''
                                                  ))
                                          nullTracer
                                          CBOR.codecPingPong client server
            return (n' == n)
#endif


--
-- Codec properties
--

instance Arbitrary (AnyMessage PingPong) where
  arbitrary = elements
    [ AnyMessage MsgPing
    , AnyMessage MsgPong
    , AnyMessage MsgDone
    ]

instance Eq (AnyMessage PingPong) where
  AnyMessage MsgPing == AnyMessage MsgPing = True
  AnyMessage MsgPong == AnyMessage MsgPong = True
  AnyMessage MsgDone == AnyMessage MsgDone = True
  _                  ==                  _ = False

prop_codec_PingPong :: AnyMessage PingPong -> Bool
prop_codec_PingPong =
    prop_codec
      runIdentity
      codecPingPong

prop_codec_splits2_PingPong :: AnyMessage PingPong -> Bool
prop_codec_splits2_PingPong =
    prop_codec_splits
      splits2
      runIdentity
      codecPingPong

prop_codec_splits3_PingPong :: AnyMessage PingPong -> Bool
prop_codec_splits3_PingPong =
    prop_codec_splits
      splits3
      runIdentity
      codecPingPong

--
-- CBOR codec properties
--

prop_codec_cbor_PingPong
  :: AnyMessage PingPong
  -> Bool
prop_codec_cbor_PingPong msg =
  runST $ prop_codecM CBOR.codecPingPong msg

prop_codec_cbor_splits2_PingPong
  :: AnyMessage PingPong
  -> Bool
prop_codec_cbor_splits2_PingPong msg =
  runST $ prop_codec_splitsM
      splits2BS
      CBOR.codecPingPong
      msg

prop_codec_cbor_splits3_PingPong
  :: AnyMessage PingPong
  -> Bool
prop_codec_cbor_splits3_PingPong msg =
  runST $ prop_codec_splitsM
      splits3BS
      CBOR.codecPingPong
      msg

--
-- Utils
--

-- | Generate all 2-splits of a string.
--
splits2 :: String -> [[String]]
splits2 str = zipWith (\a b -> [a,b]) (inits str) (tails str)

-- | Generate all 3-splits of a string.
--
splits3 :: String -> [[String]]
splits3 str =
    [ [a,b,c]
    | (a,str') <- zip (inits str)  (tails str)
    , (b,c)    <- zip (inits str') (tails str') ]

-- | Generate all 2-splits of a 'LBS.ByteString'.
--
splits2BS :: LBS.ByteString -> [[LBS.ByteString]]
splits2BS bs = zipWith (\a b -> [a,b]) (LBS.inits bs) (LBS.tails bs)

-- | Generate all 3-splits of a 'LBS.ByteString'.
--
splits3BS :: LBS.ByteString -> [[LBS.ByteString]]
splits3BS bs =
    [ [a,b,c]
    | (a,bs') <- zip (LBS.inits bs)  (LBS.tails bs)
    , (b,c)   <- zip (LBS.inits bs') (LBS.tails bs') ]
