{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}



module Network.TypedProtocol.PingPong.Tests
  ( tests
  , splits2
  , splits3
  , splits2BS
  , splits3BS
  ) where


import Network.TypedProtocol.Codec
import Network.TypedProtocol.Proofs
import Network.TypedProtocol.Channel
import Network.TypedProtocol.Driver.Simple

import Network.TypedProtocol.PingPong.Type
import Network.TypedProtocol.PingPong.Client
import Network.TypedProtocol.PingPong.Server
import Network.TypedProtocol.PingPong.Examples
import Network.TypedProtocol.PingPong.Codec
import qualified Network.TypedProtocol.PingPong.Codec.CBOR as CBOR

import Data.Functor.Identity (Identity (..))
import Control.Monad.ST (runST)
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadThrow
import Control.Monad.IOSim (runSimOrThrow)
import Control.Tracer (Tracer, nullTracer)

import Data.List (inits, tails)
import qualified Data.ByteString.Lazy as LBS

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)


--
-- The list of all properties
--

tests :: TestTree
tests = testGroup "Network.TypedProtocol.PingPong"
  [ testProperty "direct"              prop_direct
  , testProperty "directPipelined 1"   prop_directPipelined1
  , testProperty "directPipelined 2"   prop_directPipelined2
  , testProperty "connect"             prop_connect
  , testProperty "connect_pipelined 1" prop_connect_pipelined1
  , testProperty "connect_pipelined 2" prop_connect_pipelined2
  , testProperty "connect_pipelined 3" prop_connect_pipelined3
  , testProperty "connect_pipelined 4" prop_connect_pipelined4
  , testProperty "connect_pipelined 5" prop_connect_pipelined5
  , testProperty "channel ST"          prop_channel_ST
  , testProperty "channel IO"          prop_channel_IO
  , testGroup "Codec"
    [ testProperty "codec"             prop_codec_PingPong
    , testProperty "codec 2-splits"    prop_codec_splits2_PingPong
    , testProperty "codec 3-splits"    prop_codec_splits3_PingPong
    , testGroup "CBOR"
      [ testProperty "codec"           prop_codec_cbor_PingPong
      , testProperty "codec 2-splits"  prop_codec_cbor_splits2_PingPong
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


directPipelined :: Monad m
                => PingPongClientPipelined m a
                -> PingPongServer          m b
                -> m (a, b)
directPipelined (PingPongClientPipelined client0) server0 =
    go EmptyQ client0 server0
  where
    go :: Monad m
       => Queue n c
       -> PingPongSender n c m a
       -> PingPongServer     m b
       -> m (a, b)
    go EmptyQ (SendMsgDonePipelined clientResult) PingPongServer{recvMsgDone} =
      pure (clientResult, recvMsgDone)

    go q (SendMsgPingPipelined kPong client') PingPongServer{recvMsgPing} = do
      server' <- recvMsgPing
      x       <- kPong
      go (enqueue x q) client' server'

    go (ConsQ x q) (CollectPipelined _ k) server =
      go q (k x) server


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
         (connect
           (pingPongClientPeer (pingPongClientCount n))
           (pingPongServerPeer  pingPongServerCount))

    of ((), n', TerminalStates TokDone TokDone) -> n == n'


--
-- Properties using connect, with pipelining.
--

-- | Run a pipelined ping\/pong client with a normal server. The client
-- should return the interleaving of messages it sent and received. This
-- will be used to exercise various interleavings in properties below.
--
connect_pipelined :: PingPongClientPipelined Identity [Either Int Int]
                  -> [Bool]
                  -> (Int, [Either Int Int])
connect_pipelined client cs =
  case runIdentity
         (connectPipelined cs
            (pingPongClientPeerPipelined client)
            (pingPongServerPeer pingPongServerCount))

    of (reqResps, n, TerminalStates TokDone TokDone) -> (n, reqResps)


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


--
-- Properties using channels, codecs and drivers.
--

-- | Run a non-pipelined client and server over a channel using a codec.
--
prop_channel :: (MonadSTM m, MonadAsync m, MonadCatch m)
             => NonNegative Int
             -> Tracer m (PeerRole, TraceSendRecv PingPong)
             -> m Bool
prop_channel (NonNegative n) tr = do
    ((), n') <- runConnectedPeers createConnectedChannels
                                  tr
                                  codecPingPong client server
    return (n' == n)
  where
    client = pingPongClientPeer (pingPongClientCount n)
    server = pingPongServerPeer  pingPongServerCount


prop_channel_IO :: NonNegative Int -> Property
prop_channel_IO n =
    ioProperty (prop_channel n nullTracer)

prop_channel_ST :: NonNegative Int -> Bool
prop_channel_ST n =
    runSimOrThrow (prop_channel n nullTracer)


--
-- Codec properties
--

instance Arbitrary (AnyMessageAndAgency PingPong) where
  arbitrary = elements
    [ AnyMessageAndAgency (ClientAgency TokIdle) MsgPing
    , AnyMessageAndAgency (ServerAgency TokBusy) MsgPong
    , AnyMessageAndAgency (ClientAgency TokIdle) MsgDone
    ]

instance Eq (AnyMessage PingPong) where
  AnyMessage MsgPing == AnyMessage MsgPing = True
  AnyMessage MsgPong == AnyMessage MsgPong = True
  AnyMessage MsgDone == AnyMessage MsgDone = True
  _                  ==                  _ = False

prop_codec_PingPong :: AnyMessageAndAgency PingPong -> Bool
prop_codec_PingPong =
    prop_codec
      runIdentity
      codecPingPong

prop_codec_splits2_PingPong :: AnyMessageAndAgency PingPong -> Bool
prop_codec_splits2_PingPong =
    prop_codec_splits
      splits2
      runIdentity
      codecPingPong

prop_codec_splits3_PingPong :: AnyMessageAndAgency PingPong -> Bool
prop_codec_splits3_PingPong =
    prop_codec_splits
      splits3
      runIdentity
      codecPingPong

--
-- CBOR codec properties
--

prop_codec_cbor_PingPong
  :: AnyMessageAndAgency PingPong
  -> Bool
prop_codec_cbor_PingPong msg =
  runST $ prop_codecM CBOR.codecPingPong msg

prop_codec_cbor_splits2_PingPong
  :: AnyMessageAndAgency PingPong
  -> Bool
prop_codec_cbor_splits2_PingPong msg =
  runST $ prop_codec_splitsM
      splits2BS
      CBOR.codecPingPong
      msg

prop_codec_cbor_splits3_PingPong
  :: AnyMessageAndAgency PingPong
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
