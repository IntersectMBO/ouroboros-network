{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TupleSections            #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeOperators            #-}

-- orphaned arbitrary instances
{-# OPTIONS_GHC -Wno-orphans #-}


module Network.TypedProtocol.ReqResp.Tests (tests) where

import           Network.TypedProtocol.Channel
import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Core hiding (SingQueue (..), (|>))
import           Network.TypedProtocol.Driver.Simple
import           Network.TypedProtocol.Proofs

import           Network.TypedProtocol.ReqResp.Client
import           Network.TypedProtocol.ReqResp.Codec
import           Network.TypedProtocol.ReqResp.Examples
import           Network.TypedProtocol.ReqResp.Server
import           Network.TypedProtocol.ReqResp.Type

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.IOSim (runSimOrThrow)
import           Control.Monad.ST (runST)
import           Control.Tracer (nullTracer)

import           Data.Functor.Identity (Identity (..))
import           Data.Kind (Type)
import           Data.List (mapAccumL)
import           Data.Tuple (swap)

import           Network.TypedProtocol.PingPong.Tests (splits2, splits3)

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Text.Show.Functions ()


--
-- The list of all properties
--

tests :: TestTree
tests = testGroup "Network.TypedProtocol.ReqResp"
  [ testProperty "direct"              prop_direct
  , testProperty "directPipelined"     prop_directPipelined
  , testProperty "connect"             prop_connect
  , testProperty "connectPipelined"    prop_connectPipelined
  , testProperty "channel ST"          prop_channel_ST
  , testProperty "channel IO"          prop_channel_IO
  , testGroup "Codec"
    [ testProperty "codec"             prop_codec_ReqResp
    , testProperty "codec 2-splits"    prop_codec_splits2_ReqResp
    , testProperty "codec 3-splits"    (withMaxSuccess 33 prop_codec_splits3_ReqResp)
    , testGroup "CBOR"
      [ testProperty "codec"           prop_codec_cbor_ReqResp
      , testProperty "codec 2-splits"  prop_codec_cbor_splits2_ReqResp
      , testProperty "codec 3-splits"  $ withMaxSuccess 30 prop_codec_cbor_splits3_ReqResp
      ]
    ]
  ]


--
-- Properties going directly, not via Peer.
--

direct :: Monad m
       => ReqRespClient req resp m a
       -> ReqRespServer req resp m b
       -> m (a, b)

direct (SendMsgDone clientResult) ReqRespServer{recvMsgDone} =
    (,) <$> clientResult <*> recvMsgDone

direct (SendMsgReq req kResp) ReqRespServer{recvMsgReq} = do
    (resp, server') <- recvMsgReq req
    client' <- kResp resp
    direct client' server'

type SingQueue :: Type -> Queue ps -> Type
data SingQueue resp q where
    SingEmpty :: SingQueue resp Empty
    SingCons  :: forall ps (st :: ps) (st' :: ps)
                           resp
                           (q :: Queue ps).
                 resp
              -> SingQueue resp q
              -> SingQueue resp (Tr st st' <| q)

-- | Term level '|>' (snoc).
--
(|>) :: forall ps (st :: ps) (st' :: ps) resp (q :: Queue ps).
        SingQueue resp q
     -> (resp, SingTrans (Tr st st'))
     -> SingQueue resp (q |> Tr st st')

(|>)  SingEmpty        (resp, !_) = SingCons resp SingEmpty

(|>) (SingCons resp q)  a         = SingCons resp (q |> a)


directPipelined :: forall req resp m a b. Monad m
                => ReqRespClientPipelined req resp m a
                -> ReqRespServer          req resp m b
                -> m (a, b)
directPipelined (ReqRespClientPipelined client0) server0 =
    go SingEmpty client0 server0
  where
    go :: forall (q :: Queue (ReqResp req resp)).
          SingQueue         resp q
       -> ReqRespIdle   req resp q m a
       -> ReqRespServer req resp   m b
       -> m (a, b)
    go SingEmpty (SendMsgDonePipelined clientResult) ReqRespServer{recvMsgDone} =
      (clientResult,) <$> recvMsgDone

    go q (SendMsgReqPipelined req client') ReqRespServer{recvMsgReq} = do
      (resp, server') <- recvMsgReq req
      let singTr :: SingTrans (Tr StBusy StIdle)
          singTr = SingTr
      go (q |> (resp, singTr)) client' server'

    go (SingCons resp q) (CollectPipelined _ k) server = do
      client' <- k resp
      go q client' server


prop_direct :: (Int -> Int -> (Int, Int)) -> [Int] -> Bool
prop_direct f xs =
    runIdentity
      (direct
        (reqRespClientMap xs)
        (reqRespServerMapAccumL (\a -> pure . f a) 0))
 ==
    swap (mapAccumL f 0 xs)

prop_directPipelined :: (Int -> Int -> (Int, Int)) -> [Int] -> Bool
prop_directPipelined f xs =
    runIdentity
      (directPipelined
        (reqRespClientMapPipelined xs)
        (reqRespServerMapAccumL (\a -> pure . f a) 0))
 ==
    swap (mapAccumL f 0 xs)


--
-- Properties using connect
--

prop_connect :: (Int -> Int -> (Int, Int)) -> [Int] -> Bool
prop_connect f xs =
    case runIdentity
           (connect
             [] []
             (reqRespClientPeer (reqRespClientMap xs))
             (reqRespServerPeer (reqRespServerMapAccumL (\a -> pure . f a) 0)))

      of (c, s, TerminalStates SingDone ReflNobodyAgency
                               SingDone ReflNobodyAgency) ->
           (s, c) == mapAccumL f 0 xs


prop_connectPipelined :: [Bool] -> (Int -> Int -> (Int, Int)) -> [Int] -> Bool
prop_connectPipelined cs f xs =
    case runIdentity
           (connect cs []
             (reqRespClientPeerPipelined (reqRespClientMapPipelined xs))
             (reqRespServerPeer          (reqRespServerMapAccumL
                                            (\a -> pure . f a) 0)))

      of (c, s, TerminalStates SingDone ReflNobodyAgency
                               SingDone ReflNobodyAgency) ->
           (s, c) == mapAccumL f 0 xs


--
-- Properties using channels, codecs and drivers.
--

prop_channel :: (MonadSTM m, MonadAsync m, MonadCatch m)
             => (Int -> Int -> (Int, Int)) -> [Int]
             -> m Bool
prop_channel f xs = do
    (c, s) <- runConnectedPeers createConnectedChannels
                                nullTracer
                                codecReqResp client server
    return ((s, c) == mapAccumL f 0 xs)
  where
    client = reqRespClientPeer (reqRespClientMap xs)
    server = reqRespServerPeer (reqRespServerMapAccumL
                                 (\a -> pure . f a) 0)

prop_channel_IO :: (Int -> Int -> (Int, Int)) -> [Int] -> Property
prop_channel_IO f xs =
    ioProperty (prop_channel f xs)

prop_channel_ST :: (Int -> Int -> (Int, Int)) -> [Int] -> Bool
prop_channel_ST f xs =
    runSimOrThrow (prop_channel f xs)


--
-- Codec properties
--

instance (Arbitrary req, Arbitrary resp) =>
         Arbitrary (AnyMessage (ReqResp req resp)) where
  arbitrary = oneof
    [ AnyMessage . MsgReq <$> arbitrary
    , AnyMessage . MsgResp <$> arbitrary
    , return (AnyMessage MsgDone)
    ]

  shrink (AnyMessage (MsgReq r))  =
    [ AnyMessage (MsgReq r')
    | r' <- shrink r ]

  shrink (AnyMessage (MsgResp r)) =
    [ AnyMessage (MsgResp r')
    | r' <- shrink r ]

  shrink (AnyMessage MsgDone)     = []

instance (Eq req, Eq resp) => Eq (AnyMessage (ReqResp req resp)) where
  (AnyMessage (MsgReq  r1)) == (AnyMessage (MsgReq  r2)) = r1 == r2
  (AnyMessage (MsgResp r1)) == (AnyMessage (MsgResp r2)) = r1 == r2
  (AnyMessage MsgDone)      == (AnyMessage MsgDone)      = True
  _                         == _                         = False

prop_codec_ReqResp :: AnyMessage (ReqResp String String) -> Bool
prop_codec_ReqResp =
    prop_codec
      runIdentity
      codecReqResp

prop_codec_splits2_ReqResp :: AnyMessage (ReqResp String String)
                           -> Bool
prop_codec_splits2_ReqResp =
    prop_codec_splits
      splits2
      runIdentity
      codecReqResp

prop_codec_splits3_ReqResp :: AnyMessage (ReqResp String String)
                           -> Bool
prop_codec_splits3_ReqResp =
    prop_codec_splits
      splits3
      runIdentity
      codecReqResp

prop_codec_cbor_ReqResp
  :: AnyMessage (ReqResp String String)
  -> Bool
prop_codec_cbor_ReqResp msg =
  runST $ prop_codecM codecReqResp msg

prop_codec_cbor_splits2_ReqResp
  :: AnyMessage (ReqResp String String)
  -> Bool
prop_codec_cbor_splits2_ReqResp msg =
  runST $ prop_codec_splitsM
      splits2
      codecReqResp
      msg

prop_codec_cbor_splits3_ReqResp
  :: AnyMessage (ReqResp String String)
  -> Bool
prop_codec_cbor_splits3_ReqResp msg =
  runST $ prop_codec_splitsM
      splits3
      codecReqResp
      msg
