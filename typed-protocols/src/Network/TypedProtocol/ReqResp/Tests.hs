{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}


module Network.TypedProtocol.ReqResp.Tests (tests) where

import Network.TypedProtocol.Core
import Network.TypedProtocol.Codec
import Network.TypedProtocol.Proofs
import Network.TypedProtocol.Channel
import Network.TypedProtocol.Driver
import Network.TypedProtocol.Driver.ByteLimit

import Network.TypedProtocol.ReqResp.Type
import Network.TypedProtocol.ReqResp.Client
import Network.TypedProtocol.ReqResp.Server
import Network.TypedProtocol.ReqResp.Codec
import Network.TypedProtocol.ReqResp.Examples

import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadThrow
import Control.Monad.IOSim (runSimOrThrow)
import Control.Tracer (nullTracer)

import Control.Monad (replicateM, void)
import Data.Int (Int64)
import Data.Functor.Identity (Identity (..))
import Data.Tuple (swap)
import Data.List (mapAccumL)

import Network.TypedProtocol.PingPong.Tests (splits2, splits3)

import Test.QuickCheck
import Text.Show.Functions ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)


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
  , testProperty "codec"               prop_codec_ReqResp
  , testProperty "codec 2-splits"      prop_codec_splits2_ReqResp
  , testProperty "codec 3-splits"      (withMaxSuccess 33
                                       prop_codec_splits3_ReqResp)

  , testProperty "runPeerWithByteLimit ST"
                                       prop_runPeerWithByteLimit_ST
  , testProperty "runPeerWithByteLimit IO"
                                       prop_runPeerWithByteLimit_IO
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


directPipelined :: Monad m
                => ReqRespClientPipelined req resp m a
                -> ReqRespServer          req resp m b
                -> m (a, b)
directPipelined (ReqRespClientPipelined client0) server0 =
    go EmptyQ client0 server0
  where
    go :: Monad m
       => Queue n c
       -> ReqRespSender req resp n c m a
       -> ReqRespServer req resp     m b
       -> m (a, b)
    go EmptyQ (SendMsgDonePipelined clientResult) ReqRespServer{recvMsgDone} =
      (clientResult,) <$> recvMsgDone

    go q (SendMsgReqPipelined req kResp client') ReqRespServer{recvMsgReq} = do
      (resp, server') <- recvMsgReq req
      x               <- kResp resp
      go (enqueue x q) client' server'

    go (ConsQ x q) (CollectPipelined _ k) server =
      go q (k x) server


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
             (reqRespClientPeer (reqRespClientMap xs))
             (reqRespServerPeer (reqRespServerMapAccumL (\a -> pure . f a) 0)))

      of (c, s, TerminalStates TokDone TokDone) ->
           (s, c) == mapAccumL f 0 xs


prop_connectPipelined :: [Bool] -> (Int -> Int -> (Int, Int)) -> [Int] -> Bool
prop_connectPipelined cs f xs =
    case runIdentity
           (connectPipelined cs
             (reqRespClientPeerPipelined (reqRespClientMapPipelined xs))
             (reqRespServerPeer          (reqRespServerMapAccumL
                                            (\a -> pure . f a) 0)))

      of (c, s, TerminalStates TokDone TokDone) ->
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
                                codecReqResp "client" "server" client server
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
-- runPeerWithByteLimit properties
--


-- |
-- Run the server peer using @runPeerWithByteLimit@, which will receive requests
-- with the given payloads.
--
prop_runPeerWithByteLimit
  :: forall m. (MonadSTM m, MonadAsync m, MonadCatch m)
  => Int64
  -- ^ byte limit
  -> [String]
  -- ^ request payloads
  -> m Bool
prop_runPeerWithByteLimit limit reqPayloads = do
      (c1, c2) <- createConnectedChannels

      res <- try $
        runPeerWithByteLimit limit (fromIntegral . length) nullTracer codecReqResp "receiver" c1 recvPeer
          `concurrently`
        void (runPeer nullTracer codecReqResp "sender" c2 sendPeer)

      case res :: Either (DecoderFailureOrTooMuchInput CodecFailure) ([String], ()) of
        Right _           -> pure $ not shouldFail
        Left TooMuchInput -> pure $ shouldFail
        Left _            -> pure $ False

    where
      sendPeer :: Peer (ReqResp String ()) AsClient StIdle m [()]
      sendPeer = reqRespClientPeer $ reqRespClientMap reqPayloads

      recvPeer :: Peer (ReqResp String ()) AsServer StIdle m [String]
      recvPeer = reqRespServerPeer $ reqRespServerMapAccumL
        (\acc req -> pure (req:acc, ()))
        []

      encoded :: [String]
      encoded =
        -- add @MsgDone@ which is always sent
        (encode (codecReqResp @String @() @m) (ClientAgency TokIdle) MsgDone)
        : map (encode (codecReqResp @String @() @m) (ClientAgency TokIdle) . MsgReq) reqPayloads

      shouldFail :: Bool
      shouldFail = any (> limit) $ map (fromIntegral . length) encoded

data ReqRespPayloadWithLimit = ReqRespPayloadWithLimit Int64 String
  deriving Show

instance Arbitrary ReqRespPayloadWithLimit where
    arbitrary = do
      -- @MsgDone@ is encoded with 8 characters
      limit <- (+7) . getSmall . getPositive <$> arbitrary
      len <- frequency
        -- close to the limit
        [ (2, choose (max 0 (limit - 5), limit + 5))
        -- below the limit
        , (2, choose (0, limit))
        -- above the limit
        , (2, choose (limit, 10 * limit))
        -- right at the limit
        , (1, choose (limit, limit))
        ]
      ReqRespPayloadWithLimit limit <$> replicateM (fromIntegral len) arbitrary

    shrink (ReqRespPayloadWithLimit l p) =
      [ ReqRespPayloadWithLimit l' p
      | l' <- shrink l
      ]
      ++
      [ ReqRespPayloadWithLimit l p'
      | p' <- shrink p
      ]

-- TODO: This test could be improved: it will not test the case in which
-- @runDecoderWithByteLimit@ receives trailing bytes.
--
prop_runPeerWithByteLimit_ST
  :: ReqRespPayloadWithLimit
  -> Property
prop_runPeerWithByteLimit_ST (ReqRespPayloadWithLimit limit payload) =
      tabulate "Limit Boundaries" (labelExamples limit payload) $
        runSimOrThrow (prop_runPeerWithByteLimit limit [payload])
    where
      labelExamples :: Int64 -> String -> [String]
      labelExamples l p =
        [ case length p `compare` fromIntegral l of
            LT -> "BelowTheLimit"
            EQ -> "AtTheLimit"
            GT -> "AboveTheLimit"
        ]
        ++
          if abs (length p - fromIntegral l) <= 5
            then ["CloseToTheLimit"]
            else []

prop_runPeerWithByteLimit_IO
  :: ReqRespPayloadWithLimit
  -> Property
prop_runPeerWithByteLimit_IO (ReqRespPayloadWithLimit limit payload) =
  ioProperty (prop_runPeerWithByteLimit limit [payload])

--
-- Codec properties
--

instance (Arbitrary req, Arbitrary resp) =>
         Arbitrary (AnyMessageAndAgency (ReqResp req resp)) where
  arbitrary = oneof
    [ AnyMessageAndAgency (ClientAgency TokIdle) . MsgReq <$> arbitrary
    , AnyMessageAndAgency (ServerAgency TokBusy) . MsgResp <$> arbitrary
    , return (AnyMessageAndAgency (ClientAgency TokIdle) MsgDone)
    ]

  shrink (AnyMessageAndAgency a (MsgReq r))  =
    [ AnyMessageAndAgency a (MsgReq r')
    | r' <- shrink r ]

  shrink (AnyMessageAndAgency a (MsgResp r)) =
    [ AnyMessageAndAgency a (MsgResp r')
    | r' <- shrink r ]

  shrink (AnyMessageAndAgency _ MsgDone)     = []

instance (Eq req, Eq resp) => Eq (AnyMessage (ReqResp req resp)) where
  (AnyMessage (MsgReq  r1)) == (AnyMessage (MsgReq  r2)) = r1 == r2
  (AnyMessage (MsgResp r1)) == (AnyMessage (MsgResp r2)) = r1 == r2
  (AnyMessage MsgDone)      == (AnyMessage MsgDone)      = True
  _                         == _                         = False

instance (Show req, Show resp) =>
         Show (AnyMessageAndAgency (ReqResp req resp)) where
  show (AnyMessageAndAgency _ msg) = show msg

prop_codec_ReqResp :: AnyMessageAndAgency (ReqResp String String) -> Bool
prop_codec_ReqResp =
    prop_codec
      runIdentity
      codecReqResp

prop_codec_splits2_ReqResp :: AnyMessageAndAgency (ReqResp String String)
                           -> Bool
prop_codec_splits2_ReqResp =
    prop_codec_splits
      splits2
      runIdentity
      codecReqResp

prop_codec_splits3_ReqResp :: AnyMessageAndAgency (ReqResp String String)
                           -> Bool
prop_codec_splits3_ReqResp =
    prop_codec_splits
      splits3
      runIdentity
      codecReqResp
