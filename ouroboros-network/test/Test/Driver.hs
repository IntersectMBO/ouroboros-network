{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Driver where

import           Control.Monad (replicateM)
import           Data.Int (Int64)
import           Data.Functor (void)

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.IOSim
import           Control.Tracer (nullTracer)

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Channel
import           Network.TypedProtocol.Driver
import           Network.TypedProtocol.Pipelined
import           Network.TypedProtocol.ReqResp.Type
import           Network.TypedProtocol.ReqResp.Codec
import           Network.TypedProtocol.ReqResp.Client
import           Network.TypedProtocol.ReqResp.Server
import           Network.TypedProtocol.ReqResp.Examples

import           Ouroboros.Network.Driver

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup "Ouroboros.Network.Driver"
  [ testProperty "runPeerWithByteLimit ST"
                                       prop_runPeerWithByteLimit_ST
  , testProperty "runPeerWithByteLimit IO"
                                       prop_runPeerWithByteLimit_IO
  , testProperty "runPipelinedPeerWithByteLimit ST"
                                       prop_runPipelinedPeerWithByteLimit_ST
  , testProperty "runPipelinedPeerWithByteLimit IO"
                                       prop_runPipelinedPeerWithByteLimit_IO
  ]

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
        runPeerWithByteLimit (ByteLimit limit (fromIntegral . length)) nullTracer codecReqResp "receiver" c1 recvPeer
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


-- |
-- Run the client peer using @runPipelinedPeerWithByteLimit@, which will receive
-- requests with the given payloads (the server echoes the requests to the
-- client).
--
prop_runPipelinedPeerWithByteLimit
  :: forall m. (MonadSTM m, MonadAsync m, MonadCatch m)
  => Int64
  -- ^ byte limit
  -> [String]
  -- ^ request payloads
  -> m Bool
prop_runPipelinedPeerWithByteLimit limit reqPayloads = do
      (c1, c2) <- createConnectedChannels

      res <- try $
        runPeer nullTracer codecReqResp "receiver" c1 recvPeer
          `concurrently`
        runPipelinedPeerWithByteLimit (ByteLimit limit (fromIntegral . length)) nullTracer codecReqResp "sender" c2 sendPeer

      case res :: Either (DecoderFailureOrTooMuchInput CodecFailure) ((), [String]) of
        Right _           -> pure $ not shouldFail
        Left TooMuchInput -> pure $ shouldFail
        Left _            -> pure $ False

    where
      sendPeer :: PeerPipelined (ReqResp String String) AsClient StIdle m [String]
      sendPeer = reqRespClientPeerPipelined $ reqRespClientMapPipelined reqPayloads

      recvPeer :: Peer (ReqResp String String) AsServer StIdle m ()
      recvPeer = reqRespServerPeer $ reqRespServerMapAccumL
        (\acc req -> pure (acc, req))
        ()

      encoded :: [String]
      encoded =
        map (encode (codecReqResp @String @String @m) (ServerAgency TokBusy) . MsgResp) reqPayloads

      shouldFail :: Bool
      shouldFail = any (> limit) $ map (fromIntegral . length) encoded


prop_runPipelinedPeerWithByteLimit_ST
  :: ReqRespPayloadWithLimit
  -> Bool
prop_runPipelinedPeerWithByteLimit_ST (ReqRespPayloadWithLimit limit payload) =
      runSimOrThrow (prop_runPipelinedPeerWithByteLimit limit [payload])


prop_runPipelinedPeerWithByteLimit_IO
  :: ReqRespPayloadWithLimit
  -> Property
prop_runPipelinedPeerWithByteLimit_IO (ReqRespPayloadWithLimit limit payload) =
      ioProperty (prop_runPipelinedPeerWithByteLimit limit [payload])

