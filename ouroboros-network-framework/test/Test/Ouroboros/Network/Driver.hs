{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}


module Test.Ouroboros.Network.Driver (tests) where

import Network.TypedProtocol.Core

import Ouroboros.Network.Codec
import Ouroboros.Network.Channel
import Ouroboros.Network.Driver
import Ouroboros.Network.Driver.Limits

import Network.TypedProtocol.ReqResp.Type
import Network.TypedProtocol.ReqResp.Client
import Network.TypedProtocol.ReqResp.Server
import Network.TypedProtocol.ReqResp.Codec
import Network.TypedProtocol.ReqResp.Examples

import Control.Monad (void, replicateM)
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadTime
import Control.Monad.Class.MonadTimer
import Control.Monad.Class.MonadThrow
import Control.Monad.IOSim (runSimOrThrow)
import Control.Tracer (nullTracer)

import Test.QuickCheck
import Text.Show.Functions ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)


--
-- The list of all properties
--

tests :: TestTree
tests = testGroup "Ouroboros.Network.Driver.Limits"
  [ testProperty "runPeerWithLimits ST"
                                       prop_runPeerWithLimits_ST
  , testProperty "runPeerWithLimits IO"
                                       prop_runPeerWithLimits_IO
  ]



-- | Byte limits
byteLimitsReqResp
  :: Word
  -> ProtocolSizeLimits (ReqResp req resp) String
byteLimitsReqResp limit = ProtocolSizeLimits stateToLimit (fromIntegral . length)
  where
    stateToLimit :: forall (pr :: PeerRole) (st  :: ReqResp req resp).
                    PeerHasAgency pr st -> Word
    stateToLimit (ClientAgency TokIdle) = limit
    stateToLimit (ServerAgency TokBusy) = limit

-- TODO add test cases for time limits

-- Time limits
timeLimitsReqResp :: ProtocolTimeLimits (ReqResp req resp)
timeLimitsReqResp = ProtocolTimeLimits stateToLimit
  where
    stateToLimit :: forall (pr :: PeerRole) (st  :: ReqResp req resp).
                    PeerHasAgency pr st -> Maybe DiffTime
    stateToLimit (ClientAgency TokIdle) = Nothing
    stateToLimit (ServerAgency TokBusy) = Nothing

--
-- runPeerWithLimits properties
--


-- |
-- Run the server peer using @runPeerWithByteLimit@, which will receive requests
-- with the given payloads.
--
prop_runPeerWithLimits
  :: forall m. (MonadSTM m, MonadAsync m, MonadCatch m, MonadTimer m)
  => Word
  -- ^ byte limit
  -> [String]
  -- ^ request payloads
  -> m Bool
prop_runPeerWithLimits limit reqPayloads = do
      (c1, c2) <- createConnectedChannels

      res <- try $
        runPeerWithLimits nullTracer codecReqResp (byteLimitsReqResp limit) timeLimitsReqResp c1 recvPeer
          `concurrently`
        void (runPeer nullTracer codecReqResp c2 sendPeer)

      case res :: Either ProtocolLimitFailure ([String], ()) of
        Right _                -> pure $ not shouldFail
        Left ExceededSizeLimit -> pure $ shouldFail
        Left _                 -> pure $ False

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

data ReqRespPayloadWithLimit = ReqRespPayloadWithLimit Word String
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
prop_runPeerWithLimits_ST
  :: ReqRespPayloadWithLimit
  -> Property
prop_runPeerWithLimits_ST (ReqRespPayloadWithLimit limit payload) =
      tabulate "Limit Boundaries" (labelExamples limit payload) $
        runSimOrThrow (prop_runPeerWithLimits limit [payload])
    where
      labelExamples :: Word -> String -> [String]
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

prop_runPeerWithLimits_IO
  :: ReqRespPayloadWithLimit
  -> Property
prop_runPeerWithLimits_IO (ReqRespPayloadWithLimit limit payload) =
  ioProperty (prop_runPeerWithLimits limit [payload])

