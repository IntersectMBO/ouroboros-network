{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}


module Test.Ouroboros.Network.Driver (tests) where

import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Peer.Client (Client)
import           Network.TypedProtocol.Peer.Server (Server)

import           Ouroboros.Network.Channel
import           Ouroboros.Network.Driver
import           Ouroboros.Network.Driver.Limits

import           Network.TypedProtocol.ReqResp.Client
import           Network.TypedProtocol.ReqResp.Codec
import           Network.TypedProtocol.ReqResp.Examples
import           Network.TypedProtocol.ReqResp.Server
import           Network.TypedProtocol.ReqResp.Type

import           Control.Applicative (Alternative)
import           Control.Monad (replicateM, void)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime.SI
import           Control.Monad.Class.MonadTimer.SI
import           Control.Monad.IOSim
import           Control.Tracer

import           Ouroboros.Network.Test.Orphans ()

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Text.Show.Functions ()

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
  :: forall req resp.
     Word
  -> ProtocolSizeLimits (ReqResp req resp) String
byteLimitsReqResp limit = ProtocolSizeLimits stateToLimit (fromIntegral . length)
  where
    stateToLimit :: forall (st  :: ReqResp req resp).  ActiveState st
                 => StateToken st -> Word
    stateToLimit SingIdle   = limit
    stateToLimit SingBusy   = limit
    stateToLimit a@SingDone = notActiveState a


serverTimeout :: DiffTime
serverTimeout = 0.2 -- 200 ms

-- Time limits
timeLimitsReqResp :: forall req resp. ProtocolTimeLimits (ReqResp req resp)
timeLimitsReqResp = ProtocolTimeLimits stateToLimit
  where
    stateToLimit :: forall (st  :: ReqResp req resp).
                    ActiveState st
                 => StateToken st -> Maybe DiffTime
    stateToLimit SingIdle   = Just serverTimeout
    stateToLimit SingBusy   = Just serverTimeout
    stateToLimit a@SingDone = notActiveState a

-- Unlimited Time
timeUnLimitsReqResp :: forall req resp. ProtocolTimeLimits (ReqResp req resp)
timeUnLimitsReqResp = ProtocolTimeLimits stateToLimit
  where
    stateToLimit :: forall (st  :: ReqResp req resp).
                    ActiveState st
                 => StateToken st -> Maybe DiffTime
    stateToLimit SingIdle   = Nothing
    stateToLimit SingBusy   = Nothing
    stateToLimit a@SingDone = notActiveState a


--
-- runPeerWithLimits properties
--


data ShouldFail
    = ShouldExceededTimeLimit
    | ShouldExceededSizeLimit
  deriving Eq


-- |
-- Run the server peer using @runPeerWithByteLimit@, which will receive requests
-- with the given payloads.
--
prop_runPeerWithLimits
  :: forall m. ( Alternative (STM m), MonadAsync m, MonadDelay m, MonadFork m,
                 MonadMask m, MonadThrow (STM m), MonadTimer m)
  => Tracer m (TraceSendRecv (ReqResp String ()))
  -> Word
  -- ^ byte limit
  -> [(String, DiffTime)]
  -- ^ request payloads
  -> m Bool
prop_runPeerWithLimits tracer limit reqPayloads = do
      (c1, c2) <- createConnectedChannels

      res <- try $
        (fst <$> runPeerWithLimits tracer codecReqResp (byteLimitsReqResp limit) timeUnLimitsReqResp c1 recvPeer)
          `concurrently`
        void (runPeerWithLimits tracer codecReqResp (byteLimitsReqResp maxBound) timeLimitsReqResp
              c2 sendPeer)

      case res :: Either ProtocolLimitFailure ([DiffTime], ()) of
        Right _ ->
          pure $ shouldFail reqPayloads == Nothing
        Left ExceededSizeLimit{} ->
          pure $ case shouldFail reqPayloads of
            Just ShouldExceededSizeLimit -> True
            Just ShouldExceededTimeLimit -> False
            Nothing                      -> False
        Left ExceededTimeLimit{} ->
          pure $ case shouldFail reqPayloads of
            Just ShouldExceededTimeLimit -> True
            Just ShouldExceededSizeLimit -> False
            Nothing                      -> False

    where
      sendPeer :: Client (ReqResp String ()) NonPipelined Empty StIdle m stm [()]
      sendPeer = reqRespClientPeer $ reqRespClientMap $ map fst reqPayloads

      recvPeer :: Server (ReqResp String ()) NonPipelined Empty StIdle m stm [DiffTime]
      recvPeer = reqRespServerPeer $ reqRespServerMapAccumL
        (\a _ -> case a of
          [] -> error "prop_runPeerWithLimits: empty list"
          delay : acc -> do
            threadDelay delay
            return (acc, ()))
        (map snd reqPayloads)

      -- It is not enough to check if a testcase is expected to fail, we need to
      -- calculate which type of failure is going to happen first.
      shouldFail ::  [(String, DiffTime)] -> Maybe ShouldFail
      shouldFail [] =
          -- Check @MsgDone@ which is always sent
          let msgDone = encode (codecReqResp @String @() @m) MsgDone in
          if length msgDone > fromIntegral limit
             then Just ShouldExceededSizeLimit
             else Nothing
      shouldFail ((msg, delay):cmds) =
          let msg' = encode (codecReqResp @String @() @m) (MsgReq msg) in
          if length msg' > fromIntegral limit
          then Just ShouldExceededSizeLimit
          else if delay >= serverTimeout
          then Just ShouldExceededTimeLimit
          else shouldFail cmds

data ReqRespPayloadWithLimit = ReqRespPayloadWithLimit Word (String, DiffTime)
  deriving (Eq, Show)

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
      msgs <- replicateM (fromIntegral len) arbitrary
      delay <- frequency
        -- no delay
        [ (1, pure 0.0)
        -- below the limit
        , (1, pure 0.1)
        -- above the limit
        , (2, pure 0.3)
        ]
      return $ ReqRespPayloadWithLimit limit (msgs, delay)

    shrink (ReqRespPayloadWithLimit l (p, d)) =
      [ ReqRespPayloadWithLimit l' (p, d)
      | l' <- shrink l
      ]
      ++
      [ ReqRespPayloadWithLimit l (p', d)
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
        runSimOrThrow (prop_runPeerWithLimits nullTracer limit [payload])
    where
      labelExamples :: Word -> (String, DiffTime) -> [String]
      labelExamples l (p,_) =
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
  ioProperty  (prop_runPeerWithLimits nullTracer limit [payload])

