{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}


module Test.Ouroboros.Network.Driver (tests) where

import           Data.Bifunctor (bimap)
import           Data.Functor (($>))
import           Data.List (intercalate)
import qualified Data.List as List
import           Data.Monoid (Endo (..))

import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Peer.Client (Client)
import           Network.TypedProtocol.Peer.Server (Server)
import qualified Network.TypedProtocol.Stateful.Codec as Stateful
import qualified Network.TypedProtocol.Stateful.Peer.Client as Stateful

import           Ouroboros.Network.Channel
import           Ouroboros.Network.Driver
import           Ouroboros.Network.Driver.Limits
import           Ouroboros.Network.Driver.Simple
import qualified Ouroboros.Network.Driver.Stateful as Stateful

import           Network.TypedProtocol.ReqResp.Client
import           Network.TypedProtocol.ReqResp.Codec
import           Network.TypedProtocol.ReqResp.Examples
import           Network.TypedProtocol.ReqResp.Server
import           Network.TypedProtocol.ReqResp.Type
import qualified Network.TypedProtocol.Stateful.ReqResp.Client as Stateful
import           Network.TypedProtocol.Stateful.ReqResp.Examples
                     (ReqRespStateCallbacks (..))
import qualified Network.TypedProtocol.Stateful.ReqResp.Examples as Stateful

import           Network.TypedProtocol.PingPong.Client
import           Network.TypedProtocol.PingPong.Codec
import           Network.TypedProtocol.PingPong.Examples
import           Network.TypedProtocol.PingPong.Server
import           Network.TypedProtocol.PingPong.Type (PingPong)

import           Control.Applicative (Alternative)
import           Control.Exception (throw)
import           Control.Monad (replicateM, void)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTest
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
tests =
  testGroup "Ouroboros.Network.Driver"
  [ testGroup "Limits"
    [ testProperty "channel ReqResp ST"              prop_channel_reqresp_ST
    , testProperty "channel ReqResp IO"              prop_channel_reqresp_IO
    , testProperty "channel PingPong ST"             prop_channel_ping_pong_ST
    , testProperty "channel PingPong IO"             prop_channel_ping_pong_IO
    , testProperty "channel PingPong STM ST"         prop_channel_ping_pong_stm_ST
    , testProperty "channel PingPong STM IO"         prop_channel_ping_pong_stm_IO
    , testProperty "channel PingPong with limits ST" prop_channel_ping_pong_with_limits_ST
    , testProperty "channel PingPong with limits STM ST"
                    prop_channel_ping_pong_with_limits_stm_ST
    ]
  , testGroup "Stateful"
    [ testProperty "channel Stateful ReqResp ST"     prop_channel_stateful_reqresp_ST
    , testProperty "channel Stateful ReqResp IO"     (withMaxSuccess 33 $ prop_channel_stateful_reqresp_IO)
    ]
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
prop_channel_reqresp
  :: forall m. ( Alternative (STM m), MonadAsync m, MonadDelay m, MonadFork m, MonadMask m,
                 MonadThrow (STM m), MonadTime m, MonadTimer m)
  => Tracer m (TraceSendRecv (ReqResp String ()))
  -> Word
  -- ^ byte limit
  -> [(String, DiffTime)]
  -- ^ request payloads
  -> m Bool
prop_channel_reqresp tracer limit reqPayloads = do
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
          [] -> error "prop_channel_reqresp: empty list"
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
prop_channel_reqresp_ST
  :: ReqRespPayloadWithLimit
  -> Property
prop_channel_reqresp_ST (ReqRespPayloadWithLimit limit payload) =
      tabulate "Limit Boundaries" (labelExamples limit payload) $
        let trace = runSimTrace (prop_channel_reqresp (Tracer (say . show)) limit [payload])
        in counterexample (intercalate "\n" $ map show $ traceEvents trace)
           $ case traceResult True trace of
               Left e  -> throw e
               Right x -> x
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

prop_channel_reqresp_IO
  :: ReqRespPayloadWithLimit
  -> Property
prop_channel_reqresp_IO (ReqRespPayloadWithLimit limit payload) =
  ioProperty  (prop_channel_reqresp nullTracer limit [payload])


prop_channel_ping_pong
  :: ( Alternative (STM m)
     , MonadAsync       m
     , MonadDelay       m
     , MonadLabelledSTM m
     , MonadMask        m
     , MonadTest        m
     , MonadThrow  (STM m)
     )
  => DiffTime
  -> DiffTime
  -> Int
  -> Tracer m (Role, TraceSendRecv PingPong)
  -> m Bool
prop_channel_ping_pong a b n tr = do
    exploreRaces
    (_, r) <- runConnectedPeers (bimap (delayChannel a)
                                       (delayChannel b)
                                  <$> createConnectedChannels)
                                tr
                                codecPingPong client server
    return (r == n)
  where
    client = pingPongClientPeerPipelined (pingPongClientPipelinedMin n)
    server = pingPongServerPeer  pingPongServerCount


data ReqRespState a (st :: ReqResp req resp) where
    ReqRespState :: a -> ReqRespState a st


reqRespStateCallbacks :: (Int -> Int) -> ReqRespStateCallbacks (ReqRespState Int)
reqRespStateCallbacks f =
    ReqRespStateCallbacks {
        rrBusyToIdle = \(ReqRespState a) -> ReqRespState $! f a
      , rrBusyToBusy = id
      , rrBusyToDone = \(ReqRespState a) -> ReqRespState $! f a
      }


-- | Run the server peer using @runPeerWithByteLimit@, which will receive requests
-- with the given payloads.
--
prop_channel_stateful_reqresp
  :: forall m. ( Alternative (STM m), MonadAsync m, MonadDelay m,
                 MonadLabelledSTM m, MonadMask m, MonadThrow (STM m))
  => Tracer m (TraceSendRecv (ReqResp String ()))
  -> [(String, DiffTime)]
  -- ^ request payloads
  -> (Int -> Int)
  -> m Property
prop_channel_stateful_reqresp tracer reqPayloads f = do
      (c1, c2) <- createConnectedChannels

      res <- try $
        (fst <$> runPeer tracer codecReqResp c1 recvPeer)
          `concurrently`
        ((\((_, ReqRespState a), _) -> a)
             <$> Stateful.runPeer tracer (Stateful.liftCodec codecReqResp) c2 (ReqRespState 0) sendPeer)

      pure $ case res :: Either ProtocolLimitFailure ([DiffTime], Int) of
        Right (_, a)             -> a === appEndo (mconcat (reqPayloads $> Endo f)) 0
        Left ExceededSizeLimit{} -> property False
        Left ExceededTimeLimit{} -> property False

    where
      sendPeer :: Stateful.Client (ReqResp String ()) NonPipelined Empty StIdle (ReqRespState Int) m stm
                                  ([()], ReqRespState Int (StDone :: ReqResp String ()))
      sendPeer = Stateful.reqRespClientPeer
               $ Stateful.reqRespClientMap
                   (reqRespStateCallbacks f)
                   (ReqRespState 0)
                   (map fst reqPayloads)

      recvPeer :: Server (ReqResp String ()) NonPipelined Empty StIdle m stm [DiffTime]
      recvPeer = reqRespServerPeer $ reqRespServerMapAccumL
        (\a _ -> case a of
          [] -> error "prop_runPeerWithLimits: empty list"
          delay : acc -> do
            threadDelay delay
            return (acc, ()))
        (map snd reqPayloads)


prop_channel_stateful_reqresp_ST
  :: ReqRespPayloadWithLimit
  -> (Int -> Int)
  -> Property
prop_channel_stateful_reqresp_ST (ReqRespPayloadWithLimit _limit payload) f =
  let trace = runSimTrace (prop_channel_stateful_reqresp (Tracer (say . show)) [payload] f)
  in counterexample (intercalate "\n" $ map show $ traceEvents trace)
   $ case traceResult True trace of
       Left e  -> throw e
       Right x -> x


prop_channel_stateful_reqresp_IO
  :: ReqRespPayloadWithLimit
  -> (Int -> Int)
  -> Property
prop_channel_stateful_reqresp_IO (ReqRespPayloadWithLimit _limit payload) f =
  ioProperty (prop_channel_stateful_reqresp nullTracer [payload] f)


prop_channel_ping_pong_ST
  :: NonNegative Int
  -- delay in simulated seconds
  -> NonNegative Int
  -- delay in simulated seconds
  -> NonNegative Int
  -> Property
prop_channel_ping_pong_ST (NonNegative a) (NonNegative b) (NonNegative n) =
    let trace = runSimTrace sim in
        counterexample (ppTrace trace)
      $ case traceResult True trace of
          Left e  -> counterexample (show e) False
          Right r -> property r
  where
    sim :: IOSim s Bool
    sim = prop_channel_ping_pong (fromIntegral a)
                                 (fromIntegral b)
                                 n
                                 nullTracer

prop_channel_ping_pong_IO
  :: NonNegative Int
  -- delay in micro seconds
  -> NonNegative Int
  -- delay in micro seconds
  -> NonNegative Int
  -> Property
prop_channel_ping_pong_IO (NonNegative a) (NonNegative b) (NonNegative n) =
    ioProperty (prop_channel_ping_pong (fromIntegral a / 1_000_000)
                                       (fromIntegral b / 1_000_000)
                                       n nullTracer)



prop_channel_ping_pong_stm
  :: ( Alternative (STM m)
     , MonadAsync       m
     , MonadDelay       m
     , MonadLabelledSTM m
     , MonadMask        m
     , MonadTest        m
     , MonadThrow  (STM m)
     )
  => DiffTime
  -> DiffTime
  -> Int -- ^ pipelining depth
  -> Int
  -> Tracer m (Role, TraceSendRecv PingPong)
  -> m Bool
prop_channel_ping_pong_stm a b omax n tr = do
    exploreRaces
    (_, r) <- runConnectedPeers (bimap (delayChannel a)
                                       (delayChannel b)
                                  <$> createConnectedBufferedChannels
                                       (fromIntegral omax))
                                tr
                                codecPingPong client server
    return (r == n)
  where
    client = pingPongClientPeerPipelinedSTM (pingPongClientPipelinedLimited omax n)
    server = pingPongServerPeer  pingPongServerCount

prop_channel_ping_pong_stm_ST
  :: NonNegative Int
  -- ^ delay in simulated seconds
  -> NonNegative Int
  -- ^ delay in simulated seconds
  -> Positive    Int
  -- ^ maximal pipelining depth
  -> NonNegative Int
  -> Property
prop_channel_ping_pong_stm_ST (NonNegative a) (NonNegative b)
                              (Positive omax) (NonNegative n) =
    exploreSimTrace id sim $ \_ trace ->
    counterexample (ppTrace trace)
      $ case traceResult True trace of
          Left e  -> counterexample (show e) False
          Right r -> property r
  where
    sim :: IOSim s Bool
    sim = prop_channel_ping_pong_stm (fromIntegral a)
                                     (fromIntegral b)
                                     omax
                                     n
                                     (Tracer $ say . show)


prop_channel_ping_pong_stm_IO
  :: NonNegative Int
  -- delay in micro seconds
  -> NonNegative Int
  -- delay in micro seconds
  -> Positive Int
  -- ^ pipelining depth
  -> NonNegative Int
  -> Property
prop_channel_ping_pong_stm_IO (NonNegative a) (NonNegative b)
                              (Positive omax) (NonNegative n) =
    ioProperty (prop_channel_ping_pong_stm (fromIntegral a / 1_000_000)
                                           (fromIntegral b / 1_000_000)
                                           omax n nullTracer)

prop_channel_ping_pong_with_limits
  :: ( Alternative (STM m)
     , MonadAsync       m
     , MonadDelay       m
     , MonadLabelledSTM m
     , MonadFork        m
     , MonadMask        m
     , MonadTest        m
     , MonadThrow  (STM m)
     , MonadTimer       m
     )
  => DiffTime
  -> DiffTime
  -> Int
  -> Tracer m (Role, TraceSendRecv PingPong)
  -> ProtocolSizeLimits PingPong String
  -> ProtocolTimeLimits PingPong
  -> m Bool
prop_channel_ping_pong_with_limits a b n tr slimits tlimits = do
    exploreRaces
    (_, r) <- runConnectedPeersWithLimits
                                (bimap (delayChannel a)
                                       (delayChannel b)
                                  <$> createConnectedBufferedChannelsUnbounded)
                                tr
                                codecPingPong
                                slimits tlimits
                                client server
    return (r == n)
  where
    client = pingPongClientPeerPipelined (pingPongClientPipelinedMin n)
    server = pingPongServerPeer  pingPongServerCount


prop_channel_ping_pong_with_limits_stm
  :: ( Alternative (STM m)
     , MonadAsync       m
     , MonadDelay       m
     , MonadLabelledSTM m
     , MonadFork        m
     , MonadMask        m
     , MonadTest        m
     , MonadThrow  (STM m)
     , MonadTimer       m
     )
  => DiffTime
  -> DiffTime
  -> Int
  -> Tracer m (Role, TraceSendRecv PingPong)
  -> ProtocolSizeLimits PingPong String
  -> ProtocolTimeLimits PingPong
  -> m Bool
prop_channel_ping_pong_with_limits_stm a b n tr slimits tlimits = do
    exploreRaces
    (_, r) <- runConnectedPeersWithLimits
                                (bimap (delayChannel a)
                                       (delayChannel b)
                                  <$> createConnectedBufferedChannelsUnbounded)
                                tr
                                codecPingPong
                                slimits tlimits
                                client server
    return (r == n)
  where
    client = pingPongClientPeerPipelinedSTM (pingPongClientPipelinedMin n)
    server = pingPongServerPeer  pingPongServerCount


data ArbDelaysAndTimeouts = ArbDelaysAndTimeouts DiffTime -- ^ channel delay
                                                 DiffTime -- ^ channel delay
                                                 DiffTime -- ^ timeout limit
  deriving Show

instance Arbitrary ArbDelaysAndTimeouts where
    arbitrary = do
      NonNegative delay   <- arbitrary
      NonNegative delay'  <- arbitrary
      tlimit <-
        frequency
          [ (1, getPositive <$> resize (delay + delay') arbitrary)
          , (9, (\(Positive t) -> delay + delay' + t) <$> arbitrary)
          ]
      return $ ArbDelaysAndTimeouts (fromIntegral delay  / 1_000_000)
                                    (fromIntegral delay  / 1_000_000)
                                    (fromIntegral tlimit / 1_000_000)


expectExceedTimeLimit :: ArbDelaysAndTimeouts -> Bool
expectExceedTimeLimit (ArbDelaysAndTimeouts delay delay' timelimit) =
    let rtt = delay + delay' in timelimit <= rtt


data ArbSizeLimit = ArbExceedSize
                  | ArbEnoughSize
  deriving Show

instance Arbitrary ArbSizeLimit where
    arbitrary = frequency [ (1, pure ArbExceedSize)
                          , (9, pure ArbEnoughSize)
                          ]

toSize :: ArbSizeLimit -> Word
toSize ArbExceedSize = 4
toSize ArbEnoughSize = 5


labelLimits :: ArbDelaysAndTimeouts
            -> ArbSizeLimit
            -> Property
            -> Property
labelLimits timelimit sizelimit =
    tabulate "Limit Boundaries" $
         if expectExceedTimeLimit timelimit
            then ["AboveTimeLimit"]
            else ["BelowTimeLimit"]
      ++ if closeToTheTimeLimit
            then ["CloseToTimeLimit"]
            else []
      ++ if toSize sizelimit >= 5
            then ["AboveSizeLimit"]
            else ["BelowSizeLimit"]
  where
    closeToTheTimeLimit =
      case timelimit of
        ArbDelaysAndTimeouts delay delay' tlimit ->
          let rtt = delay + delay' in
              abs (rtt - tlimit) >= rtt / 10


prop_channel_ping_pong_with_limits_ST
  :: ArbDelaysAndTimeouts
  -> ArbSizeLimit
  -> NonNegative Int
  -> Property
prop_channel_ping_pong_with_limits_ST a@(ArbDelaysAndTimeouts delay delay' timelimit)
                                        sizelimit
                                        (NonNegative n) =
    labelLimits a sizelimit $
    let trace = runSimTrace sim in
        counterexample (ppTrace trace)
      $ case traceResult True trace of
          Left (FailureException e)
                  | Just ExceededTimeLimit {} <- fromException e
                  , expectExceedTimeLimit a
                  -> property True
                  | Just ExceededSizeLimit {} <- fromException e
                  , ArbExceedSize <- sizelimit
                  -> property True
          Left  e -> counterexample (show e) False
          Right r -> property r
  where
    sim :: IOSim s Bool
    sim = prop_channel_ping_pong_with_limits delay delay'
                                             n (Tracer $ say . show)
                                             slimits tlimits

    slimits :: ProtocolSizeLimits PingPong String
    slimits = ProtocolSizeLimits {
        sizeLimitForState = \_ -> toSize sizelimit,
        dataSize = fromIntegral . List.length
      }

    tlimits :: ProtocolTimeLimits PingPong
    tlimits = ProtocolTimeLimits {
        timeLimitForState = \_ -> Just timelimit
      }


prop_channel_ping_pong_with_limits_stm_ST
  :: ArbDelaysAndTimeouts
  -> ArbSizeLimit
  -> NonNegative Int
  -> Property
prop_channel_ping_pong_with_limits_stm_ST a@(ArbDelaysAndTimeouts delay delay' timelimit)
                                          sizelimit
                                          (NonNegative n) =
    labelLimits a sizelimit $
    let trace = runSimTrace sim in
        counterexample (ppTrace trace)
      $ case traceResult True trace of
          Left (FailureException e)
                  | Just ExceededTimeLimit {} <- fromException e
                  , expectExceedTimeLimit a
                  -> property True
                  | Just ExceededSizeLimit {} <- fromException e
                  , ArbExceedSize <- sizelimit
                  -> property True
          Left  e -> counterexample (show e) False
          Right r -> property r
  where
    sim :: IOSim s Bool
    sim = prop_channel_ping_pong_with_limits_stm delay delay'
                                                 n (Tracer $ say . show)
                                                 slimits tlimits

    slimits :: ProtocolSizeLimits PingPong String
    slimits = ProtocolSizeLimits {
        sizeLimitForState = \_ -> toSize sizelimit,
        dataSize = fromIntegral . List.length
      }

    tlimits :: ProtocolTimeLimits PingPong
    tlimits = ProtocolTimeLimits {
        timeLimitForState = \_ -> Just timelimit
      }
