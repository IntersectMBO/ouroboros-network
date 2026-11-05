{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Ouroboros.Network.KeepAlive (tests) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad (void)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Monad.IOSim
import Control.Tracer
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Typeable (Typeable)
import System.Random (StdGen, mkStdGen)


import Ouroboros.Network.BlockFetch
import Ouroboros.Network.Channel
import Ouroboros.Network.ControlMessage (ControlMessage (..), ControlMessageSTM)
import Ouroboros.Network.DeltaQ
import Ouroboros.Network.Driver.Limits
import Ouroboros.Network.KeepAlive
import Ouroboros.Network.Protocol.KeepAlive.Client
import Ouroboros.Network.Protocol.KeepAlive.Codec
import Ouroboros.Network.Protocol.KeepAlive.Server

import Test.QuickCheck hiding (sample)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests = testGroup "KeepAlive"
    [ testProperty "KeepAlive Convergence" prop_keepAlive_convergence
    , testCase "raw RTT is absent before the first success" unit_initialAbsence
    , testCase "raw RTT and completion time publish before slow tracing" unit_publication
    , testCase "latest raw RTT remains independent of GSV smoothing" unit_latestRaw
    , testCase "reader reacquisition preserves the live registration's sample" unit_reacquire
    , testCase "reconnect cannot revive an old reader or producer" unit_reconnect
    , testCase "cancellation retires readers and clears the registry" unit_cancellation
    , testCase "an outstanding probe does not publish a successful RTT" unit_pendingProbe
    , testCase "legacy client retains GSV behavior without raw publication" unit_legacy
    ]

-- These direct client-continuation checks model successful exchanges. The
-- convergence property below also exercises the cookie-checked wire driver.
completeProbe :: DiffTime -> KeepAliveClient (IOSim s) () -> IOSim s ()
completeProbe elapsed (KeepAliveClient start) = do
    request <- start
    case request of
      SendMsgKeepAlive _ response -> do
        threadDelay elapsed
        next <- response
        case next of
          SendMsgDone done -> done
          _ -> error "completeProbe: expected termination after one success"
      _ -> error "completeProbe: expected a probe"

rawClient :: Tracer (IOSim s) (TraceKeepAliveClient String)
          -> KeepAliveClientContext String (IOSim s)
          -> KeepAliveClient (IOSim s) ()
rawClient tracer context =
    keepAliveClientWithRtt tracer (mkStdGen 0) (pure Terminate) "peer" context
      (KeepAliveInterval 10)

registeredReader :: KeepAliveRegistry String (IOSim s)
                 -> IOSim s (KeepAliveRttReader (IOSim s))
registeredReader registry = atomically $ do
    reader <- lookupKeepAliveRtt registry "peer"
    maybe (error "registeredReader: missing registration") pure reader

gsvRtt :: PeerGSV -> DiffTime
gsvRtt PeerGSV { outboundGSV = GSV outgoing _ _, inboundGSV = GSV incoming _ _ } =
    outgoing + incoming

unit_initialAbsence :: Assertion
unit_initialAbsence =
    runSimOrThrow (do
      registry <- newKeepAliveRegistry
      before <- atomically (isNothing <$> lookupKeepAliveRtt registry "peer")
      during <- bracketKeepAliveClientWithRtt registry "peer" $ \_ -> do
        reader <- registeredReader registry
        atomically (readKeepAliveRtt reader)
      after <- atomically (isNothing <$> lookupKeepAliveRtt registry "peer")
      pure (before, during, after))
    @?= (True, Nothing, True)

unit_publication :: Assertion
unit_publication =
    runSimOrThrow (do
      registry <- newKeepAliveRegistry
      bracketKeepAliveClientWithRtt registry "peer" $ \context -> do
        reader <- registeredReader registry
        traced <- newTVarIO Nothing
        let tracer = mkTracer $ \_ -> do
              sample <- atomically (readKeepAliveRtt reader)
              atomically (writeTVar traced sample)
              threadDelay 1
        threadDelay 1
        completeProbe 0.25 (rawClient tracer context)
        sample <- atomically (readKeepAliveRtt reader)
        tracedSample <- atomically (readTVar traced)
        now <- getMonotonicTime
        pure (sample, tracedSample, now))
    @?= (Just expected, Just expected, Time 2.25)
  where
    expected = KeepAliveRtt 0.25 (Time 1.25)

unit_latestRaw :: Assertion
unit_latestRaw =
    runSimOrThrow (do
      registry <- newKeepAliveRegistry
      bracketKeepAliveClientWithRtt registry "peer" $ \context -> do
        reader <- registeredReader registry
        threadDelay 1
        completeProbe 0.1 (rawClient nullTracer context)
        completeProbe 0.3 (rawClient nullTracer context)
        sample <- atomically (readKeepAliveRtt reader)
        estimates <- atomically (readTVar (keepAliveGsvContext context))
        let estimate = gsvRtt (estimates Map.! "peer")
        pure (sample, estimate > 0.1 && estimate < 0.3))
    @?= (Just (KeepAliveRtt 0.3 (Time 1.4)), True)

unit_reacquire :: Assertion
unit_reacquire =
    runSimOrThrow (do
      registry <- newKeepAliveRegistry
      bracketKeepAliveClientWithRtt registry "peer" $ \context -> do
        first <- registeredReader registry
        completeProbe 0.1 (rawClient nullTracer context)
        second <- registeredReader registry
        cached <- atomically (readKeepAliveRtt second)
        completeProbe 0.2 (rawClient nullTracer context)
        currentFirst <- atomically (readKeepAliveRtt first)
        currentSecond <- atomically (readKeepAliveRtt second)
        pure (cached, currentFirst, currentSecond))
    @?= ( Just (KeepAliveRtt 0.1 (Time 0.1))
        , Just (KeepAliveRtt 0.2 (Time 0.3))
        , Just (KeepAliveRtt 0.2 (Time 0.3)) )

unit_reconnect :: Assertion
unit_reconnect =
    runSimOrThrow (do
      registry <- newKeepAliveRegistry
      (oldReader, oldProducer) <- bracketKeepAliveClientWithRtt registry "peer" $ \context -> do
        reader <- registeredReader registry
        completeProbe 0.1 (rawClient nullTracer context)
        pure (reader, context)
      retired <- atomically (readKeepAliveRtt oldReader)
      bracketKeepAliveClientWithRtt registry "peer" $ \context -> do
        reader <- registeredReader registry
        initial <- atomically (readKeepAliveRtt reader)
        atomically (publishKeepAliveRtt oldProducer (KeepAliveRtt 99 (Time 99)))
        completeProbe 0.2 (rawClient nullTracer context)
        old <- atomically (readKeepAliveRtt oldReader)
        current <- atomically (readKeepAliveRtt reader)
        pure (retired, initial, old, current))
    @?= (Nothing, Nothing, Nothing, Just (KeepAliveRtt 0.2 (Time 0.3)))

unit_cancellation :: Assertion
unit_cancellation =
    runSimOrThrow (do
      registry <- newKeepAliveRegistry
      started <- newEmptyTMVarIO
      withAsync (bracketKeepAliveClientWithRtt registry "peer" $ \context -> do
          reader <- registeredReader registry
          completeProbe 0.1 (rawClient nullTracer context)
          atomically (putTMVar started reader)
          threadDelay 100) $ \client -> do
        reader <- atomically (takeTMVar started)
        before <- atomically (readKeepAliveRtt reader)
        cancel client
        after <- atomically (readKeepAliveRtt reader)
        missing <- atomically (isNothing <$> lookupKeepAliveRtt registry "peer")
        estimates <- atomically (readTVar (dqRegistry registry))
        pure (before, after, missing && Map.null estimates))
    @?= (Just (KeepAliveRtt 0.1 (Time 0.1)), Nothing, True)

unit_pendingProbe :: Assertion
unit_pendingProbe =
    runSimOrThrow (do
      registry <- newKeepAliveRegistry
      (clientChannel, _) <- createConnectedChannels
      withAsync (runKeepAliveClient nullTracer (mkStdGen 0) (pure Continue)
                    registry "peer" clientChannel (KeepAliveInterval 10)) $ \client -> do
        reader <- atomically $ do
          current <- lookupKeepAliveRtt registry "peer"
          maybe retry pure current
        threadDelay 0.1
        sample <- atomically (readKeepAliveRtt reader)
        cancel client
        missing <- atomically (isNothing <$> lookupKeepAliveRtt registry "peer")
        pure (sample, missing))
    @?= (Nothing, True)

unit_legacy :: Assertion
unit_legacy =
    runSimOrThrow (do
      registry <- newKeepAliveRegistry
      bracketKeepAliveClient registry "peer" $ \dqCtx -> do
        reader <- registeredReader registry
        threadDelay 1
        completeProbe 0.125 (keepAliveClient nullTracer (mkStdGen 0)
                              (pure Terminate) "peer" dqCtx (KeepAliveInterval 10))
        sample <- atomically (readKeepAliveRtt reader)
        estimates <- atomically (readTVar dqCtx)
        pure (sample, gsvRtt (estimates Map.! "peer")))
    @?= (Nothing, 0.125)

runKeepAliveClient
    :: forall m peer.
        ( MonadAsync m
        , MonadEvaluate m
        , MonadFork m
        , MonadMask m
        , MonadST m
        , MonadTimer m
        , MonadThrow (STM m)
        , Ord peer)
    => Tracer m (TraceKeepAliveClient peer)
    -> StdGen
    -> ControlMessageSTM m
    -> KeepAliveRegistry peer m
    -> peer
    -> Channel m BL.ByteString
    -> KeepAliveInterval
    -> m ((), Maybe BL.ByteString)
runKeepAliveClient tracer rng controlMessageSTM registry peer channel keepAliveInterval =
    let kacApp dqCtx = runPeerWithLimits
                         nullTracer
                         codecKeepAlive_v2
                         (byteLimitsKeepAlive (fromIntegral . BL.length))
                         timeLimitsKeepAlive
                         channel
                         $ keepAliveClientPeer
                         $ keepAliveClientWithRtt tracer rng controlMessageSTM peer dqCtx keepAliveInterval in
    bracketKeepAliveClientWithRtt registry peer kacApp

runKeepAliveServer
    :: forall m.
        ( MonadAsync m
        , MonadEvaluate m
        , MonadFork m
        , MonadMask m
        , MonadST m
        , MonadTimer m
        , MonadThrow (STM m)
        )
    => Channel m BL.ByteString
    -> m ((), Maybe BL.ByteString)
runKeepAliveServer channel =
    runPeerWithLimits
        nullTracer
        codecKeepAlive_v2
        (byteLimitsKeepAlive (fromIntegral . BL.length))
        timeLimitsKeepAlive
        channel
        $ keepAliveServerPeer
        $ keepAliveServer

runKeepAliveClientAndServer
    :: forall m peer.
        ( MonadAsync m
        , MonadDelay m
        , MonadEvaluate m
        , MonadFork m
        , MonadMask m
        , MonadSay m
        , MonadST m
        , MonadTimer m
        , MonadThrow (STM m)
        , Ord peer
        )
    => NetworkDelay
    -> Int
    -> Tracer m (TraceKeepAliveClient peer)
    -> ControlMessageSTM m
    -> KeepAliveRegistry peer m
    -> peer
    -> KeepAliveInterval
    -> m (Async m ((), Maybe BL.ByteString), Async m ((), Maybe BL.ByteString))
runKeepAliveClientAndServer (NetworkDelay nd) seed tracer controlMessageSTM registry peer keepAliveInterval = do
    (clientChannel, serverChannel) <- createConnectedChannels

    clientAsync <- async $ runKeepAliveClient tracer (mkStdGen seed) controlMessageSTM registry peer
                               (delayChannel nd clientChannel) keepAliveInterval
    serverAsync <- async $ runKeepAliveServer serverChannel
    return (clientAsync, serverAsync)

newtype NetworkDelay = NetworkDelay {
      unNetworkDelay :: DiffTime
    } deriving Show

instance Arbitrary NetworkDelay where
    arbitrary = do
        m <- choose (1, 1000 :: Int) -- A delay between 1 and 1000 ms
        return $ NetworkDelay $ (fromIntegral m) / 1000

prop_keepAlive_convergenceM
    :: forall m.
        ( MonadAsync m
        , MonadDelay m
        , MonadEvaluate m
        , MonadFork m
        , MonadLabelledSTM m
        , MonadMask m
        , MonadSay m
        , MonadST m
        , MonadTimer m
        , MonadThrow (STM m)
        )
    => Tracer m (TraceKeepAliveClient String)
    -> NetworkDelay
    -> Int
    -> m ()
prop_keepAlive_convergenceM tracer (NetworkDelay nd) seed = do
    registry <- newKeepAliveRegistry
    controlMessageV <- newTVarIO Continue
    let controlMessageSTM = readTVar controlMessageV
        clientId = "client"
        keepAliveInterval = 10

    (c_aid, s_aid) <- runKeepAliveClientAndServer (NetworkDelay nd) seed tracer controlMessageSTM
                          registry clientId (KeepAliveInterval keepAliveInterval)

    void $ wait c_aid
    void $ wait s_aid

-- Test that our estimate of PeerGSV's G terms converge to
-- a given constant delay.
prop_keepAlive_convergence :: NetworkDelay -> Int -> Property
prop_keepAlive_convergence nd seed =
    let trace = take 1000
              . selectTraceEventsDynamic
              $ runSimTrace
              $ prop_keepAlive_convergenceM dynamicTracer nd seed
     in verifyConvergence trace
  where
    verifyConvergence :: [TraceKeepAliveClient String] -> Property
    verifyConvergence [] = property False
    verifyConvergence [e] = property $ validTrace lastG e
    verifyConvergence (e:es) =
        if validTrace validG e then verifyConvergence es
                               else property False

    validTrace :: (GSV -> Bool) -> TraceKeepAliveClient String -> Bool
    validTrace vg (AddSample _ rtt PeerGSV{outboundGSV, inboundGSV}) =
        unNetworkDelay nd == rtt && vg outboundGSV && vg inboundGSV

    validG :: GSV -> Bool
    validG (GSV g _ _) = g >= 0 && g < 2 * unNetworkDelay nd

    lastG :: GSV -> Bool
    lastG (GSV g _ _) =
        let low = 0.95 * (unNetworkDelay nd) / 2
            high = 1.05 * (unNetworkDelay nd) / 2 in
        g >= low && g <= high

dynamicTracer :: Typeable a => Tracer (IOSim s) a
dynamicTracer = mkTracer traceM
