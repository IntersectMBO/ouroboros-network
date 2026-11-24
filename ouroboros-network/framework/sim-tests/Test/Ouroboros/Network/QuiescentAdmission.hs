{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Replay for the CM/Server model: can an admission worker forked by
-- Server.with's own accept loop include its connection (and fork a handler)
-- only after the connection manager's shutdown snapshot, so that the
-- handler is never cancelled or waited for by the CM?
--
-- One listener, one client connect (no handshake is sent), and a Server
-- callback that returns as soon as the connect completes. IOSimPOR explores
-- schedules. A schedule is a witness when the CM traces 'TrIncludeConnection'
-- after Server.with has returned: the include happens inside the worker's
-- map lease, so its map take came after the shutdown snapshot read.
module Test.Ouroboros.Network.QuiescentAdmission (tests) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadTimer.SI
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadAsync
import Control.Monad (when, void)
import Control.Monad.Class.MonadFork (myThreadId)
import Data.Maybe (isNothing)
import Control.Monad.Class.MonadTest (exploreRaces)
import Control.Monad.IOSim
import Control.Tracer (mkTracer, nullTracer)
import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import System.Random (mkStdGen)

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Network.Mux qualified as Mx

import Ouroboros.Network.ConnectionManager.Core qualified as CM
import Ouroboros.Network.ConnectionManager.State qualified as CS
import Ouroboros.Network.Protocol.Handshake.Codec (timeLimitsHandshake)
import Ouroboros.Network.Server.RateLimiting (AcceptedConnectionsLimit (..))
import Ouroboros.Network.Snocket (Accept (..), Accepted (..), TestAddress (..))
import Ouroboros.Network.ConnectionId (ConnectionId (..))
import Ouroboros.Network.ConnectionManager.Types
import Ouroboros.Network.Snocket qualified as Snocket

import Simulation.Network.Snocket

import Test.Ouroboros.Network.ConnectionManager.Experiments
import Test.Ouroboros.Network.ConnectionManager.Timeouts
import Test.Ouroboros.Network.Utils (WithName (..))

tests :: TestTree
tests =
  testGroup "QuiescentAdmission"
  [ testProperty "default schedule: include before shutdown" prop_default
  , testProperty "IOSimPOR: no unreleased accepted connection" prop_pending
  , testProperty "IOSimPOR: no explored trace shows the late include itself" prop_late
  , testProperty "delayed Server worker: join precedes CM shutdown" (once (prop_worker Return))
  , testProperty "action exception joins admission" (once (prop_worker ActionError))
  , testProperty "IG-first joins admission" (once (prop_worker IGFirst))
  , testProperty "accept exception joins admission" (once (prop_worker AcceptError))
  , testProperty "external cancellation joins admission" (once (prop_worker ExternalCancellation))
  , testProperty "direct API: include after Server.with returns forks an unjoined handler" prop_direct
  ]

data Ev = EvInclude | EvShutdown | EvReturned | EvTransition String | EvResult String
  deriving (Eq, Show)

lateInclude :: [(Time, Ev)] -> Bool
lateInclude evs = EvInclude `elem` dropWhile (/= EvReturned) (map snd evs)

lateAdmissionSim :: forall s. IOSim s [(Time, Ev)]
lateAdmissionSim =
  withSnocket nullTracer noAttenuation Map.empty $ \snock _ -> do
    events <- newTVarIO []
    let record e = getMonotonicTime >>= \t -> atomically (modifyTVar events ((t, e) :))
        cmTracer = mkTracer $ \(WithName _ ev) -> case ev of
          CM.TrShutdown             -> record EvShutdown
          CM.TrIncludeConnection {} -> record EvInclude
          _                         -> pure ()
        addr0 = TestAddress (0 :: Int)
        addr1 = TestAddress 1
    bracket (Snocket.open snock Snocket.TestFamily) (Snocket.close snock) $ \listening -> do
      Snocket.bind snock listening addr0
      Snocket.listen snock listening
      connStateIdSupply <- atomically $ CS.newConnStateIdSupply (Proxy @(IOSim s))
      bracket (Snocket.open snock Snocket.TestFamily) (Snocket.close snock) $ \client -> do
        Snocket.bind snock client addr1
        withBidirectionalConnectionManager
          "server" simTimeouts nullTracer nullTracer cmTracer nullTracer Mx.nullTracers nullTracer
          (mkStdGen 0) snock makeFDBearer connStateIdSupply (\_ -> pure ())
          listening Nothing ([] :: [Int]) (pure (\_ -> pure [])) timeLimitsHandshake
          (AcceptedConnectionsLimit maxBound maxBound 0)
          (\_ _ _ -> Snocket.connect snock client addr0)
        record EvReturned
    -- After the Server scope and the test's own sockets are closed, give a
    -- still-pending admission worker a bounded chance to run its include.
    _ <- timeout 20 $ atomically $
      readTVar events >>= check . elem EvInclude . map snd
    threadDelay 1
    reverse <$> readTVarIO events

explore :: ExplorationOptions -> ExplorationOptions
explore o = o { explorationScheduleBound = 20000, explorationBranching = 20 }

-- | Same POR bounds as the baseline, now requiring strict shutdown.
prop_pending :: Property
prop_pending =
  exploreSimTrace explore (exploreRaces >> lateAdmissionSim) $ \_ tr ->
    case traceResult True tr of
      Left err  -> counterexample (show err) False
      Right evs -> counterexample (show evs) (not (lateInclude evs))

prop_default :: Property
prop_default =
  case traceResult True (runSimTrace lateAdmissionSim) of
    Left err  -> counterexample (show err) False
    Right evs -> counterexample (show evs) (EvInclude `elem` map snd evs .&&. not (lateInclude evs))

-- | No include from a Server-owned worker after the Server boundary.
prop_late :: Property
prop_late =
  exploreSimTrace explore (exploreRaces >> lateAdmissionSim) $ \_ tr ->
    case traceResult True tr of
      Left err  -> counterexample (show err) False
      Right evs -> counterexample (show evs) (not (lateInclude evs))

-- | Fallback replay: the CM handle given to the Server.with callback is used
-- after Server.with has returned (as a delayed admission worker would). The
-- include runs, a connection handler is forked, and that handler runs its
-- handshake to its own timeout: nothing cancels or waits for it.
directSim :: forall s. IOSim s [(Time, Ev)]
directSim =
  withSnocket nullTracer noAttenuation Map.empty $ \snock _ -> do
    events <- newTVarIO []
    let record e = getMonotonicTime >>= \t -> atomically (modifyTVar events ((t, e) :))
        cmTracer = mkTracer $ \(WithName _ ev) -> case ev of
          CM.TrShutdown             -> record EvShutdown
          CM.TrIncludeConnection {} -> record EvInclude
          _                         -> pure ()
        trTracer = mkTracer $ \(WithName _ tr) -> record (EvTransition (show tr))
        addr0 = TestAddress (0 :: Int)
        addr2 = TestAddress 2
        addr3 = TestAddress 3
    bracket (Snocket.open snock Snocket.TestFamily) (Snocket.close snock) $ \listening -> do
      Snocket.bind snock listening addr0
      Snocket.listen snock listening
      connStateIdSupply <- atomically $ CS.newConnStateIdSupply (Proxy @(IOSim s))
      cm <- withBidirectionalConnectionManager
          "server" simTimeouts nullTracer trTracer cmTracer nullTracer Mx.nullTracers nullTracer
          (mkStdGen 0) snock makeFDBearer connStateIdSupply (\_ -> pure ())
          listening Nothing ([] :: [Int]) (pure (\_ -> pure [])) timeLimitsHandshake
          (AcceptedConnectionsLimit maxBound maxBound 0)
          (\connectionManager _ _ -> pure connectionManager)
      record EvReturned
      bracket (Snocket.open snock Snocket.TestFamily) (Snocket.close snock) $ \lst2 -> do
        Snocket.bind snock lst2 addr2
        Snocket.listen snock lst2
        bracket (Snocket.open snock Snocket.TestFamily) (Snocket.close snock) $ \client -> do
          Snocket.bind snock client addr3
          withAsync (Snocket.connect snock client addr2) $ \_ -> do
           (accepted, _) <- Snocket.accept snock lst2 >>= runAccept
           case accepted of
             AcceptFailure err -> record (EvResult ("accept failure: " ++ show err))
             Accepted fd remote -> do
               r <- try @_ @SomeException
                      (includeInboundConnection cm maxBound fd
                         (ConnectionId { localAddress = addr2, remoteAddress = remote }))
               record (EvResult (either show resultName r))
           threadDelay 1
           reverse <$> readTVarIO events
  where
    resultName Connected {}    = "Connected"
    resultName Disconnected {} = "Disconnected"

-- | The CM finally has run and Server.with has returned; the include then
-- forks a handler (a fresh Unnegotiated Inbound connection) that runs until
-- its own handshake timeout and returns Disconnected.
prop_direct :: Property
prop_direct =
  case traceResult False (runSimTrace directSim) of
    Left err  -> counterexample (show err) False
    Right evs -> counterexample (unlines (map show evs)) $
      let after = dropWhile ((/= EvReturned) . snd) evs
      in    EvShutdown `elem` map snd (takeWhile ((/= EvReturned) . snd) evs)
       .&&. lateInclude evs
       .&&. any (isForked . snd) after
       .&&. any ((== EvResult "Disconnected") . snd) after
  where
    isForked (EvTransition t) = "UnknownConnectionSt -> UnnegotiatedSt Inbound" `isInfixOf` t
    isForked _                = False


-- | A single trace through the actual Server accept/admission and production
-- handshake handler. Pause admission after map publication, then let teardown
-- stop the listener. It must join admission without cancelling it; only then
-- may CM shutdown run. This complements the first-step POR schedules above.
data ExitMode = Return | ActionError | IGFirst | AcceptError | ExternalCancellation
  deriving (Eq, Show)

workerSim :: forall s. ExitMode -> IOSim s (Bool, Bool, Bool, Bool, Bool)
workerSim exitMode = withSnocket nullTracer noAttenuation Map.empty $ \snock _ -> do
  worker <- newEmptyTMVarIO
  entered <- newEmptyTMVarIO
  release <- newEmptyTMVarIO
  listenerStopped <- newEmptyTMVarIO
  shutdown <- newTVarIO False
  paused <- newTVarIO False
  births <- newTVarIO (0 :: Int)
  let local = TestAddress (0 :: Int)
      remote = TestAddress 1
      cmTracer = mkTracer $ \(WithName _ ev) -> case ev of
        CM.TrIncludeConnection {} -> myThreadId >>= atomically . putTMVar worker
        CM.TrShutdown -> atomically $ writeTVar shutdown True
        CM.TrConnectionManagerCounters {} -> do
          tid <- myThreadId
          mw <- atomically $ tryReadTMVar worker
          first <- atomically $ do
            seen <- readTVar paused
            if mw == Just tid && not seen
              then writeTVar paused True >> pure True
              else pure False
          when first $ do
            masking <- getMaskingState
            atomically $ putTMVar entered (masking == Unmasked)
            atomically $ takeTMVar release
        _ -> pure ()
      transitions = mkTracer $ \(WithName _ (TransitionTrace _ tr)) -> case toState tr of
        UnnegotiatedSt Inbound -> atomically $ modifyTVar births (+1)
        _ -> pure ()
      transport = snock { Snocket.close = \fd -> do
        void $ atomically $ tryPutTMVar listenerStopped ()
        Snocket.close snock fd,
        Snocket.accept = \fd -> do
          first <- Snocket.accept snock fd
          pure $ Accept $ do
            (accepted, next) <- runAccept first
            let failed = Accept $ do
                  _ <- atomically $ readTMVar entered
                  pure (AcceptFailure (toException (userError "accept failure")), next)
            pure (accepted, if exitMode == AcceptError then failed else next)
        }
  bracket (Snocket.open snock Snocket.TestFamily) (Snocket.close snock) $ \listening -> do
    Snocket.bind snock listening local
    Snocket.listen snock listening
    supply <- atomically $ CS.newConnStateIdSupply (Proxy @(IOSim s))
    bracket (Snocket.open snock Snocket.TestFamily) (Snocket.close snock) $ \client -> do
      Snocket.bind snock client remote
      let server = withBidirectionalConnectionManager
            "server" simTimeouts nullTracer transitions cmTracer nullTracer Mx.nullTracers nullTracer
            (mkStdGen 0) transport makeFDBearer supply (\_ -> pure ())
            listening Nothing ([] :: [Int]) (pure (\_ -> pure [])) timeLimitsHandshake
            (AcceptedConnectionsLimit maxBound maxBound 0)
            (\_ _ ig -> do
              Snocket.connect snock client local
              original <- atomically $ readTMVar entered
              case exitMode of
                Return -> pure original
                ActionError -> throwIO (userError "action failure")
                IGFirst -> cancel ig >> atomically retry
                _ -> atomically retry)
      withAsync server $ \scope ->
       withAsync (when (exitMode == ExternalCancellation) $
         atomically (readTMVar entered) >> cancel scope) $ \canceller -> do
        originalMask <- atomically $ readTMVar entered
        atomically $ takeTMVar listenerStopped
        threadDelay 1
        waiting <- isNothing <$> atomically (pollSTM scope)
        beforeCM <- not <$> readTVarIO shutdown
        atomically $ putTMVar release ()
        result <- waitCatch scope
        wait canceller
        let expectedExit = either (const (exitMode /= Return)) (const (exitMode == Return)) result
        n <- readTVarIO births
        stopped <- readTVarIO shutdown
        pure (waiting, beforeCM, originalMask, n == 1 && stopped, expectedExit)

prop_worker :: ExitMode -> Property
prop_worker mode = case traceResult True (runSimTrace (timeout 60 (workerSim mode))) of
  Left err -> counterexample (show err) False
  Right r  -> r === Just (True, True, True, True, True)
