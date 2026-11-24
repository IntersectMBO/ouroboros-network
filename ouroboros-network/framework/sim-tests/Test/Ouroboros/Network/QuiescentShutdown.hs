{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Physical-result conformance at the CM and full Server boundaries.
-- Synthetic transport, handshake, buffer failure and empty mux fixture; the
-- accept loop, admission, IG, CM fork and cleanup are library implementations.
module Test.Ouroboros.Network.QuiescentShutdown (tests) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad (unless, void, when)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork (myThreadId)
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTimer.SI (threadDelay, timeout)
import Control.Monad.IOSim
import Control.Tracer (mkTracer, nullTracer)
import Data.ByteString.Lazy (ByteString)
import Data.List (isInfixOf)
import Control.Monad.Class.MonadSay (say)
import Data.List.NonEmpty (NonEmpty (..))
import Network.Mux qualified as Mux
import Ouroboros.Network.ConnectionHandler (Handle (..))
import Ouroboros.Network.ControlMessage (ControlMessage (..))
import Ouroboros.Network.InboundGovernor qualified as IG
import Ouroboros.Network.Server qualified as Server
import Ouroboros.Network.Context (ResponderContext)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.Void (Void)
import Network.Mux.Types qualified as Mx
import System.Random (mkStdGen)
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Ouroboros.Network.ConnectionId (ConnectionId (..))
import Ouroboros.Network.ConnectionManager.Core qualified as CM
import Ouroboros.Network.ConnectionManager.State qualified as CS
import Ouroboros.Network.ConnectionManager.Types
import Ouroboros.Network.InboundGovernor.InformationChannel (InformationChannel (..), newInformationChannel)
import Ouroboros.Network.MuxMode
import Ouroboros.Network.Server.RateLimiting (AcceptedConnectionsLimit (..))
import Ouroboros.Network.Snocket

-- A synchronous failure in the supplied withBuffer acquisition, after Connected.
data BufferFailure = BufferFailure deriving (Eq, Show)
instance Exception BufferFailure

data Ev = AdmissionObserved | BufferFailed | Cleanup | SocketClosed
        | CleanupCounters | Shutdown | CMReturned | HandlerJoined | PublicationRolledBack
  deriving (Eq, Show)

data Report = Report {
    shutdownWaited :: Bool,
    admitted :: Bool,
    cleanupMasked :: Bool,
    emptyBeforeReturn :: Bool,
    closeCount :: Int,
    resultBeforeReturn :: String,
    resultAfterReturn :: String,
    resultAfterRelease :: String,
    events :: [Ev]
  } deriving (Eq, Show)

type Addr = TestAddress Int

resultName :: Maybe (Either SomeException ()) -> String
resultName Nothing = "pending"
resultName (Just (Right ())) = "normal"
resultName (Just (Left err))
  | Just BufferFailure <- fromException err = "BufferFailure"
  | otherwise = show err

-- | Direct-CM admission is explicitly joined. The full Server case uses its
-- own accept/admission worker and observes IG receiving the connection. Only
-- CM allocates handlers, mutates the map, cleans up and shuts down.
data Progress = Recover | BlockCleanup | Unpublished | FullQueue | Rollback deriving Eq

experiment :: forall s. Bool -> Bool -> Progress -> IOSim s Report
experiment useServer pauseCleanup progress = do
  let releaseCleanup = progress /= BlockCleanup
      failBeforePublication = progress == Unpublished
      queuePressure = progress == FullQueue
  publishGate <- newEmptyTMVarIO
  allHandlers <- newTVarIO ([] :: [Async (IOSim s) ()])
  acceptedCount <- newTVarIO (0 :: Int)
  notifiedCount <- newTVarIO (0 :: Int)
  logVar <- newTVarIO []
  closes <- newTVarIO (0 :: Int)
  handlerVar <- newEmptyTMVarIO
  failGate <- newEmptyTMVarIO
  tailEntered <- newEmptyTMVarIO
  tailRelease <- newEmptyTMVarIO
  shutdownEntered <- newEmptyTMVarIO
  stoppedGate <- newEmptyTMVarIO
  governor <- newEmptyTMVarIO
  announced <- newEmptyTMVarIO
  mux <- Mux.new Mux.nullTracers Nothing []
  controls <- traverse newTVarIO (pure Continue)
  let fixture = Handle mux (pure []) controls Duplex
      record e = atomically $ modifyTVar logVar (++ [e])
      local = TestAddress 0
      remote = TestAddress 1
      key = ConnectionId local remote
      acceptOne = Accept $ do
        n <- atomically $ do
          n <- readTVar acceptedCount
          check (n < if queuePressure then 101 else 1)
          writeTVar acceptedCount (n+1)
          pure (n+1)
        let peer = TestAddress n
        pure (Accepted peer peer, acceptOne)
      transport :: Snocket (IOSim s) Addr Addr
      transport = Snocket {
        getLocalAddr = const (pure local), getRemoteAddr = pure,
        addrFamily = const TestFamily,
        open = const (fail "unexpected open"),
        openToConnect = const (fail "unexpected outbound open"),
        connect = \_ _ -> fail "unexpected connect",
        bind = \_ _ -> fail "unexpected bind",
        listen = const (fail "unexpected listen"),
        accept = const (pure acceptOne),
        close = \socket -> do
          when (socket == remote) $ do
            atomically $ modifyTVar closes (+1)
            record SocketClosed
      }
      -- Match the source's successful promise -> withBuffer ordering. The
      -- handshake and handle are fixtures; no real mux or buffer is allocated.
      handler :: ConnectionHandler Mx.InitiatorResponderMode () Addr Addr (Handle Mx.InitiatorResponderMode () (ResponderContext Addr) DataFlow ByteString (IOSim s) () ()) Void () DataFlow (IOSim s)
      handler = ConnectionHandler (WithInitiatorResponderMode body body)
        where
          body _ _ writer _ _ _ withBuffer = MaskedAction $ \unmask -> do
            when queuePressure $ atomically $ readTMVar publishGate
            when failBeforePublication $ throwIO BufferFailure
            atomically $ writePromise writer (Right (HandshakeConnectionResult fixture ((), Duplex)))
            unmask $ atomically $ takeTMVar failGate
            withBuffer $ \_ -> fail "buffer callback must not run"
      cmTracer = mkTracer $ \event -> case event of
        CM.TrShutdown -> say "CM shutdown" >> record Shutdown >> atomically (putTMVar shutdownEntered ())
        CM.TrConnectionCleanup _ -> record Cleanup
        CM.TrConnectionManagerCounters _ -> do
          -- Admission also traces counters: pause only the physical handler.
          tid <- myThreadId
          mh <- atomically $ tryReadTMVar handlerVar
          case mh of
            Just h | asyncThreadId h == tid -> do
              masking <- getMaskingState
              record CleanupCounters
              atomically $ putTMVar tailEntered (masking == MaskedUninterruptible)
              when pauseCleanup $ atomically $ takeTMVar tailRelease
            _ -> pure ()
        _ -> pure ()
      transitions = mkTracer $ \(TransitionTrace _ tr) -> case toState tr of
        Known (CS.UnnegotiatedState Inbound _ h) -> do
          atomically $ do
            modifyTVar allHandlers (h :)
            void $ tryPutTMVar handlerVar h
          -- This source callback is after fork/registration and before map
          -- publication. Its exception makes modifyTMVar restore the old map.
          when (progress == Rollback) $ throwIO BufferFailure
        _ -> pure ()
  supply <- atomically $ CS.newConnStateIdSupply Proxy
  rawInfo <- newInformationChannel
  let info = rawInfo {
        writeMessage = \message -> do
          writeMessage rawInfo message
          modifyTVar notifiedCount (+1),
        readMessages = do
          -- Let the governor's final drain finish before publication. After
          -- publication starts, require a full queue before Server drains it.
          publishing <- not . isNothing <$> tryReadTMVar publishGate
          when (queuePressure && publishing) $
            readTVar notifiedCount >>= check . (>= 100)
          readMessages rawInfo
      }
  let withCM selectedHandler = CM.with CM.Arguments {
        CM.tracer = cmTracer, CM.trTracer = transitions,
        CM.ipv4Address = Nothing, CM.ipv6Address = Nothing,
        CM.addressType = const (Just IPv4Address), CM.snocket = transport,
        CM.makeBearer = MakeBearer $ \_ _ _ -> fail "unexpected bearer",
        CM.withBuffer = \_ -> record BufferFailed >> throwIO BufferFailure,
        CM.configureSocket = \_ _ -> fail "unexpected configuration",
        CM.connectionDataFlow = id, CM.prunePolicy = \_ _ _ -> Set.empty,
        CM.stdGen = mkStdGen 0,
        CM.connectionsLimits = AcceptedConnectionsLimit 200 200 0,
        CM.timeWaitTimeout = 1, CM.outboundIdleTimeout = 1,
        CM.updateVersionData = \a _ -> a, CM.connStateIdSupply = supply,
        CM.classifyHandlerError = const HandshakeFailure
      } (InResponderMode info) selectedHandler
      withOwner callback
        | useServer = Server.with Server.Arguments {
            Server.sockets = local :| [], Server.snocket = transport,
            Server.tracer = mkTracer $ \e -> case e of
              Server.TrServerStopped -> say "Server stopped" >> atomically (putTMVar stoppedGate ())
              _ -> pure (),
            Server.connectionLimits = AcceptedConnectionsLimit 200 200 0,
            Server.inboundGovernorArgs = IG.Arguments {
              IG.transitionTracer = nullTracer, IG.tracer = mkTracer $ \e -> case e of
                IG.TrNewConnection {} -> atomically $ putTMVar announced ()
                _ -> pure (),
              IG.debugTracer = nullTracer, IG.connectionDataFlow = id,
              IG.infoChannel = info, IG.idleTimeout = Nothing,
              IG.withConnectionManager = withCM,
              IG.mkConnectionHandler = const handler
            }
          } (\ig _ cm -> atomically (putTMVar governor ig) >> callback cm)
        | otherwise = withCM handler callback
      owner = withOwner $ \cm -> if progress == Rollback then do
        r <- withAsync (includeInboundConnection cm 10 remote key) waitCatch
        h <- atomically $ readTMVar handlerVar
        empty <- atomically $ Map.null . CS.getConnMap <$> readState cm
        pending <- isNothing <$> atomically (pollSTM h)
        let failed = case r of
              Left err -> fromException err == Just BufferFailure
              Right _ -> False
        record PublicationRolledBack
        pure (failed && empty && pending, False, empty, 0, "pending")
       else if queuePressure then atomically retry
       else if failBeforePublication then do
        h <- atomically $ readTMVar handlerVar
        result <- waitCatch h
        empty <- atomically $ Map.null . CS.getConnMap <$> readState cm
        pure (False, False, empty, 1, resultName (Just result))
       else do
        connected <- if useServer
          then atomically (takeTMVar announced) >> threadDelay 1 >> pure True
          else do
            r <- withAsync (includeInboundConnection cm 10 remote key) wait
            pure $ case r of
              Connected actual _ _ -> actual == key
              _ -> False
        record AdmissionObserved
        state <- atomically $ CS.getConnMap <$> readState cm
        let expected = Map.singleton remote (Map.singleton (CS.LocalAddr local) (InboundIdleSt Duplex))
        h <- atomically $ readTMVar handlerVar
        pending <- isNothing <$> atomically (pollSTM h)
        atomically $ putTMVar failGate ()
        masked <- atomically $ takeTMVar tailEntered
        -- Same observation delay in both cases. No join/cancel forces the control
        -- to finish: its tracer returns immediately, so its Async can complete.
        threadDelay 1
        empty <- atomically $ Map.null . CS.getConnMap <$> readState cm
        nclose <- readTVarIO closes
        before <- resultName <$> atomically (pollSTM h)
        pure (connected && state == expected && pending, masked, empty, nclose, before)
  (waited, (ok, masked, empty, nclose, before)) <- withAsync owner $ \scope -> do
    -- Ensure admissions publish only after the consumer has exited.
    when queuePressure $ do
      atomically $ readTVar allHandlers >>= check . (== 101) . length
      atomically (readTMVar governor) >>= cancel
      atomically $ takeTMVar stoppedGate
      atomically $ putTMVar publishGate ()
    atomically $ takeTMVar shutdownEntered
    -- A separate observer releases the callback only after checking that CM
    -- cannot return while the map is empty and its historical result is absent.
    threadDelay 1
    pendingScope <- isNothing <$> atomically (pollSTM scope)
    when (pauseCleanup && releaseCleanup) $ atomically $ putTMVar tailRelease ()
    report <- if queuePressure
      then do
        outcome <- waitCatch scope
        count <- readTVarIO notifiedCount
        handlers <- readTVarIO allHandlers
        (notifications, results) <- atomically $
          (,) <$> readMessages rawInfo <*> traverse pollSTM handlers
        let governorCancelled = case outcome of
              Left err -> fromException err == Just AsyncCancelled
              Right _  -> False
        unless (governorCancelled && count == 101 && null notifications
                && all (not . isNothing) results) $
          fail ("Incomplete shutdown after governor exit: " ++ show (outcome, count, length notifications))
        pure (True, False, True, 0, "pending")
      else wait scope
    pure (pendingScope == pauseCleanup, report)
  record CMReturned
  h <- atomically $ readTMVar handlerVar
  after <- resultName <$> atomically (pollSTM h)
  final <- resultName . Just <$> waitCatch h
  record HandlerJoined
  evs <- readTVarIO logVar
  actualMask <- if progress == Rollback then atomically (readTMVar tailEntered) else pure masked
  actualCloses <- readTVarIO closes
  pure (Report (waited && actualCloses == 1) ok
        (if progress == Rollback then actualMask else masked) empty nclose before after final evs)

-- Strict IOSim shutdown rejects any remaining physical thread.
prop_replay :: Bool -> Bool -> Property
prop_replay server paused =
  let observed = traceResult True (runSimTrace (timeout 10 (experiment server paused Recover)))
      readiness = if paused then "pending" else "BufferFailure"
      expected = Report True True True True 1 readiness "BufferFailure" "BufferFailure"
        [AdmissionObserved, BufferFailed, Cleanup, SocketClosed,
         CleanupCounters, Shutdown, CMReturned, HandlerJoined]
  in counterexample (show observed) $ case observed of
       Right r -> r === Just expected
       Left _  -> property False

tests :: TestTree
tests = testGroup "QuiescentShutdown"
  [ testProperty "paused cleanup: CM waits for historical result" (once (prop_replay False True))
  , testProperty "no-pause control: completed result" (once (prop_replay False False))
  , testProperty "Server self-purge: empty map still waits" (once (prop_replay True True))
  , testProperty "Server self-purge control: completed result" (once (prop_replay True False))
  , testProperty "permanently blocked cleanup prevents CM return" (once (prop_blocked False))
  , testProperty "permanently blocked cleanup prevents Server return" (once (prop_blocked True))
  , testProperty "admission joins after handler exits without publishing" (once prop_unpublished)
  , testProperty "admissions join with a full queue after IG exits" (once prop_queue)
  , testProperty "pre-publication rollback retains physical handler" (once prop_rollback)
  ]

-- These are failed progress scenarios, not quiescent shutdown results. IOSim
-- detects the permanently closed gates; a wall-clock watchdog is not recovery.
prop_blocked :: Bool -> Property
prop_blocked server =
  let tr = runSimTrace (experiment server True BlockCleanup)
  in case traceResult False tr of
       Left err -> counterexample (show err) $
         "Deadlock" `isInfixOf` show err .&&.
         ("CM shutdown" `elem` selectTraceEventsSay' tr)
       Right r -> counterexample (show r) False

prop_unpublished :: Property
prop_unpublished =
  case traceResult True (runSimTrace (timeout 10 (experiment True False Unpublished))) of
    Right (Just report) -> counterexample (show report) $
      emptyBeforeReturn report
      && resultBeforeReturn report == "BufferFailure"
      && resultAfterReturn report == "BufferFailure"
    observed -> counterexample (show observed) False

prop_queue :: Property
prop_queue =
  case traceResult True (runSimTrace (timeout 10 (experiment True False FullQueue))) of
    Right (Just _) -> property True
    observed -> counterexample (show observed) False

prop_rollback :: Property
prop_rollback =
  let tr = runSimTrace (experiment False False Rollback)
      expected = Report True True True True 0 "pending" "AsyncCancelled" "AsyncCancelled"
        [PublicationRolledBack, Shutdown, Cleanup, SocketClosed,
         CleanupCounters, CMReturned, HandlerJoined]
  in case traceResult True tr of
       Left err -> counterexample (show err) False
       Right r -> r === expected
