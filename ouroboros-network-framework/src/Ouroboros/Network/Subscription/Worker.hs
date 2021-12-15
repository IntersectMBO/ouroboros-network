{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

module Ouroboros.Network.Subscription.Worker
  ( SocketStateChange
  , SocketState (..)
  , CompleteApplication
  , ConnectResult (..)
  , Result (..)
  , Main
  , StateVar
  , LocalAddresses (..)
    -- * Subscription worker
  , WorkerCallbacks (..)
  , WorkerParams (..)
  , worker
    -- * Socket API
  , safeConnect
    -- * Constants
  , defaultConnectionAttemptDelay
  , minConnectionAttemptDelay
  , maxConnectionAttemptDelay
  , ipRetryDelay
    -- * Errors
  , SubscriberError (..)
    -- * Tracing
  , SubscriptionTrace (..)
  ) where

import           Control.Applicative ((<|>))
import qualified Control.Concurrent.STM as STM
import           Control.Exception (SomeException (..))
import           Control.Monad (forever, join, unless, when)
import           Control.Monad.Fix (MonadFix)
import           Data.Foldable (traverse_)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Void (Void)
import           GHC.Stack
import           Network.Socket (Family (AF_UNIX))
import           Text.Printf

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Tracer

import           Ouroboros.Network.ErrorPolicy (CompleteApplication,
                     CompleteApplicationResult (..), ErrorPolicyTrace,
                     Result (..), WithAddr)
import           Ouroboros.Network.Server.ConnectionTable
import           Ouroboros.Network.Snocket (Snocket (..))
import qualified Ouroboros.Network.Snocket as Snocket
import           Ouroboros.Network.Subscription.Subscriber

-- | Time to wait between connection attempts when we don't have any DeltaQ
-- info.
--
defaultConnectionAttemptDelay :: DiffTime
defaultConnectionAttemptDelay = 0.025 -- 25ms delay

-- | Minimum time to wait between connection attempts.
--
minConnectionAttemptDelay :: DiffTime
minConnectionAttemptDelay = 0.010 -- 10ms delay

-- | Maximum time to wait between connection attempts.
--
maxConnectionAttemptDelay :: DiffTime
maxConnectionAttemptDelay = 2 -- 2s delay

-- | Minimum time to wait between ip reconnects
--
ipRetryDelay :: DiffTime
ipRetryDelay = 10 -- 10s delay

data ResOrAct m addr tr r =
     Res !(Result addr r)
   | Act (Set (Async m ())) -- ^ threads to kill
         (Maybe tr)         -- ^ trace point

-- | Result queue.  The spawned threads will keep writing to it, while the main
-- server will read from it.
--
type ResultQ m addr tr r = TQueue m (ResOrAct m addr tr r)

newResultQ :: forall m addr tr r. MonadSTM m => m (ResultQ m addr tr r)
newResultQ = atomically $ newTQueue

-- | Mutable state kept by the worker.  All the workers in this module are
-- polymorphic over the state type.  The state is updated with two callbacks:
--
-- * 'CompleteConnect'     - STM transaction which runs when the connect call
--                           returned, if it thrown an exception it will be
--                           passed to the callback.
-- * 'CompleteApplication' - STM transaction which runs when application
--                           returned.  It will receive the result of the
--                           application or an exception raised by it.
--
type StateVar m s = StrictTVar m s

-- | The set of all spawned threads. Used for waiting or cancelling them when
-- the server shuts down.
--
type ThreadsVar m = StrictTVar m (Set (Async m ()))


data SocketState m addr
   = CreatedSocket !addr !(Async m ())
   | ClosedSocket  !addr !(Async m ())

-- | Callback which fires: when we create or close a socket.
--
type SocketStateChange m s addr = SocketState m addr -> s -> STM m s

-- | Given current state 'retry' too keep the subscription worker going.
-- When this transaction returns, all the threads spawned by the worker will be
-- killed.
--
type Main m s t = s -> STM m t

data LocalAddresses addr = LocalAddresses {
    -- | Local IPv4 address to use, Nothing indicates don't use IPv4
    laIpv4 :: Maybe addr
    -- | Local IPv6 address to use, Nothing indicates don't use IPv6
  , laIpv6 :: Maybe addr
    -- | Local Unix address to use, Nothing indicates don't use Unix sockets
  , laUnix :: Maybe addr
  } deriving (Eq, Show)

instance Semigroup (LocalAddresses addr) where
    a <> b = LocalAddresses {
        laIpv4 = laIpv4 a <|> laIpv4 b,
        laIpv6 = laIpv6 a <|> laIpv6 b,
        laUnix = laUnix a <|> laUnix b
      }


-- | Allocate a socket and connect to a peer, execute the continuation with
-- async exceptions masked.  The continuation receives the 'unmask' callback.
--
safeConnect :: ( MonadThrow m
               , MonadMask m
               )
            => Snocket m sock addr
            -> addr
            -- ^ remote addr
            -> addr
            -- ^ local addr
            -> m ()
            -- ^ allocate extra action; executed with async exceptions masked in
            -- the allocation action of 'bracket'
            -> m ()
            -- ^ release extra action; executed with async exceptions masked in
            -- the closing action of 'bracket'
            -> ((forall x. m x -> m x) -> sock -> Either SomeException () -> m t)
            -- ^ continuation executed with async exceptions
            -- masked; it receives: unmask function, allocated socket and
            -- connection error.
            -> m t
safeConnect sn remoteAddr localAddr malloc mclean k =
    bracket
      (do sock <- Snocket.open sn (Snocket.addrFamily sn remoteAddr)
          malloc
          pure sock
      )
      (\sock -> Snocket.close sn sock >> mclean)
      (\sock -> mask $ \unmask -> do
          let doBind = case Snocket.addrFamily sn localAddr of
                            Snocket.SocketFamily fam -> fam /= AF_UNIX
                            _                        -> False -- Bind is a nop for Named Pipes anyway
          when doBind $
            Snocket.bind sn sock localAddr
          res :: Either SomeException ()
              <- try (unmask $ Snocket.connect sn sock remoteAddr)
          k unmask sock res)


--
-- Internal API
--


-- | GADT which classifies connection result.
--
data ConnectResult =
      ConnectSuccess
    -- ^ Successful connection.
    | ConnectSuccessLast
    -- ^ Successfully connection, reached the valency target.  Other ongoing
    -- connection attempts will be killed.
    | ConnectValencyExceeded
    -- ^ Someone else manged to create the final connection to a target before
    -- us.
    deriving (Eq, Ord, Show)

-- | Traverse 'SubscriptionTarget's in an infinite loop.
--
subscriptionLoop
    :: forall m s sock localAddrs addr a x.
       ( MonadAsync m
       , MonadMask  m
       , MonadTime  m
       , MonadTimer m
       , MonadFix   m
       , Ord (Async m ())
       , Ord addr
       )
    => Tracer              m (SubscriptionTrace addr)

    -- various state variables of the subscription loop
    -> ConnectionTable     m   addr
    -> ResultQ             m   addr (WithAddr addr ErrorPolicyTrace) a
    -> StateVar            m s
    -> ThreadsVar          m

    -> Snocket             m sock addr

    -> WorkerCallbacks m s addr a x
    -> WorkerParams m localAddrs addr
    -- ^ given a remote address, pick the local one
    -> (sock -> m a)
    -- ^ application
    -> m Void
subscriptionLoop
      tr tbl resQ sVar threadsVar snocket
      WorkerCallbacks { wcSocketStateChangeTx   = socketStateChangeTx
                      , wcCompleteApplicationTx = completeApplicationTx
                      }
      WorkerParams { wpLocalAddresses         = localAddresses
                   , wpConnectionAttemptDelay = connectionAttemptDelay
                   , wpSubscriptionTarget     = subscriptionTargets
                   , wpValency                = valency
                   , wpSelectAddress
                   }
      k = do
    valencyVar <- atomically $ newValencyCounter tbl valency

    -- outer loop: set new 'conThread' variable, get targets and traverse
    -- through them trying to connect to each addr.
    forever $ do
      traceWith tr (SubscriptionTraceStart valency)
      start <- getMonotonicTime
      conThreads <- atomically $ newTVar Set.empty
      sTarget <- subscriptionTargets
      innerLoop conThreads valencyVar sTarget
      atomically $ waitValencyCounter valencyVar

      -- We always wait at least 'ipRetryDelay' seconds between calls to
      -- 'getTargets', and before trying to restart the subscriptions we also
      -- wait 1 second so that if multiple subscription targets fail around the
      -- same time we will try to restart with a valency
      -- higher than 1.
      threadDelay 1
      end <- getMonotonicTime
      let duration = diffTime end start
      currentValency <- atomically $ readValencyCounter valencyVar
      traceWith tr $ SubscriptionTraceRestart duration valency
          (valency - currentValency)

      when (duration < ipRetryDelay) $
          threadDelay $ ipRetryDelay - duration

  where
    -- a single run through @sTarget :: SubcriptionTarget m addr@.
    innerLoop :: StrictTVar m (Set (Async m ()))
              -> ValencyCounter m
              -> SubscriptionTarget m addr
              -> m ()
    innerLoop conThreads valencyVar sTarget = do
      mt <- getSubscriptionTarget sTarget
      case mt of
        Nothing -> do
          len <- fmap length $ atomically $ readTVar conThreads
          when (len > 0) $
              traceWith tr $ SubscriptionTraceSubscriptionWaiting len

          -- We wait on the list of active connection threads instead of using
          -- an async wait function since some of the connections may succeed
          -- and then should be left running.
          --
          -- Note: active connections are removed from 'conThreads' when the
          -- 'connect' call finishes.
          atomically $ do
              activeCons <- readTVar conThreads
              unless (null activeCons) retry

          valencyLeft <- atomically $ readValencyCounter valencyVar
          if valencyLeft <= 0
             then traceWith tr SubscriptionTraceSubscriptionRunning
             else traceWith tr SubscriptionTraceSubscriptionFailed

        Just (remoteAddr, sTargetNext) -> do
          valencyLeft <- atomically $ readValencyCounter valencyVar

          -- If we have already created enough connections (valencyLeft <= 0)
          -- we don't need to traverse the rest of the list.
          if valencyLeft <= 0
              then traceWith tr SubscriptionTraceSubscriptionRunning
              else innerStep conThreads valencyVar remoteAddr sTargetNext

    innerStep :: StrictTVar m (Set (Async m ()))
              -- ^ outstanding connection threads; threads are removed as soon
              -- as the connection succeeds.  They are all cancelled when
              -- valency drops to 0.  The asynchronous exception which cancels
              -- the connection thread can only occur while connecting and not
              -- when an application is running.  This is guaranteed since
              -- threads are removed from this set as soon connecting is
              -- finished (successfully or not) and before application is
              -- started.
              -> ValencyCounter m
              -> addr
              -> SubscriptionTarget m addr
              -> m ()
    innerStep conThreads valencyVar !remoteAddr sTargetNext = do
      r <- refConnection tbl remoteAddr valencyVar
      case r of
        ConnectionTableCreate ->
          case wpSelectAddress remoteAddr localAddresses of
            Nothing ->
              traceWith tr (SubscriptionTraceUnsupportedRemoteAddr remoteAddr)

            -- This part is very similar to
            -- 'Ouroboros.Network.Server.Socket.spawnOne', it should not
            -- deadlock by the same reasons.  The difference is that we are
            -- using 'mask' and 'async' as 'asyncWithUnmask' is not available.
            Just localAddr ->
             do rec
                  thread <- async $ do
                    traceWith tr $ SubscriptionTraceConnectStart remoteAddr
                    -- Try to connect; 'safeConnect' is using 'bracket' to
                    -- create / close a socket and update the states.  The
                    -- continuation, e.g.  'connAction' runs with async
                    -- exceptions masked, and receives the unmask function from
                    -- this bracket.
                    safeConnect
                      snocket
                      remoteAddr
                      localAddr
                      (do
                        traceWith tr $ SubscriptionTraceAllocateSocket remoteAddr
                        atomically $ do
                          modifyTVar conThreads (Set.insert thread)
                          modifyTVar threadsVar (Set.insert thread)
                          readTVar sVar
                            >>= socketStateChangeTx (CreatedSocket remoteAddr thread)
                            >>= (writeTVar sVar $!))
                      (do
                        atomically $ do
                          -- The thread is removed from 'conThreads'
                          -- inside 'connAction'.
                          modifyTVar threadsVar (Set.delete thread)
                          readTVar sVar
                            >>= socketStateChangeTx (ClosedSocket remoteAddr thread)
                            >>= (writeTVar sVar $!)
                        traceWith tr $ SubscriptionTraceCloseSocket remoteAddr)
                      (connAction
                        thread conThreads valencyVar
                        remoteAddr)

                let delay = case connectionAttemptDelay remoteAddr of
                                Just d  -> d `max` minConnectionAttemptDelay
                                             `min` maxConnectionAttemptDelay
                                Nothing -> defaultConnectionAttemptDelay
                traceWith tr
                          (SubscriptionTraceSubscriptionWaitingNewConnection delay)
                threadDelay delay

        ConnectionTableExist ->
          traceWith tr $ SubscriptionTraceConnectionExist remoteAddr
        ConnectionTableDuplicate -> pure ()
      innerLoop conThreads valencyVar sTargetNext

    -- Start connection thread: connect to the remote peer, run application.
    -- This function runs with asynchronous exceptions masked.
    --
    connAction :: Async m ()
               -> StrictTVar m (Set (Async m ()))
               -> ValencyCounter m
               -> addr
               -> (forall y. m y -> m y) -- unmask exceptions
               -> sock
               -> Either SomeException ()
               -> m ()
    connAction thread conThreads valencyVar remoteAddr unmask sock connectionRes = do
      localAddr <- Snocket.getLocalAddr snocket sock
      t <- getMonotonicTime
      case connectionRes of
        -- connection error
        Left (SomeException e) -> do
          traceWith tr $ SubscriptionTraceConnectException remoteAddr e
          atomically $ do
            -- remove thread from active connections threads
            modifyTVar conThreads (Set.delete thread)

            CompleteApplicationResult
              { carState
              , carThreads
              , carTrace
              } <- readTVar sVar >>= completeApplicationTx (ConnectionError t remoteAddr e)
            writeTVar sVar carState
            writeTQueue resQ (Act carThreads carTrace)

        -- connection succeeded
        Right _ -> do
          connRes <- atomically $ do
            -- we successfully connected, remove the thread from
            -- outstanding connection threads.
            modifyTVar conThreads (Set.delete thread)

            v <- readValencyCounter valencyVar
            if v > 0
              then do
                addConnection tbl remoteAddr localAddr (Just valencyVar)
                CompleteApplicationResult
                  { carState
                  , carThreads
                  , carTrace
                  } <- readTVar sVar >>= completeApplicationTx (Connected t remoteAddr)
                writeTVar sVar carState
                writeTQueue resQ (Act carThreads carTrace)
                return $ if v == 1
                          then ConnectSuccessLast
                          else ConnectSuccess
              else
                return ConnectValencyExceeded

          -- handle connection result
          traceWith tr $ SubscriptionTraceConnectEnd remoteAddr connRes
          case connRes of
            ConnectValencyExceeded -> pure ()
            -- otherwise it was a success
            _           -> do
              when (connRes == ConnectSuccessLast) $ do
                -- outstanding connection threads
                threads <- atomically $ readTVar conThreads
                mapM_ (\tid ->
                        cancelWith tid
                        (SubscriberError
                          SubscriberParallelConnectionCancelled
                          "Parallel connection cancelled"
                          callStack)
                      )threads


              -- run application
              appRes :: Either SomeException a
                <- try $ unmask (k sock)

              case appRes of
                Right _ -> pure ()
                Left e -> traceWith tr $ SubscriptionTraceApplicationException remoteAddr e

              t' <- getMonotonicTime
              atomically $ do
                case appRes of
                  Right a ->
                    writeTQueue resQ (Res (ApplicationResult t' remoteAddr a))
                  Left (SomeException e) ->
                    writeTQueue resQ (Res (ApplicationError t' remoteAddr e))
                removeConnectionSTM tbl remoteAddr localAddr

-- | Almost the same as 'Ouroboros.Network.Server.Socket.mainLoop'.
-- 'mainLoop' reads from the result queue and runs the 'CompleteApplication'
-- callback.
--
mainLoop
  :: forall s r addr t.
     Tracer IO (WithAddr addr ErrorPolicyTrace)
  -> ResultQ IO addr (WithAddr addr ErrorPolicyTrace) r
  -> ThreadsVar IO
  -> StateVar IO s
  -> CompleteApplication IO s addr r
  -> Main IO s t
  -> IO t
mainLoop errorPolicyTracer resQ threadsVar statusVar completeApplicationTx main = do
    join (atomically $ mainTx `STM.orElse` connectionTx)
  where
    -- Sample the state, and run the main action. If it does not retry, then
    -- the `mainLoop` finishes with `pure t` where `t` is the main action result.
    mainTx :: STM IO (IO t)
    mainTx = do
      t <- readTVar statusVar >>= main
      pure $ pure t

    -- Wait for some connection to finish, update the state with its result,
    -- then recurse onto `mainLoop`.
    connectionTx :: STM IO (IO t)
    connectionTx = do
      result <- STM.readTQueue resQ
      case result of
        Act threads tr -> pure $ do
          traverse_ cancel threads
          traverse_ (traceWith errorPolicyTracer) tr
          mainLoop errorPolicyTracer resQ threadsVar statusVar completeApplicationTx main
        Res r -> do
          s <- readTVar statusVar
          CompleteApplicationResult
            { carState
            , carThreads
            , carTrace
            } <- completeApplicationTx r s
          writeTVar statusVar carState
          pure $ do
            traverse_ cancel carThreads
            traverse_ (traceWith errorPolicyTracer) carTrace
            mainLoop errorPolicyTracer resQ threadsVar statusVar completeApplicationTx main


--
-- Worker
--

-- | Worker STM callbacks
--
data WorkerCallbacks m s addr a t = WorkerCallbacks {
    wcSocketStateChangeTx   :: SocketStateChange m s addr,
    wcCompleteApplicationTx :: CompleteApplication m s addr a,
    wcMainTx                :: Main m s t
  }

-- | Worker parameters
--
data WorkerParams m localAddrs addr = WorkerParams {
    wpLocalAddresses         :: localAddrs addr,
    -- ^ local addresses of the server
    wpSelectAddress          :: addr -> localAddrs addr -> Maybe addr,
    -- ^ given remote addr pick the local address
    wpConnectionAttemptDelay :: addr -> Maybe DiffTime,
    -- ^ delay after a connection attempt to 'addr'
    wpSubscriptionTarget     :: m (SubscriptionTarget m addr),
    wpValency                :: Int
  }

-- |  This is the most abstract worker, which puts all the pieces together.  It
-- will execute until @main :: Main m s t@ returns.  It runs
-- 'subscriptionLoop' in a new threads and will exit when it dies.  Spawn
-- threads are cancelled in a 'finally' callback by throwing 'SubscriberError'.
--
-- Note: This function runs in 'IO' only because 'MonadSTM' does not yet support
-- 'orElse', PR #432.
--
worker
    :: forall s sock localAddrs addr a x.
       Ord addr
    => Tracer              IO (SubscriptionTrace addr)
    -> Tracer              IO (WithAddr addr ErrorPolicyTrace)
    -> ConnectionTable     IO   addr
    -> StateVar            IO s

    -> Snocket             IO sock addr

    -> WorkerCallbacks     IO s addr a x
    -> WorkerParams        IO   localAddrs addr

    -> (sock -> IO a)
    -- ^ application
    -> IO x
worker tr errTrace tbl sVar snocket workerCallbacks@WorkerCallbacks {wcCompleteApplicationTx, wcMainTx } workerParams k = do
    resQ <- newResultQ
    threadsVar <- atomically $ newTVar Set.empty
    withAsync
      (subscriptionLoop tr tbl resQ sVar threadsVar snocket
         workerCallbacks workerParams k) $ \_ ->
           mainLoop errTrace resQ threadsVar sVar wcCompleteApplicationTx wcMainTx
           `finally` killThreads threadsVar
  where
    killThreads threadsVar = do
      let e = SubscriberError
                SubscriberWorkerCancelled
                "SubscriptionWorker exiting"
                callStack
      children <- atomically $ readTVar threadsVar
      mapM_ (\a -> cancelWith a e) children


--
-- Auxiliary types: errors, traces
--

data SubscriberError = SubscriberError {
      seType    :: !SubscriberErrorType
    , seMessage :: !String
    , seStack   :: !CallStack
    } deriving Show

-- | Enumeration of error conditions.
--
data SubscriberErrorType = SubscriberParallelConnectionCancelled
                         | SubscriberWorkerCancelled
                         deriving (Eq, Show)

instance Exception SubscriberError where
    displayException SubscriberError{seType, seMessage, seStack}
      = printf "%s %s at %s"
         (show seType)
         (show seMessage)
         (prettyCallStack seStack)


data SubscriptionTrace addr =
      SubscriptionTraceConnectStart addr
    | SubscriptionTraceConnectEnd addr ConnectResult
    | forall e. Exception e => SubscriptionTraceSocketAllocationException addr e
    | forall e. Exception e => SubscriptionTraceConnectException addr e
    | forall e. Exception e => SubscriptionTraceApplicationException addr e
    | SubscriptionTraceTryConnectToPeer addr
    | SubscriptionTraceSkippingPeer addr
    | SubscriptionTraceSubscriptionRunning
    | SubscriptionTraceSubscriptionWaiting Int
    | SubscriptionTraceSubscriptionFailed
    | SubscriptionTraceSubscriptionWaitingNewConnection DiffTime
    | SubscriptionTraceStart Int
    | SubscriptionTraceRestart DiffTime Int Int
    | SubscriptionTraceConnectionExist addr
    | SubscriptionTraceUnsupportedRemoteAddr addr
    | SubscriptionTraceMissingLocalAddress
    | SubscriptionTraceAllocateSocket addr
    | SubscriptionTraceCloseSocket addr

instance Show addr => Show (SubscriptionTrace addr) where
    show (SubscriptionTraceConnectStart dst) =
        "Connection Attempt Start, destination " ++ show dst
    show (SubscriptionTraceConnectEnd dst res) =
        "Connection Attempt End, destination " ++ show dst ++ " outcome: " ++ show res
    show (SubscriptionTraceSocketAllocationException dst e) =
        "Socket Allocation Exception, destination " ++ show dst ++ " exception: " ++ show e
    show (SubscriptionTraceConnectException dst e) =
        "Connection Attempt Exception, destination " ++ show dst ++ " exception: " ++ show e
    show (SubscriptionTraceTryConnectToPeer addr) =
        "Trying to connect to " ++ show addr
    show (SubscriptionTraceSkippingPeer addr) =
        "Skipping peer " ++ show addr
    show SubscriptionTraceSubscriptionRunning =
        "Required subscriptions started"
    show (SubscriptionTraceSubscriptionWaiting d) =
        "Waiting on " ++ show d ++ " active connections"
    show SubscriptionTraceSubscriptionFailed =
        "Failed to start all required subscriptions"
    show (SubscriptionTraceSubscriptionWaitingNewConnection delay) =
        "Waiting " ++ show delay ++ " before attempting a new connection"
    show (SubscriptionTraceStart val) = "Starting Subscription Worker, valency " ++ show val
    show (SubscriptionTraceRestart duration desiredVal currentVal) =
        "Restarting Subscription after " ++ show duration ++ " desired valency " ++
        show desiredVal ++ " current valency " ++ show currentVal
    show (SubscriptionTraceConnectionExist dst) =
        "Connection Existed to " ++ show dst
    show (SubscriptionTraceUnsupportedRemoteAddr dst) =
        "Unsupported remote target address " ++ show dst
    -- TODO: add address family
    show SubscriptionTraceMissingLocalAddress =
        "Missing local address"
    show (SubscriptionTraceApplicationException addr e) =
        "Application Exception: " ++ show addr ++ " " ++ show e
    show (SubscriptionTraceAllocateSocket addr) =
        "Allocate socket to " ++ show addr
    show (SubscriptionTraceCloseSocket addr) =
        "Closed socket to " ++ show addr
