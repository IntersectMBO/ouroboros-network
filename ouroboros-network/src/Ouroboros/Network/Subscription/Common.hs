{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Subscription.Common
    ( subscribeTo
    , sockAddrFamily
    , minConnectionAttemptDelay
    , ipSubscriptionWorker
    , subscriptionWorker
    , SubscriberError (..)
    , SubscriptionTrace
    , IPSubscriptionTarget (..)
    ) where

import           Control.Concurrent hiding (threadDelay)
import           Control.Concurrent.Async

import           Control.Monad (forever, unless, when)
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Tracer
import           Data.List (delete)
import           GHC.Stack
import qualified Network.Socket as Socket
import           Text.Printf

import           Ouroboros.Network.Socket
import           Ouroboros.Network.Subscription.Subscriber


-- | Time to wait between connection attempts when we don't have any deltaQ info.
defaultConnectionAttemptDelay :: DiffTime
defaultConnectionAttemptDelay = 0.250 -- 250ms delay

-- | Minimum time to wait between connection attempts.
minConnectionAttemptDelay :: DiffTime
minConnectionAttemptDelay = 0.010 -- 10ms delay

-- | Maximum time to wait between connection attempts.
maxConnectionAttemptDelay :: DiffTime
maxConnectionAttemptDelay = 2 -- 2s delay

-- | Minimum time to wait between ip reconnects
ipRetryDelay :: DiffTime
ipRetryDelay = 10 -- 10s delay

sockAddrFamily
    :: Socket.SockAddr
    -> Socket.Family
sockAddrFamily (Socket.SockAddrInet  _ _    ) = Socket.AF_INET
sockAddrFamily (Socket.SockAddrInet6 _ _ _ _) = Socket.AF_INET6
sockAddrFamily _                              = Socket.AF_UNSPEC

data ConnectResult = ConnectSuccess
                   | ConnectSuccessLast
                   | ConnectFail
                   deriving (Eq, Ord, Show)

subscribeTo
    :: HasCallStack
    => ConnectionTable
    -> Tracer IO SubscriptionTrace
    -> Socket.PortNumber
    -> (Socket.SockAddr -> Maybe DiffTime)
    -> TVar IO Int
    -> (Socket.Socket -> IO ())
    -> SubscriptionTarget IO Socket.SockAddr
    -> IO ()
subscribeTo tbl tracer localPort connectionAttemptDelay valencyVar k ts = do
     x <- newTVarM []
     doSubscribeTo x ts

  where
    doSubscribeTo :: TVar IO [ThreadId] -> SubscriptionTarget IO Socket.SockAddr -> IO ()
    doSubscribeTo conThreads targets = do
        target_m <- getSubscriptionTarget targets
        case target_m of
            Just (target, nextTargets) -> do
                ref <- refConnection tbl target valencyVar
                case ref of
                     ConnectionTableCreate -> do
                        caid <- async $ doConnect conThreads target
                        atomically $ modifyTVar' conThreads (\t -> asyncThreadId caid:t)

                        {- The time to wait depends on available deltaQ information for the
                        -- destination address and max/min values from RFC8305.
                        -}
                        let delay = case connectionAttemptDelay target of
                                         Just d  -> min maxConnectionAttemptDelay $
                                                        max minConnectionAttemptDelay d
                                         Nothing -> defaultConnectionAttemptDelay
                        traceWith tracer $ SubscriptionTraceSubscriptionWaitingNewConnection delay
                        threadDelay delay
                        doSubscribeTo conThreads nextTargets
                     ConnectionTableExist -> do
                         {- We already have a connection to this address. -}
                         traceWith tracer $ SubscriptionTraceConnectionExist target
                         doSubscribeTo conThreads nextTargets
                     ConnectionTableDone -> do
                        traceWith tracer SubscriptionTraceSubscriptionRunning
                        return ()
            Nothing -> do
                len <- fmap length $ atomically $ readTVar conThreads
                when (len > 0) $
                    traceWith tracer $ SubscriptionTraceSubscriptionWaiting len

                -- We wait on the list of active connection threads instead of using an async wait
                -- function since some of the connections may succed and then should be left
                -- running.
                atomically $ do
                    activeCons  <- readTVar conThreads
                    unless (null activeCons) retry

                valencyLeft <- atomically $ readTVar valencyVar
                if valencyLeft == 0
                   then traceWith tracer SubscriptionTraceSubscriptionRunning
                   else traceWith tracer SubscriptionTraceSubscriptionFailed


    doConnect conThreads remoteAddr =
        bracket
            ( do
                sd <- Socket.socket (sockAddrFamily remoteAddr) Socket.Stream
                        Socket.defaultProtocol
                hasRefVar <- newTVarM False
                return (sd, hasRefVar)
            )
            (\(sd,hasRefVar) -> do
                traceWith tracer $ SubscriptionTraceConnectCleanup remoteAddr

                tid <- myThreadId
                atomically $ modifyTVar' conThreads (delete tid)
                hasRef <- atomically $ readTVar hasRefVar
                when hasRef $ do
                    localAddr' <- Socket.getSocketName sd
                    removeConnection tbl remoteAddr localAddr'
                    --atomically $ modifyTVar' valencyVar (+ 1)

                Socket.close sd

            )
            (\(sd,hasRefVar)  -> do
                Socket.setSocketOption sd Socket.ReuseAddr 1
#if !defined(mingw32_HOST_OS)
                Socket.setSocketOption sd Socket.ReusePort 1
#endif
                let localAddr = case sockAddrFamily remoteAddr of
                                     Socket.AF_INET6 -> Socket.SockAddrInet6 localPort 0 (0,0,0,0) 0
                                     _  -> Socket.SockAddrInet localPort 0
                Socket.bind sd localAddr
                tid <- myThreadId
                traceWith tracer $ SubscriptionTraceConnectStart remoteAddr
                res <- try $ Socket.connect sd remoteAddr
                case res of
                        Left (e :: SomeException) -> do
                            traceWith tracer $ SubscriptionTraceConnectException remoteAddr e
                            throwM e
                        Right _ -> return ()

                localAddr' <- Socket.getSocketName sd

                -- We successfully connected, increase valency and start the app
                result <- atomically $ do
                        v <- readTVar valencyVar
                        if v > 0 then do
                                    --modifyTVar' valencyVar (\a -> a - 1)
                                    addConnection tbl remoteAddr localAddr' [valencyVar]
                                    writeTVar hasRefVar True
                                    modifyTVar' conThreads (delete tid)
                                    if v == 1 then return ConnectSuccessLast
                                              else return ConnectSuccess
                                else return ConnectFail
                traceWith tracer $ SubscriptionTraceConnectEnd remoteAddr result
                case result of
                    ConnectSuccess -> k sd
                    ConnectSuccessLast -> do
                            outstandingConThreads <- atomically $ readTVar conThreads
                            mapM_ (\a -> throwTo a
                                   (SubscriberError SubscriberParrallelConnectionCancelled
                                    "Parrallel connection cancelled"
                                    callStack)) outstandingConThreads
                            k sd
                    ConnectFail -> return ()

            )

data IPSubscriptionTarget = IPSubscriptionTarget {
      ispIps     :: ![Socket.SockAddr]
    , ispValency :: !Int
    } deriving (Eq, Show)

ipSubscriptionWorker
    :: ConnectionTable
    -> Tracer IO (WithIPList SubscriptionTrace)
    -> Socket.PortNumber
    -> (Socket.SockAddr -> Maybe DiffTime)
    -> IPSubscriptionTarget
    -> (Socket.Socket -> IO ())
    -> IO ()
ipSubscriptionWorker tbl tracer localPort connectionAttemptDelay ips =
    subscriptionWorker tbl (ipListTracer localPort (ispIps ips) tracer) localPort
            connectionAttemptDelay
            (return $ listSubscriptionTarget (ispIps ips)) (ispValency ips)

subscriptionWorker
    :: ConnectionTable
    -> Tracer IO SubscriptionTrace
    -> Socket.PortNumber
    -> (Socket.SockAddr -> Maybe DiffTime)
    -> IO (SubscriptionTarget IO Socket.SockAddr)
    -> Int
    -> (Socket.Socket -> IO ())
    -> IO ()
subscriptionWorker tbl tracer localPort connectionAttemptDelay getTargets valency
        k = do
    valencyVar <- newTVarM valency
    traceWith tracer $ SubscriptionTraceStart valency
    forever $ do
        start <- getMonotonicTime
        targets <- getTargets
        subscribeTo tbl tracer localPort connectionAttemptDelay valencyVar k
                targets
        atomically $ do
            v <- readTVar valencyVar
            when (v <= 0)
                retry

        {-
         - We allways wait at least ipRetryDelay seconds between calls to getTargets, and
         - before trying to restart the subscriptions we also wait 1 second so that if multiple
         - subscription targets fail around the same time we will try to restart with a valency
         - higher than 1.
         -}
        threadDelay 1
        end <- getMonotonicTime
        let duration = diffTime end start
        currentValency <- atomically $ readTVar valencyVar
        traceWith tracer $ SubscriptionTraceRestart duration currentValency
            (valency - currentValency)

        when (duration < ipRetryDelay) $
            threadDelay $ ipRetryDelay - duration

data WithIPList a = WithIPList {
      wilSrc    :: !Socket.PortNumber
    , wilDsts   :: ![Socket.SockAddr]
    , wilEvent :: !a
    }

instance (Show a) => Show (WithIPList a) where
    show WithIPList {..} = printf  "IPs: %s %s %s" (show wilSrc) (show wilDsts) (show wilEvent)

ipListTracer :: Socket.PortNumber -> [Socket.SockAddr] -> Tracer IO (WithIPList a) -> Tracer IO a
ipListTracer src ips tr = Tracer $ \s -> traceWith tr $ WithIPList src ips s

data SubscriberError = SubscriberError {
      seType    :: !SubscriberErrorType
    , seMessage :: !String
    , seStack   :: !CallStack
    } deriving Show

-- | Enumeration of error conditions.
--
data SubscriberErrorType = SubscriberParrallelConnectionCancelled
                         deriving (Eq, Show)

instance Exception SubscriberError where
    displayException SubscriberError{..} = printf "%s %s at %s"
         (show seType)
         (show seMessage)
         (prettyCallStack seStack)

data SubscriptionTrace =
      SubscriptionTraceConnectStart Socket.SockAddr
    | SubscriptionTraceConnectEnd Socket.SockAddr ConnectResult
    | SubscriptionTraceConnectException Socket.SockAddr SomeException
    | SubscriptionTraceConnectCleanup Socket.SockAddr
    | SubscriptionTraceSubscriptionRunning
    | SubscriptionTraceSubscriptionWaiting Int
    | SubscriptionTraceSubscriptionFailed
    | SubscriptionTraceSubscriptionWaitingNewConnection DiffTime
    | SubscriptionTraceStart Int
    | SubscriptionTraceRestart DiffTime Int Int
    | SubscriptionTraceConnectionExist Socket.SockAddr

instance Show SubscriptionTrace where
    show (SubscriptionTraceConnectStart dst) =
        "Connection Attempt Start, destination " ++ show dst
    show (SubscriptionTraceConnectEnd dst res) =
        "Connection Attemt End, destination " ++ show dst ++ " outcome: " ++ show res
    show (SubscriptionTraceConnectException dst e) =
        "Connection Attemt Exception, destination " ++ show dst ++ " exception: " ++ show e
    show (SubscriptionTraceConnectCleanup dst) =
        "Connection Cleanup, destination " ++ show dst
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


