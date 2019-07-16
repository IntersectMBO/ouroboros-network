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


{- The parallel connection attemps implemented in this module is inspired by
 - RFC8305, https://tools.ietf.org/html/rfc8305 .
 -}

import           Control.Concurrent hiding (threadDelay)
import           Control.Concurrent.Async

import           Control.Monad (forever, unless, when)
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Tracer
import Data.Set (Set)
import qualified Data.Set as Set
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

data ConnectResult =
      ConnectSuccess
    -- ^ We successfully created a connection.
    | ConnectSuccessLast
    -- ^ We succesfully created a connection, and reached the valency target. Other ongoing
    -- connection attempts should be killed.
    | ConnectFail
    -- ^ Someone else manged to create the final connection to the target before us.
    deriving (Eq, Ord, Show)

subscribeTo
    :: HasCallStack
    => ConnectionTable IO
    -> TVar IO (Set ThreadId)
    -> Tracer IO SubscriptionTrace
    -> Maybe Socket.SockAddr
    -- ^ Local IPv4 address to use, Nothing indicates don't use IPv4
    -> Maybe Socket.SockAddr
    -- ^ Local IPv6 address to use, Nothing indicates don't use IPv6
    -> (Socket.SockAddr -> Maybe DiffTime)
    -- ^ Lookup function, should return expected delay for the given address
    -> ValencyCounter IO
    -- ^ Tracks the number of connections left to create
    -> SubscriptionTarget IO Socket.SockAddr
    -> (Socket.Socket -> IO ())
    -> IO ()
subscribeTo tbl threadPool tracer localIPv4_m localIPv6_m connectionAttemptDelay
  valencyVar ts k = do
     x <- newTVarM Set.empty
     doSubscribeTo x ts

  where
    getLocalAddress :: Socket.SockAddr -> IO (Maybe Socket.SockAddr)
    getLocalAddress (Socket.SockAddrInet _ _) =
        case localIPv4_m of
             Nothing -> do
                 traceWith tracer $ SubscriptionTraceMissingLocalAddress Socket.AF_INET
                 return Nothing
             a  -> return a
    getLocalAddress (Socket.SockAddrInet6 _ _ _ _) =
         case localIPv6_m of
             Nothing -> do
                 traceWith tracer $ SubscriptionTraceMissingLocalAddress Socket.AF_INET6
                 return Nothing
             a  -> return a

    getLocalAddress a = do
        traceWith tracer $ SubscriptionTraceUnsupportedRemoteAddr a
        return Nothing

    doSubscribeTo :: TVar IO (Set ThreadId) -> SubscriptionTarget IO Socket.SockAddr -> IO ()
    doSubscribeTo conThreads targets = do
        atomically $ waitValencyCounter valencyVar
        target_m <- getSubscriptionTarget targets
        case target_m of
            Just (target, nextTargets) -> do
                ref <- refConnection tbl target valencyVar
                case ref of
                     ConnectionTableCreate -> do
                        localAddr_m <- getLocalAddress target
                        case localAddr_m of
                             Nothing -> -- No usable local address continue with next
                                        doSubscribeTo conThreads nextTargets
                             Just localAddr -> do
                                 -- TODO use asyncWith instead?
                                 _ <- async (bracket
                                         (allocateSocket conThreads target)
                                         (closeSocket conThreads target)
                                         (doConnect conThreads localAddr target))

                                 {- The time to wait depends on available deltaQ information for the
                                 -- destination address and max/min values from RFC8305.
                                 -}
                                 let delay = case connectionAttemptDelay target of
                                                  Just d  -> min maxConnectionAttemptDelay $
                                                                 max minConnectionAttemptDelay d
                                                  Nothing -> defaultConnectionAttemptDelay
                                 traceWith tracer $
                                     SubscriptionTraceSubscriptionWaitingNewConnection delay
                                 threadDelay delay
                                 doSubscribeTo conThreads nextTargets
                     ConnectionTableExist -> do
                         -- We already have a connection to this address.
                         traceWith tracer $ SubscriptionTraceConnectionExist target
                         doSubscribeTo conThreads nextTargets
                     ConnectionTableDuplicate -> do
                         -- This subscription worker has a connection to this address
                         doSubscribeTo conThreads nextTargets

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

                valencyLeft <- atomically $ readValencyCounter valencyVar
                if valencyLeft == 0
                   then traceWith tracer SubscriptionTraceSubscriptionRunning
                   else traceWith tracer SubscriptionTraceSubscriptionFailed

    doConnect
        :: TVar IO (Set ThreadId)
        -> Socket.SockAddr
        -> Socket.SockAddr
        -> (Socket.Socket, TVar IO Bool)
        -> IO ()
    doConnect conThreads localAddr remoteAddr (sd,hasRefVar) = do
                Socket.setSocketOption sd Socket.ReuseAddr 1
#if !defined(mingw32_HOST_OS)
                Socket.setSocketOption sd Socket.ReusePort 1
#endif
                Socket.bind sd localAddr
                traceWith tracer $ SubscriptionTraceConnectStart remoteAddr
                res <- try $ Socket.connect sd remoteAddr
                case res of
                        Left (e :: SomeException) -> do
                            traceWith tracer $ SubscriptionTraceConnectException remoteAddr e
                            throwM e
                        Right _ -> return ()

                localAddr' <- Socket.getSocketName sd

                -- We successfully connected, increase valency and start the app
                tid <- myThreadId
                result <- atomically $ do
                        v <- readValencyCounter valencyVar
                        if v > 0 then do
                                    addConnection tbl remoteAddr localAddr' $ Just valencyVar
                                    writeTVar hasRefVar True
                                    modifyTVar' conThreads (Set.delete tid)
                                    if v == 1 then return ConnectSuccessLast
                                              else return ConnectSuccess
                                else return ConnectFail
                traceWith tracer $ SubscriptionTraceConnectEnd remoteAddr result
                case result of
                    ConnectSuccess -> k sd -- TODO catch all application exception.
                    ConnectSuccessLast -> do
                            outstandingConThreads <- atomically $ readTVar conThreads
                            mapM_ (\a -> throwTo a
                                   (SubscriberError SubscriberParrallelConnectionCancelled
                                    "Parrallel connection cancelled"
                                    callStack)) outstandingConThreads
                            k sd
                    ConnectFail -> return ()

    allocateSocket
        :: TVar IO (Set ThreadId)
        -> Socket.SockAddr
        -> IO (Socket.Socket, TVar IO Bool)
    allocateSocket conThreads remoteAddr = do
        tid <- myThreadId
        atomically $ do
            modifyTVar' conThreads (Set.insert tid)
            modifyTVar' threadPool (Set.insert tid)
        sd <- Socket.socket (sockAddrFamily remoteAddr) Socket.Stream Socket.defaultProtocol
        hasRefVar <- newTVarM False
        return (sd, hasRefVar)

    closeSocket
        :: TVar IO (Set ThreadId)
        -> Socket.SockAddr
        -> (Socket.Socket, TVar IO Bool)
        -> IO ()
    closeSocket conThreads remoteAddr (sd, hasRefVar)= do
        traceWith tracer $ SubscriptionTraceConnectCleanup remoteAddr

        tid <- myThreadId
        atomically $ do
            modifyTVar' conThreads (Set.delete tid)
            modifyTVar' threadPool (Set.delete tid)
        hasRef <- atomically $ readTVar hasRefVar
        when hasRef $ do
            localAddr <- Socket.getSocketName sd
            removeConnection tbl remoteAddr localAddr

        Socket.close sd



data IPSubscriptionTarget = IPSubscriptionTarget {
    -- | List of destinations to possibly connect to
      ispIps     :: ![Socket.SockAddr]
    -- | Number of parallel connections to keep actice.
    , ispValency :: !Int
    } deriving (Eq, Show)

-- | Spawns a subscription worker which will attempt to keep the specified number
-- of connections (Valency) active towards the list of IP addresses given in IPSubscriptionTarget.
ipSubscriptionWorker
    :: ConnectionTable IO
    -> Tracer IO (WithIPList SubscriptionTrace)
    -> Maybe Socket.SockAddr
    -- ^ Local IPv4 address to use, Nothing indicates don't use IPv4
    -> Maybe Socket.SockAddr
    -- ^ Local IPv6 address to use, Nothing indicates don't use IPv6
    -> (Socket.SockAddr -> Maybe DiffTime)
    -- ^ Lookup function, should return expected delay for the given address
    -> IPSubscriptionTarget
    -> (Socket.Socket -> IO ())
    -> (Async () -> IO t)
    -> IO t
ipSubscriptionWorker tbl tracer localIPv4 localIPv6 connectionAttemptDelay ips cb k =
    subscriptionWorker
            tbl
            (ipListTracer localIPv4 localIPv6 (ispIps ips) tracer)
            localIPv4
            localIPv6
            connectionAttemptDelay
            (return $ listSubscriptionTarget (ispIps ips))
            (ispValency ips)
            cb k

subscriptionWorker
    :: ConnectionTable IO
    -> Tracer IO SubscriptionTrace
    -> Maybe Socket.SockAddr
    -> Maybe Socket.SockAddr
    -> (Socket.SockAddr -> Maybe DiffTime)
    -> IO (SubscriptionTarget IO Socket.SockAddr)
    -> Int
    -> (Socket.Socket -> IO ())
    -> (Async () -> IO t)
    -> IO t
subscriptionWorker tbl tracer localIPv4 localIPv6 connectionAttemptDelay getTargets valency
        cb k = do
    childrenVar <- newTVarM Set.empty
    withAsync (worker childrenVar)
              (\aid -> (k aid) `finally` (do
                  children <- atomically $ readTVar childrenVar
                  mapM_ (\a -> throwTo a
                                   (SubscriberError SubscriberWorkerCancelled
                                    "SubscriptionWorker exiting"
                                    callStack)) $ Set.toList children
              ))
  where
    worker :: TVar IO (Set ThreadId) -> IO ()
    worker childrenVar = do
        valencyVar <- atomically $ newValencyCounter tbl valency
        traceWith tracer $ SubscriptionTraceStart valency
        forever $ do
            start <- getMonotonicTime
            targets <- getTargets
            subscribeTo tbl childrenVar tracer localIPv4 localIPv6 connectionAttemptDelay
                    valencyVar targets cb
            atomically $ waitValencyCounter valencyVar

            {-
            - We allways wait at least ipRetryDelay seconds between calls to getTargets, and
            - before trying to restart the subscriptions we also wait 1 second so that if multiple
            - subscription targets fail around the same time we will try to restart with a valency
            - higher than 1.
            -}
            threadDelay 1
            end <- getMonotonicTime
            let duration = diffTime end start
            currentValency <- atomically $ readValencyCounter valencyVar
            traceWith tracer $ SubscriptionTraceRestart duration currentValency
                (valency - currentValency)

            when (duration < ipRetryDelay) $
                threadDelay $ ipRetryDelay - duration

data WithIPList a = WithIPList {
      wilIPv4  :: !(Maybe Socket.SockAddr)
    , wilIPv6  :: !(Maybe Socket.SockAddr)
    , wilDsts  :: ![Socket.SockAddr]
    , wilEvent :: !a
    }

instance (Show a) => Show (WithIPList a) where
    show (WithIPList Nothing (Just wilIPv6) wilDsts wilEvent) =
        printf "IPs: %s %s %s" (show wilIPv6) (show wilDsts) (show wilEvent)
    show (WithIPList (Just wilIPv4) Nothing wilDsts wilEvent) =
        printf "IPs: %s %s %s" (show wilIPv4) (show wilDsts) (show wilEvent)
    show WithIPList {..} = printf  "IPs: %s %s %s %s" (show wilIPv4) (show wilIPv6)
                                                      (show wilDsts) (show wilEvent)

ipListTracer
    :: Maybe Socket.SockAddr
    -> Maybe Socket.SockAddr
    -> [Socket.SockAddr]
    -> Tracer IO (WithIPList a)
    -> Tracer IO a
ipListTracer ipv4 ipv6 ips tr = Tracer $ \s -> traceWith tr $ WithIPList ipv4 ipv6 ips s

data SubscriberError = SubscriberError {
      seType    :: !SubscriberErrorType
    , seMessage :: !String
    , seStack   :: !CallStack
    } deriving Show

-- | Enumeration of error conditions.
--
data SubscriberErrorType = SubscriberParrallelConnectionCancelled
                         | SubscriberWorkerCancelled
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
    | SubscriptionTraceUnsupportedRemoteAddr Socket.SockAddr
    | SubscriptionTraceMissingLocalAddress Socket.Family

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
    show (SubscriptionTraceUnsupportedRemoteAddr dst) =
        "Unsupported remote target address " ++ show dst
    show (SubscriptionTraceMissingLocalAddress fam) =
        "Missing local address for " ++ show fam


