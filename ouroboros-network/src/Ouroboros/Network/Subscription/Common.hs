{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Subscription.Common
    ( subscribeTo
    , sockAddrFamily
    , minConnectionAttemptDelay
    , SubscriberError (..)
    ) where

import           Control.Concurrent hiding (threadDelay)
import           Control.Concurrent.Async

import           Control.Monad (unless, when)
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Data.List (delete)
import           Data.Word
import           GHC.Stack
import qualified Network.Socket as Socket
import           Text.Printf

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

sockAddrFamily
    :: Socket.SockAddr
    -> Socket.Family
sockAddrFamily (Socket.SockAddrInet  _ _    ) = Socket.AF_INET
sockAddrFamily (Socket.SockAddrInet6 _ _ _ _) = Socket.AF_INET6
sockAddrFamily _                              = Socket.AF_UNSPEC

data ConnectResult = ConnectSuccess
                   | ConnectSuccessLast
                   | ConnectFail
                   deriving (Eq, Ord)

subscribeTo
    :: HasCallStack
    => Socket.PortNumber
    -> (Socket.SockAddr -> Maybe DiffTime)
    -> TVar IO Word16
    -> (Socket.Socket -> IO ())
    -> SubscriptionTarget IO Socket.SockAddr
    -> IO ()
subscribeTo localPort connectionAttemptDelay valencyVar k ts = do
     x <- newTVarM []
     doSubscribeTo x ts

  where
    doSubscribeTo :: TVar IO [ThreadId] -> SubscriptionTarget IO Socket.SockAddr -> IO ()
    doSubscribeTo conThreads targets = do
        target_m <- getSubscriptionTarget targets
        case target_m of
            Just (target, nextTargets) -> do
                valencyLeft <- atomically $ readTVar valencyVar
                if valencyLeft > 0
                    then do
                        --printf "going to subscribe to %s\n" (show target)
                        caid <- async $ doConnect conThreads target
                        atomically $ modifyTVar' conThreads (\t -> asyncThreadId caid:t)

                        {- The time to wait depends on available deltaQ information for the
                        -- destination address and max/min values from RFC8305.
                        -}
                        let delay = case connectionAttemptDelay target of
                                         Just d  -> min maxConnectionAttemptDelay $
                                                        max minConnectionAttemptDelay d
                                         Nothing -> defaultConnectionAttemptDelay
                        threadDelay delay
                        doSubscribeTo conThreads nextTargets
                    else do
                        --printf "successfully started required subscriptions\n"
                        return ()
            Nothing -> do
                --printf "out of targets, waiting on active connections\n"

                -- We wait on the list of active connection threads instead of using an async wait
                -- function since some of the connections may succed and then should be left
                -- running.
                atomically $ do
                    activeCons  <- readTVar conThreads
                    unless (null activeCons) retry
                --printf "done waiting on active connections\n"
                return ()


    doConnect conThreads remoteAddr =
        bracket
            ( do
                sd <- Socket.socket (sockAddrFamily remoteAddr) Socket.Stream
                        Socket.defaultProtocol
                hasRefVar <- newTVarM False
                return (sd, hasRefVar)
            )
            (\(sd,hasRefVar) -> do
                tid <- myThreadId
                --printf "%s: dc bracket cleaning\n" $ show tid
                Socket.close sd

                atomically $ modifyTVar' conThreads (delete tid)
                hasRef <- atomically $ readTVar hasRefVar
                when hasRef $
                    atomically $ modifyTVar' valencyVar (+ 1)
            )
            (\(sd,hasRefVar)  -> do
                Socket.setSocketOption sd Socket.ReuseAddr 1
#if !defined(mingw32_HOST_OS)
                Socket.setSocketOption sd Socket.ReusePort 1
#endif
                let localAddr = case sockAddrFamily remoteAddr of
                                     Socket.AF_INET6 -> Socket.SockAddrInet6 localPort 0 (0,0,0,0) 0
                                     _  -> Socket.SockAddrInet localPort 0
                -- printf "ready to bind\n"
                Socket.bind sd localAddr
                tid <- myThreadId
                conStart <- getMonotonicTime
                --printf "%s %s connecting too %s\n" (show tid) (show conStart) (show remoteAddr)
                res <- try $ Socket.connect sd remoteAddr
                conEnd <- getMonotonicTime
                --printf "%s %s %s connected\n" (show tid) (show conEnd) (show $ diffTime conEnd conStart)
                case res of
                        Left (e :: SomeException) ->
                            {- printf "connected failed with %s\n" (show res) >> -} throwM e
                        Right _ -> return () --printf "%s connected\n" $ show tid

                -- We successfully connected, increase valency and start the app
                result <- atomically $ do
                        v <- readTVar valencyVar
                        if v > 0 then do
                                    modifyTVar' valencyVar (\a -> a - 1)
                                    writeTVar hasRefVar True
                                    modifyTVar' conThreads (delete tid)
                                    if v == 1 then return ConnectSuccessLast
                                              else return ConnectSuccess
                                else return ConnectFail
                left <- atomically $ readTVar valencyVar
                --printf "%s connected to %s, left %d\n" (show tid) (show remoteAddr)  left
                case result of
                    ConnectSuccess -> k sd

                    ConnectSuccessLast -> do
                            outstandingConThreads <- atomically $ readTVar conThreads
                            --printf "%s killing of %s\n" (show tid) (show outstandingConThreads)
                            mapM_ (\a -> throwTo a
                                   (SubscriberError SubscriberParrallelConnectionCancelled
                                    "Parrallel connection cancelled"
                                    callStack)) outstandingConThreads
                            k sd
                    ConnectFail -> do
                        --printf "%s too slow when connecting to %s\n" (show tid) (show remoteAddr)
                        return ()

            )


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

