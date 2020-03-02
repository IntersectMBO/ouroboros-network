{-# LANGUAGE CPP                 #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances  #-}

module Ouroboros.Network.Subscription.Worker
  ( -- * Subscription worker
    worker
  , workerOneTarget
    -- * Constants
  , defaultConnectionAttemptDelay
  , minConnectionAttemptDelay
  , maxConnectionAttemptDelay
    -- * Errors
  , SubscriberError (..)
    -- * Tracing
  , SubscriptionTrace (..)
  , ConnectResult (..)
  ) where

-- TODO io-sim-classes ought to give this one if we expect it to be used over
-- the original async library.
import           Control.Concurrent.Async (concurrently, forConcurrently_)
import           Control.Concurrent.STM
import           Control.Monad (forM_, when)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Void (absurd)
import           GHC.Stack
import           Text.Printf

import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Monad.Class.MonadThrow
import           Control.Tracer

import           Ouroboros.Network.ConnectionId
import           Ouroboros.Network.Connections.Types
import qualified Ouroboros.Network.Connections.Concurrent as Concurrent
import           Ouroboros.Network.Connections.Socket.Client (client)
import           Ouroboros.Network.ErrorPolicy (WithAddr, ErrorPolicies (..),
                   ErrorPolicyTrace, SuspendDecision(Throw),
                   evalErrorPolicies)
import           Ouroboros.Network.Snocket (Snocket)
import           Ouroboros.Network.Socket (ConnectionHandle, waitForConnection)

-- | Time to wait between connection attempts when we don't have any DeltaQ
-- info.
--
defaultConnectionAttemptDelay :: DiffTime
defaultConnectionAttemptDelay = 0.250 -- 250ms delay

-- | Minimum time to wait between connection attempts.
--
minConnectionAttemptDelay :: DiffTime
minConnectionAttemptDelay = 0.010 -- 10ms delay

-- | Maximum time to wait between connection attempts.
--
maxConnectionAttemptDelay :: DiffTime
maxConnectionAttemptDelay = 2 -- 2s delay

-- | Do subscription loops for a given list of targets.
--
-- This will never stop trying to connect. As soon as all of the connections
-- have been tried, it will start again: targets for connections which end are
-- put into the end of a queue, and the next target to try is the head of the
-- queue.
worker
  :: Tracer IO (SubscriptionTrace addr)
  -> Tracer IO (WithAddr addr ErrorPolicyTrace)
  -> ErrorPolicies
  -> NonEmpty (ConnectionId addr)
  -- ^ Targets for subscription. Each one indicates a local address and a remote
  -- address, so one worker can do both IPv4 and IPv6.
  -> Word
  -- ^ Valency: how many concurrent connections to try to keep up. Will be
  -- clamped to at least 1 and at most the length of the list of targets.
  -> DiffTime
  -- ^ Minimum delay between subscriptions. If a subscription attempt lasts
  -- less than this duration, it will wait for the difference.
  -> Snocket IO socket addr
  -> Connections (ConnectionId addr) socket request
       (Concurrent.Reject reject)
       (Concurrent.Accept (ConnectionHandle IO))
       IO
  -- ^ "Where" to carry out a request.
  -- The ConenctionHandle type allows for us to wait for the connection to end,
  -- before attempting a connection to the next target.
  -> request Local
  -> IO x
worker tr errTrace errPolicies targets valency delay sn connections req = do
  q <- newTQueueIO
  atomically $ forM_ (NE.toList targets) (writeTQueue q)
  let numThreads = max 1 (min (fromIntegral valency) (NE.length targets))
  -- Write it out like this so that we can convince GHC that this program
  -- really does not ever return (hence `IO x`).
  (impossible, _) <- concurrently
    (workerOneTarget tr errTrace errPolicies delay q sn connections req)
    (forConcurrently_ [1..(numThreads-1)] $ \_ ->
       workerOneTarget tr errTrace errPolicies delay q sn connections req)
  absurd impossible

-- | Worker for one subscription target sequence.
-- `worker` runs 0 or more of these concurrently.
--
-- Includes a built-in delay of `ipRetryDelay` between connections but
-- FIXME this should not be built-in.
--
workerOneTarget
  :: Tracer IO (SubscriptionTrace addr)
  -> Tracer IO (WithAddr addr ErrorPolicyTrace)
  -> ErrorPolicies
  -> DiffTime
  -> TQueue (ConnectionId addr)
  -> Snocket IO socket addr
  -> Connections (ConnectionId addr) socket request
       (Concurrent.Reject reject)
       (Concurrent.Accept (ConnectionHandle IO))
       IO
  -> request Local
  -> IO x
workerOneTarget tr errTrace errPolicies delay q sn connections req = mask $ \restore -> do
  connectionId <- restore (atomically $ readTQueue q)
  traceWith tr $ SubscriptionTraceConnectStart (remoteAddress connectionId)
  start <- getMonotonicTime
  -- Exception handling here is relevant to the resource acquisition which
  -- is part of `mkClient connectionId`. Running this against the `connections`
  -- term _may or may not_ require creating the resource, and in case it does,
  -- that may raise an exception, so it must be dealt with here.
  -- An exception on resource acquisition is different from a rejected
  -- connection. The latter is relevant only cases in which the resource was
  -- created, but the connection was rejected for other reasons.
  outcome <- restore (client sn connections connectionId req)
  case outcome of
    NotAcquired err ->
      case evalErrorPolicies err (epConErrorPolicies errPolicies) of
        Just Throw -> throwM err
        _ -> pure ()
    -- The connection was established (acquired) but rejected, so it's our
    -- responsibility to close it.
    Acquired (Rejected (Concurrent.DomainSpecific _reason) acquiredRes) -> do
      restore (closeResource acquiredRes)
      pure ()
    Acquired (Accepted (Concurrent.Accepted connhandle)) -> do
      -- NB: no error policy use here. The non "connection errors" (the
      -- "application errors") are dealt with in the connection continuation
      -- from Ouroboros.Network.Socket
      _ <- restore (waitForConnection connhandle)
      pure ()
  end <- getMonotonicTime
  let duration = diffTime end start
  when (duration < delay) (threadDelay (delay - duration))
  -- No exception handling is done to ensure this gets returned to the queue.
  -- Why? Because if this thread dies with an exception, so do all of the
  -- other threads spawned by `worker`, so who cares?
  atomically $ writeTQueue q connectionId
  workerOneTarget tr errTrace errPolicies delay q sn connections req

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
data SubscriberErrorType = SubscriberParrallelConnectionCancelled
                         | SubscriberWorkerCancelled
                         deriving (Eq, Show)

instance Exception SubscriberError where
    displayException SubscriberError{seType, seMessage, seStack}
      = printf "%s %s at %s"
         (show seType)
         (show seMessage)
         (prettyCallStack seStack)

-- | ADT which classifies connection result.
--
-- FIXME bad name. It's not about a connection, it's about the subscription
-- worker overall (judging by the constructor comments).
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

-- FIXME this is way too much.
-- Should pare it down to only those things which are relevant to choosing which
-- address to try to connect to.
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
