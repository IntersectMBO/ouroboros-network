{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | An alternative implementation of 'System.Timeout.timeout' for platforms
-- (i.e. Windows) where the standard implementation is too expensive.
--
-- The implementation provided here is for the special case where only one
-- timeout is active at once. A concurrent implementation would be possible
-- but is not currently needed.
--
module Network.Mux.Timeout
  ( TimeoutFn
  , withTimeoutSerial
  , withTimeoutSerialNative
  , withTimeoutSerialAlternative
  ) where

import Control.Concurrent.Class.MonadSTM
import Control.Exception (asyncExceptionFromException,
           asyncExceptionToException)
import Control.Monad
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI (MonadTimer, registerDelay)
import Control.Monad.Class.MonadTimer.SI qualified as MonadTimer
import Control.Monad.Class.MonadUnique (MonadUnique (..))


-- | The type of the 'System.Timeout.timeout' function.
--
type TimeoutFn m = forall a. DiffTime -> m a -> m (Maybe a)

-- | A 'System.Timeout.timeout' that is reasonably efficient for all platforms.
--
-- On Unix it uses exactly 'System.Timeout.timeout' and on Windows it uses
-- 'withTimeoutSerialAlternative'.
--
-- > withTimeoutSerial $ \timeout ->
-- >   -- now use timeout as one would use System.Timeout.timeout
-- >   -- but not concurrently!
--
-- The implementation has a serial constraint: the body action that calls
-- @timeout@ can /only do so from one thread at once/.
--
withTimeoutSerial
  :: forall m b. (MonadAsync m, MonadFork m,
                  MonadMonotonicTime m, MonadTimer m,
                  MonadMask m, MonadThrow (STM m), MonadUnique m)
  => (TimeoutFn m -> m b) -> m b

#if defined(mingw32_HOST_OS)
withTimeoutSerial = withTimeoutSerialAlternative
#else
withTimeoutSerial = withTimeoutSerialNative
#endif

-- | This version simply passes the native platform's 'MonadTimer.timeout'.
--
withTimeoutSerialNative
  :: MonadTimer m
  => (TimeoutFn m -> m b) -> m b
withTimeoutSerialNative body = body MonadTimer.timeout


-- | An alternative implementation of 'System.Timeout.timeout' for platforms
-- where the standard implementation is too expensive.
--
-- > withTimeoutSerial $ \timeout ->
-- >   -- now use timeout as one would use System.Timeout.timeout
-- >   -- but not concurrently!
--
-- This implementation has a serial constraint: the body action that calls
-- @timeout@ can /only do so from one thread at once/.
--
-- Further details for the curious:
--
-- the problem with @System.Timeout.timeout@ is that (as of base 4.12) it has
-- two implementations, one for Unix with the threaded RTS, and one for all
-- other configurations.
--
-- The Unix threaded RTS implementation is rather clever and very fast. In the
-- normal case of no timeout, it only has to allocate a timer entry. Only in
-- the case the timeout occurs does it allocate a 'forkIO' thread to do the
-- potentially-blocking operation of sending the asynchronous exception to
-- interrupt the action under timeout.
--
-- The implementation for all other configurations has to allocate a 'forkIO'
-- thread up front, whether or not the timeout fires. This is fine for many
-- applications but not for network timers which have to be created and
-- altered\/cancelled with very high frequency.
--
-- A monitoring thread is shared by all the timeouts in the scope. When a
-- timeout completes, it only cancels its own STM state: the monitor can keep
-- waiting on the existing 'registerDelay'. Once that delay expires, it picks
-- up the latest timeout. This amortises timer registration over many calls
-- when their deadlines keep moving forwards, as on mux's ingress path.
--
-- An earlier deadline wakes the monitor so it can register a shorter delay.
-- The previous 'registerDelay' is left to expire. Thus repeatedly moving
-- deadlines backwards does not benefit from the same amortisation.
--
-- Calls using the same timeout function must not overlap, including nested
-- calls. Separate scopes may be nested. Each scope allocates a unique value
-- once and uses its hash to identify its timeout exceptions.
--
withTimeoutSerialAlternative
  :: forall m b. (MonadAsync m, MonadFork m,
                  MonadMonotonicTime m, MonadTimer m,
                  MonadMask m, MonadThrow (STM m), MonadUnique m)
  => (TimeoutFn m -> m b) -> m b
withTimeoutSerialAlternative body = do

    -- State shared between the timeouts and the monitoring thread.
    monitorState <- newMonitorState

    -- Run the monitoring thread but ensure it terminates when the body is done.
    withAsync (monitoringThread monitorState) $ \_ ->

      -- Run the body and pass it the @timeout@ function.
      body (timeout monitorState)


-- | The state shared between the timeouts and the monitoring thread.
--
data MonitorState m =
     MonitorState {
       -- written by timeout combinator, read and reset by monitoring thread
       nextTimeoutVar   :: !(TVar m (NextTimeout m)),

       -- written by monitoring thread, read by timeout combinator
       curDeadlineVar   :: !(TVar m Time),

       -- written by timeout combinator, read and reset by monitoring thread
       deadlineResetVar :: !(TVar m Bool),

       -- shared `TimeoutFnId` between `timeout` and `monitoringThread`
       timeoutFnId      :: !TimeoutFnId
     }

data NextTimeout m = NoNextTimeout
                   | NextTimeout
                       !(ThreadId m)           -- Which thread to interrupt
                       !Time                   -- When to interrupt it
                       !(TVar m (TimeoutState m))  -- Synchronisation state


newMonitorState :: ( MonadSTM m
                   , MonadUnique m
                   )
                => m (MonitorState m)
newMonitorState = do
    nextTimeoutVar   <- newTVarIO NoNextTimeout
    curDeadlineVar   <- newTVarIO (Time 0)
    deadlineResetVar <- newTVarIO False
    timeoutFnId      <- TimeoutFnId . hashUnique <$> newUnique
    return MonitorState{..}


-- | An update to the shared monitor state to set a new timer.
--
setNewTimer :: MonadSTM m
            => MonitorState m
            -> ThreadId m
            -> Time
            -> TVar m (TimeoutState m)
            -> m ()
setNewTimer MonitorState{nextTimeoutVar, curDeadlineVar, deadlineResetVar}
            !tid !deadline !stateVar =
    atomically $ do
      writeTVar nextTimeoutVar (NextTimeout tid deadline stateVar)
      curDeadline <- readTVar curDeadlineVar
      -- If the timer deadline has moved backwards, notify the monitor thread
      -- so it can wake up and grab the next timeout
      when (deadline < curDeadline) $
        writeTVar deadlineResetVar True


-- | A potentially blocking wait read side, so can block.
--
readNextTimeout :: MonadSTM m
                => MonitorState m
                -> m (ThreadId m, Time, TVar m (TimeoutState m))
readNextTimeout MonitorState{nextTimeoutVar, curDeadlineVar, deadlineResetVar} = do
    atomically $ do
      nextTimeout <- readTVar nextTimeoutVar
      case nextTimeout of
        NoNextTimeout -> retry
        NextTimeout tid deadline stateVar -> do
          -- We've picked up the next timeout so reset to empty
          writeTVar nextTimeoutVar NoNextTimeout
          -- Update the current deadline var so the 'setNewTimer' can see what
          -- the current deadline we're waiting on is, so it can see if it
          -- ever needs to wake us up if it sets up a new timer with an earlier
          -- deadline.
          writeTVar curDeadlineVar deadline
          writeTVar deadlineResetVar False
          return (tid, deadline, stateVar)


-- | Cancellation and expiry race to change 'TimeoutPending'. If expiry wins,
-- cleanup must also stop the exception-delivery thread before returning.
--
data TimeoutState m = TimeoutPending
                    | TimeoutCancelled
                    | TimeoutFired !(Async m ())

-- | A unique identifier for timeouts of a given timeout function.  It allows
-- us to distinguish scoped `withTimeoutSerialAlternative` timeout exceptions.
--
newtype TimeoutFnId = TimeoutFnId Int
  deriving (Eq, Show)

-- | The hash of a unique value identifies the scope, keeping the exception
-- independent of the monad and the state parameter of 'IOSim'. This assumes
-- that 'hashUnique' does not collide between overlapping scopes.
--
newtype TimeoutException = TimeoutException TimeoutFnId
  deriving (Eq, Show)

instance Exception TimeoutException where
  toException   = asyncExceptionToException
  fromException = asyncExceptionFromException


-- | The @timeout@ action we pass to the body in 'withTimeoutSerial'.
--
timeout :: forall m a.
           (MonadAsync m, MonadMonotonicTime m,
            MonadMask m, MonadThrow (STM m))
        => MonitorState m
        -> DiffTime -> m a -> m (Maybe a)
timeout _            delay action | delay <  0 = Just <$> action
timeout _            delay _      | delay == 0 = return Nothing
timeout monitorState@MonitorState { timeoutFnId } delay action =
    handleJust (\case
                e@(TimeoutException fnid)
                  | fnid == timeoutFnId
                  -> Just e
                  | otherwise
                  -> Nothing)
               (\_ -> return Nothing) $
      bracket register cancelTimeout (\_ -> Just <$> action)
  where
    register = do
      tid <- myThreadId
      timeoutStateVar <- newTVarIO TimeoutPending
      now <- getMonotonicTime
      setNewTimer monitorState tid (delay `addTime` now) timeoutStateVar
      return timeoutStateVar

    -- This runs for successful actions and exceptions alike. Cancelling a
    -- delivery thread interrupts even a blocked throwTo. It must finish before
    -- another exception can let us leave the handler for this timeout.
    cancelTimeout timeoutStateVar = uninterruptibleMask_ $ do
      deliveryWorker <- atomically $ do
        st <- readTVar timeoutStateVar
        case st of
          TimeoutPending -> do
            writeTVar timeoutStateVar TimeoutCancelled
            return Nothing
          TimeoutFired deliveryWorker ->
            return (Just deliveryWorker)
          TimeoutCancelled ->
            throwSTM TimeoutImpossibleTimeoutState
      -- Cancel and wait for the delivery thread to exit to so no timeout
      -- can arrive after we leave this timeout's handler.
      mapM_ cancel deliveryWorker


monitoringThread :: forall m.
                    (MonadAsync m, MonadFork m, MonadMask m,
                     MonadMonotonicTime m,
                     MonadTimer m, MonadThrow (STM m))
                 => MonitorState m -> m ()
monitoringThread monitorState@MonitorState{deadlineResetVar, timeoutFnId} = do
  threadId <- myThreadId
  labelThread threadId "timeout-monitoring-thread"
  forever $ do
    -- Grab the next timeout to consider
    (tid, deadline, timeoutStateVar) <- readNextTimeout monitorState

    -- Work out how long to wait for and do so. For already-expired timeouts
    -- this is in the past, leading to a negative delay.
    now <- getMonotonicTime
    let delay = diffTime deadline now
    when (delay > 0) $ do
      timerExpired <- registerDelay delay
      atomically $
        -- Wait for either the timer to expire
        (readTVar timerExpired >>= check)
          `orElse`
        -- or to be notified that the timer deadline moved backwards
        (readTVar deadlineResetVar >>= check)

    -- Avoid allocating a delivery thread for the usual case: an action that
    -- has already completed. Recheck when publishing the worker, since cleanup
    -- can still win the race while the worker is being created.
    pending <- atomically $ do
      st <- readTVar timeoutStateVar
      case st of
        TimeoutPending   -> return True
        TimeoutCancelled -> return False
        TimeoutFired _   -> throwSTM TimeoutImpossibleMonitorState

    when pending $
      withAsync (deliveryWorker timeoutStateVar tid) $
        \deliveryWorkerHandle -> do
          atomically $ do
            st <- readTVar timeoutStateVar
            case st of
              TimeoutPending ->
                -- Publish the worker so `cancelTimeout` can cancel it before
                -- the worker is allowed to attempt delivery.
                writeTVar timeoutStateVar (TimeoutFired deliveryWorkerHandle)
              TimeoutCancelled -> return ()
              TimeoutFired _   -> throwSTM TimeoutImpossibleMonitorState
          -- Cancellation by the action's cleanup is normal. In either case the
          -- monitor can go on to the next timeout once delivery has finished.
          void $ waitCatch deliveryWorkerHandle
  where
    deliveryWorker :: TVar m (TimeoutState m) -> ThreadId m -> m ()
    deliveryWorker timeoutStateVar tid = do
      -- The delivery thread. Do not throw until `cancelTimeout` can find
      -- and cancel this worker.
      fire <- atomically $ do
        st <- readTVar timeoutStateVar
        case st of
          TimeoutPending   -> retry
          TimeoutCancelled -> return False
          TimeoutFired _   -> return True
      when fire $
        -- Deliver a timeout exception. This may block while the target
        -- masks exceptions.  If the action exits before delivery,
        -- `cancelTimeout` cancels this worker before leaving the timeout
        -- handler.
        --
        -- While `cancelTimeout` is uninterruptibly masked, a pending
        -- timeout cannot be delivered.  Cancelling the `deliveryWorker`
        -- interrupts its blocked `throwTo`, thus the race between both is
        -- resolved.
        throwTo tid (TimeoutException timeoutFnId)


-- | These are all \"impossible\" errors.
--
data TimeoutAssertion = TimeoutImpossibleTimeoutState
                      | TimeoutImpossibleMonitorState
  deriving Show

instance Exception TimeoutAssertion
