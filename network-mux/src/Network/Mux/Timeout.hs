{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
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

import           Control.Exception (asyncExceptionFromException,
                     asyncExceptionToException)
import           Control.Monad
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer (MonadTimer, registerDelay)
import qualified Control.Monad.Class.MonadTimer as MonadTimer


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
withTimeoutSerial, withTimeoutSerialNative
  :: forall m b. (MonadAsync m, MonadFork m,
                  MonadMonotonicTime m, MonadTimer m,
                  MonadMask m, MonadThrow (STM m))
  => (TimeoutFn m -> m b) -> m b

#if defined(mingw32_HOST_OS)
withTimeoutSerial = withTimeoutSerialAlternative
#else
withTimeoutSerial = withTimeoutSerialNative
#endif

-- | This version simply passes the native platform's 'MonadTimer.timeout'.
--
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
-- The implementation here only relies upon 'threadDelay' which has an
-- efficient implementation on all platforms. It uses a separate monitoring
-- thread which will throw an exception to terminate the action if the
-- timeout expires. This is why it uses the \"with\" style: because it keeps
-- a monitoring thread over a region of execution that may use many timeouts.
-- The cost of allocating this thread is amortised over all the timeouts used.
--
-- This implementation is simplified by the constraint that the timeouts only
-- be used serially. In addition, it has the limitation that timeouts may not
-- always be detected promptly: e.g. a 10s timeout on an action that finishes
-- immediately, followed by a 5s timeout on an action will only actually be
-- interrupted after 10s. So it's possible that action with the 5s timeout runs
-- for up to 10s. This is ok for many applications provided that the variance
-- in timeouts is not too large and the timeouts don't need to be too tight.
--
withTimeoutSerialAlternative
  :: forall m b. (MonadAsync m, MonadFork m,
                  MonadMonotonicTime m, MonadTimer m,
                  MonadMask m, MonadThrow (STM m))
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
       deadlineResetVar :: !(TVar m Bool)
     }

data NextTimeout m = NoNextTimeout
                   | NextTimeout
                       !(ThreadId m)           -- Which thread to interrupt
                       !Time                   -- When to interrupt it
                       !(TVar m TimeoutState)  -- Synchronisation state


newMonitorState :: MonadSTM m => m (MonitorState m)
newMonitorState = do
    nextTimeoutVar   <- newTVarIO NoNextTimeout
    curDeadlineVar   <- newTVarIO (Time 0)
    deadlineResetVar <- newTVarIO False
    return MonitorState{..}


-- | An update to the shared monitor state to set a new timer.
--
setNewTimer :: MonadSTM m
            => MonitorState m
            -> ThreadId m
            -> Time
            -> TVar m TimeoutState
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
                -> m (ThreadId m, Time, TVar m TimeoutState)
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


-- | Three normal states of the timeout, plus one pseudo state. We use these
-- states to provide proper communication between the action thread and the
-- monitoring thread.
--
-- The valid transitions are from 'TimeoutPending' to 'TimeoutCancelled', and
-- from 'TimeoutPending' to 'TimeoutFired'. Additionally there is a transition
-- from 'TimeoutFired' to 'TimeoutTerminated'.
--
data TimeoutState = TimeoutPending
                  | TimeoutCancelled
                  | TimeoutFired
                  | TimeoutTerminated


-- | Exception used by 'withTimeoutSerial' to terminate the action when the
-- timeout occurs.
--
data TimeoutException = TimeoutException deriving Show

instance Exception TimeoutException where
  toException   = asyncExceptionToException
  fromException = asyncExceptionFromException


-- | The @timeout@ action we pass to the body in 'withTimeoutSerial'.
--
timeout :: forall m a.
           (MonadFork m, MonadMonotonicTime m, MonadTimer m,
            MonadMask m, MonadThrow (STM m))
        => MonitorState m
        -> DiffTime -> m a -> m (Maybe a)
timeout _            delay action | delay <  0 = Just <$> action
timeout _            delay _      | delay == 0 = return Nothing
timeout monitorState delay action =

    -- We have to be very careful with async exceptions of course.
    mask $ \restore -> do

      -- Set up the timeout and pass it over to the monitoring thread.
      -- This overwrites any previous timeout.
      tid <- myThreadId
      timeoutStateVar <- newTVarIO TimeoutPending
      now <- getMonotonicTime
      let deadline = addTime delay now
      setNewTimer monitorState tid deadline timeoutStateVar

      -- Now we unmask exceptions to run the action. If we get the timeout
      -- exception then we drop straight down to the outer 'catch' handler.
      result <- restore action

      -- The action completed without us getting an exception, but there's
      -- a race condition with the monitoringThread to resolve. It could be
      -- just about to send us an async exception when we drop out of the
      -- masked region, or worse, outside of the scope of the 'catch' handler.
      --
      -- So we must synchronise with the monitoringThread to ensure that we
      -- know positively one way or the other whether the timeout exception is
      -- or is not going to be sent.
      --
      -- We do that by having both this thread and the monitoringThread do
      -- an STM tx on the timeoutStateVar, to atomically move it into the
      -- cancelled or fired state, depending on which thread gets there first.
      -- So this is a race, but it's a race where we can positively establish
      -- the outcome. If this thread gets there first we move it to the
      -- cancelled state. If the monitoringThread gets there first it moves it
      -- to the fired state.
      --
      -- This STM tx is not blocking, so we're guaranteed not to be interrupted
      -- while we have async exceptions masked.
      --
      timeoutFired <- atomically $ do
        st <- readTVar timeoutStateVar
        case st of
          TimeoutFired   -> return True
          TimeoutPending -> writeTVar timeoutStateVar TimeoutCancelled
                         >> return False
          _              -> throwSTM TimeoutImpossibleTimeoutState

      -- If we established that the monitoring thread is definitely not firing
      -- the timeout exception then we're done.
      --
      -- If on the other hand we lost the race and the monitoring thread is
      -- about to come for us then it's crucial that we block and wait for
      -- our own demise, otherwise we could escape the scope of the outer
      -- 'catch' handler.
      --
      -- We do that blocking wait with another STM tx on the timeoutStateVar,
      -- this time waiting for the monitoring thread to set it to the
      -- TimeoutTerminated state. But the monitoring thread only sets it to
      -- that state /after/ raising the async exception in this thread. So
      -- we will never actually rach that terminated state.
      --
      -- Note also that blocking STM txs are points where async exceptions that
      -- are pending (due to our masked state) can be raised. Hence there is no
      -- need for us to unmask for this bit.
      --
      if not timeoutFired
        then return (Just result)
        else atomically $ do
               st <- readTVar timeoutStateVar
               case st of
                 TimeoutFired      -> retry
                 TimeoutTerminated -> throwSTM TimeoutImpossibleReachedTerminated
                 _                 -> throwSTM TimeoutImpossibleTimeoutState

    `catch` \TimeoutException -> return Nothing


monitoringThread :: (MonadFork m, MonadSTM m,
                     MonadMonotonicTime m, MonadTimer m,
                     MonadThrow (STM m))
                 => MonitorState m -> m ()
monitoringThread monitorState@MonitorState{deadlineResetVar} = do
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

    -- Having woken up, we check to see if we need to send the timeout
    -- exception. If the timeoutStateVar we grabbed is still in the pending
    -- state after we have waited for the deadline then clearly it went over
    -- and we will send the timeout exception.
    --
    -- As described above, there is a race between the timeout and the action
    -- completing and we have to establish which happens first. So this is
    -- the other side of that race. Here if we win the race we move to the
    -- TimeoutFired state.
    cancelled <- atomically $ do
                   st <- readTVar timeoutStateVar
                   case st of
                     TimeoutPending   -> writeTVar timeoutStateVar TimeoutFired
                                      >> return False
                     TimeoutCancelled -> return True
                     _                -> throwSTM TimeoutImpossibleMonitorState

    -- If it turns out that the timeout was cancelled before the timeout
    -- expired then we do nothing and go back to the start.
    --
    -- But if it was not cancelled then we will send it the asynchronous
    -- exception. As described above there is a corner case where we won
    -- the race very narrowly and so the other thread has to block and wait
    -- for us to send the asynchronous exception. The way we make it block to
    -- wait for us -- without any concern that it might be classed as being
    -- blocked indefinitely -- is to make it wait for a state change that we
    -- will only perform /after/ we have raised the exception in the target
    -- thread.
    --
    unless cancelled $ do
      throwTo tid TimeoutException
      -- Set the state the other thread may be waiting for /after/ raising the
      -- exception in the other thread.
      atomically $ writeTVar timeoutStateVar TimeoutTerminated


-- | These are all \"impossible\" errors.
--
data TimeoutAssertion = TimeoutImpossibleReachedTerminated
                      | TimeoutImpossibleTimeoutState
                      | TimeoutImpossibleMonitorState
  deriving Show

instance Exception TimeoutAssertion
