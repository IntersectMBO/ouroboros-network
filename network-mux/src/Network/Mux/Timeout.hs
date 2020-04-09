{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}

-- | An alternative implementation of 'System.Timeout.timeout' for platforms
-- (i.e. Windows) where the standard implementation is too expensive.
--
-- The implementation provided here is for the special case where only one
-- timeout is active at once. A concurrent implementation would be possible
-- but is not currently needed.
--
module Network.Mux.Timeout
  ( withTimeoutSerial
  , TimeoutException (..)
  )
  where

import Control.Exception (Exception(..), asyncExceptionToException, asyncExceptionFromException)
import Control.Monad
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime
import Control.Monad.Class.MonadTimer hiding (timeout)


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
withTimeoutSerial
  :: forall m b. (MonadAsync m, MonadFork m, MonadTime m, MonadTimer m, MonadCatch m)
  => ((forall a. DiffTime -> m a -> m (Maybe a)) -> m b) -> m b
withTimeoutSerial body = do

    -- Shared var for the current timer, shared between the thread using
    -- timeouts and the thread monitoring the timeouts.
    timerStateVar <- newTVarM TimerIdle

    -- Which thread to kill if a timeout fires.
    tid <- myThreadId

    -- The @timeout@ action we pass to the body.
    let timeout :: forall a. DiffTime -> m a -> m (Maybe a)
        timeout delay action =
          bracket
            (do cancelVar <- newTVarM False
                now <- getMonotonicTime
                let !deadline = addTime delay now
                atomically $ writeTVar timerStateVar (TimerActive deadline cancelVar)
                return cancelVar)
            (\cancelVar -> atomically $ writeTVar cancelVar True)
            (\_ -> fmap Just action)
          `catch` \TimeoutException -> return Nothing

    -- Ensure the monitoring thread gets terminated when the body completes
    withAsync (monitoringThread tid timerStateVar) $ \_ ->
      body timeout

  where
    monitoringThread :: ThreadId m -> TVar m (TimerState m) -> m ()
    monitoringThread tid timerStateVar =
      forever $ do
        -- Grab the next timeout snapshot
        -- TODO: this is equivalent to an MVar with overwriting
        (deadline, cancelVar) <-
          atomically $ do
            timeoutState <- readTVar timerStateVar
            case timeoutState of
              TimerIdle                      -> retry
              TimerActive deadline cancelVar -> do
                writeTVar timerStateVar TimerIdle
                return (deadline, cancelVar)

        now <- getMonotonicTime
        let delay = diffTime deadline now
        threadDelay delay
        cancelled <- atomically $ readTVar cancelVar
        unless cancelled $
          throwTo tid TimeoutException

data TimerState m = TimerIdle
                  | TimerActive !Time !(TVar m Bool)

data TimeoutException = TimeoutException deriving Show

instance Exception TimeoutException where
  toException   = asyncExceptionToException
  fromException = asyncExceptionFromException

