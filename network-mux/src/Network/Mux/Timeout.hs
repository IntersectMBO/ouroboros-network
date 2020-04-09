{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

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
  :: forall m b. (MonadAsync m, MonadFork m, MonadTimer m, MonadThrow m)
  => ((forall a. DiffTime -> m a -> m a) -> m b) -> m b
withTimeoutSerial body = do

    -- Shared var for the current timeout, shared between the thread using
    -- timeouts and the thread monitoring the timeouts.
    tv <- newTVarM Nothing

    -- Which thread to kill if at timeout fires.
    tid <- myThreadId

    let notifyNewTimeout t =
          -- Post the timeout to the monitoring thread.
          atomically $ do

            -- Assertion to ensure we only use the timeout serially
            existing <- readTVar tv
            case existing of
              Nothing -> return ()
              Just t'  -> do
                state <- readTimeout t'
                case state of
                  TimeoutCancelled -> return ()
                  _ -> fail "withTimeoutSerial: timeout used concurrently"

            writeTVar tv (Just t)
        
        -- The @timeout@ action we pass to the body.
        timeout :: forall a. DiffTime -> m a -> m a
        timeout d action =
          bracket
            (do t <- newTimeout d
                notifyNewTimeout t
                return t)
            cancelTimeout
            (\_ -> action)
   
    -- Ensure the monitoring thread gets terminated when the body completes
    withAsync (monitoringThread tid tv) $ \_ ->
      body timeout

  where
    monitoringThread :: ThreadId m -> TVar m (Maybe (Timeout m)) -> m ()
    monitoringThread tid tv =
      forever $ do
        fired <- atomically $ do
                  mt <- readTVar tv
                  case mt of
                    Nothing -> retry
                    Just t  -> awaitTimeout t
        when fired $
          throwTo tid TimeoutException

data TimeoutException = TimeoutException deriving Show

instance Exception TimeoutException where
  toException   = asyncExceptionToException
  fromException = asyncExceptionFromException

