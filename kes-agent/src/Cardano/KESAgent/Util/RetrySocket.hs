{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Retry socket actions upon errors.
module Cardano.KESAgent.Util.RetrySocket
where

import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTimer
import System.IO

-- | Retry given action up to 10 times, using the provided reporting function
-- to signal retries. Initial retry interval is 100 milliseconds, after that
-- it doubles with each iteration, with a maximum of 5 seconds.
retrySocket ::
  MonadCatch m =>
  MonadDelay m =>
  Exception e =>
  (e -> Int -> Int -> m ()) ->
  m a ->
  m a
retrySocket =
  retrySocketWith
    (\i -> min 5000000 (i * 2))
    100000
    10

-- | Null reporter for retry attempts (don't actually report anything)
reportRetryNull :: Applicative m => e -> Int -> Int -> m ()
reportRetryNull _ _ _ = pure ()

-- | Report retry attempts on 'stderr'.
reportRetryStderr :: Exception e => e -> Int -> Int -> IO ()
reportRetryStderr ex n interval = do
  hPutStrLn stderr $
    "Socket error: "
      ++ show ex
      ++ ", retrying "
      ++ show n
      ++ " more time(s), next in "
      ++ show (fromIntegral interval / 1000000 :: Double)
      ++ " s..."

-- | Retry socket action according to given parameters.
retrySocketWith ::
  forall m a e.
  MonadCatch m =>
  MonadDelay m =>
  Exception e =>
  -- | Current interval to next interval (useful for
  -- implementing linearly or exponentially increasing
  -- intervals)
  (Int -> Int) ->
  -- | Interval between current attempt and next attempt
  Int ->
  -- | Remaining number of retries. 0 means the current
  -- attempt is the last one.
  Int ->
  -- | Reporting function. Arguments:
  --   - exception that caused the failure
  --   - remaining number of retries
  --   - delay, in microseconds, until the next retry
  (e -> Int -> Int -> m ()) ->
  -- | The action to (re-)try
  m a ->
  m a
retrySocketWith _ _ 0 _ a = a
retrySocketWith nextInterval interval n report a =
  a `catch` handler
  where
    handler :: e -> m a
    handler ex = do
      report ex n interval
      threadDelay interval
      retrySocketWith nextInterval (nextInterval interval) (pred n) report a
