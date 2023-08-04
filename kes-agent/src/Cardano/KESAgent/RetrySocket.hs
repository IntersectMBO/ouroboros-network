{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE ScopedTypeVariables #-}
module Cardano.KESAgent.RetrySocket
where

import Data.Time (picosecondsToDiffTime, DiffTime (..))
import System.Socket (socket, SocketException, close, connect)
import System.IO
import Control.Exception (Exception)
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTimer

-- | Retry given action up to 6 times, using the provided reporting function
-- to signal retries. Initial retry interval is 1 millisecond, after that
-- it doubles with each iteration, with a maximum of 0.1 second.
retrySocket :: MonadCatch m
            => MonadDelay m
            => Exception e
            => (e -> Int -> Int -> m ())
            -> m a
            -> m a
retrySocket =
  retrySocketWith
    (\i -> min 100 (i * 2))
    1
    6

reportRetryNull :: Applicative m => e -> Int -> Int -> m ()
reportRetryNull _ _ _ = pure ()

reportRetryStderr :: Exception e => e -> Int -> Int -> IO ()
reportRetryStderr ex n interval = do
  hPutStrLn stderr $ "Socket error: " ++ show ex ++ ", retrying " ++ show n ++ " more time(s), next in " ++ show (fromIntegral interval / 1000000 :: Double) ++ " s..."

retrySocketWith :: forall m a e.
                   MonadCatch m
                => MonadDelay m
                => Exception e
                => (Int -> Int)
                   -- ^ Current interval to next interval (useful for
                   -- implementing linearly or exponentially increasing
                   -- intervals)
                -> Int
                   -- ^ Interval between current attempt and next attempt
                -> Int
                   -- ^ Remaining number of retries. 0 means the current
                   -- attempt is the last one.
                -> (e -> Int -> Int -> m ())
                   -- ^ Reporting function. Arguments:
                   --   - exception that caused the failure
                   --   - remaining number of retries
                   --   - delay, in microseconds, until the next retry
                -> m a
                   -- ^ The action to (re-)try
                -> m a
retrySocketWith _ _ 0 _ a = a
retrySocketWith nextInterval interval n report a =
  a `catch` handler
  where
    handler :: e -> m a
    handler ex = do
      report ex n interval
      threadDelay interval
      retrySocketWith nextInterval (nextInterval interval) (pred n) report a
