{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE ScopedTypeVariables #-}
module Cardano.KESAgent.RetrySocket
where

import System.Socket (socket, SocketException, close, connect)
import System.IO
import Control.Concurrent (threadDelay)
import Control.Exception (catch)

-- | Retry given action up to 6 times, using the provided reporting function
-- to signal retries. Initial retry interval is 10 milliseconds, after that
-- it doubles with each iteration, with a maximum of 10 seconds.
retrySocket :: (SocketException -> Int -> Int -> IO ())
            -> IO a
            -> IO a
retrySocket =
  retrySocketWith
    (\i -> min (10000000) (i * 2))
    (10000)
    6

reportRetryNull :: SocketException -> Int -> Int -> IO ()
reportRetryNull _ _ _ = pure ()

reportRetryStderr :: SocketException -> Int -> Int -> IO ()
reportRetryStderr ex n interval = do
  hPutStrLn stderr $ "Socket error: " ++ show ex ++ ", retrying " ++ show n ++ " more time(s), next in " ++ show (fromIntegral interval / 1000000 :: Double) ++ " s..."

retrySocketWith :: forall a.
                   (Int -> Int)
                   -- ^ Current interval to next interval (useful for
                   -- implementing linearly or exponentially increasing
                   -- intervals)
                -> Int
                   -- ^ Interval between current attempt and next attempt
                -> Int
                   -- ^ Remaining number of retries. 0 means the current
                   -- attempt is the last one.
                -> (SocketException -> Int -> Int -> IO ())
                   -- ^ Reporting function. Arguments:
                   --   - exception that caused the failure
                   --   - remaining number of retries
                   --   - delay, in microseconds, until the next retry
                -> IO a
                   -- ^ The action to (re-)try
                -> IO a
retrySocketWith _ _ 0 _ a = a
retrySocketWith nextInterval interval n report a =
  a `catch` handler
  where
    handler :: SocketException -> IO a
    handler ex = do
      report ex n interval
      threadDelay interval
      retrySocketWith nextInterval (nextInterval interval) (pred n) report a
