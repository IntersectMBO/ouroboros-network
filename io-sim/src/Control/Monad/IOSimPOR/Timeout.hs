module Control.Monad.IOSimPOR.Timeout ( Timeout, timeout, unsafeTimeout ) where

-- This module provides a timeout function like System.Timeout, BUT
-- garbage collection time is not included (provided GHC stats are
-- enabled, +RTS -T -RTS). Thus this can be used more reliably to
-- limit computation time.

import Control.Monad
import Control.Concurrent
import Control.Exception   (Exception(..), handleJust, bracket,
                            uninterruptibleMask_,
                            asyncExceptionToException,
                            asyncExceptionFromException)
import Data.Unique         (Unique, newUnique)
import GHC.Stats
import System.IO.Unsafe


-- An internal type that is thrown as a dynamic exception to
-- interrupt the running IO computation when the timeout has
-- expired.

-- | An exception thrown to a thread by 'timeout' to interrupt a timed-out
-- computation.

newtype Timeout = Timeout Unique deriving Eq

-- | @since 4.0
instance Show Timeout where
    show _ = "<<timeout>>"

instance Exception Timeout where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

timeout :: Int -> IO a -> IO (Maybe a)
timeout n f
    | n <  0    = fmap Just f
    | n == 0    = return Nothing
    | otherwise = do
        pid <- myThreadId
        ex  <- fmap Timeout newUnique
        handleJust (\e -> if e == ex then Just () else Nothing)
                   (\_ -> return Nothing)
                   (bracket (forkIOWithUnmask $ \unmask ->
                                 unmask $ waitFor n >> throwTo pid ex)
                            (uninterruptibleMask_ . killThread)
                            (\_ -> fmap Just f))

waitFor :: Int -> IO ()
waitFor n = do
  t0 <- getGCTime
  threadDelay n
  t1 <- getGCTime
  when (t1 > t0) $
    -- allow some extra time because of GC
    waitFor (t1-t0)

getGCTime :: IO Int
getGCTime = fromIntegral . (`div` 1000) . gc_elapsed_ns <$> getRTSStats

-- | unsafeTimeout n a forces the evaluation of a, with a time limit of n microseconds.
unsafeTimeout :: Int -> a -> Maybe a
unsafeTimeout n a = unsafePerformIO $ timeout n $ return $! a