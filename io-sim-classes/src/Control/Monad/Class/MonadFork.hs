{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleContexts    #-}

module Control.Monad.Class.MonadFork
  ( MonadFork (..)
  ) where

import qualified Control.Concurrent as IO
import           Control.Exception (Exception(..), SomeException)
import           Control.Monad.Reader
import           System.IO (hFlush, hPutStrLn, stderr)
import           System.IO.Unsafe (unsafePerformIO)

forkPrintExceptionLock :: IO.MVar ()
{-# NOINLINE forkPrintExceptionLock #-}
forkPrintExceptionLock = unsafePerformIO $ IO.newMVar ()

class (Monad m, Eq   (ThreadId m),
                Ord  (ThreadId m),
                Show (ThreadId m)) => MonadFork m where

  type ThreadId m :: *

  fork       :: m () -> m (ThreadId m)
  myThreadId :: m (ThreadId m)
  throwTo    :: Exception e => ThreadId m -> e -> m ()


instance MonadFork IO where
  type ThreadId IO = IO.ThreadId
  fork a =
    let handleException :: Either SomeException () -> IO ()
        handleException (Left e) = do
            tid <- IO.myThreadId
            IO.withMVar forkPrintExceptionLock $ \() -> do
              hPutStrLn stderr $ "Uncaught exception in thread " ++ show tid
                              ++ ": " ++ displayException e
              hFlush stderr
        handleException (Right x) = return x
    in IO.forkFinally a handleException

  myThreadId = IO.myThreadId
  throwTo    = IO.throwTo

instance MonadFork m => MonadFork (ReaderT e m) where
  type ThreadId (ReaderT e m) = ThreadId m
  fork (ReaderT f) = ReaderT $ \e -> fork (f e)
  myThreadId  = lift myThreadId
  throwTo e t = lift (throwTo e t)

