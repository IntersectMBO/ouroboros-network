{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Control.Monad.Class.MonadFork
  ( MonadThread (..)
  , MonadFork (..)
  ) where

import qualified Control.Concurrent as IO
import qualified GHC.Conc.Sync as IO (labelThread)
import           Control.Exception (Exception(..), SomeException)
import           Control.Monad.Reader
import           System.IO (hFlush, hPutStrLn, stderr)
import           System.IO.Unsafe (unsafePerformIO)


forkPrintExceptionLock :: IO.MVar ()
{-# NOINLINE forkPrintExceptionLock #-}
forkPrintExceptionLock = unsafePerformIO $ IO.newMVar ()

class (Monad m, Eq   (ThreadId m),
                Ord  (ThreadId m),
                Show (ThreadId m)) => MonadThread m where

  type ThreadId m :: *

  myThreadId     :: m (ThreadId m)
  labelThread    :: ThreadId m -> String -> m ()


class MonadThread m => MonadFork m where

  fork           :: m () -> m (ThreadId m)
  forkWithUnmask :: ((forall a. m a -> m a) -> m ()) -> m (ThreadId m)
  throwTo        :: Exception e => ThreadId m -> e -> m ()


instance MonadThread IO where
  type ThreadId IO = IO.ThreadId
  myThreadId = IO.myThreadId
  labelThread = IO.labelThread

instance MonadFork IO where
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

  forkWithUnmask = IO.forkIOWithUnmask
  throwTo        = IO.throwTo

instance MonadThread m => MonadThread (ReaderT e m) where
  type ThreadId (ReaderT e m) = ThreadId m
  myThreadId  = lift myThreadId
  labelThread t l = lift (labelThread t l)

instance MonadFork m => MonadFork (ReaderT e m) where
  fork (ReaderT f) = ReaderT $ \e -> fork (f e)
  forkWithUnmask k = ReaderT $ \e -> forkWithUnmask $ \restore ->
                       let restore' :: ReaderT e m a -> ReaderT e m a
                           restore' (ReaderT f) = ReaderT $ restore . f
                       in runReaderT (k restore') e
  throwTo e t = lift (throwTo e t)
