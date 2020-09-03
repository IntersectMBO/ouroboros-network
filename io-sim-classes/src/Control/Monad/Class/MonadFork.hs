{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Control.Monad.Class.MonadFork
  ( MonadThread (..)
  , MonadFork (..)
  , labelThisThread
  -- * Deprecated API
  , fork
  , forkWithUnmask
  ) where

import qualified Control.Concurrent as IO
import           Control.Exception (AsyncException (ThreadKilled), Exception)
import           Control.Monad.Reader
import           Data.Kind (Type)
import qualified GHC.Conc.Sync as IO (labelThread)


class (Monad m, Eq   (ThreadId m),
                Ord  (ThreadId m),
                Show (ThreadId m)) => MonadThread m where

  type ThreadId m :: Type

  myThreadId     :: m (ThreadId m)
  labelThread    :: ThreadId m -> String -> m ()


class MonadThread m => MonadFork m where

  forkIO           :: m () -> m (ThreadId m)
  forkIOWithUnmask :: ((forall a. m a -> m a) -> m ()) -> m (ThreadId m)
  throwTo          :: Exception e => ThreadId m -> e -> m ()

  killThread       :: ThreadId m -> m ()
  killThread tid = throwTo tid ThreadKilled

fork :: MonadFork m => m () -> m (ThreadId m)
fork = forkIO
{-# DEPRECATED fork "use forkIO" #-}

forkWithUnmask :: MonadFork m => ((forall a. m a -> m a) ->  m ()) -> m (ThreadId m)
forkWithUnmask = forkIOWithUnmask
{-# DEPRECATED forkWithUnmask "use forkIO" #-}


instance MonadThread IO where
  type ThreadId IO = IO.ThreadId
  myThreadId = IO.myThreadId
  labelThread = IO.labelThread

instance MonadFork IO where
  forkIO           = IO.forkIO
  forkIOWithUnmask = IO.forkIOWithUnmask
  throwTo          = IO.throwTo
  killThread       = IO.killThread

instance MonadThread m => MonadThread (ReaderT e m) where
  type ThreadId (ReaderT e m) = ThreadId m
  myThreadId  = lift myThreadId
  labelThread t l = lift (labelThread t l)

instance MonadFork m => MonadFork (ReaderT e m) where
  forkIO (ReaderT f)   = ReaderT $ \e -> forkIO (f e)
  forkIOWithUnmask k   = ReaderT $ \e -> forkIOWithUnmask $ \restore ->
                       let restore' :: ReaderT e m a -> ReaderT e m a
                           restore' (ReaderT f) = ReaderT $ restore . f
                       in runReaderT (k restore') e
  throwTo e t = lift (throwTo e t)

-- | Apply the label to the current thread
labelThisThread :: MonadThread m => String -> m ()
labelThisThread label = myThreadId >>= \tid -> labelThread tid label
