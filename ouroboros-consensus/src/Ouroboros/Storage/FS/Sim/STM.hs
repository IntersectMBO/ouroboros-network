{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | 'HasFS' instance using 'MockFS' stored in an STM variable
module Ouroboros.Storage.FS.Sim.STM (
    -- * Mock FS implementation & monad
      SimFS
    , SimFSE
    , runSimFS
    ) where

import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

import           Control.Monad.Class.MonadSTM

import           Ouroboros.Storage.FS.Class
import           Ouroboros.Storage.FS.Class.Types
import           Ouroboros.Storage.FS.Sim.MockFS (MockFS)
import qualified Ouroboros.Storage.FS.Sim.MockFS as Mock

{------------------------------------------------------------------------------
  The simulation-related types
------------------------------------------------------------------------------}

type SimFSE m = ExceptT FsError (SimFS m)

newtype SimFS m a = SimFS { unSimFs :: ReaderT (TVar m MockFS) m a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadThrow
           , MonadCatch
           , MonadMask
           )

-- | Runs a 'SimFs' computation provided an initial 'MockFS', producing a
-- result, the final state of the filesystem and a sequence of actions occurred
-- in the filesystem.
runSimFS :: MonadSTM m => SimFS m a -> MockFS -> m (a, MockFS)
runSimFS m s = do
    fs  <- atomically (newTVar s)
    a   <- runReaderT (unSimFs m) fs
    fs' <- atomically (readTVar fs)
    return (a, fs')

instance MonadSTM m => MonadState MockFS (SimFS m) where
  state f = SimFS $ ReaderT $ \stateVar -> atomically $ do
      fs <- readTVar stateVar
      let (a, fs') = f fs
      writeTVar stateVar fs'
      return a

instance MonadTrans SimFS where
  lift = SimFS . lift

newtype TrSimFS m a = TrSimFS { trSimFS :: m a }
  deriving (Functor, Applicative, Monad)

instance MonadTrans TrSimFS where
  lift = TrSimFS

instance MonadFork m => MonadFork (SimFS m) where
  fork (SimFS f) = SimFS $ ReaderT $ \e -> fork (runReaderT f e)

instance (MonadFork (SimFS m) , MonadSTM m) => MonadSTM (SimFS m) where
  type Tr (SimFS m)      = TrSimFS (Tr m)
  type TVar (SimFS m)    = TVar m
  type TMVar (SimFS m)   = TMVar m
  type TBQueue (SimFS m) = TBQueue m

  atomically        = lift . atomically . trSimFS
  newTVar           = lift . newTVar
  readTVar          = lift . readTVar
  writeTVar       t = lift . writeTVar t
  retry             = lift   retry

  newTMVar          = lift . newTMVar
  newTMVarIO        = lift . newTMVarIO
  newEmptyTMVar     = lift   newEmptyTMVar
  newEmptyTMVarIO   = lift   newEmptyTMVarIO
  takeTMVar         = lift . takeTMVar
  tryTakeTMVar      = lift . tryTakeTMVar
  putTMVar        t = lift . putTMVar    t
  tryPutTMVar     t = lift . tryPutTMVar t
  swapTMVar       t = lift . swapTMVar   t
  readTMVar         = lift . readTMVar
  tryReadTMVar      = lift . tryReadTMVar
  isEmptyTMVar      = lift . isEmptyTMVar

  newTBQueue        = lift . newTBQueue
  readTBQueue       = lift . readTBQueue
  writeTBQueue    q = lift . writeTBQueue q
#if MIN_VERSION_stm(2,5,0)
  lengthTBQueue     = lift . lengthTBQueue
#endif

instance (MonadMask m, MonadSTM m) => HasFS (SimFSE m) where
    type FsHandle (SimFSE m) = Mock.Handle
    data Buffer   (SimFSE m) = MockBufferUnused

    dumpState                = Mock.dumpState
    newBuffer                = \_ -> return MockBufferUnused
    hOpen                    = Mock.hOpen
    hClose                   = Mock.hClose
    hSeek                    = Mock.hSeek
    hGet                     = Mock.hGet
    hPut                     = Mock.hPut
    hPutBuffer               = Mock.hPutBuffer
    hTruncate                = Mock.hTruncate
    hGetSize                 = Mock.hGetSize
    createDirectory          = Mock.createDirectory
    createDirectoryIfMissing = Mock.createDirectoryIfMissing
    listDirectory            = Mock.listDirectory
    doesDirectoryExist       = Mock.doesDirectoryExist
    doesFileExist            = Mock.doesFileExist
