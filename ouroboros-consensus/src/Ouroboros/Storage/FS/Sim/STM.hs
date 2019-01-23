{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | 'HasFS' instance using 'MockFS' stored in an STM variable
module Ouroboros.Storage.FS.Sim.STM (
    -- * Mock FS implementation & monad
      SimFS
    , runSimFS
    , simHasFS
    , liftErrSimFS
    ) where

import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Proxy
import           Data.Type.Coercion

import           Control.Monad.Class.MonadSTM

import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.FS.Sim.MockFS (MockFS)
import qualified Ouroboros.Storage.FS.Sim.MockFS as Mock
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling (..))
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

{------------------------------------------------------------------------------
  The simulation-related types
------------------------------------------------------------------------------}

newtype SimFS m a = SimFS { unSimFS :: ReaderT (TVar m MockFS) m a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadThrow
           , MonadCatch
           , MonadMask
           )

type instance FsHandle (SimFS m) = Mock.Handle
data instance Buffer   (SimFS m) = MockBufferUnused

-- | Runs a 'SimFs' computation provided an initial 'MockFS', producing a
-- result, the final state of the filesystem and a sequence of actions occurred
-- in the filesystem.
runSimFS :: MonadSTM m => SimFS m a -> MockFS -> m (a, MockFS)
runSimFS m s = do
    fs  <- atomically (newTVar s)
    a   <- runReaderT (unSimFS m) fs
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

simHasFS :: forall m. MonadSTM m => ErrorHandling FsError m -> HasFS (SimFS m)
simHasFS err = HasFS {
      dumpState                = Mock.dumpState
    , newBuffer                = \_ -> return MockBufferUnused
    , hOpen                    = Mock.hOpen                    err'
    , hClose                   = Mock.hClose                   err'
    , hSeek                    = Mock.hSeek                    err'
    , hGet                     = Mock.hGet                     err'
    , hPut                     = Mock.hPut                     err'
    , hPutBuffer               = Mock.hPutBuffer               err'
    , hTruncate                = Mock.hTruncate                err'
    , hGetSize                 = Mock.hGetSize                 err'
    , createDirectory          = Mock.createDirectory          err'
    , createDirectoryIfMissing = Mock.createDirectoryIfMissing err'
    , listDirectory            = Mock.listDirectory            err'
    , doesDirectoryExist       = Mock.doesDirectoryExist       err'
    , doesFileExist            = Mock.doesFileExist            err'
    , hasFsErr                 = err'
    }
  where
    err' :: ErrorHandling FsError (SimFS m)
    err' = liftErrSimFS err

liftErrSimFS :: forall e m. ErrorHandling e m -> ErrorHandling e (SimFS m)
liftErrSimFS = EH.liftErrNewtype Coercion
             . EH.liftErrReader (Proxy @(TVar m MockFS))
