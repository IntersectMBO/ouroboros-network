{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Ouroboros.Network.MonadClass.MonadSTM
  ( MonadSTM (..)
  , MonadTBQueue (..)
  ) where

import qualified Control.Concurrent.STM.TVar as STM
import qualified Control.Concurrent.STM.TBQueue as STM
import qualified Control.Monad.STM as STM
import           Numeric.Natural (Natural)

import           Ouroboros.Network.MonadClass.MonadFork

class (MonadFork m, Monad (Tr m)) => MonadSTM m where
  type Tr   m = (n :: * -> *) | n -> m -- STM transactions
  type TVar m :: * -> *

  atomically   :: Tr m a -> m a
  newTVar      :: a -> Tr m (TVar m a)
  readTVar     :: TVar m a -> Tr m a
  writeTVar    :: TVar m a -> a -> Tr m ()
  modifyTVar   :: TVar m a -> (a -> a) -> Tr m ()
  modifyTVar  v f = readTVar v >>= writeTVar v . f
  modifyTVar'  :: TVar m a -> (a -> a) -> Tr m ()
  modifyTVar' v f = do
    a <- readTVar v
    writeTVar v $! f a
  retry        :: Tr m a
--orElse       :: Tr m a -> Tr m a -> Tr m a --TODO

  check        :: Bool -> Tr m ()
  check True = return ()
  check _    = retry

instance MonadSTM IO where
  type Tr   IO = STM.STM
  type TVar IO = STM.TVar

  atomically  = STM.atomically
  newTVar     = STM.newTVar
  readTVar    = STM.readTVar
  writeTVar   = STM.writeTVar
  retry       = STM.retry
  modifyTVar  = STM.modifyTVar
  modifyTVar' = STM.modifyTVar'
  check       = STM.check

class MonadSTM m => MonadTBQueue m where
    type TBQueue m :: * -> *

    newTBQueue     :: Natural   -> Tr m (TBQueue m a)
    readTBQueue    :: TBQueue m a -> Tr m a
    writeTBQueue   :: TBQueue m a -> a -> Tr m ()
    lengthTBQueue  :: TBQueue m a -> Tr m Natural
    isEmptyTBQueue :: TBQueue m a -> Tr m Bool
    isFullTBQueue  :: TBQueue m a -> Tr m Bool

instance MonadTBQueue IO where
    type TBQueue IO = STM.TBQueue

    newTBQueue     = STM.newTBQueue
    readTBQueue    = STM.readTBQueue
    writeTBQueue   = STM.writeTBQueue
    lengthTBQueue  = STM.lengthTBQueue
    isEmptyTBQueue = STM.isEmptyTBQueue
    isFullTBQueue  = STM.isFullTBQueue
