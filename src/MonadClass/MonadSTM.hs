{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
module MonadClass.MonadSTM
  ( MonadSTM (..) ) where

import qualified Control.Concurrent.STM.TVar as STM
import qualified Control.Monad.STM as STM

import MonadClass.MonadFork

class (MonadFork m, Monad stm) => MonadSTM m stm | m -> stm, stm -> m where
  type TVar m :: * -> *

  atomically   :: stm a -> m a
  newTVar      :: a -> stm (TVar m a)
  readTVar     :: TVar m a -> stm a
  writeTVar    :: TVar m a -> a -> stm ()
  modifyTVar   :: TVar m a -> (a -> a) -> stm ()
  modifyTVar  v f = readTVar v >>= writeTVar v . f
  modifyTVar'  :: TVar m a -> (a -> a) -> stm ()
  modifyTVar' v f = do
    a <- readTVar v
    writeTVar v $! f a
  retry        :: stm a
--orElse       :: stm a -> stm a -> stm a --TODO

  check        :: Bool -> stm ()
  check True  = return ()
  check _     = retry

instance MonadSTM IO STM.STM where
  type TVar IO = STM.TVar

  atomically  = STM.atomically
  newTVar     = STM.newTVar
  readTVar    = STM.readTVar
  writeTVar   = STM.writeTVar
  retry       = STM.retry
  modifyTVar  = STM.modifyTVar
  modifyTVar' = STM.modifyTVar'
  check       = STM.check
