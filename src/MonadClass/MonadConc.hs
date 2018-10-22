{-# LANGUAGE TypeFamilies #-}
module MonadClass.MonadConc (
  MonadConc (..),
  newEmptyMVar,
  newMVar
  ) where

import           MonadClass.MonadFork

class MonadFork m => MonadConc m where
  type MVar m :: * -> *

  newEmptyNamedMVar :: Maybe String -> m (MVar m a)
  newNamedMVar :: Maybe String -> a -> m (MVar m a)
  takeMVar     :: MVar m a -> m a
  tryTakeMVar  :: MVar m a -> m (Maybe a)
  putMVar      :: MVar m a -> a -> m ()
  tryPutMVar   :: MVar m a -> a -> m Bool

  readMVar     :: MVar m a -> m a
  readMVar v = do x <- takeMVar v; putMVar v x; return x

  modifyMVar   :: MVar m a -> (a -> m (a, b)) -> m b
  modifyMVar v a = do
    x <- takeMVar v
    (x', y) <- a x
    putMVar v x'
    return y

  modifyMVar_   :: MVar m a -> (a -> m a) -> m ()
  modifyMVar_ v a = do
    x  <- takeMVar v
    x' <- a x
    putMVar v x'

newEmptyMVar :: MonadConc m => m (MVar m a)
newEmptyMVar = newEmptyNamedMVar Nothing

newMVar :: MonadConc m => a -> m (MVar m a)
newMVar = newNamedMVar Nothing

