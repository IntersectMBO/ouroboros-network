{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Control.Monad.Class.MonadSTM.Strict
  ( module X
    -- * 'StrictTVar'
  , StrictTVar
  , newTVar
  , newTVarM
  , newTVarWithInvariantM
  , readTVar
  , writeTVar
  , modifyTVar
  , updateTVar
    -- * 'StrictTMVar'
  , StrictTMVar
  , newTMVar
  , newTMVarM
  , newTMVarWithInvariantM
  , newEmptyTMVar
  , newEmptyTMVarM
  , newEmptyTMVarWithInvariantM
  , takeTMVar
  , tryTakeTMVar
  , putTMVar
  , tryPutTMVar
  , readTMVar
  , tryReadTMVar
  , swapTMVar
  , isEmptyTMVar
  ) where

import           Control.Exception (assert)
import           Control.Monad.Class.MonadSTM as X hiding (LazyTMVar, LazyTVar,
                     isEmptyTMVar, modifyTVar, newEmptyTMVar, newEmptyTMVarM,
                     newTMVar, newTMVarM, newTVar, newTVarM, putTMVar,
                     readTMVar, readTVar, swapTMVar, takeTMVar, tryPutTMVar,
                     tryReadTMVar, tryTakeTMVar, updateTVar, writeTVar)
import qualified Control.Monad.Class.MonadSTM as Lazy
import           GHC.Stack

{-------------------------------------------------------------------------------
  Strict TVar
-------------------------------------------------------------------------------}

data StrictTVar m a = StrictTVar
   { invariant :: !(a -> Bool)
     -- ^ Invariant checked in an 'assert' whenever storing an @a@ in the
     -- 'StrictTVar'.
   , tvar      :: !(Lazy.LazyTVar m a)
   }

newTVar :: MonadSTM m => a -> STM m (StrictTVar m a)
newTVar !a = StrictTVar (const True) <$> Lazy.newTVar a

newTVarM :: MonadSTM m => a -> m (StrictTVar m a)
newTVarM = newTVarWithInvariantM (const True)

newTVarWithInvariantM :: MonadSTM m
                      => (a -> Bool)  -- ^ Invariant
                      -> a
                      -> m (StrictTVar m a)
newTVarWithInvariantM invariant !a =
    assert (invariant a) $
    StrictTVar invariant <$> Lazy.newTVarM a

readTVar :: MonadSTM m => StrictTVar m a -> STM m a
readTVar StrictTVar { tvar } = Lazy.readTVar tvar

writeTVar :: MonadSTM m => StrictTVar m a -> a -> STM m ()
writeTVar StrictTVar { tvar, invariant } !a =
    assert (invariant a) $
    Lazy.writeTVar tvar a

modifyTVar :: MonadSTM m => StrictTVar m a -> (a -> a) -> STM m ()
modifyTVar v f = readTVar v >>= writeTVar v . f

updateTVar :: MonadSTM m => StrictTVar m a -> (a -> (a, b)) -> STM m b
updateTVar v f = do
    a <- readTVar v
    let (a', b) = f a
    writeTVar v a'
    return b

{-------------------------------------------------------------------------------
  Strict TMVar
-------------------------------------------------------------------------------}

data StrictTMVar m a = StrictTMVar
  { invariant :: !(a -> Bool)
    -- ^ Used in an 'assert' to check whether the given @a@ is in normal form
    -- whenever storing an @a@ in the 'StrictTMVar'.
  , tmvar     :: !(Lazy.LazyTMVar m a)
  }

newTMVar :: MonadSTM m => a -> STM m (StrictTMVar m a)
newTMVar !a = StrictTMVar (const True) <$> Lazy.newTMVar a

newTMVarM :: MonadSTM m => a -> m (StrictTMVar m a)
newTMVarM = newTMVarWithInvariantM (const True)

newTMVarWithInvariantM :: (MonadSTM m, HasCallStack)
                       => (a -> Bool)  -- ^ Invariant
                       -> a
                       -> m (StrictTMVar m a)
newTMVarWithInvariantM invariant !a =
    assert (invariant a) $
    StrictTMVar invariant <$> Lazy.newTMVarM a

newEmptyTMVar :: MonadSTM m => STM m (StrictTMVar m a)
newEmptyTMVar = StrictTMVar (const True) <$> Lazy.newEmptyTMVar

newEmptyTMVarM :: MonadSTM m => m (StrictTMVar m a)
newEmptyTMVarM = newEmptyTMVarWithInvariantM (const True)

newEmptyTMVarWithInvariantM :: MonadSTM m
                            => (a -> Bool)  -- ^ Invariant
                            -> m (StrictTMVar m a)
newEmptyTMVarWithInvariantM invariant =
    StrictTMVar invariant <$> Lazy.newEmptyTMVarM


takeTMVar :: MonadSTM m => StrictTMVar m a -> STM m a
takeTMVar StrictTMVar { tmvar } = Lazy.takeTMVar tmvar

tryTakeTMVar :: MonadSTM m => StrictTMVar m a -> STM m (Maybe a)
tryTakeTMVar StrictTMVar { tmvar } = Lazy.tryTakeTMVar tmvar

putTMVar :: MonadSTM m => StrictTMVar m a -> a -> STM m ()
putTMVar StrictTMVar { tmvar, invariant } !a =
    assert (invariant a) $
    Lazy.putTMVar tmvar a

tryPutTMVar :: MonadSTM m => StrictTMVar m a -> a -> STM m Bool
tryPutTMVar StrictTMVar { tmvar, invariant } !a =
    assert (invariant a) $
    Lazy.tryPutTMVar tmvar a

readTMVar :: MonadSTM m => StrictTMVar m a -> STM m a
readTMVar StrictTMVar { tmvar } = Lazy.readTMVar tmvar

tryReadTMVar :: MonadSTM m => StrictTMVar m a -> STM m (Maybe a)
tryReadTMVar StrictTMVar { tmvar } = Lazy.tryReadTMVar tmvar

swapTMVar :: MonadSTM m => StrictTMVar m a -> a -> STM m a
swapTMVar StrictTMVar { tmvar, invariant } !a =
    assert (invariant a) $
    Lazy.swapTMVar tmvar a

isEmptyTMVar :: MonadSTM m => StrictTMVar m a -> STM m Bool
isEmptyTMVar StrictTMVar { tmvar } = Lazy.isEmptyTMVar tmvar
