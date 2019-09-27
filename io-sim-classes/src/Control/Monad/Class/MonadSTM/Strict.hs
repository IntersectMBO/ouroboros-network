{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Control.Monad.Class.MonadSTM.Strict
  ( module X
    -- * 'StrictTVar'
  , StrictTVar
  , toLazyTVar
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
   { invariant :: !(a -> Maybe String)
     -- ^ Invariant checked whenever updating the 'StrictTVar'.
   , tvar      :: !(Lazy.LazyTVar m a)
   }

-- | Get the underlying @TVar@
--
-- Since we obviously cannot guarantee that updates to this 'LazyTVar' will be
-- strict, this should be used with caution.
toLazyTVar :: StrictTVar m a -> Lazy.LazyTVar m a
toLazyTVar StrictTVar { tvar } = tvar

newTVar :: MonadSTM m => a -> STM m (StrictTVar m a)
newTVar !a = StrictTVar (const Nothing) <$> Lazy.newTVar a

newTVarM :: MonadSTM m => a -> m (StrictTVar m a)
newTVarM = newTVarWithInvariantM (const Nothing)

newTVarWithInvariantM :: (MonadSTM m, HasCallStack)
                      => (a -> Maybe String) -- ^ Invariant (expect 'Nothing')
                      -> a
                      -> m (StrictTVar m a)
newTVarWithInvariantM invariant !a =
    checkInvariant (invariant a) $
    StrictTVar invariant <$> Lazy.newTVarM a

readTVar :: MonadSTM m => StrictTVar m a -> STM m a
readTVar StrictTVar { tvar } = Lazy.readTVar tvar

writeTVar :: (MonadSTM m, HasCallStack) => StrictTVar m a -> a -> STM m ()
writeTVar StrictTVar { tvar, invariant } !a =
    checkInvariant (invariant a) $
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
  { invariant :: !(a -> Maybe String)
    -- ^ Invariant checked whenever updating the 'StrictTMVar'.
  , tmvar     :: !(Lazy.LazyTMVar m a)
  }

newTMVar :: MonadSTM m => a -> STM m (StrictTMVar m a)
newTMVar !a = StrictTMVar (const Nothing) <$> Lazy.newTMVar a

newTMVarM :: MonadSTM m => a -> m (StrictTMVar m a)
newTMVarM = newTMVarWithInvariantM (const Nothing)

newTMVarWithInvariantM :: (MonadSTM m, HasCallStack)
                       => (a -> Maybe String)  -- ^ Invariant (expect 'Nothing')
                       -> a
                       -> m (StrictTMVar m a)
newTMVarWithInvariantM invariant !a =
    checkInvariant (invariant a) $
    StrictTMVar invariant <$> Lazy.newTMVarM a

newEmptyTMVar :: MonadSTM m => STM m (StrictTMVar m a)
newEmptyTMVar = StrictTMVar (const Nothing) <$> Lazy.newEmptyTMVar

newEmptyTMVarM :: MonadSTM m => m (StrictTMVar m a)
newEmptyTMVarM = newEmptyTMVarWithInvariantM (const Nothing)

newEmptyTMVarWithInvariantM :: MonadSTM m
                            => (a -> Maybe String)  -- ^ Invariant (expect 'Nothing')
                            -> m (StrictTMVar m a)
newEmptyTMVarWithInvariantM invariant =
    StrictTMVar invariant <$> Lazy.newEmptyTMVarM


takeTMVar :: MonadSTM m => StrictTMVar m a -> STM m a
takeTMVar StrictTMVar { tmvar } = Lazy.takeTMVar tmvar

tryTakeTMVar :: MonadSTM m => StrictTMVar m a -> STM m (Maybe a)
tryTakeTMVar StrictTMVar { tmvar } = Lazy.tryTakeTMVar tmvar

putTMVar :: (MonadSTM m, HasCallStack) => StrictTMVar m a -> a -> STM m ()
putTMVar StrictTMVar { tmvar, invariant } !a =
    checkInvariant (invariant a) $
    Lazy.putTMVar tmvar a

tryPutTMVar :: (MonadSTM m, HasCallStack) => StrictTMVar m a -> a -> STM m Bool
tryPutTMVar StrictTMVar { tmvar, invariant } !a =
    checkInvariant (invariant a) $
    Lazy.tryPutTMVar tmvar a

readTMVar :: MonadSTM m => StrictTMVar m a -> STM m a
readTMVar StrictTMVar { tmvar } = Lazy.readTMVar tmvar

tryReadTMVar :: MonadSTM m => StrictTMVar m a -> STM m (Maybe a)
tryReadTMVar StrictTMVar { tmvar } = Lazy.tryReadTMVar tmvar

swapTMVar :: (MonadSTM m, HasCallStack) => StrictTMVar m a -> a -> STM m a
swapTMVar StrictTMVar { tmvar, invariant } !a =
    checkInvariant (invariant a) $
    Lazy.swapTMVar tmvar a

isEmptyTMVar :: MonadSTM m => StrictTMVar m a -> STM m Bool
isEmptyTMVar StrictTMVar { tmvar } = Lazy.isEmptyTMVar tmvar

{-------------------------------------------------------------------------------
  Dealing with invariants
-------------------------------------------------------------------------------}

checkInvariant :: HasCallStack => Maybe String -> m x -> m x
#if CHECK_TVAR_INVARIANT
checkInvariant Nothing    k = k
checkInvariant (Just err) _ = error $ "Invariant violation: " ++ err
#else
checkInvariant _err k       = k
#endif
