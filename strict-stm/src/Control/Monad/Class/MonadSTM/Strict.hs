{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TypeFamilies          #-}

-- to preserve 'HasCallstack' constraint on 'checkInvariant'
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Control.Monad.Class.MonadSTM.Strict
  ( module X
  , LazyTVar
  , LazyTMVar
    -- * 'StrictTVar'
  , StrictTVar
  , labelTVar
  , labelTVarIO
  , castStrictTVar
  , toLazyTVar
  , fromLazyTVar
  , newTVar
  , newTVarIO
  , newTVarWithInvariantIO
  , readTVar
  , readTVarIO
  , writeTVar
  , modifyTVar
  , stateTVar
  , swapTVar
    -- * 'StrictTMVar'
  , StrictTMVar
  , labelTMVar
  , labelTMVarIO
  , castStrictTMVar
  , toLazyTMVar
  , fromLazyTMVar
  , newTMVar
  , newTMVarIO
  , newEmptyTMVar
  , newEmptyTMVarIO
  , takeTMVar
  , tryTakeTMVar
  , putTMVar
  , tryPutTMVar
  , readTMVar
  , tryReadTMVar
  , swapTMVar
  , isEmptyTMVar
    -- ** Low-level API
  , checkInvariant
    -- * Deprecated API
  , updateTVar
  , newTVarM
  , newTVarWithInvariantM
  , newTMVarM
  , newEmptyTMVarM
  ) where

import           Control.Monad.Class.MonadSTM as X hiding (LazyTMVar, LazyTVar,
                     TMVar, TVar, isEmptyTMVar, labelTMVar, labelTMVarIO,
                     labelTVar, labelTVarIO, modifyTVar, newEmptyTMVar,
                     newEmptyTMVarIO, newEmptyTMVarM, newTMVar, newTMVarIO,
                     newTMVarM, newTVar, newTVarIO, newTVarM, putTMVar,
                     readTMVar, readTVar, readTVarIO, stateTVar, swapTMVar,
                     swapTVar, takeTMVar, tryPutTMVar, tryReadTMVar,
                     tryTakeTMVar, writeTVar)
import qualified Control.Monad.Class.MonadSTM as Lazy
import           GHC.Stack

{-------------------------------------------------------------------------------
  Lazy TVar
-------------------------------------------------------------------------------}

type LazyTVar  m = Lazy.TVar m
type LazyTMVar m = Lazy.TMVar m

{-------------------------------------------------------------------------------
  Strict TVar
-------------------------------------------------------------------------------}

#if CHECK_TVAR_INVARIANT
data StrictTVar m a = StrictTVar
   { invariant :: !(a -> Maybe String)
     -- ^ Invariant checked whenever updating the 'StrictTVar'.
   , tvar      :: !(LazyTVar m a)
   }
#else
newtype StrictTVar m a = StrictTVar
   { tvar      :: LazyTVar m a
   }
#endif

labelTVar :: MonadLabelledSTM m => StrictTVar m a -> String -> STM m ()
labelTVar StrictTVar { tvar } = Lazy.labelTVar tvar

labelTVarIO :: MonadLabelledSTM m => StrictTVar m a -> String -> m ()
labelTVarIO v = atomically . labelTVar v

castStrictTVar :: LazyTVar m ~ LazyTVar n
               => StrictTVar m a -> StrictTVar n a
castStrictTVar v@StrictTVar {tvar} =
    mkStrictTVar (getInvariant v) tvar

-- | Get the underlying @TVar@
--
-- Since we obviously cannot guarantee that updates to this 'LazyTVar' will be
-- strict, this should be used with caution.
toLazyTVar :: StrictTVar m a -> LazyTVar m a
toLazyTVar StrictTVar { tvar } = tvar

fromLazyTVar :: LazyTVar m a -> StrictTVar m a
fromLazyTVar tvar =
#if CHECK_TVAR_INVARIANT
  StrictTVar { invariant = const Nothing
             , tvar
             }
#else
  StrictTVar { tvar }
#endif

newTVar :: MonadSTM m => a -> STM m (StrictTVar m a)
newTVar !a = (\tvar -> mkStrictTVar (const Nothing) tvar)
         <$> Lazy.newTVar a

newTVarIO :: MonadSTM m => a -> m (StrictTVar m a)
newTVarIO = newTVarWithInvariantIO (const Nothing)

newTVarM :: MonadSTM m => a -> m (StrictTVar m a)
newTVarM = newTVarIO
{-# DEPRECATED newTVarM "Use newTVarIO" #-}

newTVarWithInvariantIO :: (MonadSTM m, HasCallStack)
                       => (a -> Maybe String) -- ^ Invariant (expect 'Nothing')
                       -> a
                       -> m (StrictTVar m a)
newTVarWithInvariantIO  invariant !a =
        checkInvariant (invariant a) $
        (\tvar -> mkStrictTVar invariant tvar)
    <$> Lazy.newTVarIO a

newTVarWithInvariantM :: (MonadSTM m, HasCallStack)
                      => (a -> Maybe String) -- ^ Invariant (expect 'Nothing')
                      -> a
                      -> m (StrictTVar m a)
newTVarWithInvariantM = newTVarWithInvariantIO
{-# DEPRECATED newTVarWithInvariantM "Use newTVarWithInvariantIO" #-}

readTVar :: MonadSTM m => StrictTVar m a -> STM m a
readTVar StrictTVar { tvar } = Lazy.readTVar tvar

readTVarIO :: MonadSTM m => StrictTVar m a -> m a
readTVarIO StrictTVar { tvar } = Lazy.readTVarIO tvar

writeTVar :: (MonadSTM m, HasCallStack) => StrictTVar m a -> a -> STM m ()
writeTVar v !a =
    checkInvariant (getInvariant v a) $
    Lazy.writeTVar (tvar v) a

modifyTVar :: MonadSTM m => StrictTVar m a -> (a -> a) -> STM m ()
modifyTVar v f = readTVar v >>= writeTVar v . f

stateTVar :: MonadSTM m => StrictTVar m s -> (s -> (a, s)) -> STM m a
stateTVar v f = do
    a <- readTVar v
    let (b, a') = f a
    writeTVar v a'
    return b

swapTVar :: MonadSTM m => StrictTVar m a -> a -> STM m a
swapTVar v a' = do
    a <- readTVar v
    writeTVar v a'
    return a


updateTVar :: MonadSTM m => StrictTVar m s -> (s -> (a, s)) -> STM m a
updateTVar = stateTVar
{-# DEPRECATED updateTVar "Use stateTVar" #-}

{-------------------------------------------------------------------------------
  Strict TMVar
-------------------------------------------------------------------------------}

-- 'TMVar' that keeps its value in WHNF at all times
--
-- Does not support an invariant: if the invariant would not be satisfied,
-- we would not be able to put a value into an empty TMVar, which would lead
-- to very hard to debug bugs where code is blocked indefinitely.
newtype StrictTMVar m a = StrictTMVar { toLazyTMVar :: LazyTMVar m a }

fromLazyTMVar :: LazyTMVar m a -> StrictTMVar m a
fromLazyTMVar = StrictTMVar

labelTMVar :: MonadLabelledSTM m => StrictTMVar m a -> String -> STM m ()
labelTMVar (StrictTMVar tvar) = Lazy.labelTMVar tvar

labelTMVarIO :: MonadLabelledSTM m => StrictTMVar m a -> String -> m ()
labelTMVarIO v = atomically . labelTMVar v

castStrictTMVar :: LazyTMVar m ~ LazyTMVar n
                => StrictTMVar m a -> StrictTMVar n a
castStrictTMVar (StrictTMVar var) = StrictTMVar var

newTMVar :: MonadSTM m => a -> STM m (StrictTMVar m a)
newTMVar !a = StrictTMVar <$> Lazy.newTMVar a

newTMVarIO :: MonadSTM m => a -> m (StrictTMVar m a)
newTMVarIO !a = StrictTMVar <$> Lazy.newTMVarIO a

newTMVarM :: MonadSTM m => a -> m (StrictTMVar m a)
newTMVarM = newTMVarIO
{-# DEPRECATED newTMVarM "Use newTVarIO" #-}

newEmptyTMVar :: MonadSTM m => STM m (StrictTMVar m a)
newEmptyTMVar = StrictTMVar <$> Lazy.newEmptyTMVar

newEmptyTMVarIO :: MonadSTM m => m (StrictTMVar m a)
newEmptyTMVarIO = StrictTMVar <$> Lazy.newEmptyTMVarIO

newEmptyTMVarM :: MonadSTM m => m (StrictTMVar m a)
newEmptyTMVarM = newEmptyTMVarIO
{-# DEPRECATED newEmptyTMVarM "Use newEmptyTMVarIO" #-}

takeTMVar :: MonadSTM m => StrictTMVar m a -> STM m a
takeTMVar (StrictTMVar tmvar) = Lazy.takeTMVar tmvar

tryTakeTMVar :: MonadSTM m => StrictTMVar m a -> STM m (Maybe a)
tryTakeTMVar (StrictTMVar tmvar) = Lazy.tryTakeTMVar tmvar

putTMVar :: MonadSTM m => StrictTMVar m a -> a -> STM m ()
putTMVar (StrictTMVar tmvar) !a = Lazy.putTMVar tmvar a

tryPutTMVar :: MonadSTM m => StrictTMVar m a -> a -> STM m Bool
tryPutTMVar (StrictTMVar tmvar) !a = Lazy.tryPutTMVar tmvar a

readTMVar :: MonadSTM m => StrictTMVar m a -> STM m a
readTMVar (StrictTMVar tmvar) = Lazy.readTMVar tmvar

tryReadTMVar :: MonadSTM m => StrictTMVar m a -> STM m (Maybe a)
tryReadTMVar (StrictTMVar tmvar) = Lazy.tryReadTMVar tmvar

swapTMVar :: MonadSTM m => StrictTMVar m a -> a -> STM m a
swapTMVar (StrictTMVar tmvar) !a = Lazy.swapTMVar tmvar a

isEmptyTMVar :: MonadSTM m => StrictTMVar m a -> STM m Bool
isEmptyTMVar (StrictTMVar tmvar) = Lazy.isEmptyTMVar tmvar

{-------------------------------------------------------------------------------
  Dealing with invariants
-------------------------------------------------------------------------------}

getInvariant :: StrictTVar m a -> a -> Maybe String
mkStrictTVar :: (a -> Maybe String) -> Lazy.TVar m a -> StrictTVar m a

-- | Check invariant (if enabled) before continuing
--
-- @checkInvariant mErr x@ is equal to @x@ if @mErr == Nothing@, and throws
-- an error @err@ if @mErr == Just err@.
--
-- This is exported so that other code that wants to conditionally check
-- invariants can reuse the same logic, rather than having to introduce new
-- per-package flags.
checkInvariant :: HasCallStack => Maybe String -> a -> a

#if CHECK_TVAR_INVARIANT
getInvariant StrictTVar {invariant} = invariant
mkStrictTVar invariant  tvar = StrictTVar {invariant, tvar}

checkInvariant Nothing    k = k
checkInvariant (Just err) _ = error $ "Invariant violation: " ++ err
#else
getInvariant _               = \_ -> Nothing
mkStrictTVar _invariant tvar = StrictTVar {tvar}

checkInvariant _err       k  = k
#endif
