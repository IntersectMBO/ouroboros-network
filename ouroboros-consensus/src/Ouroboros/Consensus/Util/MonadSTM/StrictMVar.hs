{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Ouroboros.Consensus.Util.MonadSTM.StrictMVar (
    StrictMVar(..) -- constructors exported for benefit of tests
  , newMVar
  , newMVarWithInvariant
  , newEmptyMVar
  , newEmptyMVarWithInvariant
  , takeMVar
  , tryTakeMVar
  , putMVar
  , tryPutMVar
  , readMVar
  , readMVarSTM
  , tryReadMVar
  , swapMVar
  , isEmptyMVar
  , updateMVar
  ) where

import           Control.Concurrent.STM (readTVarIO)
import           Control.Monad (when)
import           Control.Monad.Class.MonadSTM (MonadSTM (..))
import qualified Control.Monad.Class.MonadSTM as Lazy
import           Control.Monad.Class.MonadSTM.Strict (checkInvariant)
import           GHC.Stack

import           Cardano.Prelude (NoUnexpectedThunks (..))

{-------------------------------------------------------------------------------
  Strict MVar
-------------------------------------------------------------------------------}

-- | Strict MVar (modelled using a lazy 'Lazy.TMVar' under the hood)
--
-- The 'StrictMVar' API is slightly stronger than the usual 'MVar' one, as we
-- offer a primitive to read the value of the MVar even if it is empty (in which
-- case we will return the oldest known stale one). See 'readMVarSTM'.
--
-- There is a weaker invariant for a 'StrictMVar' than for a 'StrictTVar':
-- although all functions that modify the 'StrictMVar' check the invariant, we
-- do /not/ guarantee that the value inside the 'StrictMVar' always satisfies
-- the invariant. Instead, we /do/ guarantee that if the 'StrictMVar' is updated
-- with a value that does not satisfy the invariant, an exception is thrown. The
-- reason for this weaker guarantee is that leaving an 'MVar' empty can lead to
-- very hard to debug "blocked indefinitely" problems.
--
-- This is also the reason we do not offer support for an invariant in
-- 'StrictTMVar': if we throw an exception from an STM transaction, the STM
-- transaction is not executed, and so we would not even be able to provide the
-- weaker guarantee that we provide for 'StrictMVar'.
data StrictMVar m a = StrictMVar
  { invariant :: !(a -> Maybe String)
    -- ^ Invariant checked whenever updating the 'StrictMVar'.
  , tmvar     :: !(Lazy.TMVar m a)
    -- ^ The main TMVar supporting this 'StrictMVar'
  , tvar      :: !(Lazy.TVar m a)
    -- ^ TVar for supporting 'readMVarSTM'
    --
    -- This TVar is always kept up to date with the 'Lazy.TMVar', but holds on
    -- the old value of the 'Lazy.TMVar' when it is empty. This is very useful
    -- to support single writer/many reader scenarios.
    --
    -- NOTE: We should always update the 'tmvar' before the 'tvar' so that if
    -- the update to the 'tmvar' fails, the 'tvar is left unchanged.
  }

newMVar :: MonadSTM m => a -> m (StrictMVar m a)
newMVar = newMVarWithInvariant (const Nothing)

newMVarWithInvariant :: (MonadSTM m, HasCallStack)
                     => (a -> Maybe String)  -- ^ Invariant (expect 'Nothing')
                     -> a
                     -> m (StrictMVar m a)
newMVarWithInvariant invariant !a =
    checkInvariant (invariant a) $
    StrictMVar invariant <$> Lazy.newTMVarM a <*> Lazy.newTVarM a

newEmptyMVar :: MonadSTM m => a -> m (StrictMVar m a)
newEmptyMVar = newEmptyMVarWithInvariant (const Nothing)

-- | Create an initially empty 'StrictMVar'
--
-- NOTE: Since 'readMVarSTM' allows to read the 'StrictMVar' even when it is
-- empty, we need an initial value of @a@ even though the 'StrictMVar' starts
-- out empty. However, we are /NOT/ strict in this value, to allow it to be
-- @error@.
newEmptyMVarWithInvariant :: MonadSTM m
                          => (a -> Maybe String)  -- ^ Invariant (expect 'Nothing')
                          -> a                    -- ^ The initial stale value
                          -> m (StrictMVar m a)
newEmptyMVarWithInvariant invariant stale =
    StrictMVar invariant <$> Lazy.newEmptyTMVarM <*> Lazy.newTVarM stale

takeMVar :: MonadSTM m => StrictMVar m a -> m a
takeMVar StrictMVar { tmvar } = atomically $ Lazy.takeTMVar tmvar

tryTakeMVar :: MonadSTM m => StrictMVar m a -> m (Maybe a)
tryTakeMVar StrictMVar { tmvar } = atomically $ Lazy.tryTakeTMVar tmvar

putMVar :: (MonadSTM m, HasCallStack) => StrictMVar m a -> a -> m ()
putMVar StrictMVar { tmvar, tvar, invariant } !a = do
    atomically $ do
        Lazy.putTMVar tmvar a
        Lazy.writeTVar tvar a
    checkInvariant (invariant a) $ return ()

tryPutMVar :: (MonadSTM m, HasCallStack) => StrictMVar m a -> a -> m Bool
tryPutMVar StrictMVar { tmvar, tvar, invariant } !a = do
    didPut <- atomically $ do
        didPut <- Lazy.tryPutTMVar tmvar a
        when didPut $ Lazy.writeTVar tvar a
        return didPut
    checkInvariant (invariant a) $ return didPut

readMVar :: MonadSTM m => StrictMVar m a -> m a
readMVar StrictMVar { tmvar } = atomically $ Lazy.readTMVar tmvar

tryReadMVar :: MonadSTM m => StrictMVar m a -> m (Maybe a)
tryReadMVar StrictMVar { tmvar } = atomically $ Lazy.tryReadTMVar tmvar

-- | Read the possibly-stale value of the @MVar@
--
-- Will return the current value of the @MVar@ if it non-empty, or the last
-- known value otherwise.
readMVarSTM :: MonadSTM m => StrictMVar m a -> STM m a
readMVarSTM StrictMVar { tmvar, tvar } = do
    ma <- Lazy.tryReadTMVar tmvar
    case ma of
      Just a  -> return a
      Nothing -> Lazy.readTVar tvar

-- | Swap value of a 'StrictMVar'
--
-- NOTE: Since swapping the value can't leave the 'StrictMVar' empty, we
-- /could/ check the invariant first and only then swap. We nonetheless swap
-- first and check the invariant after to keep the semantics the same with
-- 'putMVar', otherwise it will be difficult to understand when a 'StrictMVar'
-- is updated and when it is not.
swapMVar :: (MonadSTM m, HasCallStack) => StrictMVar m a -> a -> m a
swapMVar StrictMVar { tmvar, tvar, invariant } !a = do
    oldValue <- atomically $ do
        oldValue <- Lazy.swapTMVar tmvar a
        Lazy.writeTVar tvar a
        return oldValue
    checkInvariant (invariant a) $ return oldValue

isEmptyMVar :: MonadSTM m => StrictMVar m a -> m Bool
isEmptyMVar StrictMVar { tmvar } = atomically $ Lazy.isEmptyTMVar tmvar

updateMVar :: (MonadSTM m, HasCallStack) => StrictMVar m a -> (a -> (a, b)) -> m b
updateMVar StrictMVar { tmvar, tvar, invariant } f = do
    (a', b) <- atomically $ do
        a <- Lazy.takeTMVar tmvar
        let !(!a', b) = f a
        Lazy.putTMVar tmvar a'
        Lazy.writeTVar tvar a'
        return (a', b)
    checkInvariant (invariant a') $ return b

{-------------------------------------------------------------------------------
  NoUnexpectedThunks
-------------------------------------------------------------------------------}

instance NoUnexpectedThunks a => NoUnexpectedThunks (StrictMVar IO a) where
  showTypeOf _ = "StrictMVar IO"
  whnfNoUnexpectedThunks ctxt StrictMVar { tvar } = do
      -- We can't use @atomically $ readTVar ..@ here, as that will lead to a
      -- "Control.Concurrent.STM.atomically was nested" exception.
      a <- readTVarIO tvar
      noUnexpectedThunks ctxt a
