{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Ouroboros.Consensus.Util.STM (
    -- * 'Watcher'
    Watcher (..)
  , forkLinkedWatcher
  , withWatcher
    -- * Misc
  , Fingerprint (..)
  , WithFingerprint (..)
  , blockUntilAllJust
  , blockUntilChanged
  , blockUntilJust
  , runWhenJust
    -- * Simulate various monad stacks in STM
  , Sim (..)
  , simId
  , simStateT
  ) where

import           Control.Monad.State
import           Data.Void
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           GHC.Stack

import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry

{-------------------------------------------------------------------------------
  Misc
-------------------------------------------------------------------------------}

-- | Wait until the TVar changed
blockUntilChanged :: forall stm a b. (MonadSTMTx stm, Eq b)
                  => (a -> b) -> b -> stm a -> stm (a, b)
blockUntilChanged f b getA = do
    a <- getA
    let b' = f a
    if b' == b
      then retry
      else return (a, b')

-- | Spawn a new thread that waits for an STM value to become 'Just'
--
-- The thread will be linked to the registry.
runWhenJust :: IOLike m
            => ResourceRegistry m
            -> String  -- ^ Label for the thread
            -> STM m (Maybe a)
            -> (a -> m ())
            -> m ()
runWhenJust registry label getMaybeA action =
    void $ forkLinkedThread registry label $
      action =<< atomically (blockUntilJust getMaybeA)

blockUntilJust :: MonadSTMTx stm => stm (Maybe a) -> stm a
blockUntilJust getMaybeA = do
    ma <- getMaybeA
    case ma of
      Nothing -> retry
      Just a  -> return a

blockUntilAllJust :: MonadSTMTx stm => [stm (Maybe a)] -> stm [a]
blockUntilAllJust = mapM blockUntilJust

-- | Simple type that can be used to indicate something in a @TVar@ is
-- changed.
newtype Fingerprint = Fingerprint Word64
  deriving stock    (Show, Eq, Generic)
  deriving newtype  (Enum)
  deriving anyclass (NoThunks)

-- | Store a value together with its fingerprint.
data WithFingerprint a = WithFingerprint
  { forgetFingerprint :: !a
  , getFingerprint    :: !Fingerprint
  } deriving (Show, Eq, Functor, Generic, NoThunks)

{-------------------------------------------------------------------------------
  Simulate monad stacks
-------------------------------------------------------------------------------}

newtype Sim n m = Sim { runSim :: forall a. n a -> STM m a }

simId :: Sim (STM m) m
simId = Sim id

simStateT :: IOLike m => StrictTVar m st -> Sim n m -> Sim (StateT st n) m
simStateT stVar (Sim k) = Sim $ \(StateT f) -> do
    st       <- readTVar stVar
    (a, st') <- k (f st)
    writeTVar stVar st'
    return a

{-------------------------------------------------------------------------------
  Watchers
-------------------------------------------------------------------------------}

-- | Specification for a thread that watches a variable, and reports interesting
-- changes.
--
-- NOTE: STM does not guarantee that 'wNotify' will /literally/ be called on
-- /every/ change: when the system is under heavy load, some updates may be
-- missed.
data Watcher m a fp = Watcher {
    -- | Obtain a fingerprint from a value of the monitored variable.
    wFingerprint :: a -> fp
    -- | The initial fingerprint
    --
    -- If 'Nothing', the action is executed once immediately to obtain the
    -- initial fingerprint.
  , wInitial     :: Maybe fp
    -- | An action executed each time the fingerprint changes.
  , wNotify      :: a -> m ()
    -- | The variable to monitor.
  , wReader      :: STM m a
  }

-- | Execute a 'Watcher'
--
-- NOT EXPORTED
runWatcher :: forall m a fp. (IOLike m, Eq fp, HasCallStack)
           => Watcher m a fp
           -> m Void
runWatcher watcher = do
    initB <- case mbInitFP of
      Just fp -> return fp
      Nothing -> do
        a <- atomically getA
        notify a
        return $ f a
    loop initB
  where
    Watcher {
        wFingerprint = f
      , wInitial     = mbInitFP
      , wNotify      = notify
      , wReader      = getA
      } = watcher

    loop :: fp -> m Void
    loop fp = do
      (a, fp') <- atomically $ blockUntilChanged f fp getA
      notify a
      loop fp'

-- | Spawn a new thread that runs a 'Watcher'
--
-- The thread will be linked to the registry.
forkLinkedWatcher :: forall m a fp. (IOLike m, Eq fp, HasCallStack)
                  => ResourceRegistry m
                  -> String    -- ^ Label for the thread
                  -> Watcher m a fp
                  -> m (Thread m Void)
forkLinkedWatcher registry label watcher =
    forkLinkedThread registry label $ runWatcher watcher

-- | Spawn a new thread that runs a 'Watcher'
--
-- The thread is bracketed via 'withAsync' and 'link'ed.
--
-- We do not provide the 'Async' handle only because our anticipated use cases
-- don't need it.
withWatcher :: forall m a fp r. (IOLike m, Eq fp, HasCallStack)
            => String    -- ^ Label for the thread
            -> Watcher m a fp
            -> m r
            -> m r
withWatcher label watcher k =
    withAsync
      (do labelThisThread label; runWatcher watcher)
      (\h -> do link h; k)
