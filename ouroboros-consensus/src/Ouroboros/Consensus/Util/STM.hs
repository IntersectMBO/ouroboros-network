{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Ouroboros.Consensus.Util.STM (
    -- * Misc
    blockUntilChanged
  , onEachChange
  , runWhenJust
  , blockUntilJust
  , blockUntilAllJust
  , Fingerprint (..)
    -- * Simulate various monad stacks in STM
  , Sim
  , simId
  , simReaderT
  , simWriterT
  , simStateT
  , simOuroborosStateT
  , simChaChaT
  ) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer

import           Data.Void (Void)
import           Data.Word (Word64)
import           GHC.Stack

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork (MonadFork)
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow

import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.Random
import           Ouroboros.Consensus.Util.ResourceRegistry

{-------------------------------------------------------------------------------
  Misc
-------------------------------------------------------------------------------}

-- | Wait until the TVar changed
blockUntilChanged :: forall m a b. (MonadSTM m, Eq b)
                  => (a -> b) -> b -> STM m a -> STM m (a, b)
blockUntilChanged f b getA = do
    a <- getA
    let b' = f a
    if b' == b
      then retry
      else return (a, b')

-- | Spawn a new thread that executes an action each time an STM value changes.
--
-- NOTE: STM does not guarantee that 'onEachChange' will /literally/ be called
-- on /every/ change: when the system is under heavy load, some updates may
-- be missed.
--
-- The life time of the child thread (the thread executing the action) is tied
-- to the lifetime of the registry. Typically this registry is local to the
-- parent thread (the thread calling 'onEachChange'), though if that parent
-- thread was created using 'forkLinkedTransfer', the lifetime of the child
-- thread will be tied to a (great) grandparent thread.
--
-- Moreover, the child thread is linked to the parent one, so that if the action
-- throws an exception the parent thread is also killed.
--
-- The action is passed a registry which it can use to allocate resources that
-- should survive past the end of each action. These resources will be cleaned
-- up only when the parent thread terminates.
--
-- If the action creates resources that should only be alive during the action
-- itself, it should create its own local registry using
-- 'Ouroboros.Consensus.Util.ResourceRegistry.with'.
onEachChange :: forall m a b. (
                    MonadAsync m
                  , MonadMask  m
                  , MonadFork  m
                  , Eq b
                  , HasCallStack
                  )
             => ResourceRegistry m
             -> (a -> b)  -- ^ Obtain a fingerprint
             -> Maybe b   -- ^ Optional initial fingerprint, if 'Nothing', the
                          -- action is executed once immediately to obtain the
                          -- initial fingerprint.
             -> STM m a
             -> (ResourceRegistry m -> a -> m ())
             -> m ()
onEachChange registry f mbInitB getA notify = do
    -- No point using 'forkLinked', since 'go' never terminates
    void $ forkLinked registry $ with $ \registry' -> do
      initB <- case mbInitB of
        Just initB -> return initB
        Nothing    -> do
          a <- atomically getA
          notify registry' a
          return $ f a
      go registry' initB
  where
    go :: ResourceRegistry m -> b -> m Void
    go registry' b = do
      (a, b') <- atomically $ blockUntilChanged f b getA
      notify registry' a
      go registry' b'

-- | Spawn a new thread that waits for an STM value to become 'Just'
--
-- The lifetime of the child thread (the one waiting for the STM value) is tied
-- to the lifetime of the input ("parent") resource registry. The action is
-- provided a "child" resource registry which it can use to allocate resources
-- that should  survive past the lifetime of the action; such resources will be
-- transferred the parent registry.
--
-- The child thread is linked to the parent one (the one calling 'runWhenJust'),
-- so that if the child thread throws an exception, the parent thread is killed
-- also.
runWhenJust :: ( MonadMask  m
               , MonadFork  m
               , MonadAsync m
               , HasCallStack
               )
            => ResourceRegistry m
            -> STM m (Maybe a)
            -> (ResourceRegistry m -> a -> m ())
            -> m ()
runWhenJust registry getMaybeA action =
    void $ forkLinkedTransfer registry $ \registry' -> do
      a <- atomically $ blockUntilJust getMaybeA
      action registry' a

blockUntilJust :: MonadSTM m => STM m (Maybe a) -> STM m a
blockUntilJust getMaybeA = do
    ma <- getMaybeA
    case ma of
      Nothing -> retry
      Just a  -> return a

blockUntilAllJust :: MonadSTM m => [STM m (Maybe a)] -> STM m [a]
blockUntilAllJust = mapM blockUntilJust

-- | Simple type that can be used to indicate something in a 'TVar' is
-- changed.
newtype Fingerprint = Fingerprint Word64
  deriving (Show, Eq, Enum)

{-------------------------------------------------------------------------------
  Simulate monad stacks
-------------------------------------------------------------------------------}

type Sim n m = forall a. n a -> STM m a

simId :: Sim (STM m) m
simId = id

simReaderT :: MonadSTM m => TVar m st -> Sim n m -> Sim (ReaderT st n) m
simReaderT tvar k (ReaderT f) = do
    st <- readTVar tvar
    k (f st)

simWriterT :: MonadSTM m => TVar m st -> Sim n m -> Sim (WriterT st n) m
simWriterT tvar k (WriterT f) = do
    (a, st') <- k f
    writeTVar tvar st'
    return a

simStateT :: MonadSTM m => TVar m st -> Sim n m -> Sim (StateT st n) m
simStateT tvar k (StateT f) = do
    st       <- readTVar tvar
    (a, st') <- k (f st)
    writeTVar tvar st'
    return a

simOuroborosStateT :: MonadSTM m
                   => TVar m s
                   -> Sim n m
                   -> Sim (NodeStateT_ s n) m
simOuroborosStateT tvar k n = do
    st       <- readTVar tvar
    (a, st') <- k (runNodeStateT n st)
    writeTVar tvar st'
    return a

simChaChaT :: MonadSTM m
           => TVar m ChaChaDRG
           -> Sim n m
           -> Sim (ChaChaT n) m
simChaChaT tvar k n = do
    st       <- readTVar tvar
    (a, st') <- k (runChaChaT n st)
    writeTVar tvar st'
    return a

-- | Example of composition
_exampleComposition :: MonadSTM m
                    => TVar m r
                    -> TVar m w
                    -> TVar m s
                    -> Sim n m
                    -> Sim (ReaderT r (WriterT w (StateT s n))) m
_exampleComposition r w s k = simReaderT r $ simWriterT w $ simStateT s $ k
