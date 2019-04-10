{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Util.STM (
    -- * Misc
    blockUntilChanged
  , onEachChange
  , blockUntilJust
  , blockUntilAllJust
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

import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM

import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.Random

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

-- | Spawn a new thread that executes an action each time a TVar changes
onEachChange :: forall m a b. (MonadSTM m, MonadFork m, Eq b)
             => (a -> b) -> b -> STM m a -> (a -> m ()) -> m ()
onEachChange f initB getA notify = void $ fork $ go initB
  where
    go :: b -> m ()
    go b = do
      (a, b') <- atomically $ blockUntilChanged f b getA
      notify a
      go b'

blockUntilJust :: MonadSTM m => STM m (Maybe a) -> STM m a
blockUntilJust getMaybeA = do
    ma <- getMaybeA
    case ma of
      Nothing -> retry
      Just a  -> return a

blockUntilAllJust :: MonadSTM m => [STM m (Maybe a)] -> STM m [a]
blockUntilAllJust = mapM blockUntilJust

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
