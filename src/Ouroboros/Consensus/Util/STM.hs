{-# LANGUAGE RankNTypes #-}

module Ouroboros.Consensus.Util.STM (
    Sim
  , simId
  , simReaderT
  , simWriterT
  , simStateT
  , simOuroborosStateT
  , simMonadPseudoRandomT
  ) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer

import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.Random
import           Ouroboros.Network.MonadClass

type Sim n m = forall a. n a -> Tr m a

simId :: Sim (Tr m) m
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
                   => TVar m (OuroborosNodeState p)
                   -> Sim n m
                   -> Sim (OuroborosNodeStateT p n) m
simOuroborosStateT tvar k n = do
    st       <- readTVar tvar
    (a, st') <- k (runOuroborosNodeStateT n st)
    writeTVar tvar st'
    return a

simMonadPseudoRandomT :: MonadSTM m
                      => TVar m gen
                      -> Sim n m
                      -> Sim (MonadPseudoRandomT gen n) m
simMonadPseudoRandomT tvar k n = do
    st       <- readTVar tvar
    (a, st') <- k (withDRGT n st)
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
