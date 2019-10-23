{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Combinators for projecting node state
module Ouroboros.Consensus.Protocol.HardFork.ProjectState (
    runBefore
  , runAfter
  ) where

import           Crypto.Random (MonadRandom (..))

import           Ouroboros.Consensus.Protocol.Abstract

newtype Before m a = Before { runBefore :: m a }
  deriving (Functor, Applicative, Monad)
newtype After  m a = After  { runAfter  :: m a }
  deriving (Functor, Applicative, Monad)

instance MonadRandom m => MonadRandom (Before m) where
  getRandomBytes = Before . getRandomBytes

instance MonadRandom m => MonadRandom (After m) where
  getRandomBytes = After  . getRandomBytes

instance HasNodeState_ (x, y) m => HasNodeState_ x (Before m) where
  getNodeState   = Before $ fst <$> getNodeState
  putNodeState x = Before $ do (_, y) <- getNodeState ; putNodeState (x, y)

instance HasNodeState_ (x, y) m => HasNodeState_ y (After m) where
  getNodeState   = After  $ snd <$> getNodeState
  putNodeState y = After  $ do (x, _) <- getNodeState ; putNodeState (x, y)
