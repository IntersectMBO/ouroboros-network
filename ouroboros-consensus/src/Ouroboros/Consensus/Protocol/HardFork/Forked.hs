{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}

module Ouroboros.Consensus.Protocol.HardFork.Forked (
    -- * Forked data
    Forked(..)
  , forked
  , forkedPair
  , forkedTriple
  , unsafeBeforeFork
  , unsafeAfterFork
  ) where

import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bitraversable
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Util.Condense

{-------------------------------------------------------------------------------
  Forked Data
-------------------------------------------------------------------------------}

-- | A sum type to represent values before and after a hard fork
data Forked a b = BeforeFork a | AfterFork b
  deriving (Eq, Ord, Show, Generic, NoUnexpectedThunks)

instance Bifunctor Forked where
  bimap f g = forked (BeforeFork . f) (AfterFork . g)

instance Bifoldable Forked where
  bifoldMap = forked

instance Bitraversable Forked where
  bitraverse f g = forked (fmap BeforeFork . f) (fmap AfterFork . g)

instance (Condense a, Condense b) => Condense (Forked a b) where
  condense (BeforeFork a) = condense a
  condense (AfterFork b)  = condense b

forked :: (a -> c) -> (b -> c) -> Forked a b -> c
forked f _ (BeforeFork a) = f a
forked _ g (AfterFork  b) = g b

forkedPair :: Forked a  b
           -> Forked a' b'
           -> Maybe (Forked (a, a') (b, b'))
forkedPair (BeforeFork a) (BeforeFork a') = Just $ BeforeFork (a, a')
forkedPair (AfterFork  b) (AfterFork  b') = Just $ AfterFork  (b, b')
forkedPair _              _               = Nothing

forkedTriple :: Forked a   b
             -> Forked a'  b'
             -> Forked a'' b''
             -> Maybe (Forked (a, a', a'') (b, b', b''))
forkedTriple (BeforeFork a) (BeforeFork a') (BeforeFork a'') = Just $ BeforeFork (a, a', a'')
forkedTriple (AfterFork  b) (AfterFork  b') (AfterFork  b'') = Just $ AfterFork  (b, b', b'')
forkedTriple _              _               _                = Nothing

unsafeBeforeFork :: HasCallStack => Forked a b -> a
unsafeBeforeFork (BeforeFork a) = a
unsafeBeforeFork _              = error "unsafeBeforeFork: Got AfterFork"

unsafeAfterFork :: HasCallStack => Forked a b -> b
unsafeAfterFork (AfterFork b) = b
unsafeAfterFork _             = error "unsafeAfterFork: Got BeforeFork"
