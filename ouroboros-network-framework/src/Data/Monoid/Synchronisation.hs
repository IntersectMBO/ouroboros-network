{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable          #-}

module Data.Monoid.Synchronisation
  ( FirstToFinish (..)
  , LastToFinish (..)
  , firstToLast
  , lastToFirst
  ) where

import           Data.Coerce (coerce)
import           Data.Functor.Contravariant (Contravariant)
import           Data.Monoid (Alt (..), Ap (..))
import           GHC.Generics (Generic, Generic1)

import           Control.Applicative (Alternative (..))
import           Control.Monad (MonadPlus (..))


-- | First-to-finish synchronisation.  Like 'Alt' it is a monoid under '<|>'.
-- First-to-finish is the additive monoid of
-- a [semiring](https://www.wikiwand.com/en/Semiring) for which multiplication
-- is given by 'LastToFinish'.
--
-- > -- | Return first available value.
-- > --
-- > readFirstMVar :: [TMVar a] -> STM a
-- > readFirstMVar = runFirstToFinish . foldMap (FirstToFinish . readTMVar)
--
--
newtype FirstToFinish m a = FirstToFinish { runFirstToFinish :: m a }
  deriving newtype Functor
  deriving         ( Generic
                   , Generic1
                   , Applicative
                   , Alternative
                   , Monad
                   , MonadPlus
                   , Traversable
                   )
  deriving Semigroup     via (Alt m a)
  deriving Monoid        via (Alt m a)
  deriving Foldable      via (Alt m)
  deriving Contravariant via (Alt m)


-- | Last-to-finish synchronisation. Like 'Ap' it is a monoid under '<*>'.
-- Last-to-finish is the multiplicative monoid of
-- a [semiring](https://www.wikiwand.com/en/Semiring) for which addition is
-- given by 'FirstToFinish', althouth note that the 'LastToFinish' as declared
-- here is not the same as one that is part of the semiring except for the
-- special case of @LastToFinish m ()@.
--
-- > -- | Read all 'MVar's and combine the result using 'Monoid' instance.
-- > --
-- > readAllMVars :: Monoid a => [TMVar a] -> STM a
-- > readAllMVars = runLastToFinish . foldMap (LastToFinish . readTMVar)
--
newtype LastToFinish  m a = LastToFinish  { runLastToFinish  :: m a }
  deriving newtype Functor
  deriving         ( Generic
                   , Generic1
                   , Applicative
                   , Alternative
                   , Monad
                   , MonadPlus
                   , Traversable
                   )
  deriving Semigroup via (Ap m a)
  deriving Monoid    via (Ap m a)
  deriving Foldable  via (Ap m)

firstToLast :: FirstToFinish m a -> LastToFinish m a
firstToLast = coerce

lastToFirst :: LastToFinish m a -> FirstToFinish m a
lastToFirst = coerce
