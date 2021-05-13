{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Data.Monoid.Synchronisation
  ( FirstToFinish (..)
  , LastToFinish (..)
  , firstToLast
  , lastToFirst
  , LastToFinishM (..)
  , firstToLastM
  , lastToFirstM
  ) where

import           Data.Coerce (coerce)
import           Data.Functor.Contravariant (Contravariant)
import           Data.Monoid (Alt (..), Ap (..))
import           GHC.Generics (Generic, Generic1)

import           Control.Applicative (Alternative (..))
import           Control.Monad (MonadPlus (..))


-- | First-to-finish synchronisation.  Like 'Alt' it is a monoid under '<|>'.
-- First-to-finish is the additive monoid of
-- [near-semiring](https://www.wikiwand.com/en/Near-semiring) for which
-- multiplication is given by 'LastToFinish'.
--
-- > -- | Return first available value.
-- > --
-- > readFirstMVar :: [TMVar a] -> STM a
-- > readFirstMVar = runFirstToFinish
-- >               . foldMap (FirstToFinish . readTMVar)
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


-- | Last-to-finish synchronisation.  It is the multiplicative semigroup of
-- the [near-semiring](https://www.wikiwand.com/en/Near-semiring) for which addition is
-- given by 'FirstToFinish'.
--
-- This is similar to 'Ap' (see 'LastToFinishM') in the sense that it will wait
-- for all monadic effects, but unlike 'Ap' it will not combine all results
-- using a monoid instance, but rather it will return the last one.  Also unlike
-- 'Ap' it does not have a monoidal unit.
--
-- > -- | Read all 'TMVar's and return the one that was filled last.
-- > --
-- > readAllTMVars :: Monoid a => NonEmpty (TMVar a) -> STM a
-- > readAllTMVars = runLastToFinish
-- >               . foldMap1 (LastToFinish . readTMVar)
-- >                 -- from 'semigroupoids' package or use 'foldr1' and 'fmap'
-- >                 -- from 'base'
--
newtype LastToFinish m a = LastToFinish { runLastToFinish :: m a }
  deriving newtype Functor
  deriving         ( Generic
                   , Generic1
                   , Applicative
                   , Alternative
                   , Monad
                   , MonadPlus
                   , Traversable
                   )
  deriving Foldable via (Alt m)

instance MonadPlus m => Semigroup (LastToFinish m a) where
    LastToFinish left <> LastToFinish right = LastToFinish $ do
      a <-  Left  <$> left
        <|> Right <$> right
      case a of
        Left  {} -> right
        Right {} -> left

firstToLast :: FirstToFinish m a -> LastToFinish m a
firstToLast = coerce

lastToFirst :: LastToFinish m a -> FirstToFinish m a
lastToFirst = coerce


-- | Last-to-finish synchronisation. Like 'Ap' it is a monoid under '<*>'.
-- The advantage over 'LastToFinish' is that it has a 'Monoid' instance, but
-- 'a' must be a 'Monoid' as well.  'LastToFinishM' and 'FirstToFinish' form
-- a unitial near-ring when @m ~ STM@.
--
-- > -- | Read all 'TMVar's and combine the result using 'Monoid' instance.
-- > --
-- > readAllTMVars :: Monoid a => [TMVar a] -> STM a
-- > readAllTMVars = runLastToFinish
-- >               . foldMap (LastToFinish . readTMVar)
--
newtype LastToFinishM m a = LastToFinishM { runLastToFinishM :: m a }
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

firstToLastM :: FirstToFinish m a -> LastToFinishM m a
firstToLastM = coerce

lastToFirstM :: LastToFinishM m a -> FirstToFinish m a
lastToFirstM = coerce
