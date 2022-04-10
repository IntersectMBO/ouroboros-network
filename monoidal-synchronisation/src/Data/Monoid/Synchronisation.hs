{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE StandaloneKindSignatures   #-}

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
import           Control.Monad (MonadPlus (..), forever)
import           Control.Monad.STM
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async


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
  deriving Foldable      via (Alt m)
  deriving Contravariant via (Alt m)


instance Semigroup (FirstToFinish IO a) where
    FirstToFinish a <> FirstToFinish b = FirstToFinish $ either id id <$> race a b

instance Monoid (FirstToFinish IO a) where
    mempty = FirstToFinish $ forever $ threadDelay 3_600_000

instance Semigroup (FirstToFinish STM a) where
    FirstToFinish a <> FirstToFinish b = FirstToFinish . getAlt
                                       $ Alt a <|> Alt b

instance Monoid (FirstToFinish STM a) where
    mempty = FirstToFinish . getAlt $ mempty

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
  deriving Foldable via (Ap m)

instance Semigroup (LastToFinish IO a) where
    LastToFinish left <> LastToFinish right = LastToFinish $ do
      withAsync left $ \a ->
        withAsync right $ \b ->
          atomically $ runLastToFinish $
               LastToFinish (waitSTM a)
            <> LastToFinish (waitSTM b)

instance Semigroup (LastToFinish STM a) where
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
-- The advantage over 'LastToFinish' is that it has a 'Monoid' instance for the
-- stm monad, although 'a' must be a 'Monoid'.  'LastToFinishM' and
-- 'FirstToFinish' form a unitial near-ring when @m ~ STM@.
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
  deriving Foldable  via (Ap m)

instance Semigroup (LastToFinishM IO a) where
    LastToFinishM left <> LastToFinishM right = LastToFinishM $ do
      withAsync left $ \a ->
        withAsync right $ \b ->
          atomically $ runLastToFinishM $
               LastToFinishM (waitSTM a)
            <> LastToFinishM (waitSTM b)

instance Semigroup (LastToFinishM STM a) where
    LastToFinishM left <> LastToFinishM right = LastToFinishM $ do
      a <-  Left  <$> left
        <|> Right <$> right
      case a of
        Left  {} -> right
        Right {} -> left

instance Monoid a => Monoid (LastToFinishM STM a) where
    mempty = LastToFinishM (pure mempty)

firstToLastM :: FirstToFinish m a -> LastToFinishM m a
firstToLastM = coerce

lastToFirstM :: LastToFinishM m a -> FirstToFinish m a
lastToFirstM = coerce
