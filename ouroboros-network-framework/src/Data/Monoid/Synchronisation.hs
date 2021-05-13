{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module Data.Monoid.Synchronisation
  ( FirstToFinish (..)
  , LastToFinish (..)
  , firstToLast
  , lastToFirst
  , FirstToFinishSTM
  , LastToFinishSTM (..)
  , firstToLastSTM
  , lastToFirstSTM
  ) where

import           Data.Coerce (coerce)
import           Data.Functor.Contravariant (Contravariant)
import           Data.Monoid (Alt (..), Ap (..))
import           GHC.Generics (Generic, Generic1)

import           Control.Applicative (Alternative (..))
import           Control.Monad (MonadPlus (..))
import           Control.Monad.Class.MonadSTM


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


-- | The correct last-to-finish synchronisation (i.e. the multiplicative
-- semigroup of the semiring), but restricted to 'STM'.  It is easier to use
-- than 'LastToFinish' since its 'Semigroup' instance does not have any
-- constraints on @a@.
--
newtype LastToFinishSTM m a = LastToFinishSTM { runLastToFinishSTM  :: STM m a }
  deriving ( Generic
           , Generic1
           )

deriving instance Functor     (STM m) => Functor     (LastToFinishSTM m)
deriving instance Applicative (STM m) => Applicative (LastToFinishSTM m)
deriving instance Alternative (STM m) => Alternative (LastToFinishSTM m)
deriving instance Monad       (STM m) => Monad       (LastToFinishSTM m)
deriving instance MonadPlus   (STM m) => MonadPlus   (LastToFinishSTM m)

instance MonadSTM m => Semigroup (LastToFinishSTM m a) where
    LastToFinishSTM stm <> LastToFinishSTM stm' = LastToFinishSTM $ do
      v <- newEmptyTMVar
      (Left <$> stm) `orElse` (Right <$> stm') >>= putTMVar v
      a <- takeTMVar v
      case a of
        Left _  -> stm'
        Right _ -> stm

type FirstToFinishSTM m a = FirstToFinish (STM m) a

firstToLastSTM :: FirstToFinishSTM m a -> LastToFinishSTM m a
firstToLastSTM = coerce

lastToFirstSTM :: LastToFinishSTM m a -> FirstToFinishSTM m a
lastToFirstSTM = coerce


