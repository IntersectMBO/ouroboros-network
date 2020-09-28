{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

-- | Bounds, either user-specified or computed.

module Test.QuickCheck.Randomized.Common.Bounds (
  -- * Bounds
  Leniency (..),
  MaxBound (..),
  MinBound (..),
  complementMaxBound,
  complementMinBound,
  tradeLeniency,
  tradeMaxBound,
  tradeMinBound,
  ) where

import           Data.Proxy
import           Data.Semigroup (Max (..), Min (..))
import           Data.Type.Coercion

import           GHC.Generics
import           Quiet (Quiet (..))

import           Test.QuickCheck.Randomized.Common.Complements
import           Test.QuickCheck.Randomized.Common.Count
import           Test.QuickCheck.Randomized.Common.Probability
import           Test.QuickCheck.Randomized.Common.Validation

{-------------------------------------------------------------------------------
  Boundaries
-------------------------------------------------------------------------------}

-- | The left argument to @<=@.
newtype MinBound a = MinBound {unMinBound :: a}
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Validate)
  deriving (Semigroup) via Max a
  deriving (Show) via Quiet (MinBound a)

instance Monoid (MinBound (Count_ pol ev)) where mempty = MinBound 0
instance Monoid (MinBound (Prob_  pol ev)) where mempty = MinBound 0

-- | The right argument to @<=@.
newtype MaxBound a = MaxBound {unMaxBound :: a}
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Validate)
  deriving (Semigroup) via Min a
  deriving (Show) via Quiet (MaxBound a)

instance Monoid (MaxBound (Prob_ pol ev)) where mempty = MaxBound 1

-- | This is an additive difference from some nominal value.
--
-- It's a /positive/ value, interpretable as the 'MaxBound' of some /absolute/
-- /difference/.
newtype Leniency a = Leniency {unLeniency :: a}
  deriving stock (Generic)
  deriving newtype (Eq, Ord)
  deriving (Semigroup) via Min a
  deriving (Show) via Quiet (Leniency a)

instance Monoid (Leniency (Prob_ pol ev)) where mempty = Leniency (Prob 1)

-- | Positive
instance (Num a, Ord a, Show a) => Validate (Leniency a) where
  validate (Leniency diff) =
      invalidityUnless (0 < diff) $
      "Invalid Leniency! " <> show diff <> " is nonpositive."

-- | A 'MinBound' for some event typically determines a 'MaxBound' on its
--
-- EG @'MinBound' ('Prob' ev) -> 'MaxBound' ('ProbNot' ev)@
complementMinBound :: forall pol1 pol2 k f (ev :: k).
     Complements pol1 pol2
  => (f pol1 ev -> f pol2 ev)
  -> MinBound (f pol1 ev)
  -> MaxBound (f pol2 ev)
complementMinBound complement =
    MaxBound . complement . unMinBound
  where
    _ = complements (Proxy :: Proxy pol1) (Proxy :: Proxy pol2)

-- | See 'complementMinBound'.
complementMaxBound :: forall pol1 pol2 k f (ev :: k).
     Complements pol1 pol2
  => (f pol1 ev -> f pol2 ev)
  -> MaxBound (f pol1 ev)
  -> MinBound (f pol2 ev)
complementMaxBound complement =
    MinBound . complement . unMaxBound
  where
    _ = complements (Proxy :: Proxy pol1) (Proxy :: Proxy pol2)

-- | A 'MinBound' for some event is also a 'MinBound' on the complement of its
-- complement.
--
-- EG @'MinBound' ('Prob' ev) ``Coercion`` 'MinBound' ('ProbNot' ('Not' ev))@
tradeMinBound :: forall pol1 pol2 k f (ev1 :: k) (ev2 :: k).
     (Complements pol1 pol2, Complements ev1 ev2)
  => (f pol1 ev1 `Coercion` f pol2 ev2)
  -> MinBound (f pol1 ev1) `Coercion` MinBound (f pol2 ev2)
tradeMinBound Coercion =
    Coercion
  where
    _ = complements (Proxy :: Proxy pol1) (Proxy :: Proxy pol2)
    _ = complements (Proxy :: Proxy ev1)  (Proxy :: Proxy ev2)

-- | See 'tradeMinBound'.
tradeMaxBound :: forall pol1 pol2 k f (ev1 :: k) (ev2 :: k).
     (Complements pol1 pol2, Complements ev1 ev2)
  => (f pol1 ev1 `Coercion` f pol2 ev2)
  -> MaxBound (f pol1 ev1) `Coercion` MaxBound (f pol2 ev2)
tradeMaxBound Coercion =
    Coercion
  where
    _ = complements (Proxy :: Proxy pol1) (Proxy :: Proxy pol2)
    _ = complements (Proxy :: Proxy ev1)  (Proxy :: Proxy ev2)

-- | See 'tradeMinBound'.
tradeLeniency :: forall pol1 pol2 k f (ev1 :: k) (ev2 :: k).
     (Complements pol1 pol2, Complements ev1 ev2)
  => (f pol1 ev1 `Coercion` f pol2 ev2)
  -> Leniency (f pol1 ev1) `Coercion` Leniency (f pol2 ev2)
tradeLeniency Coercion =
    Coercion
  where
    _ = complements (Proxy :: Proxy pol1) (Proxy :: Proxy pol2)
    _ = complements (Proxy :: Proxy ev1)  (Proxy :: Proxy ev2)
