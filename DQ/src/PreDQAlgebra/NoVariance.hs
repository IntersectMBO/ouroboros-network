{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PreDQAlgebra.NoVariance
    ( Natural
    , NoVar (..)
    , Nat (..)
    ) where

import           Data.Monoid (Sum (..))
import           Numeric.Natural (Natural)

import           PreDQAlgebra.Class

data NoVar a = Exact a | Never
    deriving (Show, Read, Eq, Ord)

instance Semigroup a => Semigroup (NoVar a) where
    Exact a <> Exact b = Exact $ a <> b
    Never   <> _       = Never
    _       <> Never   = Never

instance Monoid a => Monoid (NoVar a) where
    mempty = Exact mempty

instance (Monoid a, Ord a) => PreDQAlgebra (NoVar a) where

    firstToFinish = min
    lastToFinish = max
    bottom = Never

newtype Nat = Nat {getNat :: Natural}
    deriving newtype (Show, Read, Eq, Ord)
    deriving (Semigroup, Monoid) via Sum Natural
