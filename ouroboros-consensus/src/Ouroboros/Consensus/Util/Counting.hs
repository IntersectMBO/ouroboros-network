{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}

-- | Type-level counting
module Ouroboros.Consensus.Util.Counting (
    Exactly(..)
  , AtMost(..)
    -- * Working with 'AtMost'
  , atMostOne
  , atMostInit
    -- * Working with 'Exactly'
  , exactlyOne
  , exactlyHead
  , exactlyZip
  , exactlyZipAtMost
  ) where

import qualified Data.Foldable as Foldable

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | Exactly one value for each type level index
data Exactly :: [*] -> * -> * where
  ExactlyNil  :: Exactly '[] a
  ExactlyCons :: !a -> !(Exactly xs a) -> Exactly (x ': xs) a

-- | At most one value for each type level index
data AtMost :: [*] -> * -> * where
  AtMostNil  :: AtMost xs a
  AtMostCons :: !a -> !(AtMost xs a) -> AtMost (x ': xs) a

deriving instance Show a => Show (AtMost  xs a)
deriving instance Show a => Show (Exactly xs a)

deriving instance Functor  (Exactly xs)
deriving instance Foldable (Exactly xs)

deriving instance Functor  (AtMost xs)
deriving instance Foldable (AtMost xs)

{-------------------------------------------------------------------------------
  Working with 'Exactly'
-------------------------------------------------------------------------------}

-- | Singleton
exactlyOne :: a -> Exactly '[x] a
exactlyOne a = ExactlyCons a ExactlyNil

-- | Analogue of 'head'
exactlyHead :: Exactly (x ': xs) a -> a
exactlyHead (ExactlyCons a _) = a

-- | Analogue of 'zip'
exactlyZip :: Exactly xs a -> Exactly xs b -> Exactly xs (a, b)
exactlyZip = go
  where
    go :: Exactly xs a -> Exactly xs b -> Exactly xs (a, b)
    go ExactlyNil ExactlyNil                 = ExactlyNil
    go (ExactlyCons a as) (ExactlyCons b bs) = ExactlyCons (a, b) $ go as bs

-- | Analogue of 'zip' where the length of second argument is unknown
exactlyZipAtMost :: Foldable t => Exactly xs a -> t b -> AtMost xs (a, b)
exactlyZipAtMost = \as bs -> go as (Foldable.toList bs)
  where
    go :: Exactly xs a -> [b] -> AtMost xs (a, b)
    go _          []             = AtMostNil
    go ExactlyNil _              = AtMostNil
    go (ExactlyCons a as) (b:bs) = AtMostCons (a, b) $ go as bs

{-------------------------------------------------------------------------------
  Working with 'AtMost'
-------------------------------------------------------------------------------}

-- | Singleton
atMostOne :: a -> AtMost (x ': xs) a
atMostOne x = AtMostCons x AtMostNil

-- | Analogue of 'init'
--
-- For simplicity we don't shrink the type-level index.
atMostInit :: AtMost xs a -> Maybe (AtMost xs a, a)
atMostInit = go
  where
    go :: AtMost xs a -> Maybe (AtMost xs a, a)
    go AtMostNil         = Nothing
    go (AtMostCons a as) = Just $
                             case go as of
                               Nothing        -> (AtMostNil, a)
                               Just (as', a') -> (AtMostCons a as', a')
