{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}

-- | Type-level counting
module Ouroboros.Consensus.Util.Counting (
    Exactly(..)
  , AtMost(..)
    -- * Working with 'Exactly'
  , exactlyOne
  , exactlyHead
  , exactlyTail
  , exactlyZip
  , exactlyZipFoldable
  , exactlyWeaken
  , exactlyReplicate
    -- * Working with 'AtMost'
  , atMostOne
  , atMostInit
  , atMostLast
  , atMostZipFoldable
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

deriving instance Functor     (Exactly xs)
deriving instance Foldable    (Exactly xs)
deriving instance Traversable (Exactly xs)

deriving instance Functor     (AtMost xs)
deriving instance Foldable    (AtMost xs)
deriving instance Traversable (AtMost xs)

{-------------------------------------------------------------------------------
  Working with 'Exactly'
-------------------------------------------------------------------------------}

-- | Singleton
exactlyOne :: a -> Exactly '[x] a
exactlyOne a = ExactlyCons a ExactlyNil

-- | Analogue of 'head'
exactlyHead :: Exactly (x ': xs) a -> a
exactlyHead (ExactlyCons a _) = a

-- | Analogue of 'tail'
exactlyTail :: Exactly (x ': xs) a -> Exactly xs a
exactlyTail (ExactlyCons _ as) = as

-- | Analogue of 'zip'
exactlyZip :: Exactly xs a -> Exactly xs b -> Exactly xs (a, b)
exactlyZip = go
  where
    go :: Exactly xs a -> Exactly xs b -> Exactly xs (a, b)
    go ExactlyNil         ExactlyNil         = ExactlyNil
    go (ExactlyCons a as) (ExactlyCons b bs) = ExactlyCons (a, b) $ go as bs

-- | Analogue of 'zip' where the length of second argument is unknown
exactlyZipFoldable :: Foldable t => Exactly xs a -> t b -> AtMost xs (a, b)
exactlyZipFoldable = \as bs -> go as (Foldable.toList bs)
  where
    go :: Exactly xs a -> [b] -> AtMost xs (a, b)
    go _          []             = AtMostNil
    go ExactlyNil _              = AtMostNil
    go (ExactlyCons a as) (b:bs) = AtMostCons (a, b) $ go as bs

exactlyWeaken :: Exactly xs a -> AtMost xs a
exactlyWeaken = go
  where
    go :: Exactly xs a -> AtMost xs a
    go ExactlyNil         = AtMostNil
    go (ExactlyCons x xs) = AtMostCons x (go xs)

-- | Analogue of 'replicate'
--
-- In CPS style because the @xs@ type parameter is not statically known.
exactlyReplicate :: forall a r. Word -> a -> (forall xs. Exactly xs a -> r) -> r
exactlyReplicate = go
  where
    go :: Word -> a -> (forall xs. Exactly xs a -> r) -> r
    go 0 _ k = k ExactlyNil
    go n a k = go (n - 1) a $ \xs -> k (ExactlyCons a xs)

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

-- | Analogue of 'last'
atMostLast :: AtMost xs a -> Maybe a
atMostLast = fmap snd . atMostInit

atMostZipFoldable :: Foldable t => AtMost xs a -> t b -> AtMost xs (a, b)
atMostZipFoldable = \as bs -> go as (Foldable.toList bs)
  where
    go :: AtMost xs a -> [b] -> AtMost xs (a, b)
    go AtMostNil         _      = AtMostNil
    go _                 []     = AtMostNil
    go (AtMostCons a as) (b:bs) = AtMostCons (a, b) (go as bs)
