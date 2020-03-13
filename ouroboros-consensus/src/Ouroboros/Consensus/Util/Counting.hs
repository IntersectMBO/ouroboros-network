{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}

-- | Type-level counting
module Ouroboros.Consensus.Util.Counting (
    AtMost(..)
  , LessThan(..)
  , Exactly(..)
    -- * Working with 'AtMost'
  , atMostInit
  , atMostHead
  , atMostLast
  , atMostMarkLast
  , atMostZipExactly
  , atMostMapMaybe
  , atMostBetween
    -- * Working with 'LessThan'
  , lessThanEmpty
  , lessThanNarrow
    -- * Working with 'Exactly'
  , exactlyHead
  , exactlyZip
  , exactlyZipList
  ) where

import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Void

{-------------------------------------------------------------------------------
  Type-level counting
-------------------------------------------------------------------------------}

-- | At most one value for each type level index, and at least one
data AtMost :: [*] -> * -> * where
  AtMostOne :: !a -> AtMost (x ': xs) a
  AtMostSuc :: !a -> !(AtMost xs a) -> AtMost (x ': xs) a

-- | Strictly less than one value for each type level index (possibly none)
data LessThan :: [*] -> * -> * where
  LessThanOne :: LessThan (x ': xs) a
  LessThanSuc :: !a -> LessThan xs a -> LessThan (x ': xs) a

-- | Exactly one value for each type levle index
data Exactly :: [*] -> * -> * where
  ExactlyOne :: !a -> Exactly '[x] a
  ExactlySuc :: !a -> !(Exactly xs a) -> Exactly (x ': xs) a

deriving instance Show a => Show (AtMost   xs a)
deriving instance Show a => Show (LessThan xs a)
deriving instance Show a => Show (Exactly  xs a)

{-------------------------------------------------------------------------------
  Working with 'AtMost'
-------------------------------------------------------------------------------}

instance Functor (AtMost xs) where
  fmap f (AtMostOne a)    = AtMostOne (f a)
  fmap f (AtMostSuc a as) = AtMostSuc (f a) (fmap f as)

instance Foldable (AtMost xs) where
  foldMap f (AtMostOne a)    = f a
  foldMap f (AtMostSuc a as) = f a <> foldMap f as

-- | Analogue of 'init' on lists
--
-- The type level index can stay the same since it's an upper bound only.
atMostInit :: AtMost xs a -> (Maybe (AtMost xs a), a)
atMostInit (AtMostOne a)    = (Nothing, a)
atMostInit (AtMostSuc a as) =
    case atMostInit as of
      (Nothing , lastA) -> (Just (AtMostOne a), lastA)
      (Just as', lastA) -> (Just (AtMostSuc a as'), lastA)

-- | Analogue of 'head'
atMostHead :: AtMost xs a -> a
atMostHead (AtMostOne a)   = a
atMostHead (AtMostSuc a _) = a

-- | Analogue of 'last'
atMostLast :: AtMost xs a -> a
atMostLast = go
  where
    go :: AtMost xs a -> a
    go (AtMostOne a)    = a
    go (AtMostSuc _ as) = go as

-- | Mark the last value
--
-- Primarily useful in combination with 'atMostBetween'
atMostMarkLast :: AtMost xs a -> AtMost xs (a, Bool)
atMostMarkLast = go
  where
    go :: AtMost xs a -> AtMost xs (a, Bool)
    go (AtMostOne a)    = AtMostOne (a, True)
    go (AtMostSuc a as) = AtMostSuc (a, False) (go as)

-- | Construct a @b@ for every pair of adjacent @a@s
atMostBetween :: forall xs a b. (a -> a -> b) -> AtMost xs a -> LessThan xs b
atMostBetween f = \case
    AtMostOne _    -> LessThanOne
    AtMostSuc a as -> go a as
  where
    go :: forall x xs'. a -> AtMost xs' a -> LessThan (x ': xs') b
    go a (AtMostOne a')    = LessThanSuc (f a a') LessThanOne
    go a (AtMostSuc a' as) = LessThanSuc (f a a') (go a' as)

atMostZipExactly :: Exactly xs a -> AtMost xs b -> AtMost xs (a, b)
atMostZipExactly = go
  where
    go :: Exactly xs a -> AtMost xs b -> AtMost xs (a, b)
    go (ExactlyOne a)    (AtMostOne b)    = AtMostOne (a, b)
    go (ExactlySuc a _)  (AtMostOne b)    = AtMostOne (a, b)
    go (ExactlySuc a as) (AtMostSuc b bs) = AtMostSuc (a, b) (go as bs)
    go (ExactlyOne _)    (AtMostSuc _ bs) = absurd $ atMostEmpty bs

atMostMapMaybe :: forall xs x a b.
                  (a -> Maybe b) -> AtMost xs a -> LessThan (x ': xs) b
atMostMapMaybe f = go
  where
    go :: forall xs' x'. AtMost xs' a -> LessThan (x' ': xs') b
    go (AtMostOne a) =
        case f a of
          Nothing -> LessThanOne
          Just b  -> LessThanSuc b LessThanOne
    go (AtMostSuc a as) =
        case f a of
          Nothing -> LessThanOne
          Just b  -> LessThanSuc b (go as)

atMostEmpty :: AtMost '[] a -> Void
atMostEmpty = \case {}

{-------------------------------------------------------------------------------
  Working with 'LessThan'
-------------------------------------------------------------------------------}

instance Functor (LessThan xs) where
  fmap _ LessThanOne        = LessThanOne
  fmap f (LessThanSuc a as) = LessThanSuc (f a) (fmap f as)

instance Foldable (LessThan xs) where
  foldMap _ LessThanOne        = mempty
  foldMap f (LessThanSuc a as) = f a <> foldMap f as

lessThanEmpty :: LessThan '[] a -> Void
lessThanEmpty = \case {}

-- | Narrow
--
-- The first argument is merely there as a (term-level) counter (a singleton).
lessThanNarrow :: Exactly xs a -> LessThan (x ': xs) b -> Maybe (LessThan xs b)
lessThanNarrow = go
  where
    go :: Exactly xs a -> LessThan (x ': xs) b -> Maybe (LessThan xs b)
    -- We want one value fewer than 1, and we have 0; perfect
    go (ExactlyOne _)    LessThanOne        = Just LessThanOne
    -- We want one value fewer than (n + 1); we have 0; perfect
    go (ExactlySuc _ _)  LessThanOne        = Just LessThanOne
    -- We want one value fewer than 1, but we have at least 1; failure
    go (ExactlyOne _)    (LessThanSuc _ _)  = Nothing
    -- Recurse
    go (ExactlySuc _ as) (LessThanSuc b bs) = (LessThanSuc b) <$> go as bs

{-------------------------------------------------------------------------------
  Working with 'Exactly'
-------------------------------------------------------------------------------}

instance Functor (Exactly xs) where
  fmap f (ExactlyOne a)    = ExactlyOne (f a)
  fmap f (ExactlySuc a as) = ExactlySuc (f a) (fmap f as)

instance Foldable (Exactly xs) where
  foldMap f (ExactlyOne a)    = f a
  foldMap f (ExactlySuc a as) = f a <> foldMap f as

-- | Analogue of 'head'
exactlyHead :: Exactly xs a -> a
exactlyHead (ExactlyOne a)   = a
exactlyHead (ExactlySuc a _) = a

exactlyZip :: Exactly xs a -> Exactly xs b -> Exactly xs (a, b)
exactlyZip = go
  where
    go :: Exactly xs a -> Exactly xs b -> Exactly xs (a, b)
    go (ExactlyOne a)    (ExactlyOne b)    = ExactlyOne (a, b)
    go (ExactlySuc a as) (ExactlySuc b bs) = ExactlySuc (a, b) (go as bs)
    go (ExactlyOne _)    (ExactlySuc _ bs) = absurd $ exactlyEmpty bs
    go (ExactlySuc _ as) (ExactlyOne _)    = absurd $ exactlyEmpty as

exactlyZipList :: Exactly xs a -> NonEmpty b -> AtMost xs (a, b)
exactlyZipList = go
  where
    go :: Exactly xs a -> NonEmpty b -> AtMost xs (a, b)
    go (ExactlyOne a)    (b :| _)         = AtMostOne (a, b)
    go (ExactlySuc a _)  (b :| [])        = AtMostOne (a, b)
    go (ExactlySuc a as) (b :| (b' : bs)) = AtMostSuc (a, b) (go as (b' :| bs))

exactlyEmpty :: Exactly '[] a -> Void
exactlyEmpty = \case {}
