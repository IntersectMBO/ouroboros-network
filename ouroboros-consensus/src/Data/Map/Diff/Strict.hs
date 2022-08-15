{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Data.Map.Diff.Strict (
    Diff (..)
  , DiffEntry (..)
  , DiffHistory (..)
  , singletonDelete
  , singletonInsert
  ) where

import           Data.Group
import qualified Data.Map.Merge.Strict as Merge
import           Data.Map.Strict (Map)
import           Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

{------------------------------------------------------------------------------
  General-purposes diffs for key-value stores
------------------------------------------------------------------------------}

-- | A diff for key-value stores.
newtype Diff k v = Diff (Map k (DiffHistory v))
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NoThunks)

-- | A history of changes to a value in a key-value store.
--
-- A history has an implicit sense of ordering according to time: from left to
-- right. This means that the left-most element in the history is the
-- /earliest/ change, while the right-most element in the history is the
-- /latest/ change.
newtype DiffHistory v = DiffHistory (Seq (DiffEntry v))
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NoThunks)

singleton :: DiffEntry v -> DiffHistory v
singleton = DiffHistory . Seq.singleton

singletonInsert :: v -> DiffHistory v
singletonInsert = singleton . Insert

singletonDelete :: v -> DiffHistory v
singletonDelete = singleton . Delete

-- | A change to a value in a key-value store.
--
-- Note: updates are equivalent to inserts, since we consider them to
-- overwrite previous values.
data DiffEntry v = Insert !v | Delete !v
  deriving stock (Generic, Show, Eq, Functor)
  deriving anyclass (NoThunks)

{------------------------------------------------------------------------------
  Class instances for @'Diff'@
------------------------------------------------------------------------------}

instance (Ord k, Eq v) => Semigroup (Diff k v) where
  Diff m1 <> Diff m2 = Diff $
    Merge.merge
      Merge.preserveMissing
      Merge.preserveMissing
      (Merge.zipWithMaybeMatched
        (\_k h1 h2 -> isNonEmptyHistory $ h1 <> h2)
      )
      m1
      m2

instance (Ord k, Eq v) => Monoid (Diff k v) where
  mempty = Diff mempty

instance (Ord k, Eq v) => Group (Diff k v) where
  invert (Diff m) = Diff $ fmap invert m

{------------------------------------------------------------------------------
  Class instances for @'DiffHistory'@
------------------------------------------------------------------------------}

instance Eq v => Semigroup (DiffHistory v) where
  DiffHistory s1 <> DiffHistory s2 = DiffHistory $ s1 `mappend'` s2
    where
      mappend' Empty Empty                   = Empty
      -- At the ``touching'' ends of the sequences, take off diff entries that
      -- are each other's inverse until we find two non-inverse entries. In this
      -- case, we can not continue so we return the concatenated remainders.
      mappend' xs0@(xs :|> x) ys0@(y :<| ys)
        | areInverses x y                    = mappend' xs ys
        | otherwise                          = xs0 Seq.>< ys0
      mappend' xs ys                         = xs Seq.>< ys

instance Eq v => Monoid (DiffHistory v) where
  mempty = DiffHistory $ mempty

instance Eq v => Group (DiffHistory v) where
  invert (DiffHistory s) = DiffHistory $ Seq.reverse . fmap invertDiffEntry $ s

{------------------------------------------------------------------------------
  Utility
------------------------------------------------------------------------------}

-- | @'isNonEmptyHisory' h@ checks whether the history is empty.
--
-- In the context of diffs, this function is used to filter empty diff
-- histories from the larger diff, since they are then only inflating the size
-- of the larger diff.
isNonEmptyHistory :: DiffHistory v -> Maybe (DiffHistory v)
isNonEmptyHistory h@(DiffHistory s)
  | Seq.null s = Nothing
  | otherwise  = Just h

-- | @`invertDiffEntry` e@ inverts a @'DiffEntry' e@ to its Anti- counterpart.
--
-- Note: We invert @DiffEntry@s, but it is not a @Group@: We do not have an
-- identity element, so it is not a @Monoid@ or @Semigroup@.
invertDiffEntry :: DiffEntry v -> DiffEntry v
invertDiffEntry = \case
  Insert x -> Delete x
  Delete x -> Insert x

-- | @'areInverses e1 e2@ checks whether @e1@ and @e2@ are each other's
-- inverse.
--
-- For simplicity, we simply compare the inverse of the first argument to
-- the second argument. That is, inversion should be invertible.
areInverses :: Eq v => DiffEntry v -> DiffEntry v -> Bool
areInverses e1 e2 = invertDiffEntry e1 == e2
