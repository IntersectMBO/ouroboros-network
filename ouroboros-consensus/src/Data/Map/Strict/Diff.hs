{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Data.Map.Strict.Diff (
    Diff (..)
  , DiffEntry (..)
  , DiffHistory (..)
    -- * Anti-diffs
  , AntiDiff (..)
  , lift
  , project
  ) where

import           Data.Group
import qualified Data.Map.Merge.Strict as Merge
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Data.FingerTree.Strict.Alt (TopMeasured (..))

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
--
-- TODO(jdral): Should we use @Seq@s, should consider a different datatype?
-- Does easy access to both ends of the @Seq@ outweigh possible (memory)
-- overheads?
newtype DiffHistory v = DiffHistory (Seq (DiffEntry v))
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NoThunks)
  deriving newtype Semigroup

-- | A change to a value in a key-value store.
--
-- Note: updates are equivalent to inserts, since we consider them to
-- overwrite previous values.
data DiffEntry v = Insert !v | Delete !v
  deriving stock (Generic, Show, Eq, Functor)
  deriving anyclass (NoThunks)

instance Ord k => Semigroup (Diff k v) where
  Diff m1 <> Diff m2 = Diff $ Map.unionWith (<>) m1 m2

instance Ord k => Monoid (Diff k v) where
  mempty = Diff mempty

-- | @'isNonEmptyHisory' h@ checks whether the history is empty.
--
-- In the context of diffs, this function is used to filter empty diff
-- histories from the larger diff, since they are then only inflating the size
-- of the larger diff.
isNonEmptyHistory :: DiffHistory v -> Maybe (DiffHistory v)
isNonEmptyHistory h@(DiffHistory s)
  | Seq.null s = Nothing
  | otherwise  = Just h

-- | @'dropPrefix' pref h@ removes a substring @pref@ from the left end of
-- @h@ if @pref@ is a prefix of @h@.
--
-- In the context of diffs, this function is used when we @mappend@ an inverted
-- sub-diff to the left end of a diff, in which case we truncate diff histories
-- at the left end.
dropPrefix :: Eq v => DiffHistory v -> DiffHistory v -> DiffHistory v
dropPrefix _pref@(DiffHistory xs0) _h@(DiffHistory ys0)=
    DiffHistory $ dropPrefix' xs0 ys0
  where
    dropPrefix' :: Eq a => Seq a -> Seq a -> Seq a
    dropPrefix' Empty Empty = Empty
    dropPrefix' (x :<| xs) (y :<| ys)
      | x == y      = dropPrefix' xs ys
      | otherwise   = error "impossible"
    dropPrefix' _ _ = error "impossible"

-- | @'dropSuffix' h suff@ removes a substring @suff@ from the right end of
-- @h@ if @suff@ is a suffix of @h@.
--
-- In the context of diffs, this function is used when we @mappend@ an inverted
-- sub-diff to the right end of a diff, in which case we truncate diff
-- histories at the right end.
dropSuffix :: Eq v => DiffHistory v -> DiffHistory v -> DiffHistory v
dropSuffix _h@(DiffHistory xs0) _suff@(DiffHistory ys0)=
    DiffHistory $ dropSuffix' xs0 ys0
  where
    dropSuffix' :: Eq a => Seq a -> Seq a -> Seq a
    dropSuffix' Empty Empty = Empty
    dropSuffix' (xs :|> x) (ys :|> y)
      | x == y      = dropSuffix' xs ys
      | otherwise   = error "impossible"
    dropSuffix' _ _ = error "impossible"

{------------------------------------------------------------------------------
  @'Diff'@s as @'Group'@s
------------------------------------------------------------------------------}

{- Note [@mappend@ing negative diffs]

  Lemma:
    -x + (-y) = -(y + x)
  Proof:
    We know that inverses in monoids are unique (see ref), so, if @-x + (-y)@
    is /an/ inverse of @y + x@, then it is /the/ inverse of @y + x@. As such,
    the lemma holds if @(-x + (-y)) + (y + x) = 0@ AND
    @(y + x) + (-x + (-y)) = 0@. We only provide the proof for the case of the
    left inverse, since the proof for the left inverse is symmetric.

      (-x + (-y)) + (y + x)
    = { semigroup law: associativity }
      (-x + (-y + y)) + x
    = { group law: inverse }
      (-x + 0) + x
    = { monoid law: identity element }
      -x + x
    = { group law: inverse }
      0

  ref: https://proofwiki.org/wiki/Inverse_in_Monoid_is_Unique.
-}

-- | A diff for key-value stores that can be inverted.
--
-- TODO(jdral): Consider using @AntiDiffHistory@ instead of
-- @AntiDiff@. What are pros and cons?
-- * @'Map.unionWith'@ does not allow filtering out @mempty@ values like
--    @'Merge.zipWithMaybeMatched'@ does.
data AntiDiff k v = Positive !(Diff k v) | Negative !(Diff k v)
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NoThunks)

lift :: Diff k v -> AntiDiff k v
lift = Positive

project :: AntiDiff k v -> Diff k v
project (Positive d) = d
project _            = error "impossible"

instance (Ord k, Eq v) => Semigroup (AntiDiff k v) where
  Positive d1 <> Positive d2 = Positive $ d1  <>  d2
  Negative d1 <> Positive d2 = Positive $ d1 !<>  d2
  Positive d1 <> Negative d2 = Positive $ d1  <>! d2
  -- We do not expect to @mappend@ negative diffs to negative diffs.
  -- See Note [@mappend@ing negative diffs]
  Negative d1 <> Negative d2 = Negative $ d2  <>  d1

-- | @d1 !<> d2@ ``subtracts'' @d1@ from the left end of @d2@.
--
-- Note: This ``subtraction'' comes down to dropping prefixes of @d2@ at the
-- diff history level. See @'dropPrefix'@.
--
-- PRECONDITION: @d1@ should only contain keys that are in @d2.
(!<>) :: (Ord k, Eq v) => Diff k v -> Diff k v -> Diff k v
(!<>) (Diff m1) (Diff m2) = Diff $
  Merge.merge
    (error "impossible")
    Merge.preserveMissing
    (Merge.zipWithMaybeMatched
      (\_k h1 h2 -> isNonEmptyHistory $ dropPrefix h1 h2)
    )
    m1
    m2

-- | @d1 <>! d2@ ``subtracts'' @d2@ from the right end of @d1@.
--
-- Note: This ``subtraction'' comes down to dropping suffixes of @d1@ at the
-- diff history level. See @'dropSuffix'@.
--
-- PRECONDITION: @d2@ should only contain keys that are in @d1@.
(<>!) :: (Ord k, Eq v) => Diff k v -> Diff k v -> Diff k v
(<>!) (Diff m1) (Diff m2) = Diff $
  Merge.merge
    Merge.preserveMissing
    (error "impossible")
    (Merge.zipWithMaybeMatched
      (\_k h1 h2 -> isNonEmptyHistory $ dropSuffix h1 h2)
    )
    m1
    m2

instance (Ord k, Eq v) => Monoid (AntiDiff k v) where
  mempty = Positive mempty

instance (Ord k, Eq v) => Group (AntiDiff k v) where
  invert (Positive diff) = Negative diff
  invert (Negative diff) = Positive diff

{------------------------------------------------------------------------------
  Measuring @'Diff'@s as @'AntiDiff'@s
------------------------------------------------------------------------------}

instance (Ord k, Eq v) => TopMeasured (AntiDiff k v) (Diff k v) where
  measureTop = lift
