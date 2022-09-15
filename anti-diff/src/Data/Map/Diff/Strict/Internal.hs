{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Data.Map.Diff.Strict.Internal (
    Diff (..)
  , DiffEntry (..)
  , DiffHistory (..)
    -- * Construction
  , diff
  , fromList
  , fromListDeletes
  , fromListInserts
  , fromSeq
  , singletonDelete
  , singletonInsert
    -- * Values and keys
  , Keys (..)
  , Values (..)
  , diffKeys
  , keysFromList
  , restrictValues
  , valuesFromList
    -- * Forwarding keys and values
  , forwardValues
  , forwardValuesAndKeys
    -- * Utilities
  , isNonEmptyHistory
  ) where

import           Prelude hiding (length, splitAt)

import           Data.Bifunctor
import           Data.Group
import qualified Data.Map.Merge.Strict as Merge
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Semigroupoid
import           Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           NoThunks.Class (NoThunks)

{------------------------------------------------------------------------------
  General-purposes diffs for key-value stores
------------------------------------------------------------------------------}

-- | A diff for key-value stores.
--
-- INVARIANT: A key @k@ is present in the @'Map'@, iff the corresponding
-- @'DiffHistory'@ is non-empty. This prevents the @'Map'@ from getting bloated
-- with empty diff histories.
newtype Diff k v = Diff (Map k (DiffHistory v))
  deriving stock (Generic, Show, Eq, Functor)
  deriving anyclass (NoThunks)

-- | A history of changes to a value in a key-value store.
--
-- A history has an implicit sense of ordering according to time: from left to
-- right. This means that the left-most element in the history is the /earliest/
-- change, while the right-most element in the history is the /latest/ change.
newtype DiffHistory v = DiffHistory (Seq (DiffEntry v))
  deriving stock (Generic, Show, Eq, Functor)
  deriving anyclass (NoThunks)

-- | A change to a value in a key-value store.
--
-- Note: The @Anti-@ constructors are only used to cancel out entries in a diff
-- history. These constructors should not be exposed from the module.
data DiffEntry v = Insert !v | Delete !v | AntiInsert !v | AntiDelete !v
  deriving stock (Generic, Show, Eq, Functor)
  deriving anyclass (NoThunks)

{------------------------------------------------------------------------------
  Construction
------------------------------------------------------------------------------}

-- | Compute the difference between two 'Map's.
diff :: Ord k => Map k v -> Map k v -> Diff k v
diff m1 m2 = Diff $
  Merge.merge
    (Merge.mapMissing $ \_k v -> singletonDelete v)
    (Merge.mapMissing $ \_k v -> singletonInsert v)
    (Merge.zipWithMaybeMatched $ \ _k _v1 v2 -> Just $ singletonInsert v2)
    m1
    m2

fromList :: Ord k => [(k, DiffHistory v)] -> Diff k v
fromList = Diff . Map.fromList

fromListInserts :: Ord k => [(k, v)] -> Diff k v
fromListInserts = fromList . fmap (second singletonInsert)

fromListDeletes :: Ord k => [(k, v)] -> Diff k v
fromListDeletes = fromList . fmap (second singletonDelete)

singleton :: DiffEntry v -> DiffHistory v
singleton = DiffHistory . Seq.singleton

singletonInsert :: v -> DiffHistory v
singletonInsert = singleton . Insert

singletonDelete :: v -> DiffHistory v
singletonDelete = singleton . Delete

fromSeq :: DiffEntry v -> Seq (DiffEntry v) -> DiffHistory v
fromSeq x xs = DiffHistory $ x Seq.:<| xs

{------------------------------------------------------------------------------
  Class instances for @'Diff'@
------------------------------------------------------------------------------}

-- Note: The use of @'isNonEmptyHistory'@ prevents the merged @'Diff'@s from
-- getting bloated with empty @'DiffHistory'@s.
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

-- | @h1 <> h2@ sums @h1@ and @h2@ by cancelling out as many consecutive diff
-- entries as possible.
--
-- Diff entries that are each other's inverse can cancel out. In this case, both
-- diff entries are removed the diff history.
--
-- Examples:
--
-- > [Insert 1, AntiDelete 2] <> [Delete 2, AntiInsert 1] = []
--
-- > [Insert 1, Delete 2]     <> [AntiDelete 2, Delete 3] = [Insert 1, Delete 3]
--
-- > [Insert 1, AntiInsert 1] <> [] = [Ins 1, AntiInsert 1]
--
-- Note: This implementation does not make any assumptions about the inputs it
-- is given, and the results it produces. Because of this, the resulting diff
-- history can be nonsenical. To illustrate, consider example 2 given above: how
-- could we expect to delete a value @3@ if we inserted a value @1@? As such, it
-- is the using code's responsibility to ensure that the inputs given to the sum
-- lead to sensible results.
--
-- Note: We do not cancel out consecutive elements in @h1@ and @h2@
-- individually. It is only at the border between @h1@ and @h2@ that we cancel
-- out elements (see the third example given above).
instance Eq v => Semigroup (DiffHistory v) where
  DiffHistory s1 <> DiffHistory s2 = DiffHistory $ s1 `mappend'` s2
    where
      -- At the ``touching'' ends of the sequences, take off diff entries that
      -- are each other's inverse until we find two non-inverse entries. In this
      -- case, we can not continue so we return the concatenated remainders.
      mappend' (xs :|> x) (y :<| ys)
        | areInverses x y                    = mappend' xs ys
      mappend' xs ys                         = xs Seq.>< ys

instance Eq v => Monoid (DiffHistory v) where
  mempty = DiffHistory mempty

instance Eq v => Group (DiffHistory v) where
  invert (DiffHistory s) = DiffHistory $ Seq.reverse . fmap invertDiffEntry $ s

{------------------------------------------------------------------------------
  Utility
------------------------------------------------------------------------------}

-- | @'isNonEmptyHistory' h@ checks whether the history is empty.
--
-- In the context of diffs, this function is used to filter empty diff
-- histories from the larger diff, since they are then only inflating the size
-- of the larger diff.
isNonEmptyHistory :: DiffHistory v -> Maybe (DiffHistory v)
isNonEmptyHistory h@(DiffHistory s)
  | Seq.null s = Nothing
  | otherwise  = Just h

-- | @`invertDiffEntry` e@ inverts a @'DiffEntry' e@ to its counterpart.
--
-- Note: We invert @'DiffEntry'@s, but it is not a @'Group'@: We do not have an
-- identity element, so it is not a @'Monoid'@ or @'Semigroup'@.
invertDiffEntry :: DiffEntry v -> DiffEntry v
invertDiffEntry = \case
  Insert x     -> AntiInsert x
  Delete x     -> AntiDelete x
  AntiInsert x -> Insert x
  AntiDelete x -> Delete x

-- | @'areInverses e1 e2@ checks whether @e1@ and @e2@ are each other's inverse.
--
-- For simplicity, we simply compare the inverse of the first argument to the
-- second argument. That is, inversion should be invertible.
areInverses :: Eq v => DiffEntry v -> DiffEntry v -> Bool
areInverses e1 e2 = invertDiffEntry e1 == e2

{------------------------------------------------------------------------------
  Values and keys
------------------------------------------------------------------------------}

-- | A key-value store.
newtype Values k v = Values (Map k v)
  deriving stock (Generic, Show, Eq, Functor)
  deriving anyclass (NoThunks)

newtype Keys k v = Keys (Set k)
  deriving stock (Generic, Show, Eq, Functor)
  deriving anyclass (NoThunks)

valuesFromList :: Ord k => [(k, v)] -> Values k v
valuesFromList = Values . Map.fromList

keysFromList :: Ord k => [k] -> Keys k v
keysFromList = Keys . Set.fromList

diffKeys :: Diff k v -> Keys k v
diffKeys (Diff m) = Keys $ Map.keysSet m

restrictValues :: Ord k => Values k v -> Keys k v -> Values k v
restrictValues (Values m) (Keys s) = Values (Map.restrictKeys m s)

{------------------------------------------------------------------------------
  Forwarding values and keys
------------------------------------------------------------------------------}

forwardValues :: (Ord k, Eq v, HasCallStack) => Values k v -> Diff k v -> Values k v
forwardValues (Values values) (Diff diffs) = Values $
    Merge.merge
      Merge.preserveMissing
      (Merge.mapMaybeMissing     newKeys)
      (Merge.zipWithMaybeMatched oldKeys)
      values
      diffs
  where
    newKeys :: Eq v => k -> DiffHistory v -> Maybe v
    newKeys _k (DiffHistory Empty) = error "impossible"
    newKeys _k h = case foldToAct h of
      Nothing             -> error "impossible"
      Just (Ins x)        -> Just x
      Just (Del _x)       -> error "impossible"
      Just (DelIns _x _y) -> error "impossible"
      Just InsDel         -> Nothing


    oldKeys :: Eq v => k -> v -> DiffHistory v -> Maybe v
    oldKeys _k _v1 (DiffHistory Empty) = error "impossible"
    oldKeys _k v1 h = case foldToAct h of
      Nothing                  -> error "impossible"
      Just (Ins _x)            -> error "impossible"
      Just (Del x) | x == v1   -> Nothing
                   | otherwise -> error "impossible"
      Just (DelIns x y)
                   | x == v1   -> Just y
                   | otherwise -> error "impossible"
      Just InsDel              -> error "impossible"

forwardValuesAndKeys ::
     (Ord k, Eq v, HasCallStack)
  => Values k v
  -> Keys k v
  -> Diff k v
  -> Values k v
forwardValuesAndKeys v@(Values values) (Keys keys) (Diff diffs) =
  forwardValues
    v
    (Diff $ diffs `Map.restrictKeys` (Map.keysSet values `Set.union` keys))

-- | A diff action to apply to a key-value pair.
data Act v = Del !v | Ins !v | DelIns !v !v | InsDel
  deriving stock (Generic, Show, Eq, Functor)
  deriving anyclass (NoThunks)

instance Eq v => Semigroupoid (Act v) where
    l <>? r = case l of
        Del x -> case r of
            Del{}    -> Nothing   -- disallow double delete
            Ins y    -> Just $ DelIns x y

            DelIns{} -> Nothing   -- disallow double delete

            InsDel   -> Just $ Del x

        Ins x -> case r of
            Del y ->
                if x /= y then Nothing   -- disallow inaccurate delete
                else Just InsDel
            Ins{} -> Nothing   -- disallow overwrite

            DelIns y z ->
                if x /= y then Nothing   -- disallow inaccurate delete
                else Just $ Ins z

            InsDel{} -> Nothing   -- disallow overwrite

        DelIns x y -> case r of
            Del z ->
                if y /= z then Nothing   -- disallow inaccurate delete
                else Just $ Del x
            Ins{} -> Nothing   -- disallow overwrite

            DelIns z aa ->
                if y /= z then Nothing   -- disallow inaccurate delete
                else Just $ DelIns x aa

            InsDel{} -> Nothing   -- disallow overwrite

        InsDel -> case r of
            Del{}    -> Nothing   -- disallow double delete
            Ins x    -> Just $ Ins x

            DelIns{} -> Nothing   -- disallow double delete

            InsDel   -> Just InsDel

instance Eq v => Groupoid (Act v) where
  pinv = \case
      Del v      -> Ins v
      Ins v      -> Del v

      DelIns x y -> DelIns y x

      InsDel     -> InsDel

-- | Given a valid @'DiffHistory'@, its @'DiffEntry'@s should fold to a sensible
-- @'Act'@.
--
-- Note: Only @'Insert'@s and @'Delete'@s translate to an @'Act'@.
foldToAct :: Eq v => DiffHistory v -> Maybe (Act v)
foldToAct (DiffHistory Seq.Empty) = error "Impossible: Invariant"
foldToAct (DiffHistory (z Seq.:<| zs)) = foldl (\x y -> pappendM x (fromDiffEntry y)) (fromDiffEntry z) zs
  where
    fromDiffEntry = \case
      Insert x      -> Just $ Ins x
      Delete x      -> Just $ Del x
      AntiInsert _x -> Nothing
      AntiDelete _x -> Nothing
