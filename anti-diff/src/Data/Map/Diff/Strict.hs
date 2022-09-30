{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Data.Map.Diff.Strict (
    Diff (..)
  , DiffEntry (..)
  , DiffHistory (.., DiffHistory)
  , NEDiffHistory (.., NEDiffHistory)
  , UnsafeDiffHistory (..)
  , unDiffHistory
  , unNEDiffHistory
    -- * Conversions between empty and non-empty diff histories
  , nonEmptyDiffHistory
  , toDiffHistory
  , unsafeFromDiffHistory
    -- * Construction
  , diff
  , fromList
  , fromListDeletes
  , fromListEntries
  , fromListInserts
  , singleton
  , singletonDelete
  , singletonInsert
    -- * Values and keys
  , Keys (..)
  , Values (..)
  , castKeys
  , diffKeys
  , keysFromList
  , restrictValues
  , valuesFromList
  , valuesKeys
    -- * Forwarding keys and values
  , Act (..)
  , forwardValues
  , forwardValuesAndKeys
  , traverseActs_
    -- * Folds over actions
  , foldMapAct
  , unsafeFoldMapDiffEntry
  ) where

import           Prelude hiding (length, splitAt)

import           Data.Bifunctor
import           Data.Foldable (toList)
import           Data.Group
import qualified Data.Map.Merge.Strict as Merge
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Semigroupoid
import           Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import           Data.Sequence.NonEmpty (NESeq (..))
import qualified Data.Sequence.NonEmpty as NESeq
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           NoThunks.Class (NoThunks (..), noThunksInValues)

{------------------------------------------------------------------------------
  General-purposes diffs for key-value stores
------------------------------------------------------------------------------}

-- | A diff for key-value stores.
--
-- INVARIANT: A key @k@ is present in the @'Map'@, iff the corresponding
-- @'DiffHistory'@ is non-empty. This prevents the @'Map'@ from getting bloated with
-- empty diff histories.
newtype Diff k v = Diff (Map k (NEDiffHistory v))
  deriving stock (Generic, Show, Eq, Functor)
  deriving anyclass (NoThunks)

-- | A history of changes to a value in a key-value store.
--
-- A history has an implicit sense of ordering according to time: from left to
-- right. This means that the left-most element in the history is the /earliest/
-- change, while the right-most element in the history is the /latest/ change.
newtype UnsafeDiffHistory t v = UnsafeDiffHistory (t (DiffEntry v))
  deriving stock (Generic, Functor, Foldable)

deriving stock instance Show v => Show (UnsafeDiffHistory Seq v)
deriving stock instance Show v => Show (UnsafeDiffHistory NESeq v)
deriving stock instance Eq v => Eq (UnsafeDiffHistory Seq v)
deriving stock instance Eq v => Eq (UnsafeDiffHistory NESeq v)

{-# COMPLETE DiffHistory #-}

newtype DiffHistory v = MkDiffHistory (UnsafeDiffHistory Seq v)
  deriving stock (Generic, Show, Eq, Functor)
  deriving newtype (NoThunks, Foldable)

{-# COMPLETE NEDiffHistory #-}

-- | A non-empty diff history.
newtype NEDiffHistory v = MkNEDiffHistory (UnsafeDiffHistory NESeq v)
  deriving stock (Generic, Show, Eq, Functor)
  deriving newtype (NoThunks, Foldable)

pattern DiffHistory :: Seq (DiffEntry v) -> DiffHistory v
pattern DiffHistory { unDiffHistory } =
  MkDiffHistory (UnsafeDiffHistory unDiffHistory)

pattern NEDiffHistory :: NESeq (DiffEntry v) -> NEDiffHistory v
pattern NEDiffHistory { unNEDiffHistory } =
  MkNEDiffHistory (UnsafeDiffHistory unNEDiffHistory)

-- | Instance for @'Seq'@ checks elements only
--
-- The internal fingertree in @'Seq'@ might have thunks, which is essential for
-- its asymptotic complexity.
instance (NoThunks v, Foldable t) => NoThunks (UnsafeDiffHistory t v) where
  showTypeOf _ = "DiffHistory"
  wNoThunks ctxt = noThunksInValues ctxt . toList

-- | A change to a value in a key-value store.
--
-- Note: The @Anti-@ constructors are only used to cancel out entries in a diff
-- history. These constructors should not be exposed from the module.
data DiffEntry v =
      Insert !v
    | Delete !v
    | UnsafeAntiInsert !v
    | UnsafeAntiDelete !v
  deriving stock (Generic, Show, Eq, Functor, Foldable)
  deriving anyclass (NoThunks)

{------------------------------------------------------------------------------
  Conversions between empty and non-empty diff histories
------------------------------------------------------------------------------}

toDiffHistory :: NEDiffHistory v -> DiffHistory v
toDiffHistory (NEDiffHistory sq) = DiffHistory $ NESeq.toSeq sq

unsafeFromDiffHistory :: DiffHistory v -> NEDiffHistory v
unsafeFromDiffHistory (DiffHistory sq) = NEDiffHistory $ NESeq.unsafeFromSeq sq

nonEmptyDiffHistory :: DiffHistory v -> Maybe (NEDiffHistory v)
nonEmptyDiffHistory (DiffHistory sq) = NEDiffHistory <$> NESeq.nonEmptySeq sq

{------------------------------------------------------------------------------
  Construction
------------------------------------------------------------------------------}

-- | Compute the difference between @'Values'@.
diff :: (Ord k, Eq v) => Values k v -> Values k v -> Diff k v
diff (Values m1) (Values m2) = Diff $
    Merge.merge
      (Merge.mapMissing $ \_k v -> singletonDelete v)
      (Merge.mapMissing $ \_k v -> singletonInsert v)
      (Merge.zipWithMaybeMatched $ \ _k v1 v2 ->
        if v1 == v2 then
          Nothing
        else
          Just $ singletonDelete v1 `unsafeMappend` singletonInsert v2
      )
      m1
      m2
  where
    -- Bypass the @'Semigroupoid'@ instance for @'NEDiffHistory'@, because we
    -- know that @h1@ and @h2@ will not cancel out.
    unsafeMappend (NEDiffHistory h1) (NEDiffHistory h2) =
      NEDiffHistory $ h1 <> h2

fromList :: Ord k => [(k, NEDiffHistory v)] -> Diff k v
fromList = Diff . Map.fromList

fromListEntries :: Ord k => [(k, DiffEntry v)] -> Diff k v
fromListEntries = fromList . fmap (second singleton)

fromListInserts :: Ord k => [(k, v)] -> Diff k v
fromListInserts = fromList . fmap (second singletonInsert)

fromListDeletes :: Ord k => [(k, v)] -> Diff k v
fromListDeletes = fromList . fmap (second singletonDelete)

singleton :: DiffEntry v -> NEDiffHistory v
singleton = NEDiffHistory . NESeq.singleton

singletonInsert :: v -> NEDiffHistory v
singletonInsert = singleton . Insert

singletonDelete :: v -> NEDiffHistory v
singletonDelete = singleton . Delete

{------------------------------------------------------------------------------
  Class instances for @'Diff'@
------------------------------------------------------------------------------}

instance (Ord k, Eq v) => Semigroup (Diff k v) where
  Diff m1 <> Diff m2 = Diff $
    Merge.merge
      Merge.preserveMissing
      Merge.preserveMissing
      (Merge.zipWithMaybeMatched(\_k h1 h2 -> h1 <>? h2))
      m1
      m2

instance (Ord k, Eq v) => Monoid (Diff k v) where
  mempty = Diff mempty

instance (Ord k, Eq v) => Group (Diff k v) where
  invert (Diff m) = Diff $
    fmap (unsafeFromDiffHistory . invert . toDiffHistory) m

{------------------------------------------------------------------------------
  Class instances for @'DiffHistory'@
------------------------------------------------------------------------------}

-- | @h1 <> h2@ sums @h1@ and @h2@ by cancelling out as many consecutive diff
-- entries as possible.
--
-- Diff entries that are each other's inverse can cancel out. In this case, both
-- diff entries are removed from the diff history.
--
-- Examples:
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

-- | @`invertDiffEntry` e@ inverts a @'DiffEntry' e@ to its counterpart.
--
-- Note: We invert @DiffEntry@s, but it is not a @Group@: We do not have an
-- identity element, so it is not a @Monoid@ or @Semigroup@.
invertDiffEntry :: DiffEntry v -> DiffEntry v
invertDiffEntry = \case
  Insert x           -> UnsafeAntiInsert x
  Delete x           -> UnsafeAntiDelete x
  UnsafeAntiInsert x -> Insert x
  UnsafeAntiDelete x -> Delete x

-- | @'areInverses e1 e2@ checks whether @e1@ and @e2@ are each other's inverse.
--
-- For simplicity, we simply compare the inverse of the first argument to the
-- second argument. That is, inversion should be invertible.
areInverses :: Eq v => DiffEntry v -> DiffEntry v -> Bool
areInverses e1 e2 = invertDiffEntry e1 == e2

instance Eq v => Semigroupoid (NEDiffHistory v) where
  dh1 <>? dh2 = nonEmptyDiffHistory (toDiffHistory dh1 <> toDiffHistory dh2)

{------------------------------------------------------------------------------
  Values and keys
------------------------------------------------------------------------------}

-- | A key-value store.
newtype Values k v = Values (Map k v)
  deriving stock (Generic, Show, Eq, Functor)
  deriving newtype (Semigroup, Monoid)
  deriving anyclass (NoThunks)

newtype Keys k v = Keys (Set k)
  deriving stock (Generic, Show, Eq, Functor)
  deriving newtype (Semigroup, Monoid)
  deriving anyclass (NoThunks)

valuesFromList :: Ord k => [(k, v)] -> Values k v
valuesFromList = Values . Map.fromList

keysFromList :: Ord k => [k] -> Keys k v
keysFromList = Keys . Set.fromList

diffKeys :: Diff k v -> Keys k v
diffKeys (Diff m) = Keys $ Map.keysSet m

valuesKeys :: Values k v -> Keys k v
valuesKeys (Values m) = Keys $ Map.keysSet m

restrictValues :: Ord k => Values k v -> Keys k v -> Values k v
restrictValues (Values m) (Keys s) = Values (Map.restrictKeys m s)

castKeys :: Keys k v -> Keys k v'
castKeys (Keys s) = Keys s

{------------------------------------------------------------------------------
  Forwarding values and keys
------------------------------------------------------------------------------}

-- | Forward values through a diff.
--
-- Note: Errors if a fold of a diff history to an action fails.
forwardValues ::
     forall k v. (Ord k, Eq v, HasCallStack)
  => Values k v
  -> Diff k v
  -> Values k v
forwardValues (Values values) (Diff diffs) = Values $
    Merge.merge
      Merge.preserveMissing
      (Merge.mapMaybeMissing     newKeys)
      (Merge.zipWithMaybeMatched oldKeys)
      values
      diffs
  where
    newKeys :: k -> NEDiffHistory v -> Maybe v
    newKeys _k h = case unsafeFoldToAct h of
      Ins x        -> Just x
      Del _x       -> error "impossible"
      DelIns _x _y -> error "impossible"
      InsDel       -> Nothing


    oldKeys :: k -> v -> NEDiffHistory v -> Maybe v
    oldKeys _k v1 h = case unsafeFoldToAct h of
      Ins _x                 -> error "impossible"
      Del x      | x == v1   -> Nothing
                 | otherwise -> error "impossible"
      DelIns x y
                 | x == v1   -> Just y
                 | otherwise -> error "impossible"
      InsDel                 -> error "impossible"

-- | Forwards values through a diff for a specific set of keys.
--
-- Note: Errors if a fold of a diff history to an action fails.
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

{------------------------------------------------------------------------------
  Folding diff entries to concrete actions
------------------------------------------------------------------------------}

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

-- | Given a valid @'NEDiffHistory'@, its @'DiffEntry'@s should fold to a sensible
-- @'Act'@.
--
-- Note: Only @'Insert'@s and @'Delete'@s translate to an @'Act'@.
--
-- Note: For a diff history to be valid, the diff entries in the diff history
-- should not fail to fold to a sensible action.
foldToAct :: Eq v => NEDiffHistory v -> Maybe (Act v)
foldToAct (NEDiffHistory (z NESeq.:<|| zs)) =
    foldl (\x y -> pappendM x (fromDiffEntry y)) (fromDiffEntry z) zs
  where
    fromDiffEntry = \case
      Insert x            -> Just $ Ins x
      Delete x            -> Just $ Del x
      UnsafeAntiInsert _x -> Nothing
      UnsafeAntiDelete _x -> Nothing

-- | Like @'foldToAct'@, but errors if the fold fails.
unsafeFoldToAct :: (Eq v, HasCallStack) => NEDiffHistory v -> Act v
unsafeFoldToAct dh = case foldToAct dh of
  Nothing  -> error "Could not fold diff history to a sensible action."
  Just act -> act

-- | Traverse over folded actions and discard the result.
--
-- Note: Errors if a fold of a diff history to an action fails.
traverseActs_ ::
     (Applicative t, Eq v)
  => (k -> Act v -> t a)
  -> Diff k v
  -> t ()
traverseActs_ f (Diff m) = () <$ Map.traverseWithKey g m
  where
    g k dh = f k (unsafeFoldToAct dh)

foldMapAct :: (Monoid m, Eq v) => (Act v -> m) -> Diff k v -> Maybe m
foldMapAct f (Diff m) = foldMap (fmap f . foldToAct)  m

-- | @'foldMap'@ over the last diff entry in each diff history.
--
-- Deemed unsafe, because the diff history can be invalid and we bypass the
-- validity check.
unsafeFoldMapDiffEntry :: (Monoid m) => (DiffEntry v -> m) -> Diff k v -> m
unsafeFoldMapDiffEntry f (Diff m) =
  foldMap (f . NESeq.last . unNEDiffHistory) m
