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
{-# LANGUAGE ViewPatterns               #-}

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
    -- * Predicates
  , null
    -- * Class instances for @'DiffHistory'@
  , areInverses
    -- * Values and keys
  , Keys (..)
  , Values (..)
  , castKeys
  , diffKeys
  , keysFromList
  , restrictValues
  , valuesFromList
  , valuesKeys
    -- * Applying diffs
  , ApplyDiffError (..)
  , applyDiff
  , applyDiffForKeys
  , applyDiffForKeysScrutinous
  , applyDiffScrutinous
    -- * Folds over actions
  , Act (..)
  , foldMapAct
  , traverseActs_
  , unsafeFoldMapDiffEntry
  ) where

import           Prelude hiding (last, length, null, splitAt)

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
-- right. This means that the leftmost element in the history is the /earliest/
-- change, while the rightmost element in the history is the /latest/ change.
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
          Just $ singletonDelete v1 `unsafeAppend` singletonInsert v2
      )
      m1
      m2
  where
    unsafeAppend (NEDiffHistory h1) (NEDiffHistory h2) =
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
  Deconstruction
------------------------------------------------------------------------------}

last :: NEDiffHistory v -> DiffEntry v
last (unNEDiffHistory -> _ NESeq.:||> e) = e

{------------------------------------------------------------------------------
  Predicates
------------------------------------------------------------------------------}

null :: Diff k v -> Bool
null (Diff m) = Map.null m


{------------------------------------------------------------------------------
  Class instances for @'Diff'@
------------------------------------------------------------------------------}

instance (Ord k, Eq v) => Semigroup (Diff k v) where
  Diff m1 <> Diff m2 = Diff $
    Merge.merge
      Merge.preserveMissing
      Merge.preserveMissing
      (Merge.zipWithMaybeMatched(\_k h1 h2 ->
        nonEmptyDiffHistory (toDiffHistory h1 <> toDiffHistory h2)
      ))
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
  Applying diffs
------------------------------------------------------------------------------}

-- | Applies a diff to values.
--
-- This function throws an error if an @Unsafe@ diff entry like
-- @'UnsafeAntiInsert'@ or @'UnsafeAntiDelete'@ is found. These @Unsafe@ diff
-- entries can not be applied, but are necessary for the @'Group'@ instance of
-- the @'Diff'@ datatype.
--
-- FIXME(jdral): In this section, we distinguish between scrutinous and
-- non-scrutinous application of diffs. For now, `consensus` libraries uses the
-- non-scrutinous version, since the scrutinous version will consistently throw
-- errors as sanity checks fail due to era translations and the multi-era nature
-- of on-disk ledger values like the UTxO. In particular, a UTxO that is read
-- from disk can either be from the current era, but also any one era before it.
-- A UTxO is translated to the current era such that we can use it in the
-- current era's ledger rules, but this translation information is then lost. A
-- translation to a new era is essentially an update, and as such should be
-- reflected in diffs, but that is currently not the case. We should use the
-- scrutinous version once the ledger implements tracking maps, in which case we
-- can keep track of exactly which era translations have happened.
applyDiff ::
     Ord k
  => Values k v
  -> Diff k v
  -> Values k v
applyDiff (Values values) (Diff diffs) = Values $
    Merge.merge
      Merge.preserveMissing
      (Merge.mapMaybeMissing     newKeys)
      (Merge.zipWithMaybeMatched oldKeys)
      values
      diffs
  where
    newKeys :: k -> NEDiffHistory v -> Maybe v
    newKeys _k h = case last h of
      Insert x            -> Just x
      Delete _x           -> Nothing
      UnsafeAntiInsert _x -> error "Can not apply UnsafeAntiInsert diff"
      UnsafeAntiDelete _x -> error "Can not apply UnsafeAntiDelete diff"

    oldKeys :: k -> v -> NEDiffHistory v -> Maybe v
    oldKeys _k _v1 h = case last h of
      Insert x            -> Just x
      Delete _x           -> Nothing
      UnsafeAntiInsert _x -> error "Can not apply UnsafeAntiInsert diff"
      UnsafeAntiDelete _x -> error "Can not apply UnsafeAntiDelete diff"

-- | Applies a diff to values for a specific set of keys.
--
-- See @'applyDiff'@ for more information about the scenarios in which
-- @'applyDiffForKeys'@ fail.
applyDiffForKeys ::
     Ord k
  => Values k v
  -> Keys k v
  -> Diff k v
  -> Values k v
applyDiffForKeys v@(Values values) (Keys keys) (Diff diffs) =
  applyDiff
    v
    (Diff $ diffs `Map.restrictKeys` (Map.keysSet values `Set.union` keys))

data ApplyDiffError k v =
    FoldToActFailed k (NEDiffHistory v)
  | DelMissingKey k v (NEDiffHistory v)
  | DelInsMissingKey k v v (NEDiffHistory v)
  | InsMatchingKey k v v (NEDiffHistory v)
  | BadDelMatchingKey k v v (NEDiffHistory v)
  | BadDelInsMatchingKey k v v v (NEDiffHistory v)
  | InsDelMatchingKey k v (NEDiffHistory v)
  deriving (Show, Eq)

-- | Applies a diff to values, performs sanity checks.
--
-- This a /scrutinous/ version of @'applyDiff'@ in the sense that
-- @'applyDiffScrutinous'@ performs sanity checks like (i) a diff history should
-- be sensible (if we insert x, we can only delete x), (ii) we can not delete a
-- key from @'Values'@ if it is not already present, etc. If a sanity check
-- fails, an @'ApplyDiffError'@ will be returned.
applyDiffScrutinous ::
     forall k v. (Ord k, Eq v)
  => Values k v
  -> Diff k v
  -> Either (ApplyDiffError k v) (Values k v)
applyDiffScrutinous (Values v) (Diff d) = Values <$>
    Merge.mergeA
      Merge.preserveMissing
      (Merge.traverseMaybeMissing newKeys)
      (Merge.zipWithMaybeAMatched oldKeys)
      v
      d
  where
    newKeys :: k -> NEDiffHistory v -> Either (ApplyDiffError k v) (Maybe v)
    newKeys k h = case foldToAct h of
      Nothing -> Left $ FoldToActFailed k h
      Just a  -> case a of
        Ins x      -> Right $ Just x
        Del x      -> Left  $ DelMissingKey k x h
        DelIns x y -> Left  $ DelInsMissingKey k x y h
        InsDel     -> Right   Nothing

    oldKeys :: k -> v -> NEDiffHistory v -> Either (ApplyDiffError k v) (Maybe v)
    oldKeys k v1 h = case foldToAct h of
      Nothing -> Left $ FoldToActFailed k h
      Just a  -> case a of
        Ins x                  -> Left  $ InsMatchingKey k v1 x h
        Del x      | x == v1   -> Right   Nothing
                   | otherwise -> Left  $ BadDelMatchingKey k v1 x h
        DelIns x y | x == v1   -> Right $ Just y
                   | otherwise -> Left  $ BadDelInsMatchingKey k v1 x y h
        InsDel                 -> Left  $ InsDelMatchingKey k v1 h

-- | Applies a diff to values for a specific set of keys, performs sanity
-- checks.
--
-- See @'applyDiffScrutinous'@ for more information about the sanity checks
-- this function performs, and how this affects the result of
-- @'applyDiffForKeysScrutinous'@.
applyDiffForKeysScrutinous ::
     (Ord k, Eq v)
  => Values k v
  -> Keys k v
  -> Diff k v
  -> Either (ApplyDiffError k v) (Values k v)
applyDiffForKeysScrutinous v@(Values values) (Keys keys) (Diff diffs) =
  applyDiffScrutinous
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
unsafeFoldToAct :: Eq v => NEDiffHistory v -> Act v
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
