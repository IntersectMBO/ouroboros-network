{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE StandaloneKindSignatures   #-}
{-# LANGUAGE TupleSections              #-}

module Test.Util.MockDiffSeq (
    -- * Types
    MockDiff (..)
  , MockDiffEntry (..)
  , MockDiffSeq (..)
  , MockKeys (..)
  , MockSlotNo (..)
  , MockValues (..)
    -- * Diff sequence operations
  , mFlush
  , mForwardValues
  , mForwardValuesAndKeys
  , mPush
  , mRollback
  , mTotalDiff
    -- * Utility functions
  , mDiffKeys
  , mFromListDeletes
  , mFromListInserts
  , mFromListKeys
  , mFromListValues
  , mLength
  , mRestrictValues
  , mSingletonDelete
  , mSingletonInsert
  ) where

import           Prelude hiding (splitAt)

import           Control.DeepSeq
import           Data.Bifunctor
import           Data.Kind
import qualified Data.Map.Merge.Strict as MapMerge
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid (Sum (..))
import           Data.Semigroup (Max (..))
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics (Generic)

import           Data.FingerTree.Strict

import           Test.Util.Orphans.NFData ()


{------------------------------------------------------------------------------
  Types
------------------------------------------------------------------------------}

type MockDiffSeq :: Type -> Type -> Type
newtype MockDiffSeq k v = MockDiffSeq {
    fromMockDiffSeq ::
      StrictFingerTree
        (MockLength, MockDiff k v)
        (MockSlotNo, MockDiff k v)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

newtype MockLength = MockLength { unMockLength :: Int }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype Num
  deriving anyclass NFData

newtype MockDiff k v = MockDiff (Map k (Seq (MockDiffEntry v)))
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

data MockDiffEntry v = MockInsert v | MockDelete v
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

newtype MockValues k v = MockValues (Map k v)
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

newtype MockKeys k v = MockKeys (Set k)
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

newtype MockSlotNo = MockSlotNo Int
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype Num
  deriving anyclass NFData

{------------------------------------------------------------------------------
  Measured instances
------------------------------------------------------------------------------}

instance Ord k
      => Measured (MockLength, MockDiff k v) (MockSlotNo, MockDiff k v) where
  measure = (1,) . snd

deriving via Sum Int instance Semigroup MockLength
deriving via Sum Int instance Monoid MockLength

{------------------------------------------------------------------------------
  Monoid instances
------------------------------------------------------------------------------}

instance Ord k => Semigroup (MockDiffSeq k v) where
  MockDiffSeq ft1 <> MockDiffSeq ft2 = MockDiffSeq $ ft1 >< ft2

instance Ord k => Monoid (MockDiffSeq k v) where
  mempty = MockDiffSeq empty

deriving via Max Int instance Semigroup MockSlotNo
deriving via Max Int instance Monoid MockSlotNo

instance Ord k => Semigroup (MockDiff k v) where
  MockDiff d1 <> MockDiff d2 = MockDiff $
    MapMerge.merge
      MapMerge.preserveMissing
      MapMerge.preserveMissing
      (MapMerge.zipWithMatched (\_ x y -> x <> y))
      d1
      d2

instance Ord k => Monoid (MockDiff k v) where
  mempty = MockDiff mempty

{------------------------------------------------------------------------------
  Diff sequence operations
------------------------------------------------------------------------------}

-- | Push a diff to the right end of a diff sequence.
mPush ::
     Ord k
  => MockDiffSeq k v
  -> MockSlotNo
  -> MockDiff k v
  -> MockDiffSeq k v
mPush (MockDiffSeq ft) sl d = MockDiffSeq $ case viewr ft of
  EmptyR           -> singleton (sl, d)
  _ :> (sl', _)
    | sl <= sl'    -> error "Slots should be monotonically increasing"
    | otherwise    -> ft |> (sl, d)

-- | Flush @n@ diffs from a diff sequence by dropping @n@ diffs at the left end.
mFlush ::
     Ord k
  => Int
  -> MockDiffSeq k v
  -> ( MockDiffSeq k v
     , MockDiffSeq k v
     )
mFlush n (MockDiffSeq ft) = bimap MockDiffSeq MockDiffSeq $
    split ((MockLength n <) . fst) ft

-- | Roll back @n@ diffs in a diff sequence by dropping @n@ diffs at the right
-- end.
mRollback ::
     Ord k
  => Int
  -> MockDiffSeq k v
  -> ( MockDiffSeq k v
     , MockDiffSeq k v
     )
mRollback n (MockDiffSeq ft)
    | m < 0     =
        error "Can not roll back more than the length of a diff sequence"
    | otherwise = bimap MockDiffSeq MockDiffSeq $
        split ((MockLength m <) . fst) ft
  where
    m = length ft - n

mForwardValuesAndKeys ::
     Ord k
  => MockValues k v
  -> MockKeys k v
  -> MockDiff k v
  -> MockValues k v
mForwardValuesAndKeys vs0@(MockValues vs) (MockKeys ks) (MockDiff d) =
    mForwardValues
      vs0
      (MockDiff $ d `Map.restrictKeys` (Map.keysSet vs `Set.union` ks))

mForwardValues ::
     Ord k
  => MockValues k v
  -> MockDiff k v
  -> MockValues k v
mForwardValues (MockValues m1) (MockDiff m2) = MockValues $
  MapMerge.merge
    MapMerge.preserveMissing
    (MapMerge.mapMaybeMissing (const shouldKeep))
    (MapMerge.zipWithMaybeMatched (\_ _ -> shouldKeep))
    m1
    m2
  where
    shouldKeep = \case
      Seq.Empty   -> error "shouldKeep: sequence is empty"
      _ Seq.:|> x -> case x of
        MockInsert v -> Just v
        MockDelete _ -> Nothing

mTotalDiff :: Ord k => MockDiffSeq k v -> MockDiff k v
mTotalDiff (MockDiffSeq ft) = snd $ measure ft

{------------------------------------------------------------------------------
  Utility functions
------------------------------------------------------------------------------}

mLength :: Ord k => MockDiffSeq k v -> Int
mLength (MockDiffSeq ft) = unMockLength . fst $ measure ft

mDiffKeys :: MockDiff k v -> MockKeys k v
mDiffKeys (MockDiff m) = MockKeys $ Map.keysSet m

mRestrictValues :: Ord k => MockValues k v -> MockKeys k v -> MockValues k v
mRestrictValues (MockValues m) (MockKeys s) = MockValues $ m `Map.restrictKeys` s

mSingletonDelete :: v -> Seq (MockDiffEntry v)
mSingletonDelete = Seq.singleton . MockDelete

mSingletonInsert :: v -> Seq (MockDiffEntry v)
mSingletonInsert = Seq.singleton . MockInsert

mFromListDeletes :: Ord k => [(k, v)] -> MockDiff k v
mFromListDeletes = MockDiff . Map.fromList . fmap (second mSingletonDelete)

mFromListInserts :: Ord k => [(k, v)] -> MockDiff k v
mFromListInserts = MockDiff . Map.fromList . fmap (second mSingletonInsert)

mFromListKeys :: Ord k => [k] -> MockKeys k v
mFromListKeys = MockKeys . Set.fromList

mFromListValues :: Ord k => [(k, v)] -> MockValues k v
mFromListValues = MockValues . Map.fromList
