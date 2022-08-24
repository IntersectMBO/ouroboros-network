{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Util.Orphans.DiffSeq.Arbitrary () where

import           Test.QuickCheck

import qualified Data.FingerTree.TopMeasured.Strict as TMSFT
import qualified Data.Map.Diff.Strict as MapDiff
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD.DiffSeq as DS

import           Test.Util.Orphans.Slotting.Arbitrary ()

{------------------------------------------------------------------------------
  Diffs
------------------------------------------------------------------------------}

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (MapDiff.Diff k v) where
  arbitrary = MapDiff.fromList <$> arbitrary
instance (Arbitrary v) => Arbitrary (MapDiff.DiffHistory v) where
  arbitrary = oneof [
      MapDiff.singletonInsert <$> arbitrary
    , MapDiff.singletonDelete <$> arbitrary
    ]

{------------------------------------------------------------------------------
  Sequences of diffs
------------------------------------------------------------------------------}

instance (TMSFT.SuperMeasured vt vi a, Arbitrary a)
      => Arbitrary (TMSFT.StrictFingerTree vt vi a) where
  arbitrary = TMSFT.fromList <$> arbitrary

instance (Ord k, Arbitrary k, Arbitrary v)
      => Arbitrary (DS.TopMeasure k v) where
  arbitrary = DS.TopMeasure <$> arbitrary <*> arbitrary

instance Arbitrary (DS.InternalMeasure k v) where
  arbitrary = DS.InternalMeasure <$> arbitrary <*> arbitrary

deriving newtype instance Arbitrary DS.Length
deriving newtype instance Arbitrary DS.SlotNo
