{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Util.Orphans.DiffSeq.Arbitrary () where

import           Test.QuickCheck

import qualified Data.FingerTree.RootMeasured.Strict as RMFT
import           Data.Map.Diff.Strict (Diff, DiffEntry (..), NEDiffHistory (..))
import qualified Data.Map.Diff.Strict as MapDiff
import           Data.Sequence.NonEmpty (NESeq (..))

import           Ouroboros.Consensus.Storage.LedgerDB.HD.DiffSeq
                     (InternalMeasure (..), RootMeasure (..))
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD.DiffSeq as DS
                     (Length (..), SlotNoLB (..), SlotNoUB (..))

import           Test.Util.Orphans.Slotting.Arbitrary ()

{------------------------------------------------------------------------------
  Diffs
------------------------------------------------------------------------------}

deriving newtype instance (Ord k, Arbitrary k, Arbitrary v)
                       => Arbitrary (Diff k v)

instance (Arbitrary v) => Arbitrary (NEDiffHistory v) where
  arbitrary = NEDiffHistory <$>
    ((:<||) <$> arbitrary <*> arbitrary)

instance (Arbitrary v) => Arbitrary (DiffEntry v) where
  arbitrary = oneof [
      Insert <$> arbitrary
    , Delete <$> arbitrary
    , UnsafeAntiInsert <$> arbitrary
    , UnsafeAntiDelete <$> arbitrary
    ]

{-------------------------------------------------------------------------------
  DiffSeq
-------------------------------------------------------------------------------}

instance (RMFT.SuperMeasured vt vi a, Arbitrary a)
      => Arbitrary (RMFT.StrictFingerTree vt vi a) where
  arbitrary = RMFT.fromList <$> arbitrary

instance (Ord k, Arbitrary k, Arbitrary v)
      => Arbitrary (RootMeasure k v) where
  arbitrary = RootMeasure <$> arbitrary <*> arbitrary

instance Arbitrary (InternalMeasure k v) where
  arbitrary = InternalMeasure <$> arbitrary <*> arbitrary <*> arbitrary

deriving newtype instance Arbitrary DS.Length
deriving newtype instance Arbitrary DS.SlotNoUB
deriving newtype instance Arbitrary DS.SlotNoLB
