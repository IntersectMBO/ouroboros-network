{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Util.Orphans.DiffSeq.Arbitrary () where

import           Test.QuickCheck hiding (Fixed (..))

import qualified Data.Map.Diff.Strict as MapDiff
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD.DiffSeq as DS

import           Test.Util.Orphans.Slotting.Arbitrary ()

{------------------------------------------------------------------------------
  Diffs
------------------------------------------------------------------------------}

deriving newtype instance (Ord k, Arbitrary k, Arbitrary v)
                       => Arbitrary (MapDiff.Diff k v)
deriving newtype instance (Arbitrary v) => Arbitrary (MapDiff.DiffHistory v)
instance (Arbitrary v) => Arbitrary (MapDiff.DiffEntry v) where
  arbitrary = do
    constr <- elements [MapDiff.Insert, MapDiff.Delete]
    constr <$> arbitrary

{------------------------------------------------------------------------------
  Sequences of diffs
------------------------------------------------------------------------------}

instance (Ord k, Arbitrary k, Arbitrary v)
      => Arbitrary (DS.TopMeasure k v) where
  arbitrary = DS.TopMeasure <$> arbitrary <*> arbitrary

instance Arbitrary (DS.InternalMeasure k v) where
  arbitrary = DS.InternalMeasure <$> arbitrary <*> arbitrary

deriving newtype instance Arbitrary DS.Length
deriving newtype instance Arbitrary DS.SlotNo
