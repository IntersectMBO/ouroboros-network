{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Util.Orphans.DiffSeq.Arbitrary () where

import           Test.QuickCheck hiding (Fixed (..))

import qualified Data.Map.Strict.Diff as D1
import qualified Data.Map.Strict.Diff2 as D2
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD.DiffSeq as DS
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD.TableTypes as TT

import           Test.Util.Orphans.Slotting.Arbitrary ()

{------------------------------------------------------------------------------
  Diffs
------------------------------------------------------------------------------}

deriving newtype instance (Ord k, Arbitrary k, Arbitrary v)
                       => Arbitrary (D1.Diff k v)
deriving newtype instance (Arbitrary v) => Arbitrary (D1.DiffHistory v)
instance (Arbitrary v) => Arbitrary (D1.DiffEntry v) where
  arbitrary = do
    constr <- elements [D1.Insert, D1.Delete]
    constr <$> arbitrary
instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (D1.AntiDiff k v) where
  arbitrary = do
    constr <- elements [D1.Positive, D1.Negative]
    constr <$> arbitrary


deriving newtype instance (Ord k, Arbitrary k, Arbitrary v)
                       => Arbitrary (D2.Diff k v)
deriving newtype instance (Arbitrary v) => Arbitrary (D2.DiffHistory v)
instance (Arbitrary v) => Arbitrary (D2.DiffEntry v) where
  arbitrary = do
    constr <- elements [D2.Insert, D2.Delete]
    constr <$> arbitrary

{------------------------------------------------------------------------------
  Sequences of diffs
------------------------------------------------------------------------------}

instance (Ord k, Arbitrary k, Arbitrary v)
      => Arbitrary (DS.TopMeasure ts k v) where
  arbitrary = DS.TopMeasure <$> arbitrary <*> arbitrary

instance Arbitrary (DS.InternalMeasure ts k v) where
  arbitrary = DS.InternalMeasure <$> arbitrary <*> arbitrary

deriving newtype instance (Ord k, Arbitrary k, Arbitrary v)
                       => Arbitrary (TT.TableDiff ts k v)

deriving newtype instance Arbitrary DS.Length
deriving newtype instance Arbitrary DS.SlotNo
