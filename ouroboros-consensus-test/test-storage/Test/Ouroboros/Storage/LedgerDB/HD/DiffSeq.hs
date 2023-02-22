{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ouroboros.Storage.LedgerDB.HD.DiffSeq (tests) where

import           Control.Monad (liftM)
import qualified Data.FingerTree.RootMeasured.Strict as RMFT
import           Data.Map.Diff.Strict (Diff, DiffEntry (..))
import           Data.Map.Diff.Strict.Internal (Diff (..), NEDiffHistory (..))
import           Data.Maybe.Strict (StrictMaybe (..))
import           Data.Proxy
import           Data.Semigroupoid.Simple.Auto
import           Data.Semigroupoid.Simple.Laws
import           Data.Sequence.NonEmpty (NESeq (..))
import           Ouroboros.Consensus.Storage.LedgerDB.DiffSeq
import qualified Ouroboros.Consensus.Storage.LedgerDB.DiffSeq as DS
                     (Length (..), SlotNoLB (..), SlotNoUB (..))
import           Test.Cardano.Ledger.Binary.Arbitrary ()
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.Orphans.Arbitrary ()

tests :: TestTree
tests = testGroup "DiffSeq" [
    testGroupWithProxy (Proxy @(RootMeasure Key Val)) [
        testSemigroupLaws
      , testMonoidLaws
      , testGroupLaws
      ]
  , testGroupWithProxy (Proxy @(Auto (RootMeasure Key Val))) [
        testSemigroupoidLaws
      , testGroupoidLaws
      ]
  , testGroupWithProxy (Proxy @(InternalMeasure Key Val)) [
        testSemigroupLaws
      , testMonoidLaws
      ]
  , testGroupWithProxy (Proxy @(Auto (InternalMeasure Key Val))) [
        testSemigroupoidLaws
      ]
  ]

type Key = Small Int
type Val = Small Int

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

instance Arbitrary1 StrictMaybe where
  liftArbitrary arb = frequency [(1, return SNothing), (3, liftM SJust arb)]

  liftShrink shr (SJust x) = SNothing : [ SJust x' | x' <- shr x ]
  liftShrink _   SNothing  = []
