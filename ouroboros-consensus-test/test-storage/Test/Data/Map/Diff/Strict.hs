{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Data.Map.Diff.Strict (tests) where

import           Data.Map.Strict.Diff
import qualified Data.Map.Strict.Diff2 as D2

import           Test.QuickCheck (Arbitrary)
import qualified Test.QuickCheck as QC
import           Test.Tasty
import           Test.Tasty.QuickCheck (testProperty)

import           Test.Util.Laws


-- | Testing @'Semigroup'@, @'Monoid'@ and @'Group'@ laws for diff datatypes.
--
-- Note: We expect the tests for @'AntiDiff'@s to fail, since the
-- @'AntiDiff'@ datatype makes assumptions about the shape of the underlying
-- diff. For example, in the context of consensus, we should currently never
-- expect to encounter a negative diff as a top-level result. The @'D2.Diff'@
-- is more general, and so does not fail in these cases.
tests :: TestTree
tests = testGroup "Strict" [
    testGroup "Semigroup laws" [
        testGroup "Associativity" [
            testProperty "Diff k v" $
              associativity @(Diff Int Int)
          , testProperty "DiffHistory v" $
              associativity @(DiffHistory Int)
          , testProperty "AntiDiff k v" $
              QC.expectFailure $
                associativity @(AntiDiff Int Int)
          , testProperty "D2.Diff k v" $
              associativity @(D2.Diff Int Int)
          , testProperty "D2.DiffHistory v" $
              associativity @(D2.DiffHistory Int)
          ]
      ]
  , testGroup "Monoid laws" [
        testGroup "Right identity" [
            testProperty "Diff k v" $
              rightIdentity @(Diff Int Int)
          , testProperty "AntiDiff k v" $
              QC.expectFailure $
                rightIdentity @(AntiDiff Int Int)
          , testProperty "D2.Diff k v" $
              rightIdentity @(D2.Diff Int Int)
          , testProperty "D2.DiffHistory v" $
              rightIdentity @(D2.DiffHistory Int)
          ]
      , testGroup "Left identity" [
            testProperty "Diff k v" $
              leftIdentity @(Diff Int Int)
          , testProperty "AntiDiff k v" $
              QC.expectFailure $
                leftIdentity @(AntiDiff Int Int)
          , testProperty "D2.Diff k v" $
              leftIdentity @(D2.Diff Int Int)
          , testProperty "D2.DiffHistory v" $
              leftIdentity @(D2.DiffHistory Int)
          ]
      , testGroup "Concatenation" [
            testProperty "Diff k v" $
              concatenation @(Diff Int Int)
          , testProperty "AntiDiff k v" $
              QC.expectFailure $
                concatenation @(AntiDiff Int Int)
          , testProperty "D2.Diff k v" $
              concatenation @(D2.Diff Int Int)
          , testProperty "D2.DiffHistory v" $
              concatenation @(D2.DiffHistory Int)
          ]
      ]
  , testGroup "Group laws" [
        testGroup "Right inverse" [
            testProperty "AntiDiff k v" $
              QC.expectFailure $
                rightInverse @(AntiDiff Int Int)
          , testProperty "D2.Diff k v" $
                rightInverse @(D2.Diff Int Int)
          , testProperty "D2.DiffHistory v" $
              rightInverse @(D2.DiffHistory Int)
          ]
      , testGroup "Left inverse" [
            testProperty "AntiDiff k v" $
              QC.expectFailure $
                leftInverse @(AntiDiff Int Int)
          , testProperty "D2.Diff k v" $
              leftInverse @(D2.Diff Int Int)
          , testProperty "D2.DiffHistory v" $
              leftInverse @(D2.DiffHistory Int)
          ]
      ]
  ]

{------------------------------------------------------------------------------
  Orphan @'Arbitrary'@ instances
------------------------------------------------------------------------------}

deriving newtype instance (Ord k, Arbitrary k, Arbitrary v)
                       => Arbitrary (Diff k v)
deriving newtype instance (Arbitrary v) => Arbitrary (DiffHistory v)
instance (Arbitrary v) => Arbitrary (DiffEntry v) where
  arbitrary = do
    constr <- QC.elements [Insert, Delete]
    constr <$> QC.arbitrary
instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (AntiDiff k v) where
  arbitrary = do
    constr <- QC.elements [Positive, Negative]
    constr <$> QC.arbitrary


deriving newtype instance (Ord k, Arbitrary k, Arbitrary v)
                       => Arbitrary (D2.Diff k v)
deriving newtype instance (Arbitrary v) => Arbitrary (D2.DiffHistory v)
instance (Arbitrary v) => Arbitrary (D2.DiffEntry v) where
  arbitrary = do
    constr <- QC.elements [D2.Insert, D2.Delete]
    constr <$> QC.arbitrary
