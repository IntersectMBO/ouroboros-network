{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Data.FingerTree.TopMeasured.Strict (
    -- * Tests
    tests
    -- * Properties
  , appendProp
  , splitProp
  ) where

import           Data.Group (Group (..))
import           Data.Monoid (Sum (..))
import           Data.Proxy

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Data.FingerTree.TopMeasured.Strict

import           AntiDiff.Util.Tasty
import           AntiDiff.Util.X



tests :: TestTree
tests = testGroup "Data.FingerTree.TopMeasured.Strict" [
    testGroupWithProxy (Proxy @(StrictFingerTree (Sum Int) (Sum Int) X)) [
      \pr -> testProperty "splitProp (simple Group)" $
        \lr n sft -> splitProp pr lr (getPositive n <) sft
    , testProperty "appendProp (simple Group)" .
        appendProp
    ]
  ]

instance (SuperMeasured vt vi a, Arbitrary a)
      => Arbitrary (StrictFingerTree vt vi a) where
  arbitrary = fromList <$> arbitrary


instance Measured (Sum Int) X where
  measure _ = Sum 1

instance TopMeasured (Sum Int) X where
  measureTop _ = Sum 1

instance Arbitrary LR where
  arbitrary = elements [L, R]

{------------------------------------------------------------------------------
  Properties
------------------------------------------------------------------------------}

splitProp ::
     (SuperMeasured vt vi a, Group vt, Eq vt, Show vt)
  => Proxy (StrictFingerTree vt vi a)
  -> LR
  -> (vi -> Bool)
  -> StrictFingerTree vt vi a
  -> Property
splitProp _ lr p sft = conjoin [prop0, prop1, prop2, prop3, prop4]
  where
    (l, r) = case lr of
      L -> splitl p sft
      R -> splitr p sft

    prop0
      | null l = label "dead1" ()
      | null r = label "dead2" ()
      | otherwise = label "alive" ()

    prop1 = counterexample "a)" $
      foldMap measureTop l === measureTop l

    prop2 = counterexample "b)" $
      foldMap measureTop r === measureTop r

    prop3 = counterexample "c)" $
      foldMap measureTop sft === measureTop sft

    prop4 = counterexample "d)" $
      measureTop sft === measureTop l <> measureTop r

appendProp ::
     (SuperMeasured vt vi a, Eq vt, Show vt)
  => Proxy (StrictFingerTree vt vi a)
  -> StrictFingerTree vt vi a
  -> StrictFingerTree vt vi a
  -> Property
appendProp _ l r = conjoin [prop0, prop1, prop2, prop3, prop4]
  where
    sft = l <> r

    prop0
      | null l = label "dead1" ()
      | null r = label "dead2" ()
      | otherwise = label "alive" ()

    prop1 = counterexample "e)" $
      foldMap measureTop sft === measureTop sft

    prop2 = counterexample "g)" $
      foldMap measureTop l === measureTop l

    prop3 = counterexample "h)" $
      foldMap measureTop r === measureTop r

    prop4 = counterexample "f)" $
      measureTop sft === measureTop l <> measureTop r
