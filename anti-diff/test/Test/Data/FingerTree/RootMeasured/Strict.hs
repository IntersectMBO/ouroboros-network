{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Data.FingerTree.RootMeasured.Strict (
    -- * Tests
    tests
    -- * Properties
  , appendProp
  , splitProp
  ) where

import           Data.Group (Group (..))
import           Data.Monoid (Sum (..))
import           Data.Proxy

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Data.FingerTree.RootMeasured.Strict
import           Data.Semigroupoid.Laws (testGroupWithProxy)


tests :: TestTree
tests = testGroup "Data.FingerTree.TopMeasured.Strict" [
    testGroupWithProxy (Proxy @T) [
        \pr -> testProperty "splitProp (simple Group)" $
          \lr n sft -> splitProp pr lr (getPositive n <) sft
      , testProperty "appendProp (simple Group)" .
          appendProp
      ]
  ]

type T = StrictFingerTree (Sum Int) (Sum Int) (Small Int)

{------------------------------------------------------------------------------
  Types
------------------------------------------------------------------------------}

instance (SuperMeasured vt vi a, Arbitrary a)
      => Arbitrary (StrictFingerTree vt vi a) where
  arbitrary = fromList <$> arbitrary

instance Measured (Sum Int) (Small Int) where
  measure _ = Sum 1

instance RootMeasured (Sum Int) (Small Int) where
  measureRoot _ = Sum 1

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
      foldMap measureRoot l === measureRoot l

    prop2 = counterexample "b)" $
      foldMap measureRoot r === measureRoot r

    prop3 = counterexample "c)" $
      foldMap measureRoot sft === measureRoot sft

    prop4 = counterexample "d)" $
      measureRoot sft === measureRoot l <> measureRoot r

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
      foldMap measureRoot sft === measureRoot sft

    prop2 = counterexample "g)" $
      foldMap measureRoot l === measureRoot l

    prop3 = counterexample "h)" $
      foldMap measureRoot r === measureRoot r

    prop4 = counterexample "f)" $
      measureRoot sft === measureRoot l <> measureRoot r
