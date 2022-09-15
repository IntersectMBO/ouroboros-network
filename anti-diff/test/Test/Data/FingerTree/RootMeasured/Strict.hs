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

instance (SuperMeasured vr vi a, Arbitrary a)
      => Arbitrary (StrictFingerTree vr vi a) where
  arbitrary = fromList <$> arbitrary

instance Measured (Sum Int) (Small Int) where
  measure _ = Sum 1

instance RootMeasured (Sum Int) (Small Int) where
  measureRoot _ = Sum 1

{------------------------------------------------------------------------------
  Properties
------------------------------------------------------------------------------}

splitProp ::
     (SuperMeasured vr vi a, Eq vr, Show vr)
  => Proxy (StrictFingerTree vr vi a)
  -> Bool
  -> (vi -> Bool)
  -> StrictFingerTree vr vi a
  -> Property
splitProp _ lr p sft = conjoin [prop0, prop1, prop2, prop3, prop4]
  where
    (l, r) = (if lr then splitl else splitr) p sft

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
     (SuperMeasured vr vi a, Eq vr, Show vr)
  => Proxy (StrictFingerTree vr vi a)
  -> StrictFingerTree vr vi a
  -> StrictFingerTree vr vi a
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
