{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Data.Map.Diff.Strict (tests) where

import           Data.Proxy (Proxy (Proxy))
import           Data.Sequence.NonEmpty (NESeq (..))

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck

import           Data.Map.Diff.Strict

import           Data.Semigroupoid.Auto
import           Data.Semigroupoid.Laws

tests :: TestTree
tests = testGroup "Data.Map.Diff.Strict" [
      testGroupWithProxy (Proxy @(DiffEntry (Small Int))) [
      ]
    , testGroupWithProxy (Proxy @(DiffHistory (Small Int))) [
          testSemigroupLaws
        , testMonoidLaws
        , testGroupLaws
        ]
    , testGroupWithProxy (Proxy @(Auto (DiffHistory (Small Int)))) [
          testSemigroupoidLaws
        , testGroupoidLaws
        ]
    , testGroupWithProxy (Proxy @(NEDiffHistory (Small Int))) [
          testSemigroupoidLaws
        ]
    , testGroupWithProxy (Proxy @(Diff (Small Int) (Small Int))) [
          testSemigroupLaws
        , testMonoidLaws
        , testGroupLaws
        ]
    , testGroupWithProxy (Proxy @(Auto (Diff (Small Int) (Small Int)))) [
          testSemigroupoidLaws
        , testGroupoidLaws
        ]
    , testGroupWithProxy (Proxy @(Act (Small Int))) [
          testSemigroupoidLaws
        , testGroupoidLaws
        ]
    ]

{------------------------------------------------------------------------------
  Types
------------------------------------------------------------------------------}

deriving newtype instance (Ord k, Eq v, Arbitrary k, Arbitrary v)
                       => Arbitrary (Diff k v)

instance (Eq v, Arbitrary v) => Arbitrary (NEDiffHistory v) where
  arbitrary = NEDiffHistory <$>
    ((:<||) <$> arbitrary <*> arbitrary)

instance (Eq v, Arbitrary v) => Arbitrary (DiffHistory v) where
  arbitrary = DiffHistory <$> arbitrary

instance (Arbitrary v) => Arbitrary (DiffEntry v) where
  arbitrary = oneof [
      Insert <$> arbitrary
    , Delete <$> arbitrary
    , UnsafeAntiInsert <$> arbitrary
    , UnsafeAntiDelete <$> arbitrary
    ]

instance Arbitrary v => Arbitrary (Act v) where
  arbitrary = oneof [
      Ins <$> arbitrary
    , Del <$> arbitrary
    , pure InsDel
    , DelIns <$> arbitrary <*> arbitrary
    ]
