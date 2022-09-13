{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Data.Map.Diff.Strict (tests) where

import           Data.Group
import           Data.Proxy (Proxy (Proxy))
import qualified Data.Sequence as Seq

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck

import           Data.Map.Diff.Strict
import qualified Data.Map.Diff.Strict.Internal as Internal

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
    , testGroupWithProxy (Proxy @(Diff (Small Int) (Small Int))) [
          testSemigroupLaws
        , testMonoidLaws
        , testGroupLaws
        ]
    , testGroupWithProxy (Proxy @(Auto (Diff (Small Int) (Small Int)))) [
          testSemigroupoidLaws
        , testGroupoidLaws
        ]
    ]

{------------------------------------------------------------------------------
  Types
------------------------------------------------------------------------------}


instance (Ord k, Eq v, Arbitrary k, Arbitrary v) => Arbitrary (Diff k v) where
  arbitrary = oneof [
      fromList <$> listOf ((,) <$> arbitrary <*> (arbitrary `suchThatMap` isNonEmptyHistory))
    , diff <$> arbitrary <*> arbitrary
    ]
  shrink (Internal.Diff m) = Internal.Diff <$> shrink m

instance (Eq v, Arbitrary v) => Arbitrary (DiffHistory v) where
  arbitrary = sized go
    where
      go 0             = pure mempty
      go n | n < 0     = error "QuickCheck size parameter can not be less than 0."
           | otherwise = oneof [
                            (<>) <$> gs <*> go (n - 1)
                          , (<>) <$> go (n - 1) <*> gs
                          ]
      gs = oneof [g1, g2]
      g1 = invert . fromSeq <$> arbitrary <*> arbitrary
      g2 = fromSeq <$> arbitrary <*> arbitrary

  shrink (Internal.DiffHistory sq) = Internal.DiffHistory <$>
    filter (not . Seq.null) (shrink sq)

instance (Arbitrary v) => Arbitrary (DiffEntry v) where
  arbitrary = oneof [
      Insert <$> arbitrary
    , Delete <$> arbitrary
    ]
