{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import           Data.Monoid
import           Data.Proxy

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Data.FingerTree.RootMeasured.Strict
import           Data.Map.Diff.Strict
import           Data.Semigroupoid.Laws (testGroupWithProxy)

import           Test.Data.FingerTree.RootMeasured.Strict (appendProp,
                     splitProp)
import qualified Test.Data.FingerTree.RootMeasured.Strict
import qualified Test.Data.Map.Diff.Strict



main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "Data" [
      testGroup "FingerTree" [
          testGroup "RootMeasured" [
              Test.Data.FingerTree.RootMeasured.Strict.tests
            ]
        ]
    , testGroup "Map" [
          testGroup "Diff" [
                Test.Data.Map.Diff.Strict.tests
            ]
        ]
    , diffSeqTests
    ]

{------------------------------------------------------------------------------
  Tests for sequences of diffs
------------------------------------------------------------------------------}

diffSeqTests :: TestTree
diffSeqTests = testGroup "DiffSeq" [
    testGroupWithProxy (Proxy @DiffSeq) [
      \pr -> testProperty "splitProp (diff sequences)" $
        \lr n sft -> splitProp pr lr (getPositive n <) sft
    , testProperty "appendProp (diff sequences)" .
        appendProp
    ]
  ]

type X = Small Int

type DiffSeq = StrictFingerTree (Diff X X) (Sum Int) (Diff X X)

instance Measured (Sum Int) (Diff X X) where
  measure _ = Sum 1

instance RootMeasured (Diff X X) (Diff X X) where
  measureRoot d = d
