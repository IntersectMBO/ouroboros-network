{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Data.FingerTree.TopMeasured.Strict
import           Data.Map.Diff.Strict

import           Data.Monoid
import           Data.Proxy

import           Test.Data.FingerTree.TopMeasured.Strict (appendProp, splitProp)
import qualified Test.Data.FingerTree.TopMeasured.Strict
import qualified Test.Data.Map.Diff.Strict as Test.Diff

import           AntiDiff.Util.Tasty
import           AntiDiff.Util.X



main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "anti-diffs" [
    Test.Diff.tests
  , Test.Data.FingerTree.TopMeasured.Strict.tests
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

type DiffSeq = StrictFingerTree (Diff X X) (Sum Int) (Diff X X)

instance Measured (Sum Int) (Diff X X) where
  measure _ = Sum 1

instance TopMeasured (Diff X X) (Diff X X) where
  measureTop d = d
