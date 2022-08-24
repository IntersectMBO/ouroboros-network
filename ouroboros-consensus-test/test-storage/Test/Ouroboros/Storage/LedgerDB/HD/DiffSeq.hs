{-# LANGUAGE TypeApplications #-}

module Test.Ouroboros.Storage.LedgerDB.HD.DiffSeq (tests) where

import           Data.Proxy

import           Test.Tasty

import           AntiDiff.Util.Auto
import           AntiDiff.Util.Laws (testGroupLaws, testGroupoidLaws,
                     testMonoidLaws, testSemigroupLaws, testSemigroupoidLaws)
import           AntiDiff.Util.Tasty
import           AntiDiff.Util.X

import           Test.Util.Orphans.DiffSeq.Arbitrary ()

import           Ouroboros.Consensus.Storage.LedgerDB.HD.DiffSeq



tests :: TestTree
tests = testGroup "DiffSeq" [
    testGroupWithProxy (Proxy @(TopMeasure X X)) [
        testSemigroupLaws
      , testMonoidLaws
      , testGroupLaws
      ]
  , testGroupWithProxy (Proxy @(Auto (TopMeasure X X))) [
        testSemigroupoidLaws
      , testGroupoidLaws
      ]
  , testGroupWithProxy (Proxy @(InternalMeasure X X)) [
        testSemigroupLaws
      , testMonoidLaws
      ]
  , testGroupWithProxy (Proxy @(Auto (InternalMeasure X X))) [
        testSemigroupoidLaws
      ]
  ]
