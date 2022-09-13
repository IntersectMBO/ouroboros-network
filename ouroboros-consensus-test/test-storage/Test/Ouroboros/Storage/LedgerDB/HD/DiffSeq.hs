{-# LANGUAGE TypeApplications #-}

module Test.Ouroboros.Storage.LedgerDB.HD.DiffSeq (tests) where

import           Data.Proxy

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Data.Semigroupoid.Auto
import           Data.Semigroupoid.Laws

import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Orphans.DiffSeq.Arbitrary ()

import           Ouroboros.Consensus.Storage.LedgerDB.HD.DiffSeq



tests :: TestTree
tests = testGroup "DiffSeq" [
    testGroupWithProxy (Proxy @(RootMeasure X X)) [
        testSemigroupLaws
      , testMonoidLaws
      , testGroupLaws
      ]
  , testGroupWithProxy (Proxy @(Auto (RootMeasure X X))) [
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

type X = Small Int
