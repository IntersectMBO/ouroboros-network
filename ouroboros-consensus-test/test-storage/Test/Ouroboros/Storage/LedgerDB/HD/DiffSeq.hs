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
    testGroupWithProxy (Proxy @(RootMeasure Key Val)) [
        testSemigroupLaws
      , testMonoidLaws
      , testGroupLaws
      ]
  , testGroupWithProxy (Proxy @(Auto (RootMeasure Key Val))) [
        testSemigroupoidLaws
      , testGroupoidLaws
      ]
  , testGroupWithProxy (Proxy @(InternalMeasure Key Val)) [
        testSemigroupLaws
      , testMonoidLaws
      ]
  , testGroupWithProxy (Proxy @(Auto (InternalMeasure Key Val))) [
        testSemigroupoidLaws
      ]
  ]

type Key = Small Int
type Val = Small Int
