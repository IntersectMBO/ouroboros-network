{-# LANGUAGE TypeApplications #-}

module Test.Ouroboros.Storage.LedgerDB.HD.DiffSeq (tests) where

import           Data.Proxy

import           Test.Tasty

import           Test.Util.Laws
import           Test.Util.Orphans.DiffSeq.Arbitrary ()

import           Ouroboros.Consensus.Storage.LedgerDB.HD.DiffSeq

-- | Testing @'Semigroup'@, @'Monoid'@ and @'Group'@ laws for the measures used
-- in the diff sequence datatype.
tests :: TestTree
tests = testGroup "DiffSeq" [
    testGroup "TopMeasure ts Int Int" [
        testSemigroupLaws p1
      , testMonoidLaws p1
      , testGroupLaws p1
      ]
  , testGroup "InternalMeasure ts Int Int" [
        testSemigroupLaws p2
      , testMonoidLaws p2
      ]
  ]
    where
      p1 = Proxy @(TopMeasure Int Int)
      p2 = Proxy @(InternalMeasure Int Int)
