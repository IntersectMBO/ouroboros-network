{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-}

module Test.Ouroboros.Storage.LedgerDB.HD.DiffSeq (tests) where

import           Data.Proxy

import           Test.Tasty

import           Test.Util.Laws
import           Test.Util.Orphans.DiffSeq.Arbitrary ()

import           Ouroboros.Consensus.Storage.LedgerDB.HD.DiffSeq
import           Ouroboros.Consensus.Storage.LedgerDB.HD.ToStore

-- | Testing @'Semigroup'@, @'Monoid'@ and @'Group'@ laws for diff datatypes.
--
-- Note: We expect the tests for @'AntiDiff'@s to fail, since the
-- @'AntiDiff'@ datatype makes assumptions about the shape of the underlying
-- diff. For example, in the context of consensus, we should currently never
-- expect to encounter a negative diff as a top-level result. The @'D2.Diff'@
-- is more general, and so does not fail in these cases.
tests :: forall (ts :: ToStoreKind). TestTree
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
      p1 = Proxy @(TopMeasure ts Int Int)
      p2 = Proxy @(InternalMeasure ts Int Int)
