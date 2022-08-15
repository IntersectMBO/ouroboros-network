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

-- | Testing @'Semigroup'@, @'Monoid'@ and @'Group'@ laws for the measures used
-- in the diff sequence datatype.
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
