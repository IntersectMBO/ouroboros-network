{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Data.Map.Diff.Strict (tests) where

import           Data.Proxy (Proxy (Proxy))

import qualified Data.Map.Strict.Diff as D1
import qualified Data.Map.Strict.Diff2 as D2

import           Test.Tasty (TestTree, testGroup)

import           Test.Util.Laws
import           Test.Util.Orphans.DiffSeq.Arbitrary ()


-- | Testing @'Semigroup'@, @'Monoid'@ and @'Group'@ laws for diff datatypes.
--
-- We test two versions of diffs and related datatypes, which we qualify by
-- @D1@ and @D2@. @D2@ defines the most general version of a diff with a
-- @'Group'@ instance.
--
-- Note: We expect this style of tests for @'AntiDiff'@s to fail, since the
-- @'AntiDiff'@ datatype makes assumptions about the shape of the underlying
-- diff. For example, in the context of consensus, we should currently never
-- expect to encounter a negative diff as a top-level result. Since the tests
-- fail anyway, we do not test laws for the @'AntiDiff'@ datatype.
-- The @'D2.Diff'@ is more general, and so does not fail in these cases.
tests :: TestTree
tests = testGroup "Strict" [
    testGroup "D1.Diff Int Int" [
        testSemigroupLaws p1
      , testMonoidLaws p1
      ]
  , testGroup "D1.DiffHistory Int" [
        testSemigroupLaws p2
      ]
  , testGroup "D2.Diff Int Int" [
        testSemigroupLaws p3
      , testMonoidLaws p3
      , testGroupLaws p3
      ]
  , testGroup "D2.DiffHistory Int" [
        testSemigroupLaws p4
      , testMonoidLaws p4
      , testGroupLaws p4
      ]
  ]
    where
      p1 = Proxy @(D1.Diff Int Int)
      p2 = Proxy @(D1.DiffHistory Int)
      p3 = Proxy @(D2.Diff Int Int)
      p4 = Proxy @(D2.DiffHistory Int)
