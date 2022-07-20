{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Data.Map.Diff.Strict (tests) where

import           Data.Proxy (Proxy (Proxy))

import           Data.Map.Strict.Diff
import qualified Data.Map.Strict.Diff2 as D2

import           Test.Tasty (TestTree, testGroup)

import           Test.Util.Laws
import           Test.Util.Orphans.DiffSeq.Arbitrary ()


-- | Testing @'Semigroup'@, @'Monoid'@ and @'Group'@ laws for diff datatypes.
--
-- Note: We expect the tests for @'AntiDiff'@s to fail, since the
-- @'AntiDiff'@ datatype makes assumptions about the shape of the underlying
-- diff. For example, in the context of consensus, we should currently never
-- expect to encounter a negative diff as a top-level result. The @'D2.Diff'@
-- is more general, and so does not fail in these cases.
tests :: TestTree
tests = testGroup "Strict" [
    testGroup "Diff Int Int" [
        testSemigroupLaws p1
      , testMonoidLaws p1
      ]
  , testGroup "DiffHistory Int" [
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
      p1 = Proxy @(Diff Int Int)
      p2 = Proxy @(DiffHistory Int)
      p3 = Proxy @(D2.Diff Int Int)
      p4 = Proxy @(D2.DiffHistory Int)

{-
    testGroup "Semigroup laws" [
        testGroup "Associativity" [
            testProperty "Diff k v" $
              associativity @(Diff Int Int)
          , testProperty "DiffHistory v" $
              associativity @(DiffHistory Int)
          , testProperty "AntiDiff k v" $
              QC.expectFailure $
                associativity @(AntiDiff Int Int)
          , testProperty "D2.Diff k v" $
              associativity @(D2.Diff Int Int)
          , testProperty "D2.DiffHistory v" $
              associativity @(D2.DiffHistory Int)
          ]
      ]
  , testGroup "Monoid laws" [
        testGroup "Right identity" [
            testProperty "Diff k v" $
              rightIdentity @(Diff Int Int)
          , testProperty "AntiDiff k v" $
              QC.expectFailure $
                rightIdentity @(AntiDiff Int Int)
          , testProperty "D2.Diff k v" $
              rightIdentity @(D2.Diff Int Int)
          , testProperty "D2.DiffHistory v" $
              rightIdentity @(D2.DiffHistory Int)
          ]
      , testGroup "Left identity" [
            testProperty "Diff k v" $
              leftIdentity @(Diff Int Int)
          , testProperty "AntiDiff k v" $
              QC.expectFailure $
                leftIdentity @(AntiDiff Int Int)
          , testProperty "D2.Diff k v" $
              leftIdentity @(D2.Diff Int Int)
          , testProperty "D2.DiffHistory v" $
              leftIdentity @(D2.DiffHistory Int)
          ]
      , testGroup "Concatenation" [
            testProperty "Diff k v" $
              concatenation @(Diff Int Int)
          , testProperty "AntiDiff k v" $
              QC.expectFailure $
                concatenation @(AntiDiff Int Int)
          , testProperty "D2.Diff k v" $
              concatenation @(D2.Diff Int Int)
          , testProperty "D2.DiffHistory v" $
              concatenation @(D2.DiffHistory Int)
          ]
      ]
  , testGroup "Group laws" [
        testGroup "Right inverse" [
            testProperty "AntiDiff k v" $
              QC.expectFailure $
                rightInverse @(AntiDiff Int Int)
          , testProperty "D2.Diff k v" $
                rightInverse @(D2.Diff Int Int)
          , testProperty "D2.DiffHistory v" $
              rightInverse @(D2.DiffHistory Int)
          ]
      , testGroup "Left inverse" [
            testProperty "AntiDiff k v" $
              QC.expectFailure $
                leftInverse @(AntiDiff Int Int)
          , testProperty "D2.Diff k v" $
              leftInverse @(D2.Diff Int Int)
          , testProperty "D2.DiffHistory v" $
              leftInverse @(D2.DiffHistory Int)
          ]
      ]
  ]
-}
