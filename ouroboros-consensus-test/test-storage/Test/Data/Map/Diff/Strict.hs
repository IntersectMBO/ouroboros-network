{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Data.Map.Diff.Strict (tests) where

import           Data.Proxy (Proxy (Proxy))

import qualified Data.Map.Diff.Strict as MapDiff

import           Test.Tasty (TestTree, testGroup)

import           Test.Util.Laws
import           Test.Util.Orphans.DiffSeq.Arbitrary ()


-- | Testing @'Semigroup'@, @'Monoid'@ and @'Group'@ laws for diff datatypes.
tests :: TestTree
tests = testGroup "Strict" [
    testGroup "Diff Int Int" [
        testSemigroupLaws p1
      , testMonoidLaws p1
      , testGroupLaws p1
      ]
  , testGroup "DiffHistory Int" [
        testSemigroupLaws p2
      , testMonoidLaws p2
      , testGroupLaws p2
      ]
  ]
    where
      p1 = Proxy @(MapDiff.Diff Int Int)
      p2 = Proxy @(MapDiff.DiffHistory Int)
