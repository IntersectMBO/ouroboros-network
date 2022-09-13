{-# LANGUAGE TypeApplications #-}

module Test.Data.Semigroupoid (tests) where

import           Data.Monoid (Sum (..))
import           Data.Proxy

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Data.Semigroupoid.Auto
import           Data.Semigroupoid.Laws



tests :: TestTree
tests = testGroup "Semigroupoid" [
    testGroupWithProxy (Proxy @T1) [
        testSemigroupLaws
      , testMonoidLaws
      , testGroupLaws
      ]
  , testGroupWithProxy (Proxy @T2) [
        testSemigroupoidLaws
      , testGroupoidLaws
      ]
  ]

type T1 = Sum (Small Int)
type T2 = Auto T1
