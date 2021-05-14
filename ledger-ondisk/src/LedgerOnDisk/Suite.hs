{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# language TypeApplications #-}
module LedgerOnDisk.Suite where

import qualified LedgerOnDisk.QSM.Suite(tests)
import Test.Tasty
import qualified Test.Tasty.QuickCheck.Laws as Laws

import qualified LedgerOnDisk.Class
import qualified LedgerOnDisk.WWB
import Data.Proxy

testManyLaws :: forall a proxy. proxy a -> TestName -> [Proxy a -> TestTree] -> TestTree
testManyLaws _ name = testGroup name . fmap ($ Proxy @ a)

testLaws :: TestTree
testLaws = testGroup "Laws"
    [ testManyLaws (Proxy @ (LedgerOnDisk.Class.D Int)) "D"
      [Laws.testEqLaws, Laws.testSemigroupLaws, Laws.testMonoidLaws ]
    , testManyLaws (Proxy @ (LedgerOnDisk.WWB.InMemoryEntryMeasure Int Int)) "InMemoryEntryMeasure"
      [Laws.testSemigroupLaws, Laws.testMonoidLaws]
    ]

tests :: TestTree
tests = testGroup "LedgerOnDisk"
  [ LedgerOnDisk.QSM.Suite.tests
  , testLaws
  ]
