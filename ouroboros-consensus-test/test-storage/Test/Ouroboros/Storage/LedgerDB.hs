module Test.Ouroboros.Storage.LedgerDB (tests) where

import           Test.Tasty

import qualified Test.Ouroboros.Storage.LedgerDB.InMemory as InMemory
import qualified Test.Ouroboros.Storage.LedgerDB.OnDisk as OnDisk

tests :: TestTree
tests = testGroup "LedgerDB" [
      InMemory.tests
    , OnDisk.tests
    ]
