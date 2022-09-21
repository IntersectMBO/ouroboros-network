module Test.Ouroboros.Storage.LedgerDB (tests) where

import           Test.Tasty

import qualified Test.Ouroboros.Storage.LedgerDB.DbChangelog as DbChangelog
import qualified Test.Ouroboros.Storage.LedgerDB.DiskPolicy as DiskPolicy
import qualified Test.Ouroboros.Storage.LedgerDB.HD as HD
import qualified Test.Ouroboros.Storage.LedgerDB.InMemory as InMemory
import qualified Test.Ouroboros.Storage.LedgerDB.OnDisk as OnDisk

tests :: TestTree
tests = testGroup "LedgerDB" [
      HD.tests
    , InMemory.tests
    , OnDisk.tests
    , DiskPolicy.tests
    , DbChangelog.tests
    ]
