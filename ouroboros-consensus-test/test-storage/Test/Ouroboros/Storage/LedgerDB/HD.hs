module Test.Ouroboros.Storage.LedgerDB.HD (tests) where

import qualified Test.Ouroboros.Storage.LedgerDB.HD.LMDB as LMDB
import           Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests = testGroup "HD" [
      LMDB.tests
    ]
