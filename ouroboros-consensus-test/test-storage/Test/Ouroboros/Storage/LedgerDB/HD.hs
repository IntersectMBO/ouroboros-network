module Test.Ouroboros.Storage.LedgerDB.HD (tests) where

import qualified Test.Ouroboros.Storage.LedgerDB.HD.BackingStore as BS
import qualified Test.Ouroboros.Storage.LedgerDB.HD.DiffSeq as DiffSeq
import qualified Test.Ouroboros.Storage.LedgerDB.HD.LMDB as LMDB
import           Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests = testGroup "HD" [
      BS.tests
    , DiffSeq.tests
    , LMDB.tests
    ]
