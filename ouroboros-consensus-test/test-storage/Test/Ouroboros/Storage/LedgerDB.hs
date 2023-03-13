-- | Ledger DB tests.
--
-- The ledger DB consists of two subcomponents: an in-memory component, which is
-- pure Haskell (no IO anywhere) and so can be tested using normal property
-- tests, and the on-disk component, which is tested with a model based test.
--
module Test.Ouroboros.Storage.LedgerDB (tests) where

import qualified Test.Ouroboros.Storage.LedgerDB.DiskPolicy as DiskPolicy
import qualified Test.Ouroboros.Storage.LedgerDB.InMemory as InMemory
import qualified Test.Ouroboros.Storage.LedgerDB.OnDisk as OnDisk
import           Test.Tasty

tests :: TestTree
tests = testGroup "LedgerDB" [
      InMemory.tests
    , OnDisk.tests
    , DiskPolicy.tests
    ]
