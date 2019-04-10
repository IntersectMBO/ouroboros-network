module Test.Ouroboros.Storage
  ( tests
  ) where

import           GHC.Stack (HasCallStack)

import qualified Test.Ouroboros.Storage.ChainDB as ChainDB
import qualified Test.Ouroboros.Storage.FS as FS
import qualified Test.Ouroboros.Storage.ImmutableDB as ImmutableDB
import qualified Test.Ouroboros.Storage.LedgerDB as LedgerDB
import qualified Test.Ouroboros.Storage.VolatileDB as VolatileDB
import           Test.Tasty (TestTree, testGroup)

--
-- The list of all tests
--

tests :: HasCallStack => FilePath -> TestTree
tests tmpDir = testGroup "Storage"
    [ FS.tests tmpDir
    , ImmutableDB.tests
    , VolatileDB.tests
    , LedgerDB.tests
    , ChainDB.tests
    ]
