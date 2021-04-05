{-# LANGUAGE CPP #-}
module Test.Ouroboros.Storage (tests) where

import qualified Test.Ouroboros.Storage.ChainDB as ChainDB
import qualified Test.Ouroboros.Storage.FS as FS
import qualified Test.Ouroboros.Storage.ImmutableDB as ImmutableDB
import qualified Test.Ouroboros.Storage.LedgerDB as LedgerDB
import qualified Test.Ouroboros.Storage.VolatileDB as VolatileDB
import           Test.Tasty (TestTree, testGroup)

--
-- The list of all tests
--

tests :: FilePath -> TestTree
tests tmpDir = testGroup "Storage" $
    -- The FS tests fail for darwin on CI, see #352. So disable them for now.
    [ FS.tests tmpDir | not darwin ] <>
    [ ImmutableDB.tests
    , VolatileDB.tests
    , LedgerDB.tests
    , ChainDB.tests
    ]

darwin :: Bool
#ifdef darwin_HOST_OS
darwin = True
#else
darwin = False
#endif
