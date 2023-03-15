{-# LANGUAGE CPP #-}
module Test.Ouroboros.Storage (tests) where

import qualified Test.Ouroboros.Storage.ChainDB as ChainDB
import qualified Test.Ouroboros.Storage.ImmutableDB as ImmutableDB
import qualified Test.Ouroboros.Storage.LedgerDB as LedgerDB
import qualified Test.Ouroboros.Storage.VolatileDB as VolatileDB
import           Test.Tasty (TestTree, testGroup)

--
-- The list of all tests
--

tests :: TestTree
tests = testGroup "Storage"
    [ ImmutableDB.tests
    , VolatileDB.tests
    , LedgerDB.tests
    , ChainDB.tests
    ]
