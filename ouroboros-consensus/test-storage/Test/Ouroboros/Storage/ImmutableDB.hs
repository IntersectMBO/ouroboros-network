{-# LANGUAGE OverloadedStrings #-}
-- TODO: This module consists of
--
-- * Some unit tests for the immutable DB.
--   These should move to a module of their own, and be similarly be replaced
--   by q-s-m tests.
module Test.Ouroboros.Storage.ImmutableDB
  ( tests
  ) where

import           Control.Monad.Except (ExceptT(..), runExceptT)

import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy.Char8 as C8

import           GHC.Stack (HasCallStack)

import           Ouroboros.Storage.ImmutableDB

import           Test.Ouroboros.Storage.ImmutableDB.Sim (demoScript)
import           Test.Ouroboros.Storage.Util
import           Test.QuickCheck
import           Test.QuickCheck.Monadic (monadicIO, pick, run)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (testProperty)


tests :: HasCallStack => TestTree
tests = testGroup "Immutable Storage"
    [ testCase     "What you store is what you get" test_appendAndGet
    , testProperty "append/get roundtrip" prop_appendAndGetRoundtrip
    , testCase     "Inconsistent slot error equivalence" test_inconsistentSlotError
    , testCase     "Epoch is read only error equivalence" test_epochIsReadOnlyError
    , testCase     "Read from invalid epoch error equivalence" test_slotDoesNotExistError
      -- demoScript
    , testCase     "demoScript equivalence" test_demoSim
    ]



--
-- Tests for the immutable storage
--

test_appendAndGet :: Assertion
test_appendAndGet = withMockFS (\(r, _) -> expectDBResult "haskell" r) $
    withDB ["test"] 0 $ \db -> runExceptT $ do
      ExceptT $ appendBinaryBlob db (0, 0) "haskell"
      ExceptT $ getBinaryBlob db (0, 0)

prop_appendAndGetRoundtrip :: Property
prop_appendAndGetRoundtrip = monadicIO $ do
    input <- C8.pack <$> pick (arbitrary `suchThat` (not . null))
    run $ apiEquivalenceDB (expectDBResult (C8.toStrict input)) $
      withDB ["test"] 0 $ \db -> runExceptT $ do
        ExceptT $ appendBinaryBlob db (0, 0) (BS.lazyByteString input)
        ExceptT $ getBinaryBlob db (0, 0)

test_demoSim :: HasCallStack => Assertion
test_demoSim = apiEquivalenceDB (expectDBResult blobs) demoScript
  where
    blobs = ["haskell", "nice", "cardano", "test"]

-- Trying to append to a slot \"in the past\" should be an error, both in Sim
-- and IO.
test_inconsistentSlotError :: HasCallStack => Assertion
test_inconsistentSlotError =
    apiEquivalenceDB (expectDBError isInconsistentSlotError) $
      withDB ["test"] 0 $ \db -> runExceptT $ do
        ExceptT $ appendBinaryBlob db (0, 3) "test"
        ExceptT $ appendBinaryBlob db (0, 2) "haskell"
  where
    isInconsistentSlotError InconsistentSlotError {} = True
    isInconsistentSlotError _ = False

test_slotDoesNotExistError :: HasCallStack => Assertion
test_slotDoesNotExistError =
    apiEquivalenceDB (expectDBError isSlotDoesNotExistError) $
      withDB ["test"] 0 $ \db -> getBinaryBlob db (0, 0)
  where
    isSlotDoesNotExistError SlotDoesNotExistError {} = True
    isSlotDoesNotExistError _ = False

-- Trying to re-open the DB not on the most-recent-epoch should trigger an
-- error, both in Sim and IO.
test_epochIsReadOnlyError :: HasCallStack => Assertion
test_epochIsReadOnlyError =
  apiEquivalenceDB (expectDBError isEpochIsReadOnlyError) $
  runExceptT $ do
    ExceptT $ withDB ["test"] 0 $ \db -> runExceptT $ do
      ExceptT $ appendBinaryBlob db (0, 0) "test"
      ExceptT $ appendBinaryBlob db (1, 0) "haskell"
      -- The second withDB should fail.
    ExceptT $ withDB ["test"] 0 $ \_ -> return $ Right ()
    return ()
  where
    isEpochIsReadOnlyError EpochIsReadOnlyError {} = True
    isEpochIsReadOnlyError _ = False
