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

import qualified Data.ByteString.Builder as BL
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
    [ testCase "What you store is what you get" test_appendAndGet
    , testProperty "append/get roundtrip" prop_appendAndGetRoundtrip
    , testProperty "Inconsistent slot error equivalence" prop_inconsistentSlotErrorEquivalence
    , testProperty "Epoch is read only error equivalence" prop_epochIsReadOnlyErrorEquivalence
    , testProperty "Read from invalid epoch error equivalence" prop_slotDoesNotExistErrorEquivalence
      -- demoScript
    , testProperty "demoScript equivalence" prop_demoSimEquivalence
    ]



--
-- Tests for the immutable storage
--

test_appendAndGet :: Assertion
test_appendAndGet =
    withMockFS (withDB ["demo"] 0 $ \db -> runExceptT $ do
          ExceptT $ appendBinaryBlob db (0, RelativeSlot 0) (BL.lazyByteString . C8.pack $ "haskell")
          ExceptT $ getBinaryBlob db (0, RelativeSlot 0)
    ) $ \(r, _) ->
      case r of
           Left e  -> fail $ prettyImmutableDBError e
           Right b -> b @?= "haskell"

prop_appendAndGetRoundtrip :: Property
prop_appendAndGetRoundtrip = monadicIO $ do
    input <- C8.pack <$> pick arbitrary
    run $ apiEquivalence (withDB ["demo"] 0 $ \db -> runExceptT $ do
            ExceptT $ appendBinaryBlob db (0, RelativeSlot 0) (BL.lazyByteString input)
            r <- ExceptT $ getBinaryBlob db (0, RelativeSlot 0)
            return (r == C8.toStrict input, r)
        ) sameDBError prettyImmutableDBError

prop_demoSimEquivalence :: HasCallStack => Property
prop_demoSimEquivalence = monadicIO $ do
    run $ apiEquivalence demoScript sameDBError prettyImmutableDBError

-- Trying to append to a slot \"in the past\" should be an error, both in Sim
-- and IO.
prop_inconsistentSlotErrorEquivalence :: HasCallStack => Property
prop_inconsistentSlotErrorEquivalence = monadicIO $ do
    run $ apiEquivalence (withDB ["demo"] 0 $ \db -> runExceptT $ do
                             ExceptT $ appendBinaryBlob db (0, RelativeSlot 3)
                                                           (BL.lazyByteString . C8.pack $ "test")
                             ExceptT $ appendBinaryBlob db (0, RelativeSlot 2)
                                                           (BL.lazyByteString . C8.pack $ "haskell")
                             return ()
                         ) sameDBError prettyImmutableDBError

prop_slotDoesNotExistErrorEquivalence :: HasCallStack => Property
prop_slotDoesNotExistErrorEquivalence = monadicIO $ do
    run $ apiEquivalence (withDB ["demo"] 0 $ \db -> runExceptT $ do
                             _   <- ExceptT $ getBinaryBlob db (0, RelativeSlot 0)
                             return ()
                         ) sameDBError prettyImmutableDBError

-- Trying to re-open the DB not on the most-recent-epoch should trigger an
-- error, both in Sim and IO.
prop_epochIsReadOnlyErrorEquivalence :: HasCallStack => Property
prop_epochIsReadOnlyErrorEquivalence = monadicIO $ do
    run $ apiEquivalence (runExceptT $ do
                             ExceptT $ withDB ["demo"] 0 $ \db -> runExceptT $ do
                               ExceptT $ appendBinaryBlob db (0, RelativeSlot 0)
                                                             (BL.lazyByteString . C8.pack $ "test")
                               ExceptT $ appendBinaryBlob db (1, RelativeSlot 0)
                                                             (BL.lazyByteString . C8.pack $ "haskell")
                             -- The second withDB should fail.
                             ExceptT $ withDB ["demo"] 0 $ \_ -> return $ Right ()
                             return ()
                         ) sameDBError prettyImmutableDBError
