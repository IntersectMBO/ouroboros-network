{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Test.Ouroboros.Storage.Util
  ( -- * Handy combinators
    withMockFS
  , expectFsError
  , expectUserError
  , expectUnexpectedError
  , expectFsResult
  , expectImmDBResult
  , expectVolDBResult
  , apiEquivalenceFs
  , apiEquivalenceImmDB
  , tryFS
  ) where

import           System.Directory (getTemporaryDirectory)
import           System.IO.Temp (withTempDirectory)

import           Test.Tasty.HUnit

import           Ouroboros.Consensus.Util.IOLike (try)

import           Ouroboros.Consensus.Storage.FS.API (HasFS (..))
import           Ouroboros.Consensus.Storage.FS.API.Types
import           Ouroboros.Consensus.Storage.FS.IO (ioHasFS)
import           Ouroboros.Consensus.Storage.ImmutableDB (ImmutableDBError (..),
                     prettyImmutableDBError, sameImmutableDBError)
import qualified Ouroboros.Consensus.Storage.ImmutableDB as Immutable
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Util (tryImmDB)
import           Ouroboros.Consensus.Storage.IO (sameError)
import           Ouroboros.Consensus.Storage.VolatileDB (VolatileDBError)

import           Test.Util.FS.Sim.MockFS (HandleMock, MockFS)
import qualified Test.Util.FS.Sim.MockFS as Mock
import qualified Test.Util.FS.Sim.STM as Sim

{------------------------------------------------------------------------------
 Handy combinators
-------------------------------------------------------------------------------}

withMockFS :: (forall x. IO x -> IO (Either e x))
           -> (Either e (a, MockFS) -> Assertion)
           -> (HasFS IO HandleMock -> IO a)
           -> Assertion
withMockFS tryE assertion sim = do
    r <- tryE $ Sim.runSimFS Mock.empty sim
    assertion r

expectError :: (HasCallStack, Show a)
            => (e -> String)
            -> (e -> Bool)
            -> Either e a
            -> Assertion
expectError prettyError errPred r = case r of
    Left  e -> assertEqual (prettyMsg e) True (errPred e)
    Right x -> fail ("Return value " <> show x <> " was not an error.")
  where
    prettyMsg err = "Error type was "
                 <> prettyError err
                 <> ", which is not what I was expecting."

expectFsError :: (HasCallStack, Show a)
              => FsErrorType
              -> Either FsError a
              -> Assertion
expectFsError errTy = expectError prettyFsError $
    \case
       FsError { fsErrorType = errTy' } | errTy' == errTy -> True
       _ -> False

expectDBError :: (HasCallStack, Show a)
              => (ImmutableDBError -> Bool)
              -> Either ImmutableDBError a
              -> Assertion
expectDBError = expectError prettyImmutableDBError

expectUserError :: (HasCallStack, Show a)
                => (Immutable.UserError -> Bool)
                -> Either ImmutableDBError a
                -> Assertion
expectUserError userErrPred = expectDBError $ \case
    Immutable.UserError ue _ | userErrPred ue -> True
    _ -> False

expectUnexpectedError :: (HasCallStack, Show a)
                      => (Immutable.UnexpectedError -> Bool)
                      -> Either ImmutableDBError a
                      -> Assertion
expectUnexpectedError unexpectetdErrPred = expectDBError $ \case
    Immutable.UnexpectedError ue | unexpectetdErrPred ue -> True
    _ -> False

expectResult :: (e -> String)
             -> (a -> Assertion)
             -> Either e a
             -> Assertion
expectResult prettyError checkResult =
    \case
       Left e -> fail ("Unexpected error: " <> prettyError e)
       Right actualResult -> checkResult actualResult

expectFsResult :: (a -> Assertion)
               -> Either FsError a
               -> Assertion
expectFsResult = expectResult prettyFsError

expectImmDBResult :: (a -> Assertion)
                  -> Either ImmutableDBError a
                  -> Assertion
expectImmDBResult = expectResult prettyImmutableDBError

expectVolDBResult :: (a -> Assertion)
                  -> Either VolatileDBError a
                  -> Assertion
expectVolDBResult = expectResult show

-- | Given a \"script\", runs it over a simulated FS and over IO (using a
-- temporary, throw-away folder) and compare the results.
apiEquivalence :: (HasCallStack, Eq a, Show a)
               => (forall x. IO x -> IO (Either e x))
               -> (e -> String)
               -> (e -> e -> Bool)
               -> (Either e a -> Assertion)
               -> (forall h. Eq h => HasFS IO h -> IO a)
               -> Assertion
apiEquivalence tryE prettyError sameErr resAssert m = do
    sysTmpDir <- getTemporaryDirectory
    withTempDirectory sysTmpDir "cardano." $ \tmpDir -> do
        r1 <- tryE $ Sim.runSimFS Mock.empty m
        r2 <- tryE $ m (ioHasFS (MountPoint tmpDir))
        case (r1, r2) of
          (Left e1, Left e2) -> do
            assertBool ("SimFS & IOFS didn't return the same error:" <>
                        "\nSimFS returned: " <> prettyError e1 <>
                        "\nIOFS  returned: " <> prettyError e2)
                       (e1 `sameErr` e2)
            -- Doesn't matter if is e1 or e2, as they are equal
            resAssert (Left e1)
          (Right (x1, _fs'), Right x2) -> do
            assertBool ("SimFS & IOFS didn't return the same result:" <>
                        "\nSimFS returned: " <> show x1 <>
                        "\nIOFS  returned: " <> show x2)
                       (x1 == x2)
            -- Doesn't matter if is x1 or x2, as they are equal
            resAssert (Right x1)
          (Left e, Right r) ->
            fail $ "SimFS failed with: "
                <> prettyError e
                <> "\nIO succeeded with: "
                <> show r
                <> ".\n\n"
          (Right (_, fs'), Left e) ->
            fail $ "IO failed with: "
                <> prettyError e
                <> "\nSimFS succeeded with "
                <> show fs'
                <> ".\n\n"

apiEquivalenceFs :: (HasCallStack, Eq a, Show a)
                 => (Either FsError a -> Assertion)
                 -> (forall h. Eq h => HasFS IO h -> IO a)
                 -> Assertion
apiEquivalenceFs = apiEquivalence tryFS prettyFsError sameError

apiEquivalenceImmDB :: (HasCallStack, Eq a, Show a)
                    => (Either ImmutableDBError a -> Assertion)
                    -> (forall h. Eq h => HasFS IO h -> IO a)
                    -> Assertion
apiEquivalenceImmDB =
    apiEquivalence tryImmDB prettyImmutableDBError sameImmutableDBError

tryFS :: IO a -> IO (Either FsError a)
tryFS = try
