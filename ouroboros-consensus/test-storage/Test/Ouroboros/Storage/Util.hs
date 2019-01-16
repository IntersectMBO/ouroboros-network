{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RankNTypes       #-}
module Test.Ouroboros.Storage.Util where
-- TODO Move to Test.Util.Storage?

import           Control.Monad.Catch (MonadMask)

import           System.Directory (getTemporaryDirectory)
import           System.IO.Temp (withTempDirectory)

import           Test.Tasty.HUnit

import           Control.Monad.Class.MonadSTM (MonadSTM)

import           Ouroboros.Storage.FS.Class (HasFSE)
import           Ouroboros.Storage.FS.Class.Types
import           Ouroboros.Storage.ImmutableDB
import           Ouroboros.Storage.FS.IO (runIOFS)
import           Ouroboros.Storage.FS.Sim.MockFS (MockFS)
import qualified Ouroboros.Storage.FS.Sim.MockFS as Mock
import           Ouroboros.Storage.FS.Sim.STM (SimFS, runSimFS)


{------------------------------------------------------------------------------
 Handy combinators
-------------------------------------------------------------------------------}

{------------------------------------------------------------------------------
 Handy combinators
-------------------------------------------------------------------------------}

withMockFS :: MonadSTM m
           => ((Either e a, MockFS) -> m b)
           -> SimFS m (Either e a)
           -> m b
withMockFS assertion sim = do
    r <- runSimFS sim Mock.empty
    assertion r

withMockFSE :: MonadSTM m
            => ((Either FsError a, MockFS) -> m b)
            -> SimFS m (Either FsError a)
            -> m b
withMockFSE = withMockFS

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

expectResult :: (HasCallStack, Eq a, Show a)
             => (e -> String)
             -> a
             -> Either e a
             -> Assertion
expectResult prettyError expectedResult =
    \case
       Left e -> fail ("Unexpected error: " <> prettyError e)
       Right actualResult -> actualResult @?= expectedResult

expectFsResult :: (HasCallStack, Eq a, Show a)
               => a
               -> Either FsError a
               -> Assertion
expectFsResult = expectResult prettyFsError

expectDBResult :: (HasCallStack, Eq a, Show a)
               => a
               -> Either ImmutableDBError a
               -> Assertion
expectDBResult = expectResult prettyImmutableDBError


-- | Given a \"script\", runs it over a simulated FS and over IO (using a
-- temporary, throw-away folder) and compare the results.
apiEquivalence :: (HasCallStack, Eq a, Show a)
               => (e -> String)
               -> (e -> e -> Bool)
               -> (Either e a -> Assertion)
               -> (forall m. (MonadMask m, MonadSTM m, HasCallStack, HasFSE m)
                          => m (Either e a))
               -> Assertion
apiEquivalence prettyError sameError resAssert m = do
    sysTmpDir <- getTemporaryDirectory
    withTempDirectory sysTmpDir "cardano." $ \tmpDir -> do
        (r1, fs') <- runSimFS m Mock.empty
        r2 <- runIOFS (MountPoint tmpDir) m
        case (r1, r2) of
            (Left e1, Left e2) -> do
                assertBool ("SimFS & IOFS didn't return the same error:" <>
                            "\nSimFS returned: " <> prettyError e1 <>
                            "\nIOFS  returned: " <> prettyError e2)
                           (e1 `sameError` e2)
                -- Doesn't matter if is r1 or r2, as they are equal
                resAssert r1
            (Right x1, Right x2) -> do
                assertBool ("SimFS & IOFS didn't return the same result:" <>
                            "\nSimFS returned: " <> show x1 <>
                            "\nIOFS  returned: " <> show x2)
                           (x1 == x2)
                -- Doesn't matter if is r1 or r2, as they are equal
                resAssert r1
            (Left e, Right _) ->
                fail $ "SimFS returned "
                    <> prettyError e
                    <> ", but IO succeeded.\n\n"
                    <> "Sim FS: " <> show fs' <> "\n\n"
            (Right _, Left e) ->
                fail $ "IO returned "
                    <> prettyError e
                    <> ", but SimFS succeeded.\n\n"
                    <> "Sim FS: " <> show fs' <> "\n\n"

apiEquivalenceFs :: (HasCallStack, Eq a, Show a)
                 => (Either FsError a -> Assertion)
                 -> (forall m. (MonadMask m, MonadSTM m, HasCallStack, HasFSE m)
                            => m (Either FsError a))
                 -> Assertion
apiEquivalenceFs = apiEquivalence prettyFsError sameFsError

apiEquivalenceDB :: (HasCallStack, Eq a, Show a)
                 => (Either ImmutableDBError a -> Assertion)
                 -> (forall m. (MonadMask m, MonadSTM m, HasCallStack, HasFSE m)
                            => m (Either ImmutableDBError a))
                 -> Assertion
apiEquivalenceDB = apiEquivalence prettyImmutableDBError sameDBError
