{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RankNTypes       #-}
module Test.Ouroboros.Storage.Util where
-- TODO Move to Test.Util.Storage?

import           Control.Exception (Exception, SomeException)
import qualified Control.Exception as E
import           Control.Monad.Catch (MonadMask)

import           System.Directory (getTemporaryDirectory)
import           System.IO.Temp (withTempDirectory)

import           Test.Tasty.HUnit

import           Control.Monad.Class.MonadSTM (MonadSTM)

import           Ouroboros.Storage.FS.API (HasFS)
import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.FS.IO (ioHasFS)
import           Ouroboros.Storage.FS.Sim.MockFS (MockFS)
import qualified Ouroboros.Storage.FS.Sim.MockFS as Mock
import           Ouroboros.Storage.FS.Sim.STM (SimFS)
import qualified Ouroboros.Storage.FS.Sim.STM as Sim
import           Ouroboros.Storage.ImmutableDB
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling)
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

{------------------------------------------------------------------------------
 Handy combinators
-------------------------------------------------------------------------------}

withMockFS :: Exception e
           => (forall x. IO x -> IO (Either e x))
           -> (Either e (a, MockFS) -> Assertion)
           -> (    HasFS (SimFS IO)
                -> ErrorHandling e (SimFS IO)
                -> SimFS IO a
              )
           -> Assertion
withMockFS tryE assertion sim = do
    r <- tryE $ Sim.runSimFS (sim (Sim.simHasFS     EH.exceptions)
                                  (Sim.liftErrSimFS EH.exceptions))
                             Mock.empty
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
                => (UserError -> Bool)
                -> Either ImmutableDBError a
                -> Assertion
expectUserError userErrPred = expectDBError $ \case
    UserError ue _ | userErrPred ue -> True
    _ -> False

expectUnexpectedError :: (HasCallStack, Show a)
                      => (UnexpectedError -> Bool)
                      -> Either ImmutableDBError a
                      -> Assertion
expectUnexpectedError unexpectetdErrPred = expectDBError $ \case
    UnexpectedError ue | unexpectetdErrPred ue -> True
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

expectDBResult :: (a -> Assertion)
               -> Either ImmutableDBError a
               -> Assertion
expectDBResult = expectResult prettyImmutableDBError

-- | Given a \"script\", runs it over a simulated FS and over IO (using a
-- temporary, throw-away folder) and compare the results.
apiEquivalence :: (HasCallStack, Eq a, Show a, Exception e)
               => (forall x. IO x -> IO (Either e x))
               -> (e -> String)
               -> (e -> e -> Bool)
               -> (Either e a -> Assertion)
               -> (forall m. (MonadMask m, MonadSTM m)
                          => HasFS m
                          -> ErrorHandling e m
                          -> m a
                  )
               -> Assertion
apiEquivalence tryE prettyError sameError resAssert m = do
    sysTmpDir <- getTemporaryDirectory
    withTempDirectory sysTmpDir "cardano." $ \tmpDir -> do
        r1 <- tryE $ Sim.runSimFS (m (Sim.simHasFS     EH.exceptions)
                                     (Sim.liftErrSimFS EH.exceptions))
                                  Mock.empty
        r2 <- tryE $ m (ioHasFS (MountPoint tmpDir)) EH.exceptions
        case (r1, r2) of
          (Left e1, Left e2) -> do
            assertBool ("SimFS & IOFS didn't return the same error:" <>
                        "\nSimFS returned: " <> prettyError e1 <>
                        "\nIOFS  returned: " <> prettyError e2)
                       (e1 `sameError` e2)
            -- Doesn't matter if is e1 or e2, as they are equal
            resAssert (Left e1)
          (Right (x1, _fs'), Right x2) -> do
            assertBool ("SimFS & IOFS didn't return the same result:" <>
                        "\nSimFS returned: " <> show x1 <>
                        "\nIOFS  returned: " <> show x2)
                       (x1 == x2)
            -- Doesn't matter if is x1 or x2, as they are equal
            resAssert (Right x1)
          (Left e, Right _) ->
            fail $ "SimFS returned "
                <> prettyError e
                <> ", but IO succeeded.\n\n"
          (Right (_, fs'), Left e) ->
            fail $ "IO returned "
                <> prettyError e
                <> ", but SimFS succeeded.\n\n"
                <> "Sim FS: " <> show fs' <> "\n\n"

apiEquivalenceFs :: (HasCallStack, Eq a, Show a)
                 => (Either FsError a -> Assertion)
                 -> (forall m. (MonadMask m, MonadSTM m)
                            => HasFS m
                            -> ErrorHandling FsError m
                            -> m a
                    )
                 -> Assertion
apiEquivalenceFs = apiEquivalence tryFS prettyFsError sameFsError

apiEquivalenceDB :: (HasCallStack, Eq a, Show a)
                 => (Either ImmutableDBError a -> Assertion)
                 -> (forall m. (MonadMask m, MonadSTM m)
                            => HasFS m
                            -> ErrorHandling ImmutableDBError m
                            -> m a
                    )
                 -> Assertion
apiEquivalenceDB = apiEquivalence tryImmDB prettyImmutableDBError sameImmutableDBError

tryAny :: IO a -> IO (Either SomeException a)
tryAny = E.try

tryFS :: IO a -> IO (Either FsError a)
tryFS = E.try

tryImmDB :: IO a -> IO (Either ImmutableDBError a)
tryImmDB = fmap squash . E.try . E.try
  where
    -- TODO: With the redesigned error handling I'm not sure whether it's
    -- still necessary that ImmutableDBError can wrap FsError; I think we can
    -- get rid of that. If we do, this function should become
    --
    -- > squash :: Either FsError (Either ImmutableDBError x)
    -- >        -> Either (Either FsError ImmutableDBError) x
    --
    -- and the rest adjusted accordingly.
    squash :: Either FsError (Either ImmutableDBError x) -> Either ImmutableDBError x
    squash = either (Left . UnexpectedError . FileSystemError) id
