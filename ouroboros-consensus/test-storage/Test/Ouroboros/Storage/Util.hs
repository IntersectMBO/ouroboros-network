{-# LANGUAGE FlexibleContexts #-}
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
import           Ouroboros.Storage.FS.IO (runIOFS)
import           Ouroboros.Storage.FS.Sim.MockFS (MockFS)
import qualified Ouroboros.Storage.FS.Sim.MockFS as Mock
import           Ouroboros.Storage.FS.Sim.STM (SimFS, runSimFS)


{------------------------------------------------------------------------------
 Handy combinators
-------------------------------------------------------------------------------}

withMockFS :: MonadSTM m
           => SimFS m (Either e a)
           -> ((Either e a, MockFS) -> m b)
           -> m b
withMockFS sim assertion = do
    r <- runSimFS sim Mock.empty
    assertion r

withMockFSE :: MonadSTM m
            => SimFS m (Either FsError a)
            -> ((Either FsError a, MockFS) -> m b)
            -> m b
withMockFSE = withMockFS

expectError :: (HasCallStack, Show a)
            => (FsError -> Bool)
            -> Either FsError a
            -> String
            -> Assertion
expectError errPred r lbl =
    case r of
         Right x -> fail ("Return value " <> show x <> " was not an error.")
         Left  e -> assertEqual (prettyMsg e) True (errPred e)
    where
        prettyMsg :: FsError -> String
        prettyMsg err =
            lbl <> ": error type was "
                <> prettyFsError err
                <> ", which is not what I was expecting."

-- | Given a \"script\", runs it over a simulated FS and over IO (using a
-- temporary, throw-away folder) and compare the results.
apiEquivalence :: (HasCallStack, Eq a)
               => (forall m. (MonadMask m, MonadSTM m, HasCallStack, HasFSE m) => m (Either b a))
               -> (b -> b -> Bool)
               -> (b -> String)
               -> Assertion
apiEquivalence m cmpError prettyError = do
    sysTmpDir <- getTemporaryDirectory
    withTempDirectory sysTmpDir "cardano." $ \tmpDir -> do
        (r1, fs') <- runSimFS m Mock.empty
        r2 <- runIOFS (MountPoint tmpDir) m
        case (r1, r2) of
            (Left e1, Left e2) ->
                assertBool ("SimFS & IO didn't agree on the API. "
                           <> "The implementation differs.\n\n"
                           <> "Sim returned: " <> prettyError e1  <> "\n\n"
                           <> "IO  returned: " <> prettyError e2  <> "\n\n")
                           (e1 `cmpError` e2)
            (Right x1, Right x2) ->
                assertBool "SimFS & IO didn't yield the same result."
                           (x1 == x2)
            (Left e, Right _) ->
                assertFailure $ "SimFS returned "
                             <> prettyError e
                             <> ", but IO succeeded.\n\n"
                             <> "Sim FS: " <> show fs' <> "\n\n"
            (Right _, Left e) ->
                assertFailure $ "IO returned "
                             <> prettyError e
                             <> ", but SimFS succeeded.\n\n"
                             <> "Sim FS: " <> show fs' <> "\n\n"
