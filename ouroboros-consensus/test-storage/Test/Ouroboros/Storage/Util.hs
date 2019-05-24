{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Ouroboros.Storage.Util where
-- TODO Move to Test.Util.Storage?

import           Control.Exception (Exception, SomeException)
import qualified Control.Exception as E
import           Control.Monad.Class.MonadThrow (MonadCatch)
import qualified Control.Monad.Class.MonadThrow as C

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as LC8
import           Data.String (IsString (..))
import           Data.Typeable

import           System.Directory (getTemporaryDirectory)
import           System.IO.Temp (withTempDirectory)

import           Test.QuickCheck (ASCIIString (..), Arbitrary (..), Property,
                     collect, suchThat)
import           Test.Tasty.HUnit

import           Ouroboros.Consensus.Util (repeatedly)

import           Ouroboros.Storage.FS.API (HasFS (..))
import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.FS.IO (ioHasFS)
import           Ouroboros.Storage.FS.Sim.MockFS (MockFS)
import qualified Ouroboros.Storage.FS.Sim.MockFS as Mock
import qualified Ouroboros.Storage.FS.Sim.STM as Sim
import           Ouroboros.Storage.ImmutableDB (ImmutableDBError (..),
                     prettyImmutableDBError, sameImmutableDBError)
import qualified Ouroboros.Storage.ImmutableDB as Immutable
import           Ouroboros.Storage.IO (sameError)
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling)
import qualified Ouroboros.Storage.Util.ErrorHandling as EH
import           Ouroboros.Storage.VolatileDB (VolatileDBError (..),
                     sameVolatileDBError)
import           Ouroboros.Storage.VolatileDB.Util

{------------------------------------------------------------------------------
 Handy combinators
-------------------------------------------------------------------------------}

withMockFS :: Exception e
           => (forall x. IO x -> IO (Either e x))
           -> (Either e (a, MockFS) -> Assertion)
           -> (HasFS IO Mock.Handle -> ErrorHandling e IO -> IO a)
           -> Assertion
withMockFS tryE assertion sim = do
    r <- tryE $ Sim.runSimFS EH.exceptions Mock.empty (flip sim EH.exceptions)
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

expectVolDBResult :: (Show blockId)
                  => (a -> Assertion)
                  -> Either (VolatileDBError blockId) a
                  -> Assertion
expectVolDBResult = expectResult show

-- | Given a \"script\", runs it over a simulated FS and over IO (using a
-- temporary, throw-away folder) and compare the results.
apiEquivalence :: (HasCallStack, Eq a, Show a, Exception e)
               => (forall x. IO x -> IO (Either e x))
               -> (e -> String)
               -> (e -> e -> Bool)
               -> (Either e a -> Assertion)
               -> (forall h. HasFS IO h -> ErrorHandling e IO -> IO a)
               -> Assertion
apiEquivalence tryE prettyError sameErr resAssert m = do
    sysTmpDir <- getTemporaryDirectory
    withTempDirectory sysTmpDir "cardano." $ \tmpDir -> do
        r1 <- tryE $ Sim.runSimFS EH.exceptions Mock.empty (flip m EH.exceptions)
        r2 <- tryE $ m (ioHasFS (MountPoint tmpDir)) EH.exceptions
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
                 -> (forall h. HasFS IO h -> ErrorHandling FsError IO -> IO a)
                 -> Assertion
apiEquivalenceFs = apiEquivalence tryFS prettyFsError sameError

apiEquivalenceImmDB :: (HasCallStack, Eq a, Show a)
                    => (Either ImmutableDBError a -> Assertion)
                    -> (forall h. HasFS IO h -> ErrorHandling ImmutableDBError IO -> IO a)
                    -> Assertion
apiEquivalenceImmDB = apiEquivalence tryImmDB prettyImmutableDBError sameImmutableDBError

apiEquivalenceVolDB :: (HasCallStack, Eq a, Show a, Show blockId, Typeable blockId, Eq blockId)
                    => (Either (VolatileDBError blockId) a -> Assertion)
                    -> (forall h. HasFS IO h -> ErrorHandling (VolatileDBError blockId) IO -> IO a)
                    -> Assertion
apiEquivalenceVolDB = apiEquivalence tryVolDB show sameVolatileDBError

tryAny :: IO a -> IO (Either SomeException a)
tryAny = E.try

tryFS :: IO a -> IO (Either FsError a)
tryFS = E.try

tryImmDB :: MonadCatch m => m a -> m (Either ImmutableDBError a)
tryImmDB = tryDB (Immutable.UnexpectedError . Immutable.FileSystemError)

tryDB :: forall e a m. (Exception e, MonadCatch m) => (FsError -> e) -> m a -> m (Either e a)
tryDB fromFS = fmap squash . C.try . C.try
  where
    -- TODO: With the redesigned error handling I'm not sure whether it's
    -- still necessary that e can wrap FsError, for either databases;
    -- I think we can get rid of that. If we do, this function should become
    --
    -- > squash :: Either FsError (Either e x)
    -- >        -> Either (Either FsError e) x
    --
    -- and the rest adjusted accordingly.
    squash :: Either FsError (Either e x) -> Either e x
    squash = either (Left . fromFS) id


{-------------------------------------------------------------------------------
  QuickCheck auxiliary
-------------------------------------------------------------------------------}

collects :: Show a => [a] -> Property -> Property
collects = repeatedly collect



{------------------------------------------------------------------------------
  Blob
------------------------------------------------------------------------------}

-- For the custom 'Show' and 'Arbitrary' instances
--
-- A builder of a non-empty bytestring.
newtype Blob = MkBlob { getBlob :: ByteString }
    deriving (Show)

instance Arbitrary Blob where
    arbitrary = do
      str <- (getASCIIString <$> arbitrary) `suchThat` (not . null)
      return $ fromString str
    shrink (MkBlob b) =
      [ fromString s'
      | let s = ASCIIString $ LC8.unpack $ BL.fromStrict b
      , s' <- getASCIIString <$> shrink s
      , not (null s') ]

blobToBS :: Blob -> ByteString
blobToBS = getBlob

blobFromBS :: ByteString -> Blob
blobFromBS = MkBlob

instance IsString Blob where
    fromString = blobFromBS . C8.pack
