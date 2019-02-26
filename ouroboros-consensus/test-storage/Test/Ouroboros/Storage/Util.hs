{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Ouroboros.Storage.Util where
-- TODO Move to Test.Util.Storage?

import           Control.Exception (Exception, SomeException)
import qualified Control.Exception as E
import           Control.Monad.Catch (MonadCatch, MonadMask)
import qualified Control.Monad.Catch as C
import           Control.Monad.Class.MonadSTM (MonadSTM)

import qualified Data.Binary as Binary
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as LC8
import           Data.Int (Int64)
import qualified Data.Map.Strict as M
import           Data.Serialize
import           Data.String (IsString (..))
import           Data.Typeable

import           System.Directory (getTemporaryDirectory)
import qualified System.IO as IO
import           System.IO.Temp (withTempDirectory)

import           Test.QuickCheck (ASCIIString (..), Arbitrary (..), Property,
                     collect, suchThat)
import           Test.Tasty.HUnit

import           Ouroboros.Consensus.Util (repeatedly)

import           Ouroboros.Storage.FS.API (HasFS (..), withFile)
import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.FS.IO (ioHasFS)
import           Ouroboros.Storage.FS.Sim.MockFS (MockFS)
import qualified Ouroboros.Storage.FS.Sim.MockFS as Mock
import           Ouroboros.Storage.FS.Sim.STM (SimFS)
import qualified Ouroboros.Storage.FS.Sim.STM as Sim
import           Ouroboros.Storage.ImmutableDB (ImmutableDBError (..),
                     prettyImmutableDBError, sameImmutableDBError)
import qualified Ouroboros.Storage.ImmutableDB as Immutable
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling)
import qualified Ouroboros.Storage.Util.ErrorHandling as EH
import           Ouroboros.Storage.VolatileDB (Parser (..), Slot (..),
                     VolatileDBError (..), sameVolatileDBError)
import qualified Ouroboros.Storage.VolatileDB as Volatile

{------------------------------------------------------------------------------
 Handy combinators
-------------------------------------------------------------------------------}

withMockFS :: Exception e
           => (forall x. IO x -> IO (Either e x))
           -> (Either e (a, MockFS) -> Assertion)
           -> (    HasFS (SimFS IO) Mock.Handle
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
               -> (forall m h. (MonadMask m, MonadSTM m)
                            => HasFS m h
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
                 -> (forall m h. (MonadMask m, MonadSTM m)
                              => HasFS m h
                              -> ErrorHandling FsError m
                              -> m a
                    )
                 -> Assertion
apiEquivalenceFs = apiEquivalence tryFS prettyFsError sameFsError

apiEquivalenceImmDB :: (HasCallStack, Eq a, Show a)
                    => (Either ImmutableDBError a -> Assertion)
                    -> (forall m h. (MonadMask m, MonadSTM m)
                                 => HasFS m h
                                 -> ErrorHandling ImmutableDBError m
                                 -> m a
                       )
                    -> Assertion
apiEquivalenceImmDB = apiEquivalence tryImmDB prettyImmutableDBError sameImmutableDBError

apiEquivalenceVolDB :: (HasCallStack, Eq a, Show a, Show blockId, Typeable blockId, Eq blockId)
                    => (Either (VolatileDBError blockId) a -> Assertion)
                    -> (forall m h. (MonadMask m, MonadSTM m)
                                 => HasFS m h
                                 -> ErrorHandling (VolatileDBError blockId) m
                                 -> m a
                       )
                    -> Assertion
apiEquivalenceVolDB = apiEquivalence tryVolDB show sameVolatileDBError

tryAny :: IO a -> IO (Either SomeException a)
tryAny = E.try

tryFS :: IO a -> IO (Either FsError a)
tryFS = E.try

tryImmDB :: MonadCatch m => m a -> m (Either ImmutableDBError a)
tryImmDB = tryDB (UnexpectedError . Immutable.FileSystemError)

tryVolDB :: (Show blockId, Typeable blockId, MonadCatch m) => m a -> m (Either (VolatileDBError blockId) a)
tryVolDB = tryDB Volatile.FileSystemError

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
newtype Blob = MkBlob { getBlob :: BS.Builder }

instance Show Blob where
    show = show . BS.toLazyByteString . getBlob

instance Arbitrary Blob where
    arbitrary = do
      str <- (getASCIIString <$> arbitrary) `suchThat` (not . null)
      return $ fromString str
    shrink (MkBlob b) =
      [ fromString s'
      | let s = ASCIIString $ LC8.unpack $ BS.toLazyByteString b
      , s' <- getASCIIString <$> shrink s
      , not (null s') ]

blobToBS :: Blob -> ByteString
blobToBS = BL.toStrict . BS.toLazyByteString . getBlob

blobFromBS :: ByteString -> Blob
blobFromBS = MkBlob . BS.byteString

instance IsString Blob where
    fromString = blobFromBS . C8.pack

type MyBlockId = Word

type Block = (Word, Int)

toBinary :: MyBlockId -> BL.ByteString
toBinary = Binary.encode . toBlock

fromBinary :: BL.ByteString -> MyBlockId
fromBinary = fromBlock . Binary.decode

toSlot :: MyBlockId -> Slot
toSlot = Slot

toBlock :: MyBlockId -> Block
toBlock bid = (bid, 0)

fromBlock :: Block -> MyBlockId
fromBlock (bid, 0) = bid
fromBlock _        = error "wrong payload"

binarySize :: Int
binarySize = 16

myParser :: (MonadMask m) => Volatile.Parser m MyBlockId
myParser = Volatile.Parser {
    Volatile.parse = parseImpl
    }

parseImpl :: forall m h. (MonadMask m)
          => HasFS m h
          -> ErrorHandling (VolatileDBError MyBlockId) m
          -> [String]
          -> m (Int64, M.Map Int64 (Int, MyBlockId))
parseImpl hasFS@HasFS{..} err path =
    withFile hasFS path IO.ReadMode $ \hndl -> do
        let go :: M.Map Int64 (Int, MyBlockId)
               -> Int64
               -> Int
               -> [MyBlockId]
               -> m (Int64, M.Map Int64 (Int, MyBlockId))
            go mp n trials bids = do
                bs <- hGet hndl binarySize
                if BS.length bs == 0 then return (n, mp)
                else case decode bs of
                    Left str -> EH.throwError err $ VParserError $ Volatile.DecodeFailed str (BS.length bs)
                    Right bl -> do
                        let bid = fromBlock bl
                        if elem bid bids
                        then EH.throwError err $ Volatile.VParserError $ Volatile.DuplicatedSlot $ M.singleton bid (path, path)
                        else let mp' = M.insert n (binarySize, bid) mp
                            in go mp' (n + fromIntegral binarySize) (trials + 1) (bid : bids)
        go M.empty 0 0 []
