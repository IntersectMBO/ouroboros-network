{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Special file we store in the DB dir to avoid unintended deletions
module Ouroboros.Consensus.Node.DbMarker (
    DbMarkerError(..)
  , checkDbMarker
  , lockDbMarkerFile
    -- * For the benefit of testing only
  , dbMarkerFile
  , dbMarkerContents
  , dbMarkerParse
  ) where

import           Control.Monad (void)
import           Control.Monad.Except
import           Control.Monad.Trans.Class (lift)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS.Char8
import           Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.Set as Set
import           Data.Text (Text)
import           Data.Word
import           System.FileLock
import           Text.Read (readMaybe)

import           Cardano.Crypto (ProtocolMagicId (..))

import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Ouroboros.Consensus.Storage.FS.API
import           Ouroboros.Consensus.Storage.FS.API.Types

{-------------------------------------------------------------------------------
  Check proper
-------------------------------------------------------------------------------}

-- | Check database marker
--
-- The database folder will contain folders for the ImmutableDB (@immutable@),
-- the VolatileDB (@volatile@), and the LedgerDB (@ledger@). All three
-- subdatabases can delete files from these folders, e.g., outdated files or
-- files that are deemed invalid.
--
-- For example, when starting a node that will connect to a testnet with a
-- database folder containing mainnet blocks, these blocks will be deemed
-- invalid and will be deleted. This would throw away a perfectly good chain,
-- possibly consisting of gigabytes of data that will have to be synched
-- again.
--
-- To protect us from unwanted deletion of valid files, we first check whether
-- we have been given the path to the right database folder. We do this by
-- reading the 'ProtocolMagicId' of the net from a file stored in the root of
-- the database folder. This file's name is defined in 'dbMarkerFile'.
--
-- * If the 'ProtocolMagicId' from the file matches that of the net, we have
--   the right database folder.
-- * If not, we are opening the wrong database folder and abort by throwing a
--   'DbMarkerError'.
-- * If there is no such file and the folder is empty, we create it and store
--   the net's 'ProtocolMagicId' in it.
-- * If there is no such file, but the folder is not empty, we throw a
--   'DbMarkerError', because we have likely been given the wrong path,
--   maybe to a folder containing user or system files. This includes the case
--   that the 'dbMarkerFile' has been deleted.
-- * If there is such a 'dbMarkerFile', but it could not be read or its
--   contents could not be parsed, we also throw a 'DbMarkerError'.
--
-- Note that an 'FsError' can also be thrown.
checkDbMarker
  :: forall m h. MonadThrow m
  => HasFS m h
  -> MountPoint
     -- ^ Database directory. Should be the mount point of the @HasFS@. Used
     -- in error messages.
  -> ProtocolMagicId
  -> m (Either DbMarkerError ())
checkDbMarker hasFS mountPoint protocolMagicId = runExceptT $ do
    lift $ createDirectoryIfMissing hasFS True root
    fileExists <- lift $ doesFileExist hasFS pFile
    if fileExists then do
      actualProtocolMagicId <- readProtocolMagicIdFile
      when (actualProtocolMagicId /= protocolMagicId) $
        throwError $ ProtocolMagicIdMismatch
          fullPath
          actualProtocolMagicId
          protocolMagicId
    else do
      isEmpty <- lift $ Set.null <$> listDirectory hasFS root
      if isEmpty then
        createProtocolMagicIdFile
      else
        throwError $ NoDbMarkerAndNotEmpty fullPath
  where
    root     = mkFsPath []
    pFile    = fsPathFromList [dbMarkerFile]
    fullPath = fsToFilePath mountPoint pFile

    readProtocolMagicIdFile :: ExceptT DbMarkerError m ProtocolMagicId
    readProtocolMagicIdFile = ExceptT $
      withFile hasFS pFile ReadMode $ \h -> do
        bs <- toStrict <$> hGetAll hasFS h
        runExceptT $ dbMarkerParse fullPath bs

    createProtocolMagicIdFile :: ExceptT DbMarkerError m ()
    createProtocolMagicIdFile = lift $
      withFile hasFS pFile (AppendMode MustBeNew) $ \h ->
        void $ hPutAll hasFS h $
          fromStrict $ dbMarkerContents protocolMagicId

{-------------------------------------------------------------------------------
  Error
-------------------------------------------------------------------------------}

data DbMarkerError =
    -- | There was a 'dbMarkerFile' in the database folder, but it
    -- contained a different 'ProtocolMagicId' than the expected one. This
    -- indicates that this database folder corresponds to another net.
    ProtocolMagicIdMismatch
      FilePath         -- ^ The full path to the 'dbMarkerFile'
      ProtocolMagicId  -- ^ Actual
      ProtocolMagicId  -- ^ Expected

    -- | The database folder contained no 'dbMarkerFile', but also
    -- contained some files. Either the given folder is a non-database folder
    -- or it is a database folder, but its 'dbMarkerFile' has been
    -- deleted.
  | NoDbMarkerAndNotEmpty
      FilePath         -- ^ The full path to the 'dbMarkerFile'

    -- | The database folder contained a 'dbMarkerFile' that could not
    -- be read. The file has been tampered with or it was corrupted somehow.
  | CorruptDbMarker
      FilePath         -- ^ The full path to the 'dbMarkerFile'

    -- | The database is used by another process.
  | DbLocked
      FilePath         -- ^ The full path to the 'dbMarkerFile'
  deriving (Eq, Show)

instance Exception DbMarkerError where
  displayException e = case e of
    ProtocolMagicIdMismatch f actual expected ->
      "Wrong protocolMagicId in \"" <> f <> "\": " <> show actual <>
      ", but expected: " <> show expected
    NoDbMarkerAndNotEmpty f ->
      "Missing \"" <> f <> "\" but the folder was not empty"
    CorruptDbMarker f ->
      "Corrupt or unreadable \"" <> f <> "\""
    DbLocked f ->
      "The db is used by another process. File \"" <> f <> "\" is locked"

{-------------------------------------------------------------------------------
  Locking
-------------------------------------------------------------------------------}

-- | If the file is locked, it throws an exception.
-- The file is unlocked when the @registry@ is closed.
lockDbMarkerFile :: ResourceRegistry IO -> FilePath -> IO ()
lockDbMarkerFile registry dbPath = do
    mlockFile <- allocateEither
                    registry
                    (const $ justToRight <$> tryLockFile fullPath Exclusive)
                    (\f -> unlockFile f >> return True)
    case mlockFile of
      Left () -> throwM $ DbLocked fullPath
      Right _ -> return ()
  where
    pFile    = fsPathFromList [dbMarkerFile]
    fullPath = fsToFilePath (MountPoint dbPath) pFile

    justToRight = maybe (Left ()) Right

{-------------------------------------------------------------------------------
  Configuration (filename, file format)
-------------------------------------------------------------------------------}

dbMarkerFile :: Text
dbMarkerFile = "protocolMagicId"

-- Contents of the DB marker file
--
-- We show the protocol magic ID as a human readable (decimal) number.
--
-- Type annotation on @pmid@ is here so that /if/ the type changes, this
-- code will fail to build. This is important, because if we change the
-- type, we must consider how this affects existing DB deployments.
dbMarkerContents :: ProtocolMagicId -> ByteString
dbMarkerContents (ProtocolMagicId (pmid :: Word32)) =
    BS.Char8.pack $ show pmid

-- | Parse contents of the DB marker file
--
-- Must be inverse to 'dbMarkerContents'
dbMarkerParse :: Monad m
              => FilePath
              -> ByteString
              -> ExceptT DbMarkerError m ProtocolMagicId
dbMarkerParse fullPath bs =
    case readMaybe (BS.Char8.unpack bs) of
      Just pmid -> return     $ ProtocolMagicId pmid
      Nothing   -> throwError $ CorruptDbMarker fullPath
