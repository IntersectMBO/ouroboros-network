{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Ouroboros.Consensus.Node.DbLock (
    DbLocked (..)
  , withLockDB
  , dbLockFile
  ) where

import           Control.Monad.Class.MonadTimer
import           Data.Text (Text)
import qualified Data.Time.Clock as Time
import           System.FileLock

import           Ouroboros.Consensus.Storage.FS.API
import           Ouroboros.Consensus.Storage.FS.API.Types
import           Ouroboros.Consensus.Util.IOLike

-- | We use an empty file as a lock of the db so that the database cannot be
-- opened by more than one process. We wait up to 2 seconds to take the lock,
-- before timing out and throwing a 'DbLocked' exception. Some systems may
-- delete the empty file when all its handles are closed. This is not an issue,
-- since the file is created if it doesn't exist.
withLockDB :: HasFS IO h ->  FilePath -> IO a -> IO a
withLockDB hasFS dbPath action = do
    createDirectoryIfMissing hasFS True root
    bracket acquireLock unlockFile (const action)
  where
    -- We want to avoid blocking the main thread at an uninterruptible ffi, to
    -- avoid unresponsiveness to timeouts and ^C. So we use 'async' and let a
    -- new thread do the actual ffi call.
    --
    -- We shouldn't be tempted to use 'withAsync', which is usually mentioned
    -- as a better alternative, or try to synchronously cancel the forked
    -- thread during cleanup, since this would block the main thread and negate
    -- the whole point of using 'async'. We try our best to clean up resources,
    -- but this may not be entirely possible, because of the uninterruptible
    -- system call. This is not a big issue though, since if we fail to take the
    -- lock succesfully, the whole process will soon die.
    acquireLock :: IO FileLock
    acquireLock = do
      lockFileAsync <- async (lockFile lockFilePath Exclusive)
      mbLock <- timeout (Time.secondsToDiffTime 2) $ wait lockFileAsync
      case mbLock of
        -- We timed out while waiting on the lock. The db is still locked.
        Nothing   -> throwM $ DbLocked lockFilePath
        Just lock -> return lock

    root         = mkFsPath []
    pFile        = fsPathFromList [dbLockFile]
    lockFilePath = fsToFilePath (MountPoint dbPath) pFile

dbLockFile :: Text
dbLockFile = "lock"

newtype DbLocked = DbLocked FilePath
    deriving (Eq, Show)

instance Exception DbLocked where
    displayException (DbLocked f) =
      "The db is used by another process. File \"" <> f <> "\" is locked"
