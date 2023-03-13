{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Consensus.Node.DbLock (
    DbLocked (..)
  , withLockDB
    -- * Defaults
  , dbLockFsPath
  , dbLockTimeout
    -- * For testing purposes
  , withLockDB_
  ) where

import           Control.Monad.Class.MonadTimer
import qualified Data.Time.Clock as Time
import           Ouroboros.Consensus.Storage.FS.API.Types
import           Ouroboros.Consensus.Util.FileLock
import           Ouroboros.Consensus.Util.IOLike

-- | We use an empty file ('dbLockFsPath') as a lock of the database so that
-- the database cannot be opened by more than one process. We wait up to
-- 'dbLockTimeout' to take the lock, before timing out and throwing a
-- 'DbLocked' exception.
withLockDB :: MountPoint -> IO a -> IO a
withLockDB mountPoint =
    withLockDB_
      ioFileLock
      mountPoint
      dbLockFsPath
      dbLockTimeout

-- | The default lock file
dbLockFsPath :: FsPath
dbLockFsPath = fsPathFromList ["lock"]

-- | Default time to wait on the lock
dbLockTimeout :: DiffTime
dbLockTimeout = Time.secondsToDiffTime 2

-- | We use the given 'FsPath' in the 'MountPoint' as a lock of the database
-- so that the database cannot be opened by more than one process. We wait the
-- given 'DiffTime' on the thread taking the lock. In case of a timeout, we
-- throw a 'DbLocked' exception.
--
-- Some systems may delete the empty file when all its handles are closed.
-- This is not an issue, since the file is created if it doesn't exist.
withLockDB_
  :: forall m a. (IOLike m, MonadTimer m)
  => FileLock m
  -> MountPoint  -- ^ Root of the path
  -> FsPath      -- ^ File to lock
  -> DiffTime    -- ^ Timeout
  -> m a
  -> m a
withLockDB_ fileLock mountPoint lockFsPath lockTimeout action =
    bracket acquireLock id (const action)
  where
    -- We want to avoid blocking the main thread at an uninterruptible ffi, to
    -- avoid unresponsiveness to timeouts and ^C. So we use 'async' and let a
    -- new thread do the actual ffi call.
    --
    -- We shouldn't be tempted to use 'withAsync', which is usually mentioned
    -- as a better alternative, or try to synchronously cancel the forked
    -- thread during cleanup, since this would block the main thread and negate
    -- the whole point of using 'async'.
    --
    -- This means that we leave the thread taking the lock running in case of
    -- a timeout. This is not a problem, though, since if we fail to take the
    -- lock, the whole process will soon die.
    acquireLock :: m (m ())
    acquireLock = do
      lockFileAsync <- async (lockFile fileLock lockFilePath)
      timeout lockTimeout (wait lockFileAsync) >>= \case
        -- We timed out while waiting on the lock. The db is still locked.
        Nothing     -> throwIO $ DbLocked lockFilePath
        Just unlock -> return unlock

    lockFilePath = fsToFilePath mountPoint lockFsPath

newtype DbLocked = DbLocked FilePath
    deriving (Eq, Show)

instance Exception DbLocked where
    displayException (DbLocked f) =
      "The db is used by another process. File \"" <> f <> "\" is locked"
