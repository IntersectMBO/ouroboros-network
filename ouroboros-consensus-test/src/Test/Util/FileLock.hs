{-# LANGUAGE NamedFieldPuns #-}
module Test.Util.FileLock (mockFileLock) where

import           Control.Monad (join, void)
import           Data.Map (Map)
import qualified Data.Map as Map
import           GHC.Stack (HasCallStack)

import           Control.Monad.IOSim

import           Ouroboros.Consensus.Util.FileLock
import           Ouroboros.Consensus.Util.IOLike

import           Test.Util.Orphans.IOLike ()

{-------------------------------------------------------------------------------
  mockFileLock
-------------------------------------------------------------------------------}

-- | Mock in-memory implementation of 'FileLock'.
--
-- Supports an optional delay in release to simulate lazy, non-synchronous
-- unlocking as done by Linux (near instantaneous but not instant) and
-- Windows.
mockFileLock
  :: Maybe DiffTime  -- ^ Optional release delay
  -> IOSim s (FileLock (IOSim s))
mockFileLock releaseDelay = do
    locks <- newMockFileLocks releaseDelay
    return FileLock {
        lockFile = \fp -> mockUnlockFile locks <$> mockLockFile locks fp
      }

{-------------------------------------------------------------------------------
  MockFileLocks
-------------------------------------------------------------------------------}

data MockFileLocks m = MockFileLocks {
      varLocks     :: StrictTVar m (Map FilePath LockStatus)
    , releaseDelay :: Maybe DiffTime
    }

-- | The status of a file lock, required to account for lazy releases. Note
-- that we don't have to model \"unlocked\", as the absence in 'varLocks'
-- already means that the lock is not held.
data LockStatus = Held | LazyRelease

newMockFileLocks :: IOLike m => Maybe DiffTime -> m (MockFileLocks m)
newMockFileLocks releaseDelay = do
    varLocks <- uncheckedNewTVarM Map.empty
    return MockFileLocks { varLocks, releaseDelay }

mockLockFile :: IOLike m => MockFileLocks m -> FilePath -> m FilePath
mockLockFile MockFileLocks { varLocks } path = atomically $ do
    locks <- readTVar varLocks
    if Map.member path locks
      then retry
      else writeTVar varLocks $ Map.insert path Held locks
    return path

-- | We simulate lazy lock releases by changing the status of the lock to
-- 'LazyRelease' and spawning a thread that waits for 'releaseDelay' before
-- removing the lock from 'varLocks'.
mockUnlockFile :: (IOLike m, HasCallStack)
               => MockFileLocks m -> FilePath -> m ()
mockUnlockFile MockFileLocks { varLocks, releaseDelay } path =
    join $ atomically $ do
      locks <- readTVar varLocks
      case Map.lookup path locks of
        Nothing ->
          error $ "unlocking an unlocked file: " <> show path
        Just LazyRelease ->
          error $ "unlocking a file that is still being unlocked: " <> show path
        Just Held -> case releaseDelay of
          Nothing    -> do
            writeTVar varLocks $ Map.delete path locks
            return $ return ()
          Just delay -> do
            writeTVar varLocks $ Map.insert path LazyRelease locks
            return $ void $ forkIO $ do
              threadDelay delay
              atomically $ writeTVar varLocks $ Map.delete path locks
