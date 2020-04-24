module Ouroboros.Consensus.Util.FileLock (
    FileLock (..)
  , ioFileLock
  ) where

import qualified System.FileLock as IO

-- | Abstraction for file locks
data FileLock m = FileLock {
      -- | Obtain an exclusive lock on the given file.
      --
      -- Returns the function to unlock the file.
      --
      -- Blocks until the lock is available.
      --
      -- We don't guarantee the ability to read/write to a locked file, not
      -- even when holding the lock.
      lockFile   :: FilePath -> m (m ())
    }

-- | Implementation of 'FileLock' for 'IO', using on "System.FileLock".
--
-- Locking the file can block in FFI, so not interruptible.
--
-- Unlocking the file is not guaranteed to be synchronous. Near instantaneous
-- on Linux, but not synchronous. On Windows, unlocking is even more lazy.
ioFileLock :: FileLock IO
ioFileLock = FileLock {
      lockFile   = \fp ->
        IO.unlockFile <$> IO.lockFile fp IO.Exclusive
    }
