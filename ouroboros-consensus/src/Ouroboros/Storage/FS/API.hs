{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

-- | An abstract view over the filesystem.
module Ouroboros.Storage.FS.API (
      HasFS(..)
    , withFile
    ) where

import           Data.ByteString (ByteString)
import           Data.ByteString.Builder (Builder)
import           Data.Int (Int64)
import           Data.Set (Set)
import           Data.Word (Word64)
import           GHC.Stack
import           System.IO (IOMode, SeekMode)

import           Control.Monad.Class.MonadThrow

import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling)

{------------------------------------------------------------------------------
 Typeclass which abstracts over the filesystem
------------------------------------------------------------------------------}

data HasFS m h = HasFS {
    -- | Debugging: human-readable description of file system state
    dumpState                :: m String

    -- Operations of files

    -- | Open a file
  , hOpen                    :: HasCallStack => FsPath -> IOMode -> m h

    -- | Close a file
  , hClose                   :: HasCallStack => h -> m ()

    -- | Seek handle
    --
    -- The offset is an 'Int64' rather than a 'Word64' because it may be
    -- negative (for use in relative positioning).
    --
    -- Unlike the Posix @lseek@, 'hSeek' does not return the new seek position
    -- because the value returned by Posix is rather strange and unreliable
    -- and we don't want to emulate it's behaviour.
  , hSeek                    :: HasCallStack => h -> SeekMode -> Int64 -> m ()

    -- | Try to read @n@ bytes from a handle
  , hGet                     :: HasCallStack => h -> Int -> m ByteString

    -- | Write to a handle
  , hPut                     :: HasCallStack => h -> Builder -> m Word64

    -- | Truncate the file to the specified size
    --
    -- NOTE: Only supported in append mode.
  , hTruncate                :: HasCallStack => h -> Word64 -> m ()

    -- | Return current file size
    --
    -- NOTE: This is not thread safe (changes made to the file in other threads
    -- may affect this thread).
  , hGetSize                 :: HasCallStack => h -> m Word64

    -- Operations of directories

    -- | Create new directory
  , createDirectory          :: HasCallStack => FsPath -> m ()

    -- | Create new directory if it doesn't exist.
    --
    -- @createDirectoryIfMissing True@ will also try to create all parent dirs.
  , createDirectoryIfMissing :: HasCallStack => Bool -> FsPath -> m ()

    -- | List contents of a directory
  , listDirectory            :: HasCallStack => FsPath -> m (Set String)

    -- | Check if the path exists and is a directory
  , doesDirectoryExist       :: HasCallStack => FsPath -> m Bool

    -- | Check if the path exists and is a file
  , doesFileExist            :: HasCallStack => FsPath -> m Bool

    -- | Remove the file (which must exist)
  , removeFile               :: HasCallStack => FsPath -> m ()

    -- | Error handling
  , hasFsErr                 :: ErrorHandling FsError m
  }

withFile :: (HasCallStack, MonadThrow m)
         => HasFS m h -> FsPath -> IOMode -> (h -> m a) -> m a
withFile HasFS{..} fp ioMode = bracket (hOpen fp ioMode) hClose
