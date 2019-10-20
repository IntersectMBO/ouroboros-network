{-# LANGUAGE PackageImports #-}

module Ouroboros.Storage.IO (
      FHandle
    , open
    , truncate
    , seek
    , read
    , pread
    , write
    , close
    , getSize
    , sameError
    ) where

import           Prelude hiding (read, truncate)

import           Control.Monad (void)
import           Data.ByteString (ByteString)
import           Data.ByteString.Internal as Internal
import           Data.Int (Int64)
import           Data.Word (Word32, Word64, Word8)
import           Foreign (Ptr)
import           System.Posix (Fd)
import qualified System.Posix as Posix

-- Package 'unix' has a same module.
import "unix-bytestring" System.Posix.IO.ByteString (fdPreadBuf)

import           Ouroboros.Storage.FS.API.Types (AllowExisting (..), FsError,
                     OpenMode (..), SeekMode (..), sameFsError)
import           Ouroboros.Storage.FS.Handle

type FHandle = HandleOS Fd

-- | Some sensible defaults for the 'OpenFileFlags'.
--
-- NOTE: the 'unix' package /already/ exports a smart constructor called
-- @defaultFileFlags@, but we define our own to not be depedent by
-- whichever default choice unix's library authors made, and to be able to
-- change our minds later if necessary. In particular, we are interested in the
-- 'append' and 'exclusive' flags, which were largely the reason why we
-- introduced this low-level module.
defaultFileFlags :: Posix.OpenFileFlags
defaultFileFlags = Posix.OpenFileFlags {
      Posix.append    = False
    , Posix.exclusive = False
    , Posix.noctty    = False
    , Posix.nonBlock  = False
    , Posix.trunc     = False
    }

-- | Opens a file from disk.
open :: FilePath -> OpenMode -> IO Fd
open fp openMode = Posix.openFd fp posixOpenMode fileMode fileFlags
  where
    (posixOpenMode, fileMode, fileFlags) = case openMode of
      ReadMode         -> ( Posix.ReadOnly
                          , Nothing
                          , defaultFileFlags
                          )
      AppendMode    ex -> ( Posix.WriteOnly
                          , Just Posix.stdFileMode
                          , defaultFileFlags { Posix.append = True
                                             , Posix.exclusive = isExcl ex }
                          )
      ReadWriteMode ex -> ( Posix.ReadWrite
                          , Just Posix.stdFileMode
                          , defaultFileFlags { Posix.exclusive = isExcl ex }
                          )
      WriteMode     ex -> ( Posix.ReadWrite
                          , Just Posix.stdFileMode
                          , defaultFileFlags { Posix.exclusive = isExcl ex }
                          )

    isExcl AllowExisting = False
    isExcl MustBeNew     = True


-- | Writes the data pointed by the input 'Ptr Word8' into the input 'FHandle'.
write :: FHandle -> Ptr Word8 -> Int64 -> IO Word32
write h data' bytes = writeOpenHandle "write" h $ \fd ->
    fromIntegral <$> Posix.fdWriteBuf fd data' (fromIntegral bytes)

-- | Seek within the file.
--
-- The offset may be negative.
--
-- We don't return the new offset since the behaviour of lseek is rather odd
-- (e.g., the file pointer may not actually be moved until a subsequent write)
seek :: FHandle -> SeekMode -> Int64 -> IO ()
seek h seekMode offset = writeOpenHandle "seek" h $ \fd ->
    void $ Posix.fdSeek fd seekMode (fromIntegral offset)

-- | Reads a given number of bytes from the input 'FHandle'.
-- Read is not thread safe since it alters the file offset, that's why we use
-- 'writeOpenHandle' instead of 'readOpenHandle'. Use 'pread' for thread safety.
read :: FHandle -> Word64 -> IO ByteString
read h bytes = writeOpenHandle "read" h $ \fd ->
    Internal.createUptoN (fromIntegral bytes) $ \ptr ->
      fromIntegral <$> Posix.fdReadBuf fd ptr (fromIntegral bytes)

-- | Thread safe variation of read.
pread :: FHandle -> Word64 -> Int64 -> IO ByteString
pread h bytes offset = readOpenHandle "pread" h $ \fd ->
    Internal.createUptoN (fromIntegral bytes) $ \ptr -> fromIntegral <$>
      fdPreadBuf fd ptr (fromIntegral bytes) (fromIntegral offset)

-- | Truncates the file managed by the input 'FHandle' to the input size.
truncate :: FHandle -> Word64 -> IO ()
truncate h sz = writeOpenHandle "truncate" h $ \fd ->
    Posix.setFdSize fd (fromIntegral sz)

-- | Close handle
--
-- This is a no-op when the handle is already closed.
close :: FHandle -> IO ()
close h = closeHandleOS h Posix.closeFd

-- | File size of the given file pointer
--
-- NOTE: This is not thread safe in terms of other writes
-- (changes made to the file in other threads may affect this thread).
getSize :: FHandle -> IO Word64
getSize h = readOpenHandle "getSize" h $
    fmap (fromIntegral . Posix.fileSize) . Posix.getFdStatus

sameError :: FsError -> FsError -> Bool
sameError = sameFsError
