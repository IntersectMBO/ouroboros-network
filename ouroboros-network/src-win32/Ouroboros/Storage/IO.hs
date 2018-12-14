module Ouroboros.Storage.IO (
      FHandle --opaque(TM)
    , open
    , truncate
    , seek
    , read
    , write
    , close
    ) where

import           Prelude hiding (read, truncate)

import           Control.Exception (IOException, SomeException)
import qualified Control.Exception as E
import           Control.Exception.Extensible (throw, try)
import           Data.Word (Word32, Word8)
import           Foreign (Ptr)
import           System.Directory (createDirectoryIfMissing, removeFile)
import           System.IO
import           System.Win32 (HANDLE, cREATE_ALWAYS, closeHandle, createFile,
                     fILE_ATTRIBUTE_NORMAL, fILE_SHARE_NONE, flushFileBuffers,
                     gENERIC_ALL, gENERIC_READ, gENERIC_WRITE, win32_WriteFile)

data FHandle = FHandle HANDLE

open :: FilePath -> IOMode -> IO FHandle
open filename ioMode = do
    let accessMode
         | ioMode == ReadMode   = gENERIC_READ
         | ioMode == AppendMode = gENERIC_WRITE
         | otherwise = gENERIC_ALL
    fmap FHandle $ createFile filename
                              accessMode
                              fILE_SHARE_NONE -- TODO: this is wrong, needs to be changed.
                              Nothing
                              cREATE_ALWAYS -- TODO: ditto, ReadMode should enforce is there.
                              fILE_ATTRIBUTE_NORMAL
                              Nothing

write :: FHandle -> Ptr Word8 -> Word32 -> IO Word32
write (FHandle handle) data' length = win32_WriteFile handle data' length Nothing

seek :: FHandle -> SeekMode -> Word64 -> IO Word64
seek = error "seek: needs a Windows hero to implement."

read :: FHandle -> Int -> IO ByteString
read = error "read: needs a Windows hero to implement."

truncate :: FHandle -> Word64 -> IO ()
truncate = error "truncate: needs a Windows hero to implement."

close :: FHandle -> IO ()
close (FHandle handle) = closeHandle handle
