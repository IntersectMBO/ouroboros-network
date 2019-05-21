{-# LANGUAGE LambdaCase #-}
module Ouroboros.Storage.IO (
      FHandle
    , open
    , truncate
    , seek
    , read
    , write
    , close
    , getSize
    ) where

import           Prelude hiding (read, truncate)
import           Control.Monad (void, when)
import           Data.ByteString
import           Data.ByteString.Internal as Internal
import           Data.Word (Word32, Word8, Word64)
import           Foreign (Ptr, Int64)
import           System.IO
import           System.Win32
import           Ouroboros.Storage.Seek (setFilePointerEx)
import           Ouroboros.Storage.FS.Handle
import           Data.Bits((.|.))

type FHandle = HandleOS HANDLE

open :: FilePath -> IOMode -> IO HANDLE
open filename ioMode = do
    let accessMode
         | ioMode == ReadMode   = gENERIC_READ
         | ioMode == AppendMode = gENERIC_WRITE
         | otherwise            = gENERIC_READ .|. gENERIC_WRITE
        creationDisposition = case ioMode of
          ReadMode -> oPEN_EXISTING
          _        -> oPEN_ALWAYS
    h <- createFile filename
                              accessMode
                              (fILE_SHARE_READ .|. fILE_SHARE_WRITE)
                              Nothing
                              -- TODO(kde) we can use cREATE_NEW for Issue #292.
                              creationDisposition
                              fILE_ATTRIBUTE_NORMAL
                              Nothing
    -- There is not AppendMode in Windows, so we manually seek to the end of the file.
    -- For now we don't need to carry a flag that this is on AppendMode, but we may do
    -- if we add more commands (read and seek are disabled on AppendMode and truncate only
    -- works on AppendMode. write moves the file offset on all modes).
    when (ioMode == AppendMode) $
      void $ setFilePointerEx h 0 fILE_END
    return h

write :: FHandle -> Ptr Word8 -> Int64 -> IO Word32
write fh data' bytes = withOpenHandle "write" fh $ \h ->
  win32_WriteFile h data' (fromIntegral bytes) Nothing

seek :: FHandle -> SeekMode -> Int64 -> IO ()
seek fh seekMode size = void <$> withOpenHandle "seek" fh $ \h ->
  setFilePointerEx h (fromIntegral size) (fromSeekMode seekMode)

fromSeekMode :: SeekMode -> FilePtrDirection
fromSeekMode AbsoluteSeek = fILE_BEGIN
fromSeekMode RelativeSeek = fILE_CURRENT
fromSeekMode SeekFromEnd  = fILE_END

read :: FHandle -> Int -> IO ByteString
read fh bytes = withOpenHandle "read" fh $ \h ->
  Internal.createUptoN bytes $ \ptr ->
    fromIntegral <$> win32_ReadFile h ptr (fromIntegral bytes) Nothing

-- We only allow truncate in AppendMode, but Windows do not support it, so we manually seek to the end.
-- It is important that the logical end of the handle stays alligned to the physical end of the file.
truncate :: FHandle -> Word64 -> IO ()
truncate fh size =  withOpenHandle "truncate" fh $ \h -> do
  _ <- setFilePointerEx h (fromIntegral size) (fromSeekMode AbsoluteSeek)
  setEndOfFile h

close :: FHandle -> IO ()
close fh = closeHandleOS fh closeHandle
  
getSize :: FHandle -> IO Word64
getSize fh = withOpenHandle "getSize" fh $ \h -> do
  fromIntegral . bhfiSize <$>  getFileInformationByHandle h
