{-# OPTIONS_GHC -Wno-dodgy-imports #-}
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
import           Data.Bits ((.|.))
import           Data.ByteString
import           Data.ByteString.Internal as Internal
import           Data.Word (Word32, Word64, Word8)
import           Foreign (Int64, Ptr)
import           System.Win32

import           Ouroboros.Storage.FS.API.Types (AllowExisting (..),
                     FsError (..), FsErrorType (..), OpenMode (..),
                     SeekMode (..))
import           Ouroboros.Storage.FS.Handle

type FHandle = HandleOS HANDLE

open :: FilePath -> OpenMode -> IO HANDLE
open filename openMode = do
    h <- createFile filename
                    accessMode
                    (fILE_SHARE_READ .|. fILE_SHARE_WRITE)
                    Nothing
                    creationDisposition
                    fILE_ATTRIBUTE_NORMAL
                    Nothing
    -- There is no AppendMode in Windows, so we manually seek to the end of
    -- the file. For now we don't need to carry a flag that this handle is in
    -- AppendMode, but we may need to if we add more commands (read and seek
    -- are disabled in AppendMode and truncate only works in AppendMode, write
    -- moves the file offset in all modes).
    case openMode of
      AppendMode{} -> void $ setFilePointerEx h 0 fILE_END
      _            -> return ()
    return h
  where
    (accessMode, creationDisposition) = case openMode of
      ReadMode         -> (gENERIC_READ,                   oPEN_EXISTING)
      AppendMode    ex -> (                 gENERIC_WRITE, createNew ex)
      WriteMode     ex -> (gENERIC_READ .|. gENERIC_WRITE, createNew ex)
      ReadWriteMode ex -> (gENERIC_READ .|. gENERIC_WRITE, createNew ex)
    createNew AllowExisting = oPEN_ALWAYS
    createNew MustBeNew     = cREATE_NEW

write :: FHandle -> Ptr Word8 -> Int64 -> IO Word32
write fh data' bytes = withExclOpenHandle "write" fh $ \h ->
  win32_WriteFile h data' (fromIntegral bytes) Nothing

seek :: FHandle -> SeekMode -> Int64 -> IO ()
seek fh seekMode size = void <$> withExclOpenHandle "seek" fh $ \h ->
  setFilePointerEx h (fromIntegral size) (fromSeekMode seekMode)

fromSeekMode :: SeekMode -> FilePtrDirection
fromSeekMode AbsoluteSeek = fILE_BEGIN
fromSeekMode RelativeSeek = fILE_CURRENT
fromSeekMode SeekFromEnd  = fILE_END

read :: FHandle -> Word64 -> IO ByteString
read fh bytes = withExclOpenHandle "read" fh $ \h ->
  Internal.createUptoN (fromIntegral bytes) $ \ptr ->
    fromIntegral <$> win32_ReadFile h ptr (fromIntegral bytes) Nothing

getCurrentFileOffset :: HANDLE -> IO Int64
getCurrentFileOffset h = setFilePointerEx h 0 fILE_CURRENT

-- | @win32_ReadFile@ in Windows always changes the file offset, so it's
-- not thread safe. That's why we use a exclusive lock for this.
pread :: FHandle -> Word64 -> Word64 -> IO ByteString
pread fh bytes pos = withExclOpenHandle "pread" fh $ \h ->
  Internal.createUptoN (fromIntegral bytes) $ \ptr -> do
    initialOffset <- getCurrentFileOffset h
    _ <- setFilePointerEx h (fromIntegral pos) fILE_BEGIN
    n <- fromIntegral <$> win32_ReadFile h ptr (fromIntegral bytes) Nothing
    _ <- setFilePointerEx h initialOffset fILE_BEGIN
    return n

-- We only allow truncate in AppendMode, but Windows does not support it, so we
-- manually seek to the end. It is important that the logical offset of the
-- handle stays aligned to the physical end of the file.
truncate :: FHandle -> Word64 -> IO ()
truncate fh size =  withExclOpenHandle "truncate" fh $ \h -> do
  _ <- setFilePointerEx h (fromIntegral size) (fromSeekMode AbsoluteSeek)
  setEndOfFile h

close :: FHandle -> IO ()
close fh = closeHandleOS fh closeHandle

getSize :: FHandle -> IO Word64
getSize fh = withSharedOpenHandle "getSize" fh $ \h ->
  fromIntegral . bhfiSize <$>  getFileInformationByHandle h

-- | For the following error types, our mock FS implementation (and the Posix
-- implementation) throw the same errors:
--
-- * 'FsReachedEOF'
-- * 'FsDeviceFull'
-- * 'FsResourceAlreadyInUse'
--
-- For other cases, Windows throws different errors than the mock FS
-- implementation.
sameError :: FsError -> FsError -> Bool
sameError e1 e2 = fsErrorPath e1 == fsErrorPath e2
               && sameFsErrorType (fsErrorType e1) (fsErrorType e2)
  where
    sameFsErrorType ty1 ty2 = case (ty1, ty2) of
      (FsReachedEOF,           FsReachedEOF)           -> True
      (FsReachedEOF,           _)                      -> False
      (_,                      FsReachedEOF)           -> False
      (FsDeviceFull,           FsDeviceFull)           -> True
      (FsDeviceFull,           _)                      -> False
      (_,                      FsDeviceFull)           -> False
      (FsResourceAlreadyInUse, FsResourceAlreadyInUse) -> True
      (FsResourceAlreadyInUse, _)                      -> False
      (_,                      FsResourceAlreadyInUse) -> False
      (_,                      _)                      -> True
