{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

-- | IO implementation of the 'HasFS' class
module Ouroboros.Storage.FS.IO (
    -- * IO implementation & monad
      ioHasFS
    ) where

import qualified Control.Exception as E
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder.Extra as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Set as Set
import           Data.Word (Word64, Word8)
import           Foreign (ForeignPtr, Ptr, castPtr, withForeignPtr)
import           GHC.ForeignPtr (mallocPlainForeignPtrBytes)
import           GHC.Stack
import qualified System.Directory as Dir

import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Types
import qualified Ouroboros.Storage.IO as F
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

{-------------------------------------------------------------------------------
  I/O implementation of HasFS
-------------------------------------------------------------------------------}

type instance FsHandle IO = (FsPath, F.FHandle)
data instance Buffer   IO = BufferIO !(ForeignPtr Word8) !Int

ioHasFS :: MountPoint -> HasFS IO
ioHasFS mount = HasFS {
      -- TODO(adn) Might be useful to implement this properly by reading all
      -- the stuff available at the 'MountPoint'.
      dumpState = return "<dumpState@IO>"
    , newBuffer = newBufferIO
    , hOpen = \fp ioMode -> rethrowFsError fp $
        (fp, ) <$> F.open (root fp) ioMode
    , hClose = \(fp, h) -> rethrowFsError fp $
        F.close h
    , hSeek = \(fp, h) mode o -> rethrowFsError fp $
        F.seek h mode o
    , hGet = \(fp, h) n -> rethrowFsError fp $
        F.read h n
    , hTruncate = \(fp, h) sz -> rethrowFsError fp $
        F.truncate h sz
    , hPutBuffer = \(fp, h) buf bldr -> rethrowFsError fp $
        hPutBufferIO h buf bldr
    , hGetSize = \(fp, h) -> rethrowFsError fp $
        F.getSize h
    , hPut = \(fp, h) builder -> rethrowFsError fp $ do
        buf0 <- newBufferIO BS.defaultChunkSize
        hPutBufferIO h buf0 builder
    , createDirectory = \fp -> rethrowFsError fp $
        Dir.createDirectory (root fp)
    , listDirectory = \fp -> rethrowFsError fp $
        Set.fromList <$>  Dir.listDirectory (root fp)
    , doesDirectoryExist= \fp -> rethrowFsError fp $
        Dir.doesDirectoryExist (root fp)
    , doesFileExist = \fp -> rethrowFsError fp $
        Dir.doesFileExist (root fp)
    , createDirectoryIfMissing = \createParent fp -> rethrowFsError fp $
        Dir.createDirectoryIfMissing createParent (root fp)
    , hasFsErr = EH.exceptions
    }
  where
    root :: FsPath -> FilePath
    root = fsToFilePath mount

-- | Catch IO exceptions and rethrow them as 'FsError'
--
-- See comments for 'ioToFsError'
rethrowFsError :: HasCallStack => FsPath -> IO a -> IO a
rethrowFsError fp action = do
    res <- E.try action
    case res of
      Left err -> handleError err
      Right a  -> return a
  where
    handleError :: HasCallStack => IOError -> IO a
    handleError ioErr =
      case ioToFsError fp ioErr of
        Left  unexpected -> E.throwIO unexpected
        Right err        -> E.throwIO err

{-------------------------------------------------------------------------------
  Auxiliary: buffers
-------------------------------------------------------------------------------}

bufferSize :: Buffer IO -> Int
bufferSize (BufferIO _fptr len) = len

newBufferIO :: Int -> IO (Buffer IO)
newBufferIO len = do
    fptr <- mallocPlainForeignPtrBytes len
    return $! BufferIO fptr len

withBuffer :: Buffer IO
           -> (Ptr Word8 -> Int -> IO a)
           -> IO a
withBuffer (BufferIO fptr len) action =
    withForeignPtr fptr $ \ptr -> action ptr len

hPutBufferIO :: F.FHandle -> Buffer IO -> Builder -> IO Word64
hPutBufferIO hnd buf0 = go 0 buf0 . BS.runBuilder
  where
    go :: Word64
       -> Buffer IO
       -> BS.BufferWriter
       -> IO Word64
    go !bytesWritten buf write = do
      (bytesCount, next) <- withBuffer buf $ \ptr sz -> do
        -- run the builder, writing into our buffer
        (n, next) <- write ptr sz
        -- so now our buffer contains 'n' bytes
        -- write it all out to the handle leaving our buffer empty
        bytesCount <- F.write hnd ptr (fromIntegral n)
        return (bytesCount, next)
      case next of
        BS.Done -> return (bytesWritten + fromIntegral bytesCount)
        BS.More minSize write' | bufferSize buf < minSize -> do
          -- very unlikely given our strategy of flushing our buffer every time
          buf' <- newBufferIO minSize
          go (bytesWritten + fromIntegral bytesCount) buf' write'
        BS.More _minSize write' ->
          go (bytesWritten + fromIntegral bytesCount) buf write'
        BS.Chunk chunk   write' -> do
          n <- BS.unsafeUseAsCStringLen chunk $ \(ptr, len) ->
                   F.write hnd (castPtr ptr) (fromIntegral len)
          go (bytesWritten + fromIntegral n + fromIntegral bytesCount) buf write'
