{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | IO implementation of the 'HasFS' class
module Ouroboros.Storage.FS.IO (
    -- * IO implementation & monad
      IOFS -- opaque
    , IOFSE
    , runIOFS
    , runIOFSE
    ) where

import           Control.Exception (throwIO)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder.Extra as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Set as Set
import           Data.Word (Word64, Word8)
import           Foreign (ForeignPtr, Ptr, castPtr, withForeignPtr)
import           GHC.ForeignPtr (mallocPlainForeignPtrBytes)
import           GHC.Stack
import qualified System.Directory as Dir
import           System.IO.Error

import           Ouroboros.Storage.FS.Class
import           Ouroboros.Storage.FS.Class.Types
import qualified Ouroboros.Storage.IO as F

-- | Catch IO exceptions and return them as 'FsError'
--
-- See comments for 'ioToFsError'
catchFSErrorIO :: HasCallStack => FsPath -> IO a -> IO (Either FsError a)
catchFSErrorIO fp action = do
    res <- tryIOError action
    case res of
      Left err -> handleError err
      Right a  -> return (Right a)
  where
    handleError :: HasCallStack => IOError -> IO (Either FsError a)
    handleError ioErr =
      case ioToFsError fp ioErr of
        Left  unexpected -> throwIO unexpected
        Right err        -> return (Left err)

{------------------------------------------------------------------------------
  The IOFS monad
-------------------------------------------------------------------------------}

type IOFSE = ExceptT FsError IOFS
type IOFS  = ReaderT MountPoint IO

-- | Lifts an 'IO' action into the 'IOFSE' monad. Note how this /must/ be
-- used as a drop-in replacement for 'liftIO', as the former would also correctly
-- catch any relevant IO exception and reify it into a 'FsError' one.
liftIOE :: HasCallStack => FsPath -> IO a -> IOFSE a
liftIOE fp act = ExceptT $ lift $ catchFSErrorIO fp act

runIOFS :: MountPoint -> IOFS a -> IO a
runIOFS = flip runReaderT

runIOFSE :: MountPoint -> IOFSE a -> IO (Either FsError a)
runIOFSE mount = runIOFS mount . runExceptT

withAbsPath :: FsPath -> (FilePath -> IO a) -> IOFSE a
withAbsPath fp act = asks (`fsToFilePath` fp) >>= liftIOE fp . act

withHandle :: FsHandle IOFSE -> (F.FHandle -> IO a) -> IOFSE a
withHandle (fp, h) k = liftIOE fp $ k h

instance HasFS IOFSE where
    type FsHandle IOFSE = (FsPath, F.FHandle)
    data Buffer   IOFSE =
        BufferIO {-# UNPACK #-} !(ForeignPtr Word8) {-# UNPACK #-} !Int

    -- TODO(adn) Might be useful to implement this properly by reading all
    -- the stuff available at the 'MountPoint'.
    dumpState = return "<dumpState@IO>"

    -- Buffer creation never throws I/O exceptions
    newBuffer = lift . lift . newBufferIO


    hOpen path ioMode = withAbsPath path $ \fp -> (path, ) <$> F.open fp ioMode

    hClose     h          = withHandle h $ \h' -> F.close      h'
    hSeek      h mode o   = withHandle h $ \h' -> F.seek       h' mode o
    hGet       h n        = withHandle h $ \h' -> F.read       h' n
    hTruncate  h sz       = withHandle h $ \h' -> F.truncate   h' sz
    hPutBuffer h buf bldr = withHandle h $ \h' -> hPutBufferIO h' buf bldr
    hGetSize   h          = withHandle h $ \h' -> F.getSize    h'

    hPut h builder = do
        buf0 <- newBuffer BS.defaultChunkSize
        hPutBuffer h buf0 builder

    createDirectory    path = withAbsPath path $ Dir.createDirectory
    listDirectory      path = withAbsPath path $ fmap Set.fromList . Dir.listDirectory
    doesDirectoryExist path = withAbsPath path $ Dir.doesDirectoryExist
    doesFileExist      path = withAbsPath path $ Dir.doesFileExist

    createDirectoryIfMissing createParent path = withAbsPath path $
        Dir.createDirectoryIfMissing createParent

{-------------------------------------------------------------------------------
  Auxiliary: buffers
-------------------------------------------------------------------------------}

bufferSize :: Buffer IOFSE -> Int
bufferSize (BufferIO _fptr len) = len

newBufferIO :: Int -> IO (Buffer IOFSE)
newBufferIO len = do
    fptr <- mallocPlainForeignPtrBytes len
    return $! BufferIO fptr len

withBuffer :: Buffer IOFSE
           -> (Ptr Word8 -> Int -> IO a)
           -> IO a
withBuffer (BufferIO fptr len) action =
    withForeignPtr fptr $ \ptr -> action ptr len

hPutBufferIO :: F.FHandle -> Buffer IOFSE -> Builder -> IO Word64
hPutBufferIO hnd buf0 = go 0 buf0 . BS.runBuilder
  where
    go :: Word64
       -> Buffer IOFSE
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
