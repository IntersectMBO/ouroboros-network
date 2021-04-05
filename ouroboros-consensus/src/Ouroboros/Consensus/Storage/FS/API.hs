{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | An abstract view over the filesystem.
module Ouroboros.Consensus.Storage.FS.API (
    Handle (..)
  , HasFS (..)
  , SomeHasFS (..)
  , hClose'
  , hGetAll
  , hGetAllAt
  , hGetExactly
  , hGetExactlyAt
  , hPut
  , hPutAll
  , hPutAllStrict
  , withFile
  ) where

import           Control.Monad (foldM)
import qualified Data.ByteString as BS
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Int (Int64)
import           Data.Set (Set)
import           Data.Word
import           NoThunks.Class (NoThunks, OnlyCheckWhnfNamed (..))

import           Control.Monad.Class.MonadThrow

import           Ouroboros.Consensus.Storage.FS.API.Types
import           Ouroboros.Consensus.Util.CallStack

{------------------------------------------------------------------------------
 Typeclass which abstracts over the filesystem
------------------------------------------------------------------------------}

data HasFS m h = HasFS {
    -- | Debugging: human-readable description of file system state
    dumpState                :: m String

    -- Operations of files

    -- | Open a file
  , hOpen                    :: HasCallStack => FsPath -> OpenMode -> m (Handle h)

    -- | Close a file
  , hClose                   :: HasCallStack => Handle h -> m ()

    -- | Is the handle open?
  , hIsOpen                  :: HasCallStack => Handle h -> m Bool

    -- | Seek handle
    --
    -- The offset is an 'Int64' rather than a 'Word64' because it may be
    -- negative (for use in relative positioning).
    --
    -- Unlike the Posix @lseek@, 'hSeek' does not return the new seek position
    -- because the value returned by Posix is rather strange and unreliable
    -- and we don't want to emulate it's behaviour.
  , hSeek                    :: HasCallStack => Handle h -> SeekMode -> Int64 -> m ()

    -- | Try to read @n@ bytes from a handle
    --
    -- When at the end of the file, an empty bytestring will be returned.
    --
    -- The returned bytestring will typically have length @n@, but may be
    -- shorter in case of a partial read, see #277. However, a partial read
    -- will always return at least 1 byte, as returning 0 bytes would mean
    -- that we have reached EOF.
    --
    -- Postcondition: the length of the returned bytestring <= @n@ and >= 0.
  , hGetSome                 :: HasCallStack => Handle h -> Word64 -> m BS.ByteString

    -- | Same as 'hGetSome', but does not affect the file offset. An additional argument
    -- is used to specify the offset. This allows it to be called concurrently for the
    -- same file handle. However, the actual level of parallelism achieved depends on
    -- the implementation and the operating system: generally on Unix it will be
    -- \"more parallel\" than on Windows.
  , hGetSomeAt               :: HasCallStack
                             => Handle h
                             -> Word64    -- The number of bytes to read.
                             -> AbsOffset -- The offset at which to read.
                             -> m BS.ByteString

    -- | Write to a handle
    --
    -- The return value indicates the number of bytes written and will
    -- typically be equal to @l@, the length of the bytestring, but may be
    -- shorter in case of a partial write, see #277.
    --
    -- If nothing can be written at all, an exception will be thrown.
    --
    -- Postcondition: the return value <= @l@ and > 0, unless the given
    -- bytestring is empty, in which case the return value can be 0.
  , hPutSome                 :: HasCallStack => Handle h -> BS.ByteString -> m Word64

    -- | Truncate the file to the specified size
    --
    -- NOTE: Only supported in append mode.
  , hTruncate                :: HasCallStack => Handle h -> Word64 -> m ()

    -- | Return current file size
    --
    -- NOTE: This is not thread safe (changes made to the file in other threads
    -- may affect this thread).
  , hGetSize                 :: HasCallStack => Handle h -> m Word64

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

    -- | Rename the file (which must exist) from the first path to the second
    -- path. If there is already a file at the latter path, it is replaced by
    -- the new one.
    --
    -- NOTE: only works for files within the same folder.
  , renameFile                 :: HasCallStack => FsPath -> FsPath -> m ()

    -- | Useful for better error reporting
  , mkFsErrorPath            :: FsPath -> FsErrorPath
  }
  deriving NoThunks via OnlyCheckWhnfNamed "HasFS" (HasFS m h)

withFile :: (HasCallStack, MonadThrow m)
         => HasFS m h -> FsPath -> OpenMode -> (Handle h -> m a) -> m a
withFile HasFS{..} fp openMode = bracket (hOpen fp openMode) hClose

-- | Returns 'True' when the handle was still open.
hClose' :: (HasCallStack, Monad m) => HasFS m h -> Handle h -> m Bool
hClose' HasFS { hClose, hIsOpen } h = do
    isOpen <- hIsOpen h
    if isOpen then do
      hClose h
      return True
    else
      return False

-- | Makes sure it reads all requested bytes.
-- If eof is found before all bytes are read, it throws an exception.
hGetExactly :: forall m h. (HasCallStack, MonadThrow m)
            => HasFS m h
            -> Handle h
            -> Word64
            -> m BL.ByteString
hGetExactly hasFS h n = go n []
  where
    go :: Word64 -> [BS.ByteString] -> m BL.ByteString
    go remainingBytes acc
      | remainingBytes == 0 = return $ BL.fromChunks $ reverse acc
      | otherwise           = do
        bs <- hGetSome hasFS h remainingBytes
        if BS.null bs then
          throwIO FsError {
              fsErrorType   = FsReachedEOF
            , fsErrorPath   = mkFsErrorPath hasFS $ handlePath h
            , fsErrorString = "hGetExactly found eof before reading " ++ show n ++ " bytes"
            , fsErrorNo     = Nothing
            , fsErrorStack  = prettyCallStack
            , fsLimitation  = False
            }
        -- We know the length <= remainingBytes, so this can't underflow
        else go (remainingBytes - fromIntegral (BS.length bs)) (bs : acc)

-- | Like 'hGetExactly', but is thread safe since it does not change or depend
-- on the file offset. @pread@ syscall is used internally.
hGetExactlyAt :: forall m h. (HasCallStack, MonadThrow m)
              => HasFS m h
              -> Handle h
              -> Word64    -- ^ The number of bytes to read.
              -> AbsOffset -- ^ The offset at which to read.
              -> m BL.ByteString
hGetExactlyAt hasFS h n offset = go n offset []
  where
    go :: Word64 -> AbsOffset -> [BS.ByteString] -> m BL.ByteString
    go remainingBytes currentOffset acc
      | remainingBytes == 0 = return $ BL.fromChunks $ reverse acc
      | otherwise           = do
        bs <- hGetSomeAt hasFS h remainingBytes currentOffset
        let readBytes = BS.length bs
        if BS.null bs then
          throwIO FsError {
              fsErrorType   = FsReachedEOF
            , fsErrorPath   = mkFsErrorPath hasFS $ handlePath h
            , fsErrorString = "hGetExactlyAt found eof before reading " ++ show n ++ " bytes"
            , fsErrorNo     = Nothing
            , fsErrorStack  = prettyCallStack
            , fsLimitation  = False
            }
        -- We know the length <= remainingBytes, so this can't underflow.
        else go (remainingBytes - fromIntegral readBytes)
                (currentOffset + fromIntegral readBytes)
                (bs : acc)

-- | Read all the data from the given file handle 64kB at a time.
--
-- Stops when EOF is reached.
hGetAll :: Monad m => HasFS m h -> Handle h -> m BL.ByteString
hGetAll HasFS{..} hnd = go mempty
  where
    bufferSize = 64 * 1024
    go acc = do
      chunk <- hGetSome hnd bufferSize
      let acc' = chunk : acc
      if BS.null chunk
        then return $ BL.fromChunks $ reverse acc'
        else go acc'

-- | Like 'hGetAll', but is thread safe since it does not change or depend
-- on the file offset. @pread@ syscall is used internally.
hGetAllAt :: Monad m
          => HasFS m h
          -> Handle h
          -> AbsOffset -- ^ The offset at which to read.
          -> m BL.ByteString
hGetAllAt HasFS{..} hnd = go mempty
  where
    bufferSize = 64 * 1024
    go acc offset = do
      chunk <- hGetSomeAt hnd bufferSize offset
      let acc' = chunk : acc
      if BS.null chunk
        then return $ BL.fromChunks $ reverse acc'
        else go acc' (offset + fromIntegral (BS.length chunk))

-- | This function makes sure that the whole 'BS.ByteString' is written.
hPutAllStrict :: forall m h
              .  (HasCallStack, Monad m)
              => HasFS m h
              -> Handle h
              -> BS.ByteString
              -> m Word64
hPutAllStrict hasFS h = go 0
  where
    go :: Word64 -> BS.ByteString -> m Word64
    go !written bs = do
      n <- hPutSome hasFS h bs
      let bs'      = BS.drop (fromIntegral n) bs
          written' = written + n
      if BS.null bs'
        then return written'
        else go written' bs'

-- | This function makes sure that the whole 'BL.ByteString' is written.
hPutAll :: forall m h
        .  (HasCallStack, Monad m)
        => HasFS m h
        -> Handle h
        -> BL.ByteString
        -> m Word64
hPutAll hasFS h = foldM putChunk 0 . BL.toChunks
  where
    putChunk :: Word64 -> BS.ByteString -> m Word64
    putChunk written chunk = do
      written' <- hPutAllStrict hasFS h chunk
      return $! written + written'

-- | This function makes sure that the whole 'Builder' is written.
--
-- The chunk size of the resulting 'BL.ByteString' determines how much memory
-- will be used while writing to the handle.
hPut :: forall m h
     .  (HasCallStack, Monad m)
     => HasFS m h
     -> Handle h
     -> Builder
     -> m Word64
hPut hasFS g = hPutAll hasFS g . BS.toLazyByteString

{-------------------------------------------------------------------------------
  SomeHasFS
-------------------------------------------------------------------------------}

-- | It is often inconvenient to have to parameterise over @h@. One often makes
-- it existential, losing the ability to use derive 'Generic' and 'NoThunks'.
-- This data type hides an existential @h@ parameter of a 'HasFS' and provides a
-- 'NoThunks' thunks instance.
data SomeHasFS m where
  SomeHasFS :: Eq h => HasFS m h -> SomeHasFS m

  deriving NoThunks via OnlyCheckWhnfNamed "SomeHasFS" (SomeHasFS m)
