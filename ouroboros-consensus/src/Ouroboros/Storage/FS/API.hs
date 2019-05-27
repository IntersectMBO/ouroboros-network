{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | An abstract view over the filesystem.
module Ouroboros.Storage.FS.API (
      HasFS(..)
    , hGetExactly
    , hGetLenient
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
import           Data.Word (Word64)
import           GHC.Stack

import           Control.Monad.Class.MonadThrow

import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling,
                     throwError)

{------------------------------------------------------------------------------
 Typeclass which abstracts over the filesystem
------------------------------------------------------------------------------}

data HasFS m h = HasFS {
    -- | Debugging: human-readable description of file system state
    dumpState                :: m String

    -- Operations of files

    -- | Open a file
  , hOpen                    :: HasCallStack => FsPath -> OpenMode -> m h

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
    --
    -- The returned bytestring will typically have length @n@, but may be
    -- shorter in case of a partial read, see #277.
    --
    -- If nothing can be read at all, an exception will be thrown.
    --
    -- Postcondition: the length of the returned bytestring <= @n@ and > 0.
  , hGetSome                 :: HasCallStack => h -> Int -> m BS.ByteString

    -- | Write to a handle
    --
    -- The return value indicates the number of bytes written and will
    -- typically be equal to @l@, the length of the bytestring, but may be
    -- shorter in case of a partial write, see #277.
    --
    -- If nothing can be written at all, an exception will be thrown.
    --
    -- Postcondition: the return value <= @l@ and > 0.
  , hPutSome                 :: HasCallStack => h -> BS.ByteString -> m Word64

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

    -- | Return the path corresponding to the handle
    --
    -- Useful for error reporting.
    --
    -- TODO make this a pure function
  , handlePath               :: HasCallStack => h -> m FsPath

    -- | Error handling
  , hasFsErr                 :: ErrorHandling FsError m
  }

withFile :: (HasCallStack, MonadThrow m)
         => HasFS m h -> FsPath -> OpenMode -> (h -> m a) -> m a
withFile HasFS{..} fp openMode = bracket (hOpen fp openMode) hClose

-- | Makes sure it reads all requested bytes.
-- If eof is found before all bytes are read, it throws an exception.
hGetExactly :: forall m h
            . (HasCallStack, Monad m)
            => HasFS m h
            -> h
            -> Int
            -> m BL.ByteString
hGetExactly hasFS h n = go n []
  where
    go :: Int -> [BS.ByteString] -> m BL.ByteString
    go 0 bss = return $ BL.fromChunks $ reverse bss
    go remainingBytes acc = do
      bs <- hGetSome hasFS h remainingBytes
      if BS.null bs then do
        path <- handlePath hasFS h
        throwError (hasFsErr hasFS) FsError {
            fsErrorType   = FsReachedEOF
          , fsErrorPath   = path
          , fsErrorString = "hGetExactly found eof before reading " ++ show n ++ " bytes"
          , fsErrorStack  = callStack
          , fsLimitation  = False
          }
      else go (remainingBytes - BS.length bs) (bs : acc)

-- | Like hGetExactly, but it does not throw an exception if eof is found.
hGetLenient :: forall m h
            . (HasCallStack, Monad m)
            => HasFS m h
            -> h
            -> Int
            -> m BL.ByteString
hGetLenient hasFS h n = go n []
  where
    go :: Int -> [BS.ByteString] -> m BL.ByteString
    go 0 bss = return $ BL.fromChunks $ reverse bss
    go remainingBytes acc = do
      bs <- hGetSome hasFS h remainingBytes
      if BS.null bs then
        go 0 acc
      else go (remainingBytes - BS.length bs) (bs : acc)

-- | This function makes sure that the whole 'BS.ByteString' is written.
hPutAllStrict :: forall m h
              .  (HasCallStack, Monad m)
              => HasFS m h
              -> h
              -> BS.ByteString
              -> m Word64
hPutAllStrict hasFS h = go 0
  where
    go :: Word64 -> BS.ByteString -> m Word64
    go !written bs = do
      n <- hPutSome hasFS h bs
      let bs'      = BS.drop (fromIntegral n) bs
          written' = written + fromIntegral n
      if BS.null bs'
        then return written'
        else go written' bs'


-- | This function makes sure that the whole 'BL.ByteString' is written.
hPutAll :: forall m h
        .  (HasCallStack, Monad m)
        => HasFS m h
        -> h
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
     -> h
     -> Builder
     -> m Word64
hPut hasFS g = hPutAll hasFS g . BS.toLazyByteString
