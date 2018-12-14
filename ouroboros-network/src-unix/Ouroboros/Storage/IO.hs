{-# LANGUAGE LambdaCase #-}
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

import           Control.Concurrent.MVar (MVar, modifyMVar, newMVar, withMVar)
import           Control.Exception (throwIO)
import           Data.ByteString (ByteString)
import           Data.ByteString.Internal as Internal

import           Data.Word (Word32, Word64, Word8)
import           Foreign (Ptr)
import           System.IO (IOMode (..), SeekMode (..))
import           System.Posix (Fd (..), OpenFileFlags (..), OpenMode (..),
                     closeFd, fdReadBuf, fdSeek, fdWriteBuf, openFd,
                     stdFileMode)
import           System.Posix.Files (setFdSize)

-- A thin wrapper over a POSIX 'Fd', guarded by an MVar so that we can
-- implement 'close' as an idempotent operation.
newtype FHandle = FHandle (MVar (Maybe Fd))

-- | Some sensible defaults for the 'OpenFileFlags'. Note that the 'unix'
-- package /already/ exports a smart constructor called @defaultFileFlags@
-- already, but we define our own to not be depedent by whichever default
-- choice unix's library authors made, and to be able to change our minds
-- later if necessary.
-- In particular, we are interested in the 'append' and 'exclusive' flags,
-- which were largely the reason why we introduced this low-level module.
defaultFileFlags :: OpenFileFlags
defaultFileFlags = OpenFileFlags {
    append    = False
  , exclusive = False
  , noctty    = False
  , nonBlock  = False
  , trunc     = False
  }

-- | Opens a file from disk.
open :: FilePath -> IOMode -> IO FHandle
open filename ioMode = do
  let (openMode, fileMode, fileFlags)
       | ioMode == ReadMode    = ( ReadOnly
                                 , Nothing
                                 , defaultFileFlags)
       | ioMode == AppendMode  = ( WriteOnly
                                 , Just stdFileMode
                                 , defaultFileFlags { append = True })
       | otherwise             = ( ReadWrite
                                 , Just stdFileMode
                                 , defaultFileFlags)
  fd <- openFd filename openMode fileMode fileFlags
  FHandle <$> newMVar (Just fd)

-- | Writes the data pointed by the input 'Ptr Word8' into the input 'FHandle'.
write :: FHandle -> Ptr Word8 -> Word32 -> IO Word32
write (FHandle fdVar) data' bytes =
    withMVar fdVar $ \case
        Nothing -> throwIO (userError "write: the FHandle is closed.")
        Just fd -> fmap fromIntegral . fdWriteBuf fd data'
                                     $ fromIntegral bytes

-- | Seek within the file. Returns the offset within the file after the seek.
seek :: FHandle -> SeekMode -> Word64 -> IO Word64
seek (FHandle fdVar) seekMode bytes =
    withMVar fdVar $ \case
        Nothing -> throwIO (userError "seek: the FHandle is closed.")
        Just fd -> fromIntegral <$> fdSeek fd seekMode (fromIntegral bytes)

-- | Reads a given number of bytes from the input 'FHandle'.
read :: FHandle -> Int -> IO ByteString
read (FHandle fdVar) bytes =
    withMVar fdVar $ \case
        Nothing -> throwIO (userError "read: the FHandle is closed.")
        Just fd -> Internal.createUptoN bytes $ \ptr ->
                       fromIntegral <$> fdReadBuf fd ptr (fromIntegral bytes)

-- | Truncates the file managed by the input 'FHandle' to the input size.
truncate :: FHandle -> Word64 -> IO ()
truncate (FHandle fdVar) size =
    withMVar fdVar $ \case
        Nothing -> throwIO (userError "truncate: the FHandle is closed.")
        Just fd -> setFdSize fd (fromIntegral size)


-- | Closes a 'FHandle'. It's nice to be slightly more lenient here, as the
-- 'hClose' equivalent from 'System.IO' allows for this operation to be
-- idempotent.
close :: FHandle -> IO ()
close (FHandle fdVar) =
    modifyMVar fdVar $ \case
        Nothing -> return (Nothing, ())
        Just fd -> closeFd fd >> return (Nothing, ())
