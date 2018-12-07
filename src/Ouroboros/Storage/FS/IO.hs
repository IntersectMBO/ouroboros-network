{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-- |

An abstract view over the filesystem.

--}

module Ouroboros.Storage.FS.IO (
    -- * IO implementation & monad
      IOFS -- opaque
    , runIOFS
    ) where

import           Control.Exception (throwIO)
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Reader

import           GHC.IO.Exception
import           GHC.Stack

import qualified Data.ByteString.Builder.Extra as BS
import qualified Data.ByteString.Unsafe as BS
import           Data.List (foldl', stripPrefix)
import           Data.Word (Word64, Word8)

import           Foreign (ForeignPtr, Ptr, castPtr, withForeignPtr)
import           GHC.ForeignPtr (mallocPlainForeignPtrBytes)
import qualified System.Directory as Dir
import           System.FilePath (splitDirectories, (</>))
import           System.IO.Error

import           Ouroboros.Storage.FS.Class
import qualified Ouroboros.Storage.IO as F

-- | \"Roots\" a FsPath to a particular root.
makeAbsolute :: FsPath -> FilePath -> FilePath
makeAbsolute pths currentMountPoint =
    currentMountPoint </> foldl' (</>) mempty pths


-- The 'MountPoint' is conceptually the working directory from where we run
-- our concrete monad which implements 'HasFS'. To keep up the analogy with the
-- unix filesystem, we are "mounting" the particular device at the directory
-- 'MountPoint', so that each 'FsPath' we use inside our monad is
-- interpreted as a relative one respect to the 'MountPoint', and turned
-- absolute when we run our monad.
type MountPoint = FilePath

-- | Tries to catch a subset of the 'IOException's that the input 'IO' action
-- might throw, reifying them into a 'FsError', or rethrowing in case the
-- 'IOException' doesn't map to one of the supported 'FsError' constructors.
-- We need to pass as input the 'MountPoint' so we can make the filepath we
-- return in the 'FsError' relative, in compliance with other implementations.
catchFSErrorIO :: HasCallStack => MountPoint -> IO a -> IO (Either FsError a)
catchFSErrorIO (splitDirectories -> mountPoint) action = do
    res <- tryIOError action
    case res of
         Left err -> handleError err
         Right a  -> return (Right a)
    where
        getPath :: IOError -> IO FsPath
        getPath ioe = case ioe_filename ioe of
            Nothing ->
              throwIO $ FsUnexpectedException
                  (userError $ "getPath: ioe_filename was empty in " ++ displayException ioe)
                  callStack
            Just fp ->
                 case stripPrefix mountPoint (splitDirectories fp) of
                      Just path -> return path
                      Nothing -> throwIO $ FsUnexpectedException
                          (userError $ "getPath: stripPrefix returned empty path in " ++ displayException ioe)
                          callStack

        handleError :: HasCallStack => IOError -> IO (Either FsError a)
        handleError ioErr
          | isIllegalOperationErrorType eType =
            return . Left =<< (FsIllegalOperation <$> getPath ioErr <*> pure callStack)
          | isAlreadyExistsErrorType eType =
            return . Left =<< (FsResourceAlreadyExist <$> getPath ioErr <*> pure callStack)
          | isDoesNotExistErrorType eType =
            return . Left =<< (FsResourceDoesNotExist <$> getPath ioErr <*> pure callStack)
          | isAlreadyInUseErrorType eType =
            return . Left =<< (FsResourceAlreadyInUse <$> getPath ioErr <*> pure callStack)
          | isEOFErrorType eType =
            return . Left =<< (FsReachedEOF <$> getPath ioErr <*> pure callStack)
          | isFullErrorType eType =
            return . Left =<< (FsDeviceFull <$> getPath ioErr <*> pure callStack)
          | isPermissionErrorType eType =
            return . Left =<< (FsInsufficientPermissions <$> getPath ioErr <*> pure callStack)
          | eType == InappropriateType =
            return . Left =<< (FsResourceInappropriateType <$> getPath ioErr <*> pure callStack)
          | otherwise = throwIO (FsUnexpectedException ioErr callStack)
         where eType :: IOErrorType
               eType = ioeGetErrorType ioErr

{------------------------------------------------------------------------------
  The IOFS monad
-------------------------------------------------------------------------------}

type IOFSE = ExceptT FsError IOFS
type IOFS  = ReaderT MountPoint IO

-- | Lifts an 'IO' action into the 'IOFSE' monad. Note how this /must/ be
-- used as a drop-in replacement for 'liftIO', as the former would also correctly
-- catch any relevant IO exception and reify it into a 'FsError' one.
liftIOE :: IO a -> IOFSE a
liftIOE act = ExceptT $ do
    mp <- ask
    lift $ catchFSErrorIO mp act

runIOFS :: IOFS a -> MountPoint -> IO a
runIOFS r mountPoint = runReaderT r mountPoint

withAbsPath :: FsPath -> (FilePath -> IO a) -> IOFSE a
withAbsPath p act = asks (makeAbsolute p) >>= liftIOE . act

instance HasFS IOFSE where
    type FsHandle IOFSE = F.FHandle
    type FsPtr    IOFSE = Ptr Word8
    data Buffer   IOFSE =
        BufferIO {-# UNPACK #-} !(ForeignPtr Word8) {-# UNPACK #-} !Int

    -- TODO(adn) Might be useful to implement this properly by reading all
    -- the stuff available at the 'MountPoint'.
    dumpState = return "<dumpState@IO>"

    hOpen path ioMode = withAbsPath path $ \fp -> F.open fp ioMode

    newBuffer              = liftIOE . newBufferIO
    hClose                 = liftIOE . F.close
    hSeek     hnd seekMode = liftIOE . F.seek     hnd seekMode
    hGet      hnd          = liftIOE . F.read     hnd
    hTruncate hnd          = liftIOE . F.truncate hnd

    hPut hnd builder     = do
        buf0 <- newBuffer BS.defaultChunkSize
        hPutBuffer hnd buf0 builder
    hPutBuffer hnd buf0 = liftIOE . go 0 buf0 . BS.runBuilder
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

    -- Can't use withAbsPath here, negative occurrence of IO
    withFile path ioMode action = do
        fp <- asks (makeAbsolute path)
        bracket (liftIOE $ F.open fp ioMode) (liftIOE . F.close) action

    createDirectory    path = withAbsPath path $ Dir.createDirectory
    listDirectory      path = withAbsPath path $ Dir.listDirectory
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
           -> (FsPtr IOFSE -> Int -> IO a)
           -> IO a
withBuffer (BufferIO fptr len) action =
    withForeignPtr fptr $ \ptr -> action ptr len
