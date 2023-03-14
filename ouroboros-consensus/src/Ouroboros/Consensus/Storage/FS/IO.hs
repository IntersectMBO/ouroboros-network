-- | IO implementation of the 'HasFS' class
module Ouroboros.Consensus.Storage.FS.IO (
    -- * IO implementation & monad
    HandleIO
  , ioHasFS
  ) where

import           Control.Concurrent.MVar
import qualified Control.Exception as E
import           Control.Monad.IO.Class (MonadIO (..))
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Set as Set
import           Foreign (castPtr)
import           GHC.Stack
import qualified System.Directory as Dir

import           Ouroboros.Consensus.Storage.FS.API
import           Ouroboros.Consensus.Storage.FS.API.Types
import qualified Ouroboros.Consensus.Storage.FS.Handle as H
import qualified Ouroboros.Consensus.Storage.IO as F

{-------------------------------------------------------------------------------
  I/O implementation of HasFS
-------------------------------------------------------------------------------}

-- | File handlers for the IO instance for HasFS
--
-- We store the path the handle points to for better error messages
type HandleIO = F.FHandle

ioHasFS :: MonadIO m => MountPoint -> HasFS m HandleIO
ioHasFS mount = HasFS {
      -- TODO(adn) Might be useful to implement this properly by reading all
      -- the stuff available at the 'MountPoint'.
      dumpState = return "<dumpState@IO>"
    , hOpen = \fp openMode -> liftIO $ do
        let path = root fp
        osHandle <- rethrowFsError fp $
            F.open path openMode
        hVar <- newMVar $ Just osHandle
        return $ Handle (H.HandleOS path hVar) fp
    , hClose = \(Handle h fp) -> liftIO $ rethrowFsError fp $
        F.close h
    , hIsOpen = liftIO . H.isOpenHandleOS . handleRaw
    , hSeek = \(Handle h fp) mode o -> liftIO $ rethrowFsError fp $
        F.seek h mode o
    , hGetSome = \(Handle h fp) n -> liftIO $ rethrowFsError fp $
        F.read h n
    , hGetSomeAt = \(Handle h fp) n o -> liftIO $ rethrowFsError fp $
        F.pread h n (unAbsOffset o)
    , hTruncate = \(Handle h fp) sz -> liftIO $ rethrowFsError fp $
        F.truncate h sz
    , hGetSize = \(Handle h fp) -> liftIO $ rethrowFsError fp $
        F.getSize h
    , hPutSome = \(Handle h fp) bs -> liftIO $ rethrowFsError fp $ do
        BS.unsafeUseAsCStringLen bs $ \(ptr, len) ->
            fromIntegral <$> F.write h (castPtr ptr) (fromIntegral len)
    , createDirectory = \fp -> liftIO $ rethrowFsError fp $
        Dir.createDirectory (root fp)
    , listDirectory = \fp -> liftIO $ rethrowFsError fp $
        Set.fromList <$>  Dir.listDirectory (root fp)
    , doesDirectoryExist= \fp -> liftIO $ rethrowFsError fp $
        Dir.doesDirectoryExist (root fp)
    , doesFileExist = \fp -> liftIO $ rethrowFsError fp $
        Dir.doesFileExist (root fp)
    , createDirectoryIfMissing = \createParent fp -> liftIO $ rethrowFsError fp $
        Dir.createDirectoryIfMissing createParent (root fp)
    , removeDirectoryRecursive = \fp -> liftIO $ rethrowFsError fp $
        Dir.removeDirectoryRecursive (root fp)
    , removeFile = \fp -> liftIO $ rethrowFsError fp $
        Dir.removeFile (root fp)
    , renameFile = \fp1 fp2 -> liftIO $ rethrowFsError fp1 $
        Dir.renameFile (root fp1) (root fp2)
    , mkFsErrorPath = fsToFsErrorPath mount
    , unsafeToFilePath = pure . root
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
        handleError ioErr = E.throwIO $ ioToFsError errorPath ioErr

        errorPath :: FsErrorPath
        errorPath = fsToFsErrorPath mount fp
