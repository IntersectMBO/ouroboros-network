-- | IO implementation of the 'HasFS' class
module Ouroboros.Storage.FS.IO (
    -- * IO implementation & monad
      HandleIO
    , ioHasFS
    ) where

import           Control.Concurrent.MVar
import qualified Control.Exception as E
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Set as Set
import           Foreign (castPtr)
import           GHC.Stack
import qualified System.Directory as Dir

import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Types
import qualified Ouroboros.Storage.FS.Handle as H
import qualified Ouroboros.Storage.IO as F
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

{-------------------------------------------------------------------------------
  I/O implementation of HasFS
-------------------------------------------------------------------------------}

-- | File handlers for the IO instance for HasFS
--
-- We store the path the handle points to for better error messages
type HandleIO = F.FHandle

ioHasFS :: MountPoint -> HasFS IO HandleIO
ioHasFS mount = HasFS {
      -- TODO(adn) Might be useful to implement this properly by reading all
      -- the stuff available at the 'MountPoint'.
      dumpState = return "<dumpState@IO>"
    , hOpen = \fp openMode -> do
        let path = root fp
        osHandle <- rethrowFsError fp $
            F.open path openMode
        hVar <- newMVar $ Just osHandle
        return $ H.Handle fp path hVar
    , hClose = \h -> rethrowFsError (H.handlePath h) $
        F.close h
    , hSeek = \h mode o -> rethrowFsError (H.handlePath h) $
        F.seek h mode o
    , hGetSome = \h n -> rethrowFsError (H.handlePath h) $
        F.read h (fromIntegral n)
    , hTruncate = \h sz -> rethrowFsError (H.handlePath h) $
        F.truncate h sz
    , hGetSize = \h -> rethrowFsError (H.handlePath h) $
        F.getSize h
    , hPutSome = \h bs -> rethrowFsError (H.handlePath h) $ do
        BS.unsafeUseAsCStringLen bs $ \(ptr, len) ->
            fromIntegral <$> F.write h (castPtr ptr) (fromIntegral len)
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
    , removeFile = \fp -> rethrowFsError fp $
        Dir.removeFile (root fp)
    , handlePath = return . H.handlePath
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
