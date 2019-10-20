{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

-- | This is meant to be used for the implementation of HasFS
-- instances and not directly by client code.
module Ouroboros.Storage.FS.Handle (
      HandleOS (..)
    , newHandleOS
    , readOpenHandle
    , writeOpenHandle
    , closeHandleOS
    ) where

import           Control.Concurrent.ReadWriteVar
import           Control.Exception hiding (handle)
import           System.IO.Error as IO

-- | File handles for the IO instance for HasFS. This is parametric on the os.
--
-- The 'FilePath' is used to improve error messages.
--
-- The 'RWVar' is used to implement concurrent reads and sequential writes.
--
-- osHandle is Fd for unix and HANDLE for Windows.
data HandleOS osHandle = HandleOS {
      filePath :: FilePath
    , handle   :: RWVar (Maybe osHandle)
    }

instance Eq (HandleOS a) where
  h1 == h2 = handle h1 == handle h2

instance Show (HandleOS a) where
  show h = "<Handle " ++ filePath h ++ ">"

{-------------------------------------------------------------------------------
  Handle Access
-------------------------------------------------------------------------------}

newHandleOS :: FilePath -> osHandle -> IO (HandleOS osHandle)
newHandleOS path osHandle =
    HandleOS path <$> new (Just osHandle)

-- | This is meant to be used for the implementation of individual file system
-- commands. Using it for larger scopes would not be correct, since we would not
-- notice if the handle is closed.
--
-- Multiple threads can have concurrent access to the open handle using this
-- combinator. However threads which try to write or close the handle block,
-- while there is an ongoing reader. Also readers blocks if some other thread
-- write or clolse.
readOpenHandle :: String -> HandleOS osHandle -> (osHandle -> IO a) -> IO a
readOpenHandle label (HandleOS fp hVar) k =
    with hVar $ \case
      Nothing -> throwIO (handleClosedException fp label)
      Just fd -> k fd

-- | Only one writer is allown. Writers block if there are any readers. Readers
-- block if there is any writer.
writeOpenHandle :: String -> HandleOS osHandle -> (osHandle -> IO a) -> IO a
writeOpenHandle label (HandleOS fp hVar) k =
    modify hVar $ \hndl -> case hndl of
      Nothing -> throwIO (handleClosedException fp label)
      Just fd -> (hndl,) <$> k fd

-- | Like 'writeOpenHandle' in terms of locking.
-- This is a no-op when the handle is already closed.
closeHandleOS :: HandleOS osHandle -> (osHandle -> IO ()) -> IO ()
closeHandleOS (HandleOS _ hVar) close =
    modify hVar $ \case
      Nothing -> return (Nothing, ())
      Just h  -> close h >> return (Nothing, ())

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

handleClosedException :: FilePath -> String -> IOException
handleClosedException fp label =
      flip IO.ioeSetErrorType IO.illegalOperationErrorType
    $ flip IO.ioeSetFileName fp
    $ userError (label ++ ": FHandle closed")
