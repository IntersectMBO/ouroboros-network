{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

-- | This is meant to be used for the implementation of HasFS
-- instances and not directly by client code.
module Ouroboros.Storage.FS.Handle (
      HandleOS (..)
    , newHandleOS
    , withSharedOpenHandle
    , withExclOpenHandle
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
-- Multiple shared access is allowed. Blocks only the exclusive access of other
-- threads.
withSharedOpenHandle :: String -> HandleOS osHandle -> (osHandle -> IO a) -> IO a
withSharedOpenHandle label (HandleOS fp hVar) k =
    with hVar $ \case
      Nothing -> throwIO (handleClosedException fp label)
      Just fd -> k fd

-- | Only one exclusive access is allowed. Blocks any other shared or exclusive
-- access.
withExclOpenHandle :: String -> HandleOS osHandle -> (osHandle -> IO a) -> IO a
withExclOpenHandle label (HandleOS fp hVar) k =
    modify hVar $ \hndl -> case hndl of
      Nothing -> throwIO (handleClosedException fp label)
      Just fd -> (hndl,) <$> k fd

-- | Like 'withExclOpenHandle' in terms of locking.
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
