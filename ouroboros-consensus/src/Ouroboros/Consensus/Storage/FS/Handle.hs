{-# LANGUAGE LambdaCase #-}

-- | This is meant to be used for the implementation of HasFS
-- instances and not directly by client code.
module Ouroboros.Consensus.Storage.FS.Handle (
    HandleOS (..)
  , closeHandleOS
  , isHandleClosedException
  , isOpenHandleOS
  , withOpenHandle
  ) where

import           Control.Concurrent.MVar
import           Control.Exception hiding (handle)
import           Data.Maybe (isJust)
import           System.IO.Error as IO

-- | File handlers for the IO instance for HasFS.
-- This is parametric on the os.
--
-- The 'FilePath' is used to improve error messages.
-- The 'MVar' is used to implement 'close'.
-- osHandle is Fd for unix and HANDLE for Windows.
data HandleOS osHandle = HandleOS {
      filePath :: FilePath
    , handle   :: MVar (Maybe osHandle)
    }

instance Eq (HandleOS a) where
  h1 == h2 = handle h1 == handle h2

instance Show (HandleOS a) where
  show h = "<Handle " ++ filePath h ++ ">"

isOpenHandleOS :: HandleOS osHandle -> IO Bool
isOpenHandleOS = fmap isJust . readMVar . handle

-- | This is a no-op when the handle is already closed.
closeHandleOS :: HandleOS osHandle -> (osHandle -> IO ()) -> IO ()
closeHandleOS (HandleOS _ hVar) close =
  modifyMVar hVar $ \case
    Nothing -> return (Nothing, ())
    Just h  -> close h >> return (Nothing, ())

{-------------------------------------------------------------------------------
  Exceptions
-------------------------------------------------------------------------------}

-- | This is meant to be used for the implementation of individual file system commands.
-- Using it for larger scopes woud not be correct, since we would not notice if the
-- handle is closed.
withOpenHandle :: String -> HandleOS osHandle -> (osHandle -> IO a) -> IO a
withOpenHandle label (HandleOS fp hVar) k =
    withMVar hVar $ \case
        Nothing -> throwIO (handleClosedException fp label)
        Just fd -> k fd

handleClosedException :: FilePath -> String -> IOException
handleClosedException fp label =
      flip IO.ioeSetErrorType IO.illegalOperationErrorType
    $ flip IO.ioeSetFileName fp
    $ userError (label ++ ": FHandle closed")

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

isHandleClosedException :: IOException -> Bool
isHandleClosedException ioErr =
    IO.isUserErrorType (IO.ioeGetErrorType ioErr)
