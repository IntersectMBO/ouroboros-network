module Ouroboros.Consensus.Storage.FS.API.Types {-# DEPRECATED "Use System.FS.API.Types from fs-api" #-} (
    -- * Modes
    AllowExisting (..)
  , OpenMode (..)
  , SeekMode (..)
  , allowExisting
    -- * Paths
  , MountPoint (..)
  , fsFromFilePath
  , fsPathFromList
  , fsPathInit
  , fsPathSplit
  , fsPathToList
  , fsToFilePath
  , mkFsPath
    -- ** opaque
  , FsPath
    -- * Handles
  , Handle (..)
    -- * Offset
  , AbsOffset (..)
    -- * Errors
  , FsError (..)
  , FsErrorPath (..)
  , FsErrorType (..)
  , fsToFsErrorPath
  , fsToFsErrorPathUnmounted
  , hasMountPoint
  , isFsErrorType
  , prettyFsError
  , sameFsError
    -- * From 'IOError' to 'FsError'
  , ioToFsError
  , ioToFsErrorType
  ) where

import           System.FS.API.Types
