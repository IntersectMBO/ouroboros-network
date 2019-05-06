{-# LANGUAGE RecordWildCards #-}

module Ouroboros.Storage.FS.API.Types (
    -- * Paths
    FsPath
  , MountPoint(..)
  , fsToFilePath
  , fsFromFilePath
    -- * Errors
  , FsError(..)
  , FsErrorType(..)
  , sameFsError
  , isFsErrorType
  , prettyFsError
    -- * From 'IOError' to 'FsError'
  , FsUnexpectedException
  , ioToFsError
  ) where

import           Control.Exception
import           Data.List (stripPrefix)
import qualified GHC.IO.Exception as GHC
import           GHC.Stack
import           System.FilePath
import qualified System.IO.Error as IO

{-------------------------------------------------------------------------------
  Paths
-------------------------------------------------------------------------------}

type FsPath = [String]

-- | Mount point
--
-- 'FsPath's are not absolute paths, but must be interpreted with respect to
-- a particualar mount point.
newtype MountPoint = MountPoint FilePath

fsToFilePath :: MountPoint -> FsPath -> FilePath
fsToFilePath (MountPoint mp) fp = mp </> foldr (</>) "" fp

fsFromFilePath :: MountPoint -> FilePath -> Maybe FsPath
fsFromFilePath (MountPoint mp) path =
    stripPrefix (splitDirectories mp) (splitDirectories path)

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

data FsError = FsError {
      -- | Error type
      fsErrorType   :: FsErrorType

      -- | Path to the file
    , fsErrorPath   :: FsPath

      -- | Human-readable string giving additional information about the error
    , fsErrorString :: String

      -- | Call stack
    , fsErrorStack  :: CallStack

      -- | Is this error due to a limitation of the mock file system?
      --
      -- The mock file system does not all of Posix's features and quirks.
      -- This flag will be set for such unsupported IO calls. Real I/O calls
      -- would not have thrown an error for these calls.
    , fsLimitation  :: Bool
    }
  deriving Show

data FsErrorType
  = FsIllegalOperation
  | FsResourceInappropriateType
  -- ^ e.g the user tried to open a directory with hOpen rather than a file.
  | FsResourceAlreadyInUse
  | FsResourceDoesNotExist
  | FsResourceAlreadyExist
  | FsReachedEOF
  | FsDeviceFull
  | FsInsufficientPermissions
  | FsInvalidArgument
  deriving (Show, Eq)

instance Exception FsError where
    displayException = prettyFsError

-- | Check if two errors are semantically the same error
--
-- This ignores the error string and the callstack.
sameFsError :: FsError -> FsError -> Bool
sameFsError e e' = fsErrorType e == fsErrorType e'
                && fsErrorPath e == fsErrorPath e'

isFsErrorType :: FsErrorType -> FsError -> Bool
isFsErrorType ty e = fsErrorType e == ty

prettyFsError :: FsError -> String
prettyFsError FsError{..} = concat [
      show fsErrorType
    , " for "
    , show fsErrorPath
    , ": "
    , fsErrorString
    , " at "
    , prettyCallStack fsErrorStack
    ]

{-------------------------------------------------------------------------------
  From 'IOError' to 'FsError'
-------------------------------------------------------------------------------}

-- | IO exception that cannot be mapped to 'FsError'
data FsUnexpectedException = FsUnexpectedException IOException CallStack
  deriving Show

instance Exception FsUnexpectedException

-- | Translate exceptions thrown by IO functions to 'FsError'
--
-- We take the 'FsPath' as an argument. We could try to translate back from
-- a 'FilePath' to an 'FsPath' (given a 'MountPoint'), but we know the 'FsPath'
-- at all times anyway and not all IO exceptions actually include a filepath.
ioToFsError :: HasCallStack
            => FsPath -> IOError -> Either FsUnexpectedException FsError
ioToFsError fp ioErr
    -- Errors for which there is an explicit predicate
    | IO.isAlreadyExistsErrorType eType =
        Right $ mkErr FsResourceAlreadyExist
    | IO.isDoesNotExistErrorType eType =
        Right $ mkErr FsResourceDoesNotExist
    | IO.isAlreadyInUseErrorType eType =
        Right $ mkErr FsResourceAlreadyInUse
    | IO.isFullErrorType eType =
        Right $ mkErr FsDeviceFull
    | IO.isEOFErrorType eType =
        Right $ mkErr FsReachedEOF
    | IO.isIllegalOperationErrorType eType =
        Right $ mkErr FsIllegalOperation
    | IO.isPermissionErrorType eType =
        Right $ mkErr FsInsufficientPermissions
    -- Other errors
    | eType == GHC.InappropriateType =
        Right $ mkErr FsResourceInappropriateType
    | eType == GHC.InvalidArgument =
        Right $ mkErr FsInvalidArgument
    | otherwise =
        Left $ FsUnexpectedException ioErr callStack
  where
    eType :: IO.IOErrorType
    eType = IO.ioeGetErrorType ioErr

    mkErr :: FsErrorType -> FsError
    mkErr ty = FsError {
                   fsErrorType   = ty
                 , fsErrorPath   = fp
                 , fsErrorString = IO.ioeGetErrorString ioErr
                 , fsErrorStack  = callStack
                 , fsLimitation  = False
                 }
