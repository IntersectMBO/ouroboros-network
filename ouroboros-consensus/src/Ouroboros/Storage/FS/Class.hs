{-# LANGUAGE ConstraintKinds            #-}
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

{-- |

An abstract view over the filesystem.

--}

module Ouroboros.Storage.FS.Class (
    -- * Opaque types and main API
      HasFS(..)
    , FsError(..)
    , FsErrorType(..)
    , FsUnexpectedException(..)
    , FsPath
    , sameFsError
    , isResourceDoesNotExistError
    , prettyFSError
    -- * Actual HasFS monad stacks will have ExceptT at the top
    , HasFSE
    , FsHandleE
    , BufferE
    -- * Re-exports from System.IO
    , SeekMode(..)
    , IOMode(..)
    ) where

import           Control.Exception (Exception (..), IOException)
import           Control.Monad.Except

import           GHC.Stack

import           Data.ByteString (ByteString)
import           Data.ByteString.Builder (Builder)
import           Data.Semigroup ((<>))
import           Data.Word (Word64)

import           System.IO (IOMode (..), SeekMode (..))

{------------------------------------------------------------------------------
 Abstracting over file paths
------------------------------------------------------------------------------}

type FsPath = [String]

{------------------------------------------------------------------------------
 Handling failure
------------------------------------------------------------------------------}

data FsError = FsError FsErrorType (Maybe FsPath) CallStack
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

-- | We define a 'FsUnexpectedException' separated by the rest so that we
-- can still \"tag\" any exception coming from the underlying concrete monad
-- that we don't support in our 'HasFS' abstraction, but without the risk of
-- polluting the 'FsError' sum type, as introducing such a new type constructor
-- would force us to catch and deal with this \"unexpected\" exception.
data FsUnexpectedException = FsUnexpectedException IOException CallStack
                             deriving Show

instance Exception FsUnexpectedException

instance Exception FsError where
    displayException = prettyFSError

isResourceDoesNotExistError :: FsError -> Bool
isResourceDoesNotExistError (FsError FsResourceDoesNotExist _ _) = True
isResourceDoesNotExistError _                                    = False

prettyFSError :: FsError -> String
prettyFSError (FsError et fp cs) =
    show et <> " for " <> show fp <> ": " <> prettyCallStack cs


-- | Check two 'FsError' for shallow equality, i.e. if they have the same
-- error type ('FsErrorType') and filepath, ignoring the 'CallStack'.
--
-- This is very useful during tests when comparing different 'HasFS'
-- implementations and assert that they throw the same 'FsErrorType', even
-- though the callstack is naturally different.
sameFsError :: FsError -> FsError -> Bool
sameFsError (FsError et1 fp1 _) (FsError et2 fp2 _) = et1 == et2 && fp1 == fp2

{------------------------------------------------------------------------------
 Typeclass which abstracts over the filesystem
------------------------------------------------------------------------------}

class Monad m => HasFS m where
    type FsHandle m :: *
    data Buffer m   :: *

    newBuffer :: Int -> m (Buffer m)
    dumpState :: m String

    -- Operations of files
    -- hOpen returns a 'Bool' stating whether the input file is new or not.
    hOpen      :: HasCallStack => FsPath     -> IOMode -> m (FsHandle m)
    hClose     :: HasCallStack => FsHandle m -> m ()
    hSeek      :: HasCallStack => FsHandle m -> SeekMode -> Word64 -> m Word64
    hGet       :: HasCallStack => FsHandle m -> Int -> m ByteString
    hPut       :: HasCallStack => FsHandle m -> Builder -> m Word64
    hPutBuffer :: HasCallStack => FsHandle m -> Buffer m -> Builder -> m Word64
    hTruncate  :: HasCallStack => FsHandle m -> Word64 -> m ()
    withFile   :: HasCallStack => FsPath     -> IOMode -> (FsHandle m -> m r) -> m r

    -- Operations of directories
    createDirectory          :: HasCallStack => FsPath -> m ()
    createDirectoryIfMissing :: HasCallStack => Bool -> FsPath -> m ()
    listDirectory            :: HasCallStack => FsPath -> m [String]
    doesDirectoryExist       :: HasCallStack => FsPath -> m Bool
    doesFileExist            :: HasCallStack => FsPath -> m Bool

{-------------------------------------------------------------------------------
  ExceptT support

  To avoid 'HasFS' instance pinning the 'MonadError' constraint on a monad
  stack to a single type ('FsError'), 'HasFS' instances will look like

  > instance HasFS (ExceptT ..) where ..

  To make working with this a bit more convenient, we define some type aliases
  here.
-------------------------------------------------------------------------------}

type HasFSE    m = HasFS    (ExceptT FsError m)
type FsHandleE m = FsHandle (ExceptT FsError m)
type BufferE   m = Buffer   (ExceptT FsError m)
