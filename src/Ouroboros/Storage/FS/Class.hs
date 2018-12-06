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
    , FsUnexpectedException(..)
    , FsPath
    , sameFsError
    , isResourceDoesNotExistError
    , prettyFSError
    -- * Actual HasFS monad stacks will have ExceptT at the top
    , HasFSE
    , FsHandleE
    , FsPtrE
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

data FsError =
    FsIllegalOperation FsPath CallStack
  | FsResourceInappropriateType FsPath CallStack
  -- ^ e.g the user tried to open a directory with hOpen rather than a file.
  | FsResourceAlreadyInUse FsPath CallStack
  | FsResourceDoesNotExist FsPath CallStack
  | FsResourceAlreadyExist FsPath CallStack
  | FsReachedEOF FsPath CallStack
  deriving Show

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
isResourceDoesNotExistError (FsResourceDoesNotExist _ _) = True
isResourceDoesNotExistError _                            = False

prettyFSError :: FsError -> String
prettyFSError = \case
    FsIllegalOperation fp cs          ->
        "FsIllegalOperation for " <> show fp <> ": " <> prettyCallStack cs
    FsResourceInappropriateType fp cs ->
        "FsResourceInappropriateType for " <> show fp <> ": " <> prettyCallStack cs
    FsResourceAlreadyInUse fp cs      ->
        "FsResourceAlreadyInUse for " <> show fp <> ": " <> prettyCallStack cs
    FsResourceDoesNotExist fp cs      ->
        "FsResourceDoesNotExist for " <> show fp <> ": " <> prettyCallStack cs
    FsResourceAlreadyExist fp cs      ->
        "FsResourceAlreadyInUse for " <> show fp <> ": " <> prettyCallStack cs
    FsReachedEOF fp cs                ->
        "FsReachedEOF for " <> show fp <> ": " <> prettyCallStack cs


-- | Check two 'FsError' for shallow equality, i.e. if they have the same
-- type constructor, ignoring the 'CallStack' and the filepath, as different
-- implementations we will use different ways of representing it. For example
-- IO will use an absolute one, a mock implementation only a relative one.
-- This is very useful during tests
-- when comparing different 'HasFS' implementations and assert that they throw
-- the same FsError type, even though the callstack is naturally different.
sameFsError :: FsError -> FsError -> Bool
sameFsError e1 e2 = case (e1, e2) of
    (FsIllegalOperation fp1 _, FsIllegalOperation fp2 _)                   -> fp1 == fp2
    (FsIllegalOperation _ _, _)                                            -> False
    (FsResourceAlreadyInUse fp1 _, FsResourceAlreadyInUse fp2 _)           -> fp1 == fp2
    (FsResourceAlreadyInUse _ _, _)                                        -> False
    (FsResourceInappropriateType fp1 _, FsResourceInappropriateType fp2 _) -> fp1 == fp2
    (FsResourceInappropriateType _ _, _)                                   -> False
    (FsResourceDoesNotExist fp1 _, FsResourceDoesNotExist fp2 _)           -> fp1 == fp2
    (FsResourceDoesNotExist _ _, _)                                        -> False
    (FsResourceAlreadyExist fp1 _, FsResourceAlreadyExist fp2 _)           -> fp1 == fp2
    (FsResourceAlreadyExist _ _, _)                                        -> False
    (FsReachedEOF fp1 _, FsReachedEOF fp2 _)                               -> fp1 == fp2
    (FsReachedEOF _ _, _)                                                  -> False

{------------------------------------------------------------------------------
 Typeclass which abstracts over the filesystem
------------------------------------------------------------------------------}

class Monad m => HasFS m where
    type FsHandle m :: *
    type FsPtr m    :: *
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
type FsPtrE    m = FsPtr    (ExceptT FsError m)
type BufferE   m = Buffer   (ExceptT FsError m)
