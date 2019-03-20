{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
module Ouroboros.Storage.ImmutableDB.Types
  ( SlotNo(..)
  , EpochNo(..)
  , EpochSize(..)
  , SlotOffset
  , TruncateFrom(..)
  , extractTruncateFrom
  , EpochFileParser(..)
  , ValidationPolicy(..)
  , IteratorID
  , initialIteratorId
  , ImmutableDBError(..)
  , sameImmutableDBError
  , prettyImmutableDBError
  , UserError(..)
  , prettyUserError
  , UnexpectedError(..)
  , sameUnexpectedError
  , prettyUnexpectedError
  ) where

import           Control.Exception (Exception (..))

import           Data.Word (Word64)

import           GHC.Generics (Generic)
import           GHC.Stack (CallStack, prettyCallStack)

import           Ouroboros.Network.Block (SlotNo (..))

import           Ouroboros.Storage.FS.API.Types (FsError, FsPath, prettyFsError,
                     sameFsError)


-- | An epoch, i.e. the number of the epoch.
newtype EpochNo = EpochNo { unEpochNo :: Word64 }
  deriving (Eq, Ord, Enum, Num, Show, Generic)

newtype EpochSize = EpochSize { unEpochSize :: Word64 }
  deriving (Eq, Ord, Enum, Num, Show, Generic, Real, Integral)

-- | The offset of a slot in an index file.
type SlotOffset = Word64

-- | Truncate the database starting from the 'SlotNo' (inclusive). In other
-- words, all slots >= the given slot will be removed.
newtype TruncateFrom = TruncateFrom { getTruncateFrom :: SlotNo }
  deriving (Eq, Ord, Show, Generic)

-- | Try to extract a 'TruncateFrom' from a 'ImmutableDBError'.
--
-- When validation detects a missing or corrupt file, a 'InvalidFileError' or
-- 'MissingFileError' is thrown. Both contain a 'TruncateFrom' that can be
-- used to truncate the database to its last valid slot. Pass the
-- 'TruncateFrom' to 'Ouroboros.Storage.ImmutableDB.API.reopen' to truncate
-- and reopen.
extractTruncateFrom :: ImmutableDBError -> Maybe TruncateFrom
extractTruncateFrom e = case e of
    UserError {}       -> Nothing
    UnexpectedError ue -> case ue of
      FileSystemError {}         -> Nothing
      InvalidFileError _ trunc _ -> Just trunc
      MissingFileError _ trunc _ -> Just trunc
    -- TODO let validation only throw 'UnexpectedError'?

-- | Parse the contents of an epoch file.
--
-- The parsing may include validation of the contents of the epoch file.
--
-- The 'SlotOffset' is the offset (in bytes) of the start of the corresponding
-- @t@ (block). The returned 'SlotOffset's are from __first to last__ and
-- __strictly__ monotonically increasing. The first 'SlotOffset' must be 0.
--
-- We assume the output of 'EpochFileParser' to be correct, we will not
-- validate it.
--
-- An error may be returned in the form of @'Maybe' e@. The 'SlotOffset's may
-- be accompanied with other data of type @t@.
newtype EpochFileParser e m t = EpochFileParser
  { runEpochFileParser :: FsPath -> m ([(SlotOffset, t)], Maybe e) }
  deriving (Functor)

-- | The validation policy used when (re)opening an
-- 'Ouroboros.Storage.ImmutableDB.API.ImmutableDB'.
--
-- The validation policy is used by:
--
-- * 'Ouroboros.Storage.ImmutableDB.Impl.openDB': the initial opening of the
--   database, either an empty database or a database that was previously
--   closed.
--
-- * 'Ouroboros.Storage.ImmutableDB.API.reopen': when the database was closed
--   in case of an unexpected error, the user can reopen the database and keep
--   on using the same handle to it.
--
-- We will refer to both these operations using \"/open/\".
--
-- The recovery policy dictates which on-disk files /open/ should validate.
data ValidationPolicy
  = ValidateMostRecentEpoch
    -- ^ /open/ will validate the epoch and index files of the most recent
    -- epoch stored on disk.
    --
    -- Prior epoch and index files are ignored, even their presence will not
    -- be checked.
    --
    -- /open/ will throw a 'MissingFileError' or an 'InvalidFileError' in case
    -- of a missing or invalid epoch file, or an invalid index file.
    --
    -- Because not all files are validated, subsequent operations on the
    -- database after /open/ may result in unexpected errors.
  | ValidateAllEpochs
    -- ^ /open/ will validate the epoch and index files of all epochs starting
    -- from the first one up to the last epoch stored on disk.
    --
    -- /open/ will throw a 'MissingFileError' or an 'InvalidFileError' in case
    -- of a missing or invalid epoch file, or an invalid index file.
  deriving (Show, Eq, Generic)

-- | A unique identifier for an iterator.
newtype IteratorID = IteratorID { getIteratorID :: Int }
  deriving (Show, Eq, Ord, Enum, Generic)

-- | Initial identifier number, use 'succ' to generate the next one.
initialIteratorId :: IteratorID
initialIteratorId = IteratorID 0

-- | Errors that might arise when working with this database.
data ImmutableDBError
  = UserError       UserError CallStack
    -- ^ An error thrown because of incorrect usage of the immutable database
    -- by the user.
  | UnexpectedError UnexpectedError
    -- ^ An unexpected error thrown because something went wrong on a lower
    -- layer.
  deriving (Generic)

instance Show ImmutableDBError where
  show = prettyImmutableDBError

instance Exception ImmutableDBError where
  displayException = prettyImmutableDBError

-- | Check if two 'ImmutableDBError's are equal while ignoring their
-- 'CallStack's.
sameImmutableDBError :: ImmutableDBError -> ImmutableDBError -> Bool
sameImmutableDBError e1 e2 = case (e1, e2) of
    (UserError ue1 _,     UserError ue2 _)     -> ue1 == ue2
    (UserError {},        _)                   -> False
    (UnexpectedError ue1, UnexpectedError ue2) -> sameUnexpectedError ue1 ue2
    (UnexpectedError {},  _)                   -> False

-- | Pretty-print an 'ImmutableDBError', including its callstack.
prettyImmutableDBError :: ImmutableDBError -> String
prettyImmutableDBError = \case
    UserError       ue cs -> prettyUserError ue <> ": " <> prettyCallStack cs
    UnexpectedError ue    -> prettyUnexpectedError ue

data UserError
  = AppendToSlotInThePastError SlotNo SlotNo
    -- ^ When trying to append a new binary blob, the input slot was in the
    -- past, i.e. less than the next expected slot.
    --
    -- The first parameter is the input slot and the second parameter is the
    -- next slot available for appending.
  | ReadFutureSlotError SlotNo SlotNo
    -- ^ When trying to read a slot, the slot was not yet occupied, either
    -- because it's too far in the future or because it is in the process of
    -- being written.
    --
    -- The first parameter is the requested slot and the second parameter is
    -- the next slot that will be appended to, and thus the slot marking the
    -- future.
  | InvalidIteratorRangeError SlotNo SlotNo
    -- ^ When the chosen iterator range was invalid, i.e. the @start@ (first
    -- parameter) came after the @end@ (second parameter).
  | ClosedDBError
    -- ^ When performing an operation on a closed DB.
  deriving (Eq, Show, Generic)


-- | Pretty-print a 'Ouroboros.Storage.ImmutableDB.Types.UserError', including
-- its callstack.
prettyUserError :: UserError -> String
prettyUserError = \case
    AppendToSlotInThePastError is es ->
      "AppendToSlotInThePastError (input slot was " <> show is <>
      ", expected was " <> show es <> ")"
    ReadFutureSlotError requested futureSlot ->
      "ReadFutureSlotError (requested was " <> show requested <>
      ", the future starts at " <> show futureSlot <> ")"
    InvalidIteratorRangeError start end ->
      "InvalidIteratorRangeError (start was " <> show start <> " end was " <>
      show end <> ")"
    ClosedDBError -> "ClosedDBError"


data UnexpectedError
  = FileSystemError FsError -- An FsError already stores the callstack
    -- ^ An IO operation on the file-system threw an error.
  | InvalidFileError FsPath TruncateFrom CallStack
    -- ^ When loading an epoch or index file, its contents did not pass
    -- validation.
  | MissingFileError FsPath TruncateFrom CallStack
    -- ^ A missing epoch or index file.
  deriving (Show, Generic)

-- | Check if two 'Ouroboros.Storage.ImmutableDB.Types.UnexpectedError's are
-- equal while ignoring their 'CallStack's.
sameUnexpectedError :: UnexpectedError -> UnexpectedError -> Bool
sameUnexpectedError ue1 ue2 = case (ue1, ue2) of
    (FileSystemError fse1,     FileSystemError fse2)     -> sameFsError fse1 fse2
    (FileSystemError {},       _)                        -> False
    (InvalidFileError p1 t1 _, InvalidFileError p2 t2 _) -> p1 == p2 && t1 == t2
    (InvalidFileError {},      _)                        -> False
    (MissingFileError p1 t1 _, MissingFileError p2 t2 _) -> p1 == p2 && t1 == t2
    (MissingFileError {},      _)                        -> False

-- | Pretty-print an 'Ouroboros.Storage.ImmutableDB.Types.UnexpectedError',
-- including its callstack.
prettyUnexpectedError :: UnexpectedError -> String
prettyUnexpectedError = \case
    FileSystemError fse -> prettyFsError fse
    InvalidFileError path trunc cs ->
      "InvalidFileError (" <> show path <> ", " <> show trunc <> "): " <>
      prettyCallStack cs
    MissingFileError path trunc cs ->
      "MissingFileError (" <> show path <> ", " <> show trunc <> "): " <>
      prettyCallStack cs
