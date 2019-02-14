{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
module Ouroboros.Storage.ImmutableDB.Types
  ( Epoch
  , EpochSize
  , RelativeSlot(..)
  , EpochSlot(..)
  , SlotOffset
  , EpochFileParser(..)
  , RecoveryPolicy(..)
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

import           Data.Word (Word, Word64)

import           GHC.Generics (Generic)
import           GHC.Stack (CallStack, prettyCallStack)

import           Ouroboros.Storage.FS.API.Types (FsError, FsPath, prettyFsError,
                     sameFsError)

-- This is just a placeholder as we don't have (yet) a proper 'Epoch' type in
-- this codebase.
type Epoch = Word

type EpochSize = Word

-- | A /relative/ slot within an Epoch.
newtype RelativeSlot = RelativeSlot { getRelativeSlot :: Word }
                       deriving (Eq, Ord, Enum, Num, Show, Generic)

-- | The combination of an 'Epoch' and a 'RelativeSlot' within the epoch.
data EpochSlot = EpochSlot
  { _epoch        :: !Epoch
  , _relativeSlot :: !RelativeSlot
  } deriving (Eq, Ord, Generic)

instance Show EpochSlot where
  show (EpochSlot e (RelativeSlot s)) = show (e, s)


-- | The offset of a slot in an 'Index' file.
type SlotOffset = Word64

-- | Parse the contents of an epoch file.
--
-- The parsing may include validation of the contents of the epoch file.
--
-- The returned 'SlotOffset's are from __first to last__ and __strictly__
-- monotonically increasing. The first 'SlotOffset' must be 0.
--
-- We assume the output of 'EpochFileParser' to be correct, we will not
-- validate it.
--
-- An error may be returned in the form of @'Maybe' e@. The 'SlotOffset's may
-- be accompanied with other data of type @t@.
newtype EpochFileParser e m t = EpochFileParser
  { runEpochFileParser :: FsPath -> m ([(SlotOffset, t)], Maybe e) }
  deriving (Functor)


-- | The recovery policy used when (re)opening an 'ImmutableDB'.
--
-- The recovery policy is used by:
--
-- * 'openDB' (and 'openLastEpoch'): the initial opening of the database,
--   either an empty database or a database that was previously closed.
--
-- * 'reopen': when the database was closed in case of an unexpected error,
--   the user can reopen the database and keep on using the same handle to it.
--
-- We will refer to both these operations using \"/open/\".
--
-- The recovery policy dictates what /open/ should do in terms of:
--
-- * __Validation__: which on-disk files /open/ will validate.
--
-- * __Error handling__: what /open/ will do in case of an invalid or missing
--   file: throw an error or try to fix it.
--
-- * __(Re)opening__: which epoch /open/ will (re)open. When opening a
--   database ('openDB'), the epoch to open is explicitly passed. This should
--   be the most recent epoch that was started by the database the last time
--   it was open or 0 in case of an empty database. When reopening a database
--   ('reopen'), the most recent epoch that was started will be used to reopen
--   it.
--
-- Only in case of 'RestoreToLastValidEpoch' will /open/ try to repair the
-- invalid state and modify the file-system. When used with the other
-- policies, /open/ will throw an error in case of an invalid state and it
-- will not modify the file-system. If /open/ throws an error, the database it
-- will not have (re)opened the database.
--
-- Note: the 'Int' in @('EpochFileParser' e m ('Int', 'RelativeSlot'))@ is the
-- length of the blob.
data RecoveryPolicy e m
  = NoValidation
    -- ^ __Validation__: /open/ doesn't validate any files.
    --
    -- __Error handling__: /open/ throws an 'OpenFinalisedEpochError' when the
    -- epoch to reopen was already finalised and there are more recent epochs
    -- on disk.
    --
    -- Because no validation occurs, subsequent operations on the database
    -- after /open/ may result in unexpected errors.
    --
    -- __(Re)opening__: /open/ (re)opens the database at the explicitly passed
    -- epoch. This epoch can have been started in the past, in which case it
    -- is reopened. If not, it is opened as a \"fresh\" epoch.
  | ValidateMostRecentEpoch (EpochFileParser e m (Int, RelativeSlot))
    -- ^ __Validation__: /open/ validates the epoch and index files of the
    -- given epoch to open, which should correspond to the most recent epoch
    -- stored on disk. The 'EpochFileParser' will be used to verify the
    -- validity of the contents of the epoch and index files.
    --
    -- If the given epoch is a \"fresh\" epoch and has thus no files
    -- corresponding to it on disk, /open/ will not throw an error, but will
    -- not validate any files either.
    --
    -- Prior epoch and index files are ignored, even their presence will not
    -- be checked.
    --
    -- __Error handling__: /open/ throws an 'OpenFinalisedEpochError' when the
    -- epoch to reopen was already finalised and there are more recent epochs
    -- on disk.
    --
    -- /Open/ will throw a 'MissingFileError' or an 'InvalidFileError' in case
    -- of a missing or invalid epoch or index file.
    --
    -- Because not all files are validated, subsequent operations on the
    -- database after /open/ may result in unexpected errors.
    --
    -- __(Re)opening__: /open/ (re)opens the database at the explicitly passed
    -- epoch. This epoch can have been started in the past, in which case it
    -- is reopened. If not, it is opened as a \"fresh\" epoch.
  | ValidateAllEpochs (EpochFileParser e m (Int, RelativeSlot))
    -- ^ __Validation__: /open/ validates the epoch and index files of all
    -- epochs starting from the first one up to the given epoch to open, which
    -- should correspond to the most recent epoch stored on disk. The
    -- 'EpochFileParser' will be used to verify the validity of the contents
    -- of the epoch and index files.
    --
    -- __Error handling__: /open/ throws an 'OpenFinalisedEpochError' when the
    -- epoch to reopen was already finalised and there are more recent epochs
    -- on disk.
    --
    -- /Open/ will throw a 'MissingFileError' or an 'InvalidFileError' in case
    -- of a missing or invalid epoch or index file.
    --
    -- __(Re)opening__: /open/ (re)opens the database at the explicitly passed
    -- epoch. This epoch can have been started in the past, in which case it
    -- is reopened. If not, it is opened as a \"fresh\" epoch.
  | RestoreToLastValidEpoch (EpochFileParser e m (Int, RelativeSlot))
    -- ^ __Validation__: /open/ validates the epoch and index files of all
    -- epochs starting form the first one up to the given epoch to open, which
    -- should correspond to the most recent epoch stored on disk. The
    -- 'EpochFileParser' will be used to verify the validity of the contents
    -- of the epoch and index files.
    --
    -- __Error handling__: the goal is that /open/ does not throw any errors
    -- in case of invalid on-disk files, but that it restores the database to
    -- the last valid prefix and reopens it.
    --
    -- /Open/ will reconstructed missing or invalid index files from the
    -- corresponding epoch files using the 'EpochFileParser' and write them to
    -- disk.
    --
    -- /Open/ will stop the validation when it encounters an unrecoverable
    -- problem:
    --
    -- * An epoch file before the epoch to reopen is missing.
    --
    -- * An epoch file contains some invalid data. First, /open/ will truncate
    --   the epoch file to its last valid offset.
    --
    -- * An epoch file before the epoch to reopen is unfinalised (the former
    --   two cases will also result in an unfinalised epoch).
    --
    -- /Open/ will remove all epoch and index files corresponding to epochs
    -- newer than the reopened epoch.
    --
    -- __(Re)opening__: When possible, /open/ (re)opens the database at the
    -- explicitly passed epoch. This epoch can have been started in the past,
    -- in which case it is reopened. If not, it is opened as a \"fresh\"
    -- epoch.
    --
    -- In case of an unrecoverable problem, as described above, /open/ will
    -- open the database at the last valid epoch on disk instead of the
    -- explicitly passed epoch to open.


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
  deriving (Show, Generic)

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
  = OpenFinalisedEpochError Epoch Epoch
    -- ^ When opening the DB, the user requested access to an epoch which was
    -- already finalised, i.e. there are files corresponding to later epochs
    -- on disk.
    --
    -- Note that it is still possible to (re)open an epoch that can no longer
    -- be appended to if that is the most recent one on disk. This can be
    -- necessary when the epoch size of the next epoch is not yet known. The
    -- user must then call 'startNewEpoch' explicitly, passing the size of the
    -- new epoch.
    --
    -- The first parameter is the requested epoch and the second parameter is
    -- the last epoch that is on disk.
  | AppendToSlotInThePastError RelativeSlot RelativeSlot
    -- ^ When trying to append a new binary blob at the end of an epoch file,
    -- the input slot was not monotonically increasing w.r.t. the last slot
    -- that was appended to in the epoch.
    --
    -- The first parameter is the input slot and the second parameter is the
    -- next slot available for appending.
  | ReadFutureSlotError EpochSlot EpochSlot
    -- ^ When trying to read the slot from the epoch, the slot was not yet
    -- occupied, either because it's too far in the future or because it is in
    -- the process of being written.
    --
    -- The first parameter is the requested epoch + slot and the second
    -- parameter is the next slot that will be appended to, and thus the slot
    -- marking the future.
  | SlotGreaterThanEpochSizeError RelativeSlot Epoch EpochSize
    -- ^ When reading or appending to a slot in an epoch, the input slot was
    -- greater than or equal to the size of epoch, and thus beyond the last
    -- slot in the epoch.
  | MissingEpochSizeError Epoch
    -- ^ When opening a DB, the size for each past epoch and the opened epoch
    -- must be passed. The 'Epoch' parameter is the epoch for which the size
    -- was missing.
  | InvalidIteratorRangeError EpochSlot EpochSlot
    -- ^ When the chosen iterator range was invalid, i.e. the @start@ (first
    -- parameter) came after the @end@ (second parameter).
  | ClosedDBError
    -- ^ When performing an operation on a closed DB.
  deriving (Eq, Show, Generic)



-- | Pretty-print a 'UserError', including its callstack.
prettyUserError :: UserError -> String
prettyUserError = \case
    OpenFinalisedEpochError e1 e2 ->
      "OpenFinalisedEpochError (input epoch was " <>
      show e1 <> ", most recent epoch found: " <> show e2 <> ")"
    AppendToSlotInThePastError (RelativeSlot is) (RelativeSlot es) ->
      "AppendToSlotInThePastError (input slot was " <> show is <>
      ", expected was " <> show es <> ")"
    ReadFutureSlotError requested futureSlot ->
      "ReadFutureSlotError (requested was " <> show requested <>
      ", the future starts at " <> show futureSlot <> ")"
    SlotGreaterThanEpochSizeError (RelativeSlot is) e sz ->
      "SlotGreaterThanEpochSizeError (input slot was " <> show is <>
      ", size of epoch " <> show e <> " was " <> show sz <> ")"
    MissingEpochSizeError missing ->
      "MissingEpochSizeError (missing size for epoch " <> show missing <> ")"
    InvalidIteratorRangeError start end ->
      "InvalidIteratorRangeError (start was " <> show start <> " end was " <>
      show end <> ")"
    ClosedDBError -> "ClosedDBError"


data UnexpectedError
  = FileSystemError FsError -- An FsError already stores the callstack
    -- ^ An IO operation on the file-system threw an error.
  | InvalidFileError FsPath CallStack
    -- ^ When validating a DB, the contents of an epoch or index file did not
    -- pass validation.
  | MissingFileError FsPath CallStack
    -- ^ A missing epoch or index file.
  deriving (Show, Generic)

-- | Check if two 'UnexpectedError's are equal while ignoring their
-- 'CallStack's.
sameUnexpectedError :: UnexpectedError -> UnexpectedError -> Bool
sameUnexpectedError ue1 ue2 = case (ue1, ue2) of
    (FileSystemError fse1,   FileSystemError fse2)  -> sameFsError fse1 fse2
    (FileSystemError {},     _)                     -> False
    (InvalidFileError p1 _,  InvalidFileError p2 _) -> p1 == p2
    (InvalidFileError {},    _)                     -> False
    (MissingFileError p1 _,  MissingFileError p2 _) -> p1 == p2
    (MissingFileError {},    _)                     -> False

-- | Pretty-print an 'UnexpectedError', including its callstack.
prettyUnexpectedError :: UnexpectedError -> String
prettyUnexpectedError = \case
    FileSystemError fse -> prettyFsError fse
    InvalidFileError path cs ->
      "InvalidFileError (" <> show path <> "): " <> prettyCallStack cs
    MissingFileError path cs ->
      "MissingFileError (" <> show path <> "): " <> prettyCallStack cs
