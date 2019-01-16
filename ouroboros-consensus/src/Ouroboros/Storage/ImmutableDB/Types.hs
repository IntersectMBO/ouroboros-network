{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
module Ouroboros.Storage.ImmutableDB.Types
  ( Epoch
  , EpochSize
  , RelativeSlot(..)
  , EpochSlot(..)
  , SlotOffset
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

import           Data.Word (Word, Word64)

import           GHC.Generics (Generic)
import           GHC.Stack (CallStack, prettyCallStack)

import           Ouroboros.Storage.FS.Class.Types (FsError, FsPath, sameFsError,
                                                   prettyFsError)

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
    -- already finalised, i.e. not writable anymore.
    --
    -- The first parameter is the requested epoch and the second parameter is
    -- the last epoch that can be opened.
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
  | IncorrectIndexSizeError Epoch EpochSize EpochSize
    -- ^ When opening a DB, the size given for each past epoch (the first
    -- parameter) is checked against the size according to the corresponding
    -- index file on disk. When the expected size, passed to 'openDB' or
    -- 'withDB' (the second parameter) doesn't match the size according to the
    -- index (the third parameter). TODO user or unexpected?
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
    IncorrectIndexSizeError epoch expected actual ->
      "IncorrectIndexSizeError (expected size for epoch " <> show epoch <>
      " was " <> show expected <> " actual size was " <> show actual <> ")"
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
