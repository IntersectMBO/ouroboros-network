{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
module Ouroboros.Storage.ImmutableDB.Types
  ( Slot (..)
  , Epoch (..)
  , EpochSize (..)
  , SlotOffset
  , Tip (..)
  , TruncateTo (..)
  , EpochFileParser (..)
  , ValidationPolicy (..)
  , IteratorID
  , initialIteratorID
  , ImmutableDBError (..)
  , sameImmutableDBError
  , prettyImmutableDBError
  , UserError (..)
  , prettyUserError
  , UnexpectedError (..)
  , sameUnexpectedError
  , prettyUnexpectedError
  ) where

import           Codec.Serialise (DeserialiseFailure)
import           Control.Exception (Exception (..))

import           Data.Word (Word, Word64)

import           GHC.Generics (Generic)
import           GHC.Stack (CallStack, prettyCallStack)

import           Ouroboros.Network.Block (Slot (..))

import           Ouroboros.Storage.FS.API.Types (FsError, FsPath, prettyFsError,
                     sameFsError)


-- | An epoch, i.e. the number of the epoch.
newtype Epoch = Epoch { getEpoch :: Word }
  deriving (Eq, Ord, Enum, Num, Show, Generic, Real, Integral)

newtype EpochSize = EpochSize { getEpochSize :: Word }
  deriving (Eq, Ord, Enum, Num, Show, Generic, Real, Integral)

-- | The offset of a slot in an index file.
type SlotOffset = Word64

-- | The tip of the 'Ouroboros.Storage.ImmutableDB.API.ImmutableDB'.
-- TODO unify with Edsko's Tip:
--
-- data Tip r = Tip r | TipGen
data Tip
  = TipGenesis
    -- ^ The database is empty
  | TipEBB     Epoch
    -- ^ The last thing in the database is the EBB of this 'Epoch'.
  | TipBlock   Slot
    -- ^ The last thing in the database is the block at this 'Slot'.
  deriving (Eq, Show, Generic)

-- | Truncate the database to some 'Tip'. This means that everything in the
-- database that comes after the 'Tip' will be removed, excluding the EBB or
-- block the 'Tip' points to.
newtype TruncateTo = TruncateTo { getTruncateTo :: Tip }
  deriving (Eq, Show, Generic)

-- | Parse the contents of an epoch file.
--
-- The parsing may include validation of the contents of the epoch file.
--
-- The 'SlotOffset' is the offset (in bytes) of the start of the corresponding
-- @t@ (block). The returned 'SlotOffset's are from __first to last__ and
-- __strictly__ monotonically increasing. The first 'SlotOffset' must be 0.
--
-- The @Maybe hash@ is the hash of the EBB, if present. The EBB itself should
-- be the first entry in the list, if present.
--
-- We assume the output of 'EpochFileParser' to be correct, we will not
-- validate it.
--
-- An error may be returned in the form of @'Maybe' e@. The 'SlotOffset's may
-- be accompanied with other data of type @t@.
newtype EpochFileParser e hash m t = EpochFileParser
  { runEpochFileParser :: FsPath -> m ([(SlotOffset, t)], Maybe hash, Maybe e) }
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
initialIteratorID :: IteratorID
initialIteratorID = IteratorID 0

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
  = AppendToSlotInThePastError Slot Tip
    -- ^ When trying to append a new binary blob, the input slot was in the
    -- past, i.e. before or equal to the tip of the database.
  | AppendToEBBInThePastError Epoch Epoch
    -- ^ When trying to append a new EBB, the input epoch was in the past,
    -- i.e. less than the current epoch or blobs have already been appended to
    -- the current epoch.
    --
    -- The first parameter is the input epoch and the second parameter is the
    -- current epoch.
  | ReadFutureSlotError Slot Tip
    -- ^ When trying to read a slot, the slot was not yet occupied, because
    -- it's too far in the future, i.e. it is after the tip of the database.
  | ReadFutureEBBError Epoch Epoch
    -- ^ When trying to read an EBB, the requested epoch was in the future.
    --
    -- The first parameter is the requested epoch and the second parameter is
    -- the current epoch.
  | InvalidIteratorRangeError Slot Slot
    -- ^ When the chosen iterator range was invalid, i.e. the @start@ (first
    -- parameter) came after the @end@ (second parameter).
  | ClosedDBError
    -- ^ When performing an operation on a closed DB that is only allowed when
    -- the database is open.
  | OpenDBError
    -- ^ When performing an operation on an open DB that is only allowed when
    -- the database is closed.
  deriving (Eq, Show, Generic)


-- | Pretty-print a 'Ouroboros.Storage.ImmutableDB.Types.UserError', including
-- its callstack.
prettyUserError :: UserError -> String
prettyUserError = \case
    AppendToSlotInThePastError is tip ->
      "AppendToSlotInThePastError (input slot was " <> show is <>
      ", tip is " <> show tip <> ")"
    AppendToEBBInThePastError ie ce ->
      "AppendToEBBInThePastError (input epoch was " <> show ie <>
      ", current epoch is " <> show ce <> ")"
    ReadFutureSlotError requested tip ->
      "ReadFutureSlotError (requested was " <> show requested <>
      ", tip is " <> show tip <> ")"
    ReadFutureEBBError re ce ->
      "ReadFutureEBBError (requested was " <> show re <>
      ", the current epoch is " <> show ce <> ")"
    InvalidIteratorRangeError start end ->
      "InvalidIteratorRangeError (start was " <> show start <> " end was " <>
      show end <> ")"
    ClosedDBError -> "ClosedDBError"
    OpenDBError   -> "OpenDBError"


data UnexpectedError
  = FileSystemError FsError -- An FsError already stores the callstack
    -- ^ An IO operation on the file-system threw an error.
  | InvalidFileError FsPath CallStack
    -- ^ When loading an epoch or index file, its contents did not pass
    -- validation.
  | MissingFileError FsPath CallStack
    -- ^ A missing epoch or index file.
  | DeserialisationError DeserialiseFailure CallStack
    -- ^ Deserialisation ('Codec.Serialise.Serialise') went wrong.
  deriving (Show, Generic)

-- | Check if two 'Ouroboros.Storage.ImmutableDB.Types.UnexpectedError's are
-- equal while ignoring their 'CallStack's.
sameUnexpectedError :: UnexpectedError -> UnexpectedError -> Bool
sameUnexpectedError ue1 ue2 = case (ue1, ue2) of
    (FileSystemError fse1,       FileSystemError fse2)       -> sameFsError fse1 fse2
    (FileSystemError {},         _)                          -> False
    (InvalidFileError p1 _,      InvalidFileError p2 _)      -> p1 == p2
    (InvalidFileError {},        _)                          -> False
    (MissingFileError p1 _,      MissingFileError p2 _)      -> p1 == p2
    (MissingFileError {},        _)                          -> False
    (DeserialisationError df1 _, DeserialisationError df2 _) -> df1 == df2
    (DeserialisationError {},    _)                          -> False

-- | Pretty-print an 'Ouroboros.Storage.ImmutableDB.Types.UnexpectedError',
-- including its callstack.
prettyUnexpectedError :: UnexpectedError -> String
prettyUnexpectedError = \case
    FileSystemError fse -> prettyFsError fse
    InvalidFileError path cs ->
      "InvalidFileError (" <> show path <> "): " <>
      prettyCallStack cs
    MissingFileError path cs ->
      "MissingFileError (" <> show path <> "): " <>
      prettyCallStack cs
    DeserialisationError df cs ->
      "DeserialisationError (" <> displayException df <> "): " <>
      prettyCallStack cs
