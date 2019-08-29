{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
module Ouroboros.Storage.ImmutableDB.Types
  ( SlotNo (..)
  , ImmTip
  , TipEpochSlot (..)
  , TruncateTo (..)
  , EpochFileParser (..)
  , ValidationPolicy (..)
  , BaseIteratorID
  , IteratorID(..)
  , initialIteratorID
  , ImmutableDBError (..)
  , sameImmutableDBError
  , prettyImmutableDBError
  , UserError (..)
  , prettyUserError
  , UnexpectedError (..)
  , sameUnexpectedError
  , prettyUnexpectedError
  , TraceEvent(..)
    -- * Current EBB
  , CurrentEBB(..)
  , hasCurrentEBB
  , getCurrentEBB
  ) where

import           Codec.Serialise (DeserialiseFailure)
import           Control.Exception (Exception (..))

import           GHC.Generics (Generic)
import           GHC.Stack (CallStack, prettyCallStack)

import           Cardano.Prelude (NoUnexpectedThunks (..),
                     noUnexpectedThunksUsingNormalForm)

import           Ouroboros.Network.Block (SlotNo (..))

import           Ouroboros.Storage.Common
import           Ouroboros.Storage.FS.API.Types (FsError, FsPath, prettyFsError,
                     sameFsError)
import           Ouroboros.Storage.ImmutableDB.Layout

type ImmTip = Tip (Either EpochNo SlotNo)

-- | Variant of 'Tip' that uses 'EpochSlot' instead of 'EpochNo' or 'SlotNo'.
data TipEpochSlot
  = TipEpochSlotGenesis
  | TipEpochSlot EpochSlot
  deriving (Eq, Show)

instance Ord TipEpochSlot where
  compare te1 te2 = case (te1, te2) of
    (TipEpochSlotGenesis, TipEpochSlotGenesis) -> EQ
    (TipEpochSlotGenesis, _)                   -> LT
    (_,                   TipEpochSlotGenesis) -> GT
    (TipEpochSlot es1,    TipEpochSlot es2)    -> compare es1 es2


-- | Truncate the database to some 'Tip'. This means that everything in the
-- database that comes after the 'Tip' will be removed, excluding the EBB or
-- block the 'Tip' points to.
newtype TruncateTo = TruncateTo { getTruncateTo :: ImmTip }
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
-- be accompanied with other data of type @t@. Note that we are not using
-- @Either e ..@ because the error @e@ might occur after some valid slots have
-- been parsed successfully, in which case we still want these valid slots,
-- but also want to know about the error so we can truncate the file to get
-- rid of the unparseable data.
newtype EpochFileParser e hash m t = EpochFileParser
  { runEpochFileParser :: FsPath -> m ([(SlotOffset, t)], CurrentEBB hash, Maybe e) }
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

-- | ID of an iterator that is not derived from another iterator
newtype BaseIteratorID = MkBaseIteratorID Int
  deriving (Show, Eq, Ord, Generic, Enum, NoUnexpectedThunks)

-- | A unique identifier for an iterator.
data IteratorID = BaseIteratorID BaseIteratorID | DerivedIteratorID IteratorID
  deriving (Show, Eq, Ord, Generic)

-- | Initial identifier number, use 'succ' to generate the next one.
initialIteratorID :: BaseIteratorID
initialIteratorID = MkBaseIteratorID 0

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
  = AppendToSlotInThePastError SlotNo ImmTip
    -- ^ When trying to append a new binary blob, the input slot was in the
    -- past, i.e. before or equal to the tip of the database.
  | AppendToEBBInThePastError EpochNo EpochNo
    -- ^ When trying to append a new EBB, the input epoch was in the past,
    -- i.e. less than the current epoch or blobs have already been appended to
    -- the current epoch.
    --
    -- The first parameter is the input epoch and the second parameter is the
    -- current epoch.
  | ReadFutureSlotError SlotNo ImmTip
    -- ^ When trying to read a slot, the slot was not yet occupied, because
    -- it's too far in the future, i.e. it is after the tip of the database.
  | ReadFutureEBBError EpochNo EpochNo
    -- ^ When trying to read an EBB, the requested epoch was in the future.
    --
    -- The first parameter is the requested epoch and the second parameter is
    -- the current epoch.
  | InvalidIteratorRangeError SlotNo SlotNo
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

data TraceEvent e
    = NoValidLastLocation
    | ValidatedLastLocation EpochNo ImmTip
      -- Validation of previous DB
    | ValidatingEpoch EpochNo
    | ValidatingEpochMissing EpochNo
    | ValidatingEpochErrorParsing e
    | ReconstructIndexLastSlotMissing
    | ValidatingEpochIndexIncomplete EpochNo
      -- Delete after
    | DeletingAfter TipEpochSlot TipEpochSlot
      -- Closing the DB
    | DBAlreadyClosed
    | DBClosed
  deriving (Eq, Generic, Show)

{-------------------------------------------------------------------------------
  Reference to EBB in current epoch
-------------------------------------------------------------------------------}

-- | Hash of the EBB in the current epoch
data CurrentEBB hash =
    NoCurrentEBB
  | CurrentEBB !hash
  deriving (Eq, Show)

instance NoUnexpectedThunks (CurrentEBB hash) where
  showTypeOf _ = "CurrentEBB"
  whnfNoUnexpectedThunks = noUnexpectedThunksUsingNormalForm

hasCurrentEBB :: CurrentEBB hash -> Bool
hasCurrentEBB NoCurrentEBB   = False
hasCurrentEBB (CurrentEBB _) = True

getCurrentEBB :: CurrentEBB hash -> Maybe hash
getCurrentEBB NoCurrentEBB      = Nothing
getCurrentEBB (CurrentEBB hash) = Just hash
