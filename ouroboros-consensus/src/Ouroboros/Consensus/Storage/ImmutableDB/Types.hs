{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RankNTypes        #-}
module Ouroboros.Consensus.Storage.ImmutableDB.Types
  ( SlotNo (..)
  , ImmTip
  , ImmTipWithHash
  , WithHash (..)
  , WithBlockSize (..)
  , ImmTipWithInfo
  , fromTipInfo
  , TipInfo (..)
  , BlockOrEBB (..)
  , isBlockOrEBB
  , HashInfo (..)
  , BinaryInfo (..)
  , ChunkFileParser (..)
  , ValidationPolicy (..)
  , WrongBoundError (..)
  , ImmutableDBError (..)
  , sameImmutableDBError
  , prettyImmutableDBError
  , UserError (..)
  , prettyUserError
  , UnexpectedError (..)
  , sameUnexpectedError
  , prettyUnexpectedError
  , TraceEvent(..)
  , TraceCacheEvent(..)
    -- * Current EBB
  , CurrentEBB(..)
  , hasCurrentEBB
  , getCurrentEBB
  ) where

import           Control.Exception (Exception (..))
import           Data.Binary (Get, Put)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Text (Text)
import           Data.Word
import           GHC.Generics (Generic)
import           GHC.Stack (CallStack, prettyCallStack)
import           Streaming (Of, Stream)

import           Cardano.Prelude (NoUnexpectedThunks (..),
                     UseIsNormalFormNamed (..))
import           Cardano.Slotting.Block
import           Cardano.Slotting.Slot

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Storage.Common
import           Ouroboros.Consensus.Storage.FS.API.Types (FsError, FsPath,
                     prettyFsError, sameFsError)
import           Ouroboros.Consensus.Storage.FS.CRC (CRC)

-- Importing from Internal to avoid circular dependency
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal
                     (ChunkNo)

data BlockOrEBB
  = Block !SlotNo
  | EBB   !EpochNo
  deriving (Eq, Show, Generic, NoUnexpectedThunks)

isBlockOrEBB :: BlockOrEBB -> IsEBB
isBlockOrEBB (Block _) = IsNotEBB
isBlockOrEBB (EBB   _) = IsEBB

type ImmTip = WithOrigin BlockOrEBB

-- TODO let this replace 'ImmTip' and integrate @hash@ in 'BlockOrEBB'?
type ImmTipWithHash hash = WithOrigin (WithHash hash BlockOrEBB)

data WithHash hash a = WithHash
  { theHash    :: !hash
  , forgetHash :: !a
  } deriving (Eq, Show, Generic, NoUnexpectedThunks, Functor, Foldable, Traversable)

type ImmTipWithInfo hash = WithOrigin (TipInfo hash BlockOrEBB)

data TipInfo hash a = TipInfo
  { tipInfoHash    :: !hash
  , forgetTipInfo  :: !a
  , tipInfoBlockNo :: !BlockNo
  } deriving (Eq, Show, Generic, NoUnexpectedThunks, Functor, Foldable, Traversable)

fromTipInfo :: TipInfo hash a -> WithHash hash a
fromTipInfo (TipInfo hash a _blockNo) = WithHash hash a

data WithBlockSize a = WithBlockSize
  { blockSize        :: !Word32
  , withoutBlockSize :: !a
  } deriving (Eq, Show, Generic, NoUnexpectedThunks, Functor, Foldable, Traversable)

-- | How to get/put the header hash of a block and how many bytes it occupies
-- on-disk.
data HashInfo hash = HashInfo
  { hashSize :: !Word32
    -- ^ A fixed size
  , getHash  :: !(Get hash)
  , putHash  :: !(hash -> Put)
  }

-- | Parse the contents of an chunk file.
--
-- The parsing may include validation of the contents of the chunk file.
--
-- @entry@ will be instantiated with @(Secondary.Entry hash)@, i.e. an entry
-- from the secondary index corresponding to a block. To avoid cyclic
-- dependencies, it is left abstract here.
--
-- The parser should validate that each entry fits onto the previous one, i.e.
-- that the hashes line up.
--
-- We assume the output of 'ChunkFileParser' to be correct, we will not
-- validate it.
--
-- An error may be returned in the form of @'Maybe' (e, 'Word64')@. The
-- 'Word64' must correspond to the offset in the file where the last valid
-- entry ends. Truncating to this offset should remove all invalid data from
-- the file and just leave the valid entries before it. Note that we are not
-- using @Either e ..@ because the error @e@ might occur after some valid
-- entries have been parsed successfully, in which case we still want these
-- valid entries, but also want to know about the error so we can truncate the
-- file to get rid of the unparseable data.
newtype ChunkFileParser e m entry hash = ChunkFileParser
  { runChunkFileParser
      :: forall r.
         FsPath
      -> [CRC]
         -- The expected checksums are given as input. This list can be empty
         -- when the secondary index file is missing. If the expected checksum
         -- matches the actual checksum, we can avoid the expensive integrity
         -- check of the block.
      -> (Stream (Of (entry, WithOrigin hash)) m (Maybe (e, Word64)) -> m r)
         -- Continuation to ensure the file is closed
      -> m r
  }

-- | The validation policy used when (re)opening an
-- 'Ouroboros.Consensus.Storage.ImmutableDB.API.ImmutableDB'.
--
-- The validation policy is used by:
--
-- * 'Ouroboros.Consensus.Storage.ImmutableDB.Impl.openDB': the initial opening
--   of the database, either an empty database or a database that was previously
--   closed.
--
-- * 'Ouroboros.Consensus.Storage.ImmutableDB.API.reopen': when the database was
--   closed in case of an unexpected error, the user can reopen the database and
--   keep on using the same handle to it.
--
-- We will refer to both these operations using \"/open/\".
--
-- The recovery policy dictates which on-disk files /open/ should validate.
data ValidationPolicy
  = ValidateMostRecentChunk
    -- ^ /open/ will validate the chunk and index files of the most recent
    -- chunk stored on disk.
    --
    -- Prior chunk and index files are ignored, even their presence will not
    -- be checked.
    --
    -- /open/ will throw a 'MissingFileError' or an 'InvalidFileError' in case
    -- of a missing or invalid chunk file, or an invalid index file.
    --
    -- Because not all files are validated, subsequent operations on the
    -- database after /open/ may result in unexpected errors.
  | ValidateAllChunks
    -- ^ /open/ will validate the chunk and index files of all chunks starting
    -- from the first one up to the last chunk stored on disk.
    --
    -- /open/ will throw a 'MissingFileError' or an 'InvalidFileError' in case
    -- of a missing or invalid chunk file, or an invalid index file.
  deriving (Show, Eq, Generic)

-- | Returned by 'streamBlocks' and 'streamHeaders' when a bound is wrong.
--
-- NOTE: this is not a 'UserError' that is thrown as an exception, but an
-- admissible error, returned in an 'Either'. This is because we expect wrong
-- bounds to be passed in practice. The functions to stream the blocks are
-- best placed to validate the bounds, instead of requiring the users of the
-- functions to do the validation beforehand.
data WrongBoundError hash
  = EmptySlotError SlotNo
    -- ^ There is no block in the given slot.
  | WrongHashError SlotNo hash (NonEmpty hash)
    -- ^ The block and/or EBB in the given slot have a different hash.
    --
    -- The first @hash@ is the one given as bound. In the list of @hash@es,
    -- the EBB's hash will come first and the regular block's hash will come
    -- second.
  deriving (Eq, Show, Generic)

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
  | AppendToEBBInThePastError EpochNo ChunkNo
    -- ^ When trying to append a new EBB, the input epoch was in the past,
    -- i.e. less than the current epoch or blobs have already been appended to
    -- the current epoch.
    --
    -- The first parameter is the input epoch and the second parameter is the
    -- current chunk.
  | ReadFutureSlotError SlotNo ImmTip
    -- ^ When trying to read a slot, the slot was not yet occupied, because
    -- it's too far in the future, i.e. it is after the tip of the database.
  | ReadFutureEBBError EpochNo ChunkNo
    -- ^ When trying to read an EBB, the requested epoch was in the future.
    --
    -- The first parameter is the requested epoch and the second parameter is
    -- the current chunk.
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


-- | Pretty-print a 'Ouroboros.Consensus.Storage.ImmutableDB.Types.UserError',
-- including its callstack.
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
  | InvalidFileError FsPath String CallStack
    -- ^ When loading an epoch or index file, its contents did not pass
    -- validation.
  | MissingFileError FsPath CallStack
    -- ^ A missing epoch or index file.
  | ChecksumMismatchError BlockOrEBB CRC CRC FsPath CallStack
    -- ^ There was a checksum mismatch when reading the block at the given
    -- 'BlockOrEBB'. The first 'CRC' is the expected one, the second one the
    -- actual one.
  deriving (Show, Generic)

-- | Check if two
-- 'Ouroboros.Consensus.Storage.ImmutableDB.Types.UnexpectedError's are equal
-- while ignoring their 'CallStack's.
sameUnexpectedError :: UnexpectedError -> UnexpectedError -> Bool
sameUnexpectedError ue1 ue2 = case (ue1, ue2) of
    (FileSystemError fse1,     FileSystemError fse2)     -> sameFsError fse1 fse2
    (FileSystemError {},       _)                        -> False
    (InvalidFileError p1 m1 _, InvalidFileError p2 m2 _) -> p1 == p2 && m1 == m2
    (InvalidFileError {},      _)                        -> False
    (MissingFileError p1 _,    MissingFileError p2 _)    -> p1 == p2
    (MissingFileError {},      _)                        -> False
    (ChecksumMismatchError s1 e1 a1 p1 _,
     ChecksumMismatchError s2 e2 a2 p2 _)                -> s1 == s2 && e1 == e2
                                                         && a1 == a2 && p1 == p2
    (ChecksumMismatchError {}, _)                        -> False

-- | Pretty-print an
-- 'Ouroboros.Consensus.Storage.ImmutableDB.Types.UnexpectedError', including
-- its callstack.
prettyUnexpectedError :: UnexpectedError -> String
prettyUnexpectedError = \case
    FileSystemError fse -> prettyFsError fse
    InvalidFileError path msg cs ->
      "InvalidFileError (" <> show path <> "): " <>
      msg <> " " <>
      prettyCallStack cs
    MissingFileError path cs ->
      "MissingFileError (" <> show path <> "): " <>
      prettyCallStack cs
    ChecksumMismatchError chunkSlot expected actual path cs ->
      "ChecksumMismatchError (" <> show path <> "): for chunk slot " <>
      show chunkSlot <> " expected " <> show expected <>
      " but got " <> show actual <>
      prettyCallStack cs


{------------------------------------------------------------------------------
  Tracing
------------------------------------------------------------------------------}

data TraceEvent e hash
    = NoValidLastLocation
    | ValidatedLastLocation ChunkNo ImmTip
      -- Validation of previous DB
    | ValidatingChunk  ChunkNo
    | MissingChunkFile ChunkNo
    | InvalidChunkFile ChunkNo e
    | ChunkFileDoesntFit (WithOrigin hash) (WithOrigin hash)
      -- ^ The hash of the last block in the previous epoch doesn't match the
      -- previous hash of the first block in the current epoch
    | MissingPrimaryIndex   ChunkNo
    | MissingSecondaryIndex ChunkNo
    | InvalidPrimaryIndex   ChunkNo
    | InvalidSecondaryIndex ChunkNo
    | RewritePrimaryIndex   ChunkNo
    | RewriteSecondaryIndex ChunkNo
    | Migrating Text
      -- ^ Performing a migration of the on-disk files

      -- Delete after
    | DeletingAfter (ImmTipWithInfo hash)
      -- Closing the DB
    | DBAlreadyClosed
    | DBClosed
      -- Events traced by the index cache
    | TraceCacheEvent !TraceCacheEvent
  deriving (Eq, Generic, Show)

-- | The argument with type 'Word32' is the number of past chunk currently in
-- the cache.
data TraceCacheEvent
    = TraceCurrentChunkHit   !ChunkNo   !Word32
    | TracePastChunkHit      !ChunkNo   !Word32
    | TracePastChunkMiss     !ChunkNo   !Word32
    | TracePastChunkEvict    !ChunkNo   !Word32
      -- ^ The least recently used past chunk was evicted because the cache
      -- was full.
    | TracePastChunksExpired ![ChunkNo] !Word32
      -- ^ Past chunks were expired from the cache because they haven't been
      -- used for a while.
  deriving (Eq, Generic, Show)

{-------------------------------------------------------------------------------
  Reference to EBB in current chunk
-------------------------------------------------------------------------------}

-- | Hash of the EBB in the current chunk
data CurrentEBB hash =
    NoCurrentEBB
  | CurrentEBB !hash
  deriving (Eq, Show)
  deriving NoUnexpectedThunks via UseIsNormalFormNamed "CurrentEBB" (CurrentEBB hash)

hasCurrentEBB :: CurrentEBB hash -> Bool
hasCurrentEBB NoCurrentEBB   = False
hasCurrentEBB (CurrentEBB _) = True

getCurrentEBB :: CurrentEBB hash -> Maybe hash
getCurrentEBB NoCurrentEBB      = Nothing
getCurrentEBB (CurrentEBB hash) = Just hash
