{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
module Ouroboros.Consensus.Storage.ImmutableDB.Impl.Types (
    -- * Misc types
    BlockOrEBB (..)
  , WithBlockSize (..)
  , isBlockOrEBB
    -- * Validation policy
  , ValidationPolicy (..)
    -- * Chunk file error
  , ChunkFileError (..)
    -- * Tracing
  , TraceCacheEvent (..)
  , TraceEvent (..)
  ) where

import           Data.Text (Text)
import           Data.Word
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Util.CBOR (ReadIncrementalErr)

import           Ouroboros.Consensus.Storage.ImmutableDB.API (Tip)

-- Importing from Internal to avoid circular dependency
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal
                     (ChunkNo)

{------------------------------------------------------------------------------
  Misc types
------------------------------------------------------------------------------}

data BlockOrEBB =
    Block !SlotNo
  | EBB   !EpochNo
  deriving (Eq, Show, Generic, NoThunks)

isBlockOrEBB :: BlockOrEBB -> IsEBB
isBlockOrEBB (Block _) = IsNotEBB
isBlockOrEBB (EBB   _) = IsEBB

data WithBlockSize a = WithBlockSize {
      blockSize        :: !Word32
    , withoutBlockSize :: !a
    }
  deriving (Eq, Show, Generic, NoThunks, Functor, Foldable, Traversable)

{------------------------------------------------------------------------------
  Validation policy
------------------------------------------------------------------------------}

-- | The validation policy used when opening an
-- 'Ouroboros.Consensus.Storage.ImmutableDB.API.ImmutableDB'.
--
-- The validation policy is used by
-- 'Ouroboros.Consensus.Storage.ImmutableDB.Impl.openDB': the initial opening of
-- the database, either an empty database or a database that was previously
-- closed.
--
-- The recovery policy dictates which on-disk files should be validated.
data ValidationPolicy =
    ValidateMostRecentChunk
    -- ^ The chunk and index files of the most recent chunk stored on disk will
    -- be validated.
    --
    -- Prior chunk and index files are ignored, even their presence will not
    -- be checked.
    --
    -- A 'MissingFileError' or an 'InvalidFileError' will be thrown in case of a
    -- missing or invalid chunk file, or an invalid index file.
    --
    -- Because not all files are validated, subsequent operations on the
    -- database after opening may result in unexpected errors.
  | ValidateAllChunks
    -- ^ The chunk and index files of all chunks starting from the first one up
    -- to the last chunk stored on disk will be validated.
    --
    -- A 'MissingFileError' or an 'InvalidFileError' will be thrown in case of a
    -- missing or invalid chunk file, or an invalid index file.
  deriving (Show, Eq, Generic)

{------------------------------------------------------------------------------
  Chunk file error
------------------------------------------------------------------------------}

-- | Defined here instead of in the @Parser@ module because 'TraceEvent'
-- depends on it.
data ChunkFileError blk =
    -- | A block could not be decoded
    ChunkErrRead ReadIncrementalErr

    -- | The previous hash of a block did not match the hash of the previous
    -- block.
  | ChunkErrHashMismatch
      (HeaderHash blk)  -- ^ The hash of the previous block
      (ChainHash blk)   -- ^ The previous hash of the block

    -- | The integrity verification of the block with the given point returned
    -- 'False', indicating that the block got corrupted.
  | ChunkErrCorrupt (Point blk)
  deriving (Eq, Show)

{------------------------------------------------------------------------------
  Tracing
------------------------------------------------------------------------------}

data TraceEvent blk =
    NoValidLastLocation
  | ValidatedLastLocation ChunkNo (Tip blk)
    -- Validation of previous DB
  | ValidatingChunk  ChunkNo
  | MissingChunkFile ChunkNo
  | InvalidChunkFile ChunkNo (ChunkFileError blk)
  | ChunkFileDoesntFit (ChainHash blk) (ChainHash blk)
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
  | DeletingAfter (WithOrigin (Tip blk))
    -- Closing the DB
  | DBAlreadyClosed
  | DBClosed
    -- Events traced by the index cache
  | TraceCacheEvent !TraceCacheEvent
  deriving (Eq, Generic, Show)

-- | The argument with type 'Word32' is the number of past chunk currently in
-- the cache.
data TraceCacheEvent =
    TraceCurrentChunkHit   ChunkNo   Word32
  | TracePastChunkHit      ChunkNo   Word32
  | TracePastChunkMiss     ChunkNo   Word32
  | TracePastChunkEvict    ChunkNo   Word32
    -- ^ The least recently used past chunk was evicted because the cache
    -- was full.
  | TracePastChunksExpired [ChunkNo] Word32
    -- ^ Past chunks were expired from the cache because they haven't been
    -- used for a while.
  deriving (Eq, Generic, Show)
