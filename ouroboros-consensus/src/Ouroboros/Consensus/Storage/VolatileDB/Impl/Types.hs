{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
module Ouroboros.Consensus.Storage.VolatileDB.Impl.Types (
    -- * Blocks per file
    mkBlocksPerFile
  , unBlocksPerFile
    -- ** opaque
  , BlocksPerFile
    -- * Block validation policy
  , BlockValidationPolicy (..)
    -- * Parse error
  , ParseError (..)
    -- * Tracing
  , TraceEvent (..)
    -- * Internal indices
  , BlockOffset (..)
  , BlockSize (..)
  , FileId
  , InternalBlockInfo (..)
  , ReverseIndex
  , SuccessorsIndex
  ) where


import           Data.Map.Strict (Map)
import           Data.Set (Set)
import           Data.Word (Word32, Word64)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Util.CBOR (ReadIncrementalErr (..))

import           Ouroboros.Consensus.Storage.FS.API.Types (FsPath)
import           Ouroboros.Consensus.Storage.VolatileDB.API (BlockInfo)

{------------------------------------------------------------------------------
  Blocks per file
------------------------------------------------------------------------------}

-- | The maximum number of blocks to store per file.
newtype BlocksPerFile = BlocksPerFile { unBlocksPerFile :: Word32 }
    deriving (Generic, Show)

-- | Create a 'BlocksPerFile'.
--
-- PRECONDITION: the given number must be greater than 0, if not, this
-- function will throw an 'error'.
mkBlocksPerFile :: Word32 -> BlocksPerFile
mkBlocksPerFile 0 = error "BlocksPerFile must be positive"
mkBlocksPerFile n = BlocksPerFile n

{------------------------------------------------------------------------------
  Block validation policy
------------------------------------------------------------------------------}

-- | When block validation is enabled, the parser checks for each block a
-- number of properties and stops parsing if it finds any invalid blocks.
data BlockValidationPolicy =
    NoValidation
  | ValidateAll
  deriving (Eq)

{------------------------------------------------------------------------------
  Parse error
------------------------------------------------------------------------------}

-- | Note that we recover from the error, and thus never throw it as an
-- 'Exception'.
--
-- Defined here instead of in the @Parser@ module because 'TraceEvent' depends
-- on it.
data ParseError blk =
    BlockReadErr ReadIncrementalErr
    -- ^ A block could not be parsed.
  | BlockCorruptedErr (HeaderHash blk)
    -- ^ A block was corrupted, e.g., checking its signature and/or hash
    -- failed.
  | DuplicatedBlock (HeaderHash blk) FsPath FsPath
    -- ^ A block with the same hash occurred twice in the VolatileDB files.
    --
    -- We include the file in which it occurred first and the file in which it
    -- occured the second time. The two files can be the same.

deriving instance StandardHash blk => Eq   (ParseError blk)
deriving instance StandardHash blk => Show (ParseError blk)

{------------------------------------------------------------------------------
  Tracing
------------------------------------------------------------------------------}

data TraceEvent blk
    = DBAlreadyClosed
    | BlockAlreadyHere (HeaderHash blk)
    | Truncate (ParseError blk) FsPath BlockOffset
    | InvalidFileNames [FsPath]
  deriving (Eq, Generic, Show)

{------------------------------------------------------------------------------
  Internal indices
------------------------------------------------------------------------------}

-- | The 'FileId' is the unique identifier of each file found in the db.
-- For example, the file @blocks-42.dat@ has 'FileId' @42@.
type FileId = Int

-- | We map the header hash of each block to the 'InternalBlockInfo'.
type ReverseIndex blk = Map (HeaderHash blk) (InternalBlockInfo blk)

-- | For each block, we store the set of all blocks which have this block as
-- a predecessor (set of successors).
type SuccessorsIndex blk = Map (ChainHash blk) (Set (HeaderHash blk))

newtype BlockSize = BlockSize { unBlockSize :: Word32 }
  deriving (Eq, Show, Generic, NoThunks)

-- | The offset at which a block is stored in a file.
newtype BlockOffset = BlockOffset { unBlockOffset :: Word64 }
  deriving (Eq, Show, Generic, NoThunks)

-- | The internal information the db keeps for each block.
data InternalBlockInfo blk = InternalBlockInfo {
      ibiFile        :: !FsPath
    , ibiBlockOffset :: !BlockOffset
    , ibiBlockSize   :: !BlockSize
    , ibiBlockInfo   :: !(BlockInfo blk)
    , ibiNestedCtxt  :: !(SomeSecond (NestedCtxt Header) blk)
    }
  deriving (Generic, NoThunks)
