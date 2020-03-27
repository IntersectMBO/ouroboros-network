{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Consensus.Storage.VolatileDB.Types
  ( BlocksPerFile -- opaque
  , unBlocksPerFile
  , mkBlocksPerFile
  , FileId
  , ReverseIndex
  , SuccessorsIndex
  , BlockValidationPolicy (..)
  , VolatileDBError (..)
  , UserError (..)
  , UnexpectedError (..)
  , ParserError (..)
  , sameVolatileDBError
  , sameUnexpectedError
  , BlockSize (..)
  , BlockOffset
  , Parser (..)
  , ParsedInfo
  , ParsedBlockInfo
  , BlockInfo (..)
  , InternalBlockInfo (..)
    -- * Tracing
  , TraceEvent (..)
  ) where

import           Control.Exception (Exception (..), SomeException)
import           Data.Map.Strict (Map)
import           Data.Set (Set)
import           Data.Word (Word16, Word32, Word64)
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.Block hiding (Tip, decodeTip, encodeTip)
import           Ouroboros.Network.Point (WithOrigin)

import           Ouroboros.Consensus.Block (IsEBB)

import           Ouroboros.Consensus.Storage.FS.API.Types

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

-- | The 'FileId' is the unique identifier of each file found in the db.
-- For example, the file @blocks-42.dat@ has 'FileId' @42@.
type FileId = Int

-- | For each @blockId@, we store the file in which we can find the block,
-- the offset, its size in bytes and its predecessor.
type ReverseIndex blockId = Map blockId (InternalBlockInfo blockId)

-- | For each block, we store the set of all blocks which have this block as
-- a predecessor (set of successors).
type SuccessorsIndex blockId = Map (WithOrigin blockId) (Set blockId)

-- | When block validation is enabled, the parser checks for each block a number
-- of properties and stops parsing if it finds any invalid blocks.
data BlockValidationPolicy =
      NoValidation
    | ValidateAll
    deriving Eq

-- | Errors which might arise when working with this database.
data VolatileDBError =
      UserError UserError
    -- ^ An error thrown because of incorrect usage of the VolatileDB
    -- by the user.
    | UnexpectedError UnexpectedError
    -- ^ An unexpected error thrown because something went wrong on a lower
    -- layer.
    deriving Show

data UserError =
      ClosedDBError (Maybe SomeException)
      -- ^ The VolatileDB was closed. In case it was automatically closed
      -- because an unexpected error was thrown during a read operation or any
      -- exception was thrown during a write operation, that exception is
      -- embedded.
    deriving (Show)

data UnexpectedError =
      FileSystemError FsError
    deriving (Show)

instance Eq VolatileDBError where
    (==) = sameVolatileDBError

instance Exception VolatileDBError where
    displayException = show

-- | Note that we recover from the error, and thus never throw it as an
-- 'Exception'.
data ParserError blockId e =
    BlockReadErr e
    -- ^ A block could not be parsed.
  | BlockCorruptedErr blockId
    -- ^ A block was corrupted, e.g., checking its signature and/or hash
    -- failed.
  | DuplicatedBlock blockId FsPath FsPath
    -- ^ A block with the same @blockId@ occurred twice in the VolatileDB
    -- files.
    --
    -- We include the file in which it occurred first and the file in which it
    -- occured the second time. The two files can be the same.
  deriving (Eq, Show)

sameVolatileDBError :: VolatileDBError
                    -> VolatileDBError
                    -> Bool
sameVolatileDBError e1 e2 = case (e1, e2) of
    (UserError ue1, UserError ue2)             -> sameUserError ue1 ue2
    (UnexpectedError ue1, UnexpectedError ue2) -> sameUnexpectedError ue1 ue2
    _                                          -> False

sameUserError :: UserError -> UserError -> Bool
sameUserError e1 e2 = case (e1, e2) of
    (ClosedDBError mbEx1, ClosedDBError mbEx2) -> (show <$> mbEx1) == (show <$> mbEx2)

sameUnexpectedError :: UnexpectedError
                    -> UnexpectedError
                    -> Bool
sameUnexpectedError e1 e2 = case (e1, e2) of
    (FileSystemError fs1, FileSystemError fs2) -> sameFsError fs1 fs2

newtype BlockSize = BlockSize {unBlockSize :: Word64}
    deriving (Show, Generic, NoUnexpectedThunks)

-- | The offset at which a block is stored in a file.
type BlockOffset = Word64

-- | Parse the given file containing blocks.
--
-- Return the 'ParsedBlockInfo' for all the valid blocks in the file. Stop
-- when encountering an error and include the offset to truncate to.
newtype Parser e m blockId = Parser {
    parse :: FsPath
          -> m ( ParsedInfo blockId
               , Maybe (ParserError blockId e, BlockOffset)
               )
  }

-- | Information returned by the parser about a single file.
type ParsedInfo blockId = [ParsedBlockInfo blockId]

-- | Information returned by the parser about a single block.
--
-- The parser returns for each block, its offset, its size and its 'BlockInfo'
type ParsedBlockInfo blockId = (BlockOffset, (BlockSize, BlockInfo blockId))

-- | The information that the user has to provide for each new block.
data BlockInfo blockId = BlockInfo {
      bbid          :: !blockId
    , bslot         :: !SlotNo
    , bpreBid       :: !(WithOrigin blockId)
    , bisEBB        :: !IsEBB
    , bheaderOffset :: !Word16
    , bheaderSize   :: !Word16
    } deriving (Eq, Show, Generic, NoUnexpectedThunks)

-- | The internal information the db keeps for each block.
data InternalBlockInfo blockId = InternalBlockInfo {
      ibFile        :: !FsPath
    , ibBlockOffset :: !BlockOffset
    , ibBlockSize   :: !BlockSize
    , ibBlockInfo   :: !(BlockInfo blockId)
    } deriving (Show, Generic, NoUnexpectedThunks)

{------------------------------------------------------------------------------
  Tracing
------------------------------------------------------------------------------}

data TraceEvent e blockId
    = DBAlreadyClosed
    | DBAlreadyOpen
    | BlockAlreadyHere blockId
    | TruncateCurrentFile FsPath
    | Truncate (ParserError blockId e) FsPath BlockOffset
    | InvalidFileNames [FsPath]
  deriving (Eq, Generic, Show)
