{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}

module Ouroboros.Storage.VolatileDB.Types
    (
      module Ouroboros.Storage.VolatileDB.Types
    , module Ouroboros.Network.Block
    ) where

import           Control.Exception (Exception (..))
import           Data.Map.Strict (Map)
import           Data.Set (Set)
import           Data.Typeable
import           Data.Word (Word64)
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks, first)

import           Ouroboros.Network.Block hiding (Tip, decodeTip, encodeTip)
import           Ouroboros.Network.Point (WithOrigin)
import           Ouroboros.Storage.FS.API.Types

-- | The 'FileId' is the unique identifier of each file found in the db.
-- For example, the file @blocks-42.dat@ has 'FileId' @42@.
type FileId = Int

-- | For each @blockId@, we store the file in which we can find the block,
-- the offset, its size in bytes and its predecessor.
type ReverseIndex blockId = Map blockId (InternalBlockInfo blockId)

-- | For each block, we store the set of all blocks which have this block as
-- a predecessor (set of successors).
type SuccessorsIndex blockId = Map (WithOrigin blockId) (Set blockId)

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
      InvalidArgumentsError String
    | ClosedDBError
    deriving (Show, Eq)

data UnexpectedError =
      FileSystemError FsError
    | ParserError ParserError
    deriving (Show)

instance Eq VolatileDBError where
    (==) = sameVolatileDBError

instance Exception VolatileDBError where
    displayException = show

data ParserError =
      forall blockId. (Typeable blockId, Eq blockId, Show blockId) =>
        DuplicatedSlot blockId FsPath FsPath
    | InvalidFilename FsPath

deriving instance Show ParserError

instance Eq ParserError where
    (==) = sameParseError

sameVolatileDBError :: VolatileDBError
                    -> VolatileDBError
                    -> Bool
sameVolatileDBError e1 e2 = case (e1, e2) of
    (UserError ue1, UserError ue2)             -> ue1 == ue2
    (UnexpectedError ue1, UnexpectedError ue2) -> sameUnexpectedError ue1 ue2
    _                                          -> False

sameUnexpectedError :: UnexpectedError
                    -> UnexpectedError
                    -> Bool
sameUnexpectedError e1 e2 = case (e1, e2) of
    (FileSystemError fs1, FileSystemError fs2) -> sameFsError fs1 fs2
    (ParserError p1, ParserError p2)           -> p1 == p2
    _                                          -> False

-- | This is not comparing the arguments of 'DuplicatedSlot', because it's not
-- deterministic which duplication we find. In other words, it's possible that
-- there are multiple pairs of duplicate blocks and our algorithm does not
-- guarantee we always find the same.
sameParseError :: ParserError -> ParserError -> Bool
sameParseError e1 e2 = case (e1, e2) of
    (DuplicatedSlot {}, DuplicatedSlot {})       -> True
    (InvalidFilename str1, InvalidFilename str2) -> str1 == str2
    _                                            -> False

newtype FileSize  = FileSize {unFileSize :: Word64}
    deriving (Show, Generic, NoUnexpectedThunks)
newtype BlockSize = BlockSize {unBlockSize :: Word64}
    deriving (Show, Generic, NoUnexpectedThunks)

newtype Parser e m blockId = Parser {
    -- | Parse block storage at the given path.
    parse :: FsPath -> m ([(Word64, (Word64, BlockInfo blockId))], Maybe e)
    }

-- | Simply wraps around the types of the parser.
getParsedInfo :: Functor m
              => Parser e m blockId
              -> FsPath
              -> m (ParsedInfo blockId, Maybe e)
getParsedInfo (Parser parser) path =
    first toParsedInfo <$> parser path
  where
    toParsedInfo :: [(Word64, (Word64, BlockInfo blockId))]
                 -> ParsedInfo blockId
    toParsedInfo = fmap $ \(o, (s, a)) -> (o, (BlockSize s, a))

-- | The offset of a slot in a file.
type SlotOffset = Word64

-- | Information returned by the parser about a single file
--
-- The parser returns for each block, its size its blockId, its slot and its
-- predecessor's blockId.
type ParsedInfo blockId = [(SlotOffset, (BlockSize, BlockInfo blockId))]

-- | The information that the user has to provide for each new block.
data BlockInfo blockId = BlockInfo {
      bbid    :: !blockId
    , bslot   :: !SlotNo
    , bpreBid :: !(WithOrigin blockId)
    } deriving (Show, Generic, NoUnexpectedThunks)

-- | The internal information the db keeps for each block.
data InternalBlockInfo blockId = InternalBlockInfo {
      ibFile       :: !FsPath
    , ibSlotOffset :: !SlotOffset
    , ibBlockSize  :: !BlockSize
    , ibSlot       :: !SlotNo
    , ibPreBid     :: !(WithOrigin blockId)
    } deriving (Show, Generic, NoUnexpectedThunks)
