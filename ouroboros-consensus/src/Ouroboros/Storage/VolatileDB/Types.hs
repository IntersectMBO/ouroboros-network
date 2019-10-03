{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Ouroboros.Storage.VolatileDB.Types
    (
      module Ouroboros.Storage.VolatileDB.Types
    , module Ouroboros.Storage.Common
    , module Ouroboros.Network.Block
    ) where

import           Control.Exception (Exception (..))
import           Data.Map.Strict (Map)
import           Data.Set (Set)
import           Data.Typeable
import           Data.Word (Word64)
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.Point (WithOrigin)
import           Ouroboros.Network.Block hiding (Tip, decodeTip, encodeTip)
import           Ouroboros.Storage.Common
import           Ouroboros.Storage.FS.API.Types

type FileId = Int

-- For each blockId, we store the file we can find the block, the offset, its size
-- in bytes and its predecessor.
type ReverseIndex blockId = Map blockId (InternalBlockInfo blockId)

-- For each block, we store the Set of all blocks which have this block as
-- a predecessor (its successors).
type SuccessorsIndex blockId = Map (WithOrigin blockId) (Set blockId)

-- | Errors which might arise when working with this database.
data VolatileDBError blockId =
      UserError UserError
    -- ^ An error thrown because of incorrect usage of the volatile database
    -- by the user.
    | UnexpectedError (UnexpectedError blockId)
    -- ^ An unexpected error thrown because something went wrong on a lower
    -- layer.
    deriving Show

data UserError =
      InvalidArgumentsError String
    | ClosedDBError
    deriving (Show, Eq)

data UnexpectedError blockId =
      FileSystemError FsError
    | ParserError (ParserError blockId)
    deriving (Show)

instance Eq blockId => Eq (VolatileDBError blockId) where
    (==) = sameVolatileDBError

instance (Show blockId, Typeable blockId) => Exception (VolatileDBError blockId) where
    displayException = show

data ParserError blockId =
      DuplicatedSlot blockId FsPath FsPath
    | InvalidFilename FsPath
    deriving (Show)

instance Eq blockId => Eq (ParserError blockId) where
    (==) = sameParseError

sameVolatileDBError :: Eq blockId => VolatileDBError blockId -> VolatileDBError blockId -> Bool
sameVolatileDBError e1 e2 = case (e1, e2) of
    (UserError ue1, UserError ue2)             -> ue1 == ue2
    (UnexpectedError ue1, UnexpectedError ue2) -> sameUnexpectedError ue1 ue2
    _                                          -> False

-- TODO: Why is this not comparing the arguments to 'DuplicatedSlot'?
sameUnexpectedError :: Eq blockId => UnexpectedError blockId -> UnexpectedError blockId -> Bool
sameUnexpectedError e1 e2 = case (e1, e2) of
    (FileSystemError fs1, FileSystemError fs2) -> sameFsError fs1 fs2
    (ParserError p1, ParserError p2)           -> p1 == p2
    _                                          -> False

sameParseError :: ParserError blockId -> ParserError blockId -> Bool
sameParseError e1 e2 = case (e1, e2) of
    (DuplicatedSlot _ _ _, DuplicatedSlot _ _ _) -> True
    (InvalidFilename str1, InvalidFilename str2) -> str1 == str2
    _                                            -> False

type FileSize  = Word64  -- TODO: Should be newtype
type BlockSize = Word64  -- TODO: Should be newtype

newtype Parser e m blockId = Parser {
    -- | Parse block storage at the given path.
    parse :: FsPath -> m (ParsedInfo blockId, Maybe e)
    }

-- | Information returned by the parser about a single file
--
-- The parser returns for each block, its size its blockId, its slot and its
-- predecessor's blockId.
type ParsedInfo blockId = [(SlotOffset, (BlockSize, BlockInfo blockId))]

-- This is the information a user has to provide for each new block.
data BlockInfo blockId = BlockInfo {
      bbid    :: !blockId
    , bslot   :: !SlotNo
    , bpreBid :: !(WithOrigin blockId)
    } deriving (Show, Generic, NoUnexpectedThunks)

-- The Internal information the db keeps for each block.
data InternalBlockInfo blockId = InternalBlockInfo {
      ibFile       :: !FsPath
    , ibSlotOffset :: !SlotOffset
    , ibBlockSize  :: !BlockSize
    , ibSlot       :: !SlotNo
    , ibPreBid     :: !(WithOrigin blockId)
    } deriving (Show, Generic, NoUnexpectedThunks)
