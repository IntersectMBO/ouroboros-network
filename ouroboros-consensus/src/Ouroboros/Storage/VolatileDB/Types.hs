{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Ouroboros.Storage.VolatileDB.Types
    (
      module Ouroboros.Storage.VolatileDB.Types
    , module Ouroboros.Network.Block
    ) where

import           Control.Exception (Exception (..))
import           Data.Map (Map)
import           Data.Typeable
import           Data.Word (Word64)

import           Ouroboros.Network.Block
import           Ouroboros.Storage.Common
import           Ouroboros.Storage.FS.API.Types

type FileId = Int

-- For each file, we store the latest blockId, the number of blocks
-- and a Map for its contents.
type Index blockId = Map String (Maybe SlotNo, Int, Map Word64 (Word64, blockId))

-- For each blockId, we store the file we can find the block, the offset and its size
-- in bytes.
type ReverseIndex blockId = Map blockId (String, Word64, Word64)

-- | Errors which might arise when working with this database.
data VolatileDBError blockId =
      FileSystemError FsError
    | VParserError (ParserError blockId)
    | InvalidArgumentsError String
    | ClosedDBError
    deriving (Show)

instance Eq blockId => Eq (VolatileDBError blockId) where
    (==) = sameVolatileDBError

instance (Show blockId, Typeable blockId) => Exception (VolatileDBError blockId) where
    displayException = show

data ParserError blockId =
      DuplicatedSlot (Map blockId ([String], [String]))
    | SlotsPerFileError String
    | InvalidFilename String
    | DecodeFailed String Word64
    deriving (Show)

instance Eq blockId => Eq (ParserError blockId) where
    (==) = sameParseError

sameVolatileDBError :: Eq blockId => VolatileDBError blockId -> VolatileDBError blockId -> Bool
sameVolatileDBError e1 e2 = case (e1, e2) of
    (FileSystemError fs1, FileSystemError fs2)         -> sameFsError fs1 fs2
    (VParserError p1, VParserError p2)                 -> p1 == p2
    (ClosedDBError, ClosedDBError)                     -> True
    (InvalidArgumentsError _, InvalidArgumentsError _) -> True
    _                                                  -> False

-- TODO: Why is this not comparing the arguments to 'DuplicatedSlot'?
sameParseError :: ParserError blockId -> ParserError blockId -> Bool
sameParseError e1 e2 = case (e1, e2) of
    (DuplicatedSlot _, DuplicatedSlot _)         -> True
    (SlotsPerFileError _, SlotsPerFileError _)   -> True
    (InvalidFilename str1, InvalidFilename str2) -> str1 == str2
    (DecodeFailed _ _ , DecodeFailed _ _)        -> True
    _                                            -> False

type FileSize  = Word64
type BlockSize = Word64

newtype Parser e m blockId = Parser {
    -- | Parse block storage at the given path
    parse :: FsPath -> m ([(SlotOffset, (BlockSize, blockId))], Maybe e)
    }
