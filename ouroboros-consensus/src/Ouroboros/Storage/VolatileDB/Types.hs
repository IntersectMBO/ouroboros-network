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
import           Data.Int (Int64)
import           Data.Map (Map)
import           Data.Typeable

import           Ouroboros.Network.Block
import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling (..))

type FileId = Int

-- For each file, we store the latest blockId, the number of blocks
-- and a Map for its contents.
type Index blockId = Map String (Maybe SlotNo, Int, Map Int64 (Int, blockId))

-- For each blockId, we store the file we can find the block, the offset and its size
-- in bytes.
type ReverseIndex blockId = Map blockId (String, Int64, Int)

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
    | SlotsPerFileError Int String String
    | InvalidFilename String
    | DecodeFailed (Int64, Map Int64 (Int, blockId)) String Int
    deriving (Show)

instance Eq blockId => Eq (ParserError blockId) where
    (==) = sameParseError

sameVolatileDBError :: Eq blockId => VolatileDBError blockId -> VolatileDBError blockId -> Bool
sameVolatileDBError e1 e2 = case (e1, e2) of
    (FileSystemError fs1, FileSystemError fs2) -> sameFsError fs1 fs2
    (VParserError p1, VParserError p2) -> p1 == p2
    (ClosedDBError, ClosedDBError) -> True
    (InvalidArgumentsError _, InvalidArgumentsError _) -> True
    _ -> False

-- TODO: Why is this not comparing the arguments to 'DuplicatedSlot'?
sameParseError :: ParserError blockId -> ParserError blockId -> Bool
sameParseError e1 e2 = case (e1, e2) of
    (DuplicatedSlot _, DuplicatedSlot _)               -> True
    (SlotsPerFileError _ _ _, SlotsPerFileError _ _ _) -> True
    (InvalidFilename str1, InvalidFilename str2)       -> str1 == str2
    (DecodeFailed _ _, DecodeFailed _ _)               -> True
    _                                                  -> False

-- TODO(kde) unify/move/replace
newtype Parser m blockId = Parser {
    parse       :: forall h.
                   HasFS m h
                -> ErrorHandling (VolatileDBError blockId) m
                -> [String]
                -> m (Int64, Map Int64 (Int, blockId))
    }
