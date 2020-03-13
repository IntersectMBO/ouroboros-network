{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Consensus.Storage.VolatileDB.Impl.Util
    ( -- * FileId utilities
      parseFd
    , parseAllFds
    , filePath
    , findLastFd

      -- * Exception handling
    , wrapFsError
    , tryVolDB

      -- * Map of Set utilities
    , insertMapSet
    , deleteMapSet
    ) where

import           Control.Monad
import           Data.Bifunctor (first)
import           Data.List (sortOn)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Read (readMaybe)

import           Ouroboros.Network.Point (WithOrigin)

import           Ouroboros.Consensus.Util (lastMaybe)

import           Ouroboros.Consensus.Storage.FS.API.Types
import           Ouroboros.Consensus.Storage.VolatileDB.Types
import           Ouroboros.Consensus.Util.IOLike

{------------------------------------------------------------------------------
  FileId utilities
------------------------------------------------------------------------------}

parseFd :: FsPath -> Maybe FileId
parseFd file =
    parseFilename <=< lastMaybe $ fsPathToList file
  where
    parseFilename :: Text -> Maybe FileId
    parseFilename = readMaybe
                  . T.unpack
                  . snd
                  . T.breakOnEnd "-"
                  . fst
                  . T.breakOn "."

-- | Parses the 'FileId' of each 'FsPath' and zips them together. Returns
-- the results sorted on the 'FileId'.
--
-- Return separately any 'FsPath' which failed to parse.
parseAllFds :: [FsPath] -> ([(FileId, FsPath)], [FsPath])
parseAllFds = first (sortOn fst) . foldr judge ([], [])
  where
    judge fsPath (parsed, notParsed) = case parseFd fsPath of
      Nothing     -> (parsed, fsPath : notParsed)
      Just fileId -> ((fileId, fsPath) : parsed, notParsed)

-- | This also returns any 'FsPath' which failed to parse.
findLastFd :: [FsPath] -> (Maybe FileId, [FsPath])
findLastFd = first (fmap fst . lastMaybe) . parseAllFds

filePath :: FileId -> FsPath
filePath fd = mkFsPath ["blocks-" ++ show fd ++ ".dat"]

{------------------------------------------------------------------------------
  Exception handling
------------------------------------------------------------------------------}

wrapFsError :: MonadCatch m => m a -> m a
wrapFsError = handle $ throwM . UnexpectedError . FileSystemError

-- | Execute an action and catch the 'VolatileDBError' and 'FsError' that can
-- be thrown by it, and wrap the 'FsError' in an 'VolatileDBError' using the
-- 'FileSystemError' constructor.
--
-- This should be used whenever you want to run an action on the VolatileDB
-- and catch the 'VolatileDBError' and the 'FsError' (wrapped in the former)
-- it may thrown.
tryVolDB :: forall m a. MonadCatch m
         => m a
         -> m (Either VolatileDBError a)
tryVolDB = try . wrapFsError

{------------------------------------------------------------------------------
  Map of Set utilities
------------------------------------------------------------------------------}

alterfInsert :: Ord blockId
             => blockId
             -> Maybe (Set blockId)
             -> Maybe (Set blockId)
alterfInsert successor mSet = case mSet of
    Nothing  -> Just $ Set.singleton successor
    Just set -> Just $ Set.insert successor set

insertMapSet :: Ord blockId
             => SuccessorsIndex blockId
             -> (blockId, WithOrigin blockId)
             -> SuccessorsIndex blockId
insertMapSet mapSet (bid, pbid) = Map.alter (alterfInsert bid) pbid mapSet

alterfDelete :: Ord blockId
             => blockId
             -> Maybe (Set blockId)
             -> Maybe (Set blockId)
alterfDelete successor mSet = case mSet of
    Nothing
        -> Nothing
    Just set
        | Set.null set'
        -> Nothing
        | otherwise
        -> Just set'
      where
        set' = Set.delete successor set

deleteMapSet :: Ord blockId
             => SuccessorsIndex blockId
             -> (blockId, WithOrigin blockId)
             -> SuccessorsIndex blockId
deleteMapSet mapSet (bid, pbid) = Map.alter (alterfDelete bid) pbid mapSet
