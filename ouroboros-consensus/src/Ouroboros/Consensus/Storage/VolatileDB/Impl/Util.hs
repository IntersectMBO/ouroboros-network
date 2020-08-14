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
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Read (readMaybe)

import           Ouroboros.Consensus.Util (lastMaybe)
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Storage.FS.API.Types
import           Ouroboros.Consensus.Storage.VolatileDB.Types

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

insertMapSet ::
     forall k v. (Ord k, Ord v)
  => k
  -> v
  -> Map k (Set v)
  -> Map k (Set v)
insertMapSet k v = Map.alter ins k
  where
    ins :: Maybe (Set v) -> Maybe (Set v)
    ins = \case
        Nothing  -> Just $ Set.singleton v
        Just set -> Just $ Set.insert v set

deleteMapSet ::
     forall k v. (Ord k, Ord v)
  => k
  -> v
  -> Map k (Set v)
  -> Map k (Set v)
deleteMapSet k v = Map.update del k
  where
    del :: Set v -> Maybe (Set v)
    del set
      | Set.null set'
      = Nothing
      | otherwise
      = Just set'
      where
        set' = Set.delete v set
