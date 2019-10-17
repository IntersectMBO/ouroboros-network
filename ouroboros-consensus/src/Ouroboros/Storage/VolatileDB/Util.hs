{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Storage.VolatileDB.Util where

import           Control.Monad
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Read (readMaybe)

import           Control.Monad.Class.MonadThrow

import           Ouroboros.Network.Point (WithOrigin)

import           Ouroboros.Consensus.Util (lastMaybe, safeMaximum,
                     safeMaximumOn)
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling (..))
import qualified Ouroboros.Storage.Util.ErrorHandling as EH
import           Ouroboros.Storage.VolatileDB.Types

{------------------------------------------------------------------------------
  Utilities
------------------------------------------------------------------------------}


parseFd :: FsPath -> Maybe FileId
parseFd = parseFilename <=< lastMaybe . fsPathToList
  where
    parseFilename :: Text -> Maybe FileId
    parseFilename = readMaybe
                  . T.unpack
                  . snd
                  . T.breakOnEnd "-"
                  . fst
                  . T.breakOn "."

unsafeParseFd :: FsPath -> FileId
unsafeParseFd file = fromMaybe
    (error $ "could not parse filename " <> show file)
    (parseFd file)

fromEither :: Monad m
           => ErrorHandling e m
           -> Either e a
           -> m a
fromEither err = \case
    Left e -> EH.throwError err e
    Right a -> return a

modifyMVar :: IOLike m
           => StrictMVar m a
           -> (a -> m (a,b))
           -> m b
modifyMVar m action =
    snd . fst <$> generalBracket (takeMVar m)
       (\oldState ec -> case ec of
            ExitCaseSuccess (newState,_) -> putMVar m newState
            ExitCaseException _ex        -> putMVar m oldState
            ExitCaseAbort                -> putMVar m oldState
       ) action

wrapFsError :: Monad m
            => ErrorHandling FsError                   m
            -> ErrorHandling (VolatileDBError blockId) m
            -> m a -> m a
wrapFsError fsErr volDBErr action =
    tryVolDB fsErr volDBErr action >>= either (throwError volDBErr) return

-- | Parses the 'FsPath' of each filename and zips them together.
-- If any 'FsPath' does not parse, we return an error.
parseAllFds :: forall blockId.
               [FsPath]
            -> Either (VolatileDBError blockId) [(FileId, FsPath)]
parseAllFds = mapM go
  where
    go :: FsPath -> Either (VolatileDBError blockId) (FileId, FsPath)
    go file = case parseFd file of
      Nothing -> Left $ UnexpectedError $ ParserError $ InvalidFilename file
      Just fd -> Right (fd, file)

-- | Throws an error if one of the given file names does not parse.
findLastFd :: forall blockId.
              Set FsPath
           -> Either (VolatileDBError blockId) (Maybe FileId)
findLastFd = foldM go Nothing
  where
    maxMaybe :: Ord a => Maybe a -> a -> a
    maxMaybe ma a = case ma of
      Nothing -> a
      Just a' -> max a' a
    go :: Maybe FileId -> FsPath -> Either (VolatileDBError blockId) (Maybe FileId)
    go fd file = case parseFd file of
      Nothing  -> Left $ UnexpectedError $ ParserError $ InvalidFilename file
      Just fd' -> Right $ Just $ maxMaybe fd fd'

filePath :: FileId -> FsPath
filePath fd = mkFsPath ["blocks-" ++ show fd ++ ".dat"]

-- | Execute an action and catch the 'VolatileDBError' and 'FsError' that can
-- be thrown by it, and wrap the 'FsError' in an 'VolatileDBError' using the
-- 'FileSystemError' constructor.
--
-- This should be used whenever you want to run an action on the VolatileDB
-- and catch the 'VolatileDBError' and the 'FsError' (wrapped in the former)
-- it may thrown.
tryVolDB :: forall m blockId a. Monad m
         => ErrorHandling FsError                   m
         -> ErrorHandling (VolatileDBError blockId) m
         -> m a -> m (Either (VolatileDBError blockId) a)
tryVolDB fsErr volDBErr = fmap squash . EH.try fsErr . EH.try volDBErr
    where
      fromFS = UnexpectedError . FileSystemError

      squash :: Either FsError (Either (VolatileDBError blockId) a)
               -> Either (VolatileDBError blockId) a
      squash = either (Left . fromFS) id

{------------------------------------------------------------------------------
  Map of Set operations
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
    Nothing  -> Nothing
    Just set -> Just $ Set.delete successor set

deleteMapSet :: Ord blockId
             => SuccessorsIndex blockId
             -> (blockId, WithOrigin blockId)
             -> SuccessorsIndex blockId
deleteMapSet mapSet (bid, pbid) = Map.alter (alterfDelete bid) pbid mapSet

{------------------------------------------------------------------------------
  Comparing utilities
------------------------------------------------------------------------------}

maxSlotList :: [(blockId, SlotNo)] -> Maybe (blockId, SlotNo)
maxSlotList = updateSlot Nothing

cmpMaybe :: Ord a => Maybe a -> a -> Bool
cmpMaybe Nothing _   = False
cmpMaybe (Just a) a' = a >= a'

updateSlot :: forall blockId. Maybe (blockId, SlotNo) -> [(blockId, SlotNo)] -> Maybe (blockId, SlotNo)
updateSlot msl ls = safeMaximumOn snd $ case msl of
    Nothing -> ls
    Just sl -> sl : ls

updateSlotNoBlockId :: MaxSlotNo -> [SlotNo] -> MaxSlotNo
updateSlotNoBlockId msl ls = maxSlotNoFromMaybe $ safeMaximum $ case msl of
    NoMaxSlotNo  -> ls
    MaxSlotNo sl -> sl : ls
