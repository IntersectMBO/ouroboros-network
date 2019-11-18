{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Ouroboros.Storage.VolatileDB.Util
    ( -- * FileId utilities
      parseFd
    , unsafeParseFd
    , parseAllFds
    , filePath
    , findLastFd

      -- * Exception handling
    , fromEither
    , modifyMVar
    , wrapFsError
    , tryVolDB

      -- * Map of Set utilities
    , insertMapSet
    , deleteMapSet

      -- * Comparing utilities
    , maxSlotList
    , cmpMaybe
    , updateSlot
    , updateSlotNoBlockId
    ) where

import           Control.Monad
import qualified Data.Map.Strict as Map
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
  FileId utilities
------------------------------------------------------------------------------}

parseFd :: FsPath -> Either VolatileDBError FileId
parseFd file = maybe err Right $
    parseFilename <=< lastMaybe $ fsPathToList file
  where
    err = Left $ UnexpectedError $ ParserError $ InvalidFilename file

    parseFilename :: Text -> Maybe FileId
    parseFilename = readMaybe
                  . T.unpack
                  . snd
                  . T.breakOnEnd "-"
                  . fst
                  . T.breakOn "."

-- | Parses the 'FileId' of each 'FsPath' and zips them together.
-- When parsing fails, we abort with the corresponding parse error.
parseAllFds :: [FsPath] -> Either VolatileDBError [(FileId, FsPath)]
parseAllFds = mapM $ \f -> (,f) <$> parseFd f

-- | When parsing fails, we abort with the corresponding parse error.
findLastFd :: Set FsPath -> Either VolatileDBError (Maybe FileId)
findLastFd = fmap safeMaximum . mapM parseFd . Set.toList

filePath :: FileId -> FsPath
filePath fd = mkFsPath ["blocks-" ++ show fd ++ ".dat"]

unsafeParseFd :: FsPath -> FileId
unsafeParseFd file = either
    (\_ -> error $ "Could not parse filename " <> show file)
    id
    (parseFd file)

{------------------------------------------------------------------------------
  Exception handling
------------------------------------------------------------------------------}

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
            => ErrorHandling FsError         m
            -> ErrorHandling VolatileDBError m
            -> m a -> m a
wrapFsError fsErr volDBErr action =
    tryVolDB fsErr volDBErr action >>= either (throwError volDBErr) return

-- | Execute an action and catch the 'VolatileDBError' and 'FsError' that can
-- be thrown by it, and wrap the 'FsError' in an 'VolatileDBError' using the
-- 'FileSystemError' constructor.
--
-- This should be used whenever you want to run an action on the VolatileDB
-- and catch the 'VolatileDBError' and the 'FsError' (wrapped in the former)
-- it may thrown.
tryVolDB :: forall m a. Monad m
         => ErrorHandling FsError         m
         -> ErrorHandling VolatileDBError m
         -> m a -> m (Either VolatileDBError a)
tryVolDB fsErr volDBErr = fmap squash . EH.try fsErr . EH.try volDBErr
  where
    fromFS = UnexpectedError . FileSystemError

    squash :: Either FsError (Either VolatileDBError a)
           -> Either VolatileDBError a
    squash = either (Left . fromFS) id

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

updateSlot :: forall blockId. Maybe (blockId, SlotNo)
           -> [(blockId, SlotNo)]
           -> Maybe (blockId, SlotNo)
updateSlot msl ls = safeMaximumOn snd $ case msl of
    Nothing -> ls
    Just sl -> sl : ls

updateSlotNoBlockId :: MaxSlotNo -> [SlotNo] -> MaxSlotNo
updateSlotNoBlockId msl ls = maxSlotNoFromMaybe $ safeMaximum $ case msl of
    NoMaxSlotNo  -> ls
    MaxSlotNo sl -> sl : ls
