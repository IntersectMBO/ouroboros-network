{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Storage.VolatileDB.Util where

import           Control.Monad
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Word (Word64)
import           Text.Read (readMaybe)

import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling (..))
import qualified Ouroboros.Storage.Util.ErrorHandling as EH
import           Ouroboros.Storage.VolatileDB.Types

{------------------------------------------------------------------------------
  Utilities
------------------------------------------------------------------------------}


parseFd :: String -> Maybe FileId
parseFd = readMaybe
            . T.unpack
            . snd
            . T.breakOnEnd "-"
            . fst
            . T.breakOn "."
            . T.pack

unsafeParseFd :: String -> FileId
unsafeParseFd file = fromMaybe
    (error $ "could not parse filename " <> file <> " of index")
    (parseFd file)

fromEither :: Monad m
           => ErrorHandling e m
           -> Either e a
           -> m a
fromEither err = \case
    Left e -> EH.throwError err e
    Right a -> return a

modifyTMVar :: (MonadSTM m, MonadCatch m)
            => TMVar m a
            -> (a -> m (a,b))
            -> m b
modifyTMVar m action =
    snd . fst <$> generalBracket (atomically $ takeTMVar m)
       (\oldState ec -> atomically $ case ec of
            ExitCaseSuccess (newState,_) -> putTMVar m newState
            ExitCaseException _ex        -> putTMVar m oldState
            ExitCaseAbort                -> putTMVar m oldState
       ) action

-- Throws an error if one of the given file names does not parse.
findLastFd :: forall blockId.
              Set String
           -> Either (VolatileDBError blockId) (Maybe FileId)
findLastFd files = foldM go Nothing files
    where
        maxMaybe :: Ord a => Maybe a -> a -> a
        maxMaybe ma a = case ma of
            Nothing -> a
            Just a' -> max a' a
        go :: Maybe FileId -> String -> Either (VolatileDBError blockId) (Maybe FileId)
        go fd file = case parseFd file of
            Nothing  -> Left $ VParserError $ InvalidFilename file
            Just fd' -> Right $ Just $ maxMaybe fd fd'

filePath :: FileId -> String
filePath fd = "blocks-" ++ show fd ++ ".dat"

forth :: (a,b,c,d) -> d
forth (_, _, _, d) = d

third :: (a,b,c) -> c
third (_,_,c) = c

firstTwo :: (a,b,c) -> (a,b)
firstTwo (a,b,_) = (a,b)

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
             -> (blockId, blockId)
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
             -> (blockId, blockId)
             -> SuccessorsIndex blockId
deleteMapSet mapSet (bid, pbid) = Map.alter (alterfDelete bid) pbid mapSet

{------------------------------------------------------------------------------
  Comparing utilities
------------------------------------------------------------------------------}

maxSlotMap :: Map Word64 (Word64, blockId) -> (blockId -> SlotNo) -> Maybe (blockId, SlotNo)
maxSlotMap mp toSlot = maxSlotList toSlot $ snd <$> Map.elems mp

maxSlotList :: (blockId -> SlotNo) -> [blockId] -> Maybe (blockId, SlotNo)
maxSlotList toSlot = updateSlot toSlot Nothing

cmpMaybe :: Ord a => Maybe a -> a -> Bool
cmpMaybe Nothing _   = False
cmpMaybe (Just a) a' = a >= a'

updateSlot :: forall blockId. (blockId -> SlotNo) -> Maybe blockId -> [blockId] -> Maybe (blockId, SlotNo)
updateSlot toSlot mbid = foldl cmpr ((\b -> (b, toSlot b)) <$> mbid)
    where
        cmpr :: Maybe (blockId, SlotNo) -> blockId -> Maybe (blockId, SlotNo)
        cmpr Nothing bid = Just (bid, toSlot bid)
        cmpr (Just (bid, sl)) bid' =
            let sl' = toSlot bid'
            in Just $ if sl > sl' then (bid, sl) else (bid', sl')

updateSlotNoBlockId :: Maybe SlotNo -> [SlotNo] -> Maybe SlotNo
updateSlotNoBlockId = foldl cmpr
    where
        cmpr :: Maybe SlotNo -> SlotNo -> Maybe SlotNo
        cmpr Nothing sl'   = Just sl'
        cmpr (Just sl) sl' = Just $ max sl sl'
