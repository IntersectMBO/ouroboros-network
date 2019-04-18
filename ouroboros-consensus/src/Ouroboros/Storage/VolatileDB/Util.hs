{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Storage.VolatileDB.Util where

import           Control.Monad
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Data.List (maximumBy)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Typeable
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

wrapFsError :: (Show blockId, MonadCatch m, Typeable blockId)
            => ErrorHandling (VolatileDBError blockId) m -> m a -> m a
wrapFsError err action = do
    mr <- try . try $ action
    case mr of
        Left fsError    -> throwError err $ UnexpectedError $ FileSystemError fsError
        Right (Right a) -> return a
        Right (Left e)  -> throwError err e

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
            Nothing  -> Left $ UnexpectedError $ ParserError $ InvalidFilename file
            Just fd' -> Right $ Just $ maxMaybe fd fd'

filePath :: FileId -> String
filePath fd = "blocks-" ++ show fd ++ ".dat"

sizeAndId :: (BlockSize, BlockInfo blockId) -> (BlockSize, blockId)
sizeAndId (size, bInfo) = (size, bbid bInfo)

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
             -> (blockId, Maybe blockId)
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
             -> (blockId, Maybe blockId)
             -> SuccessorsIndex blockId
deleteMapSet mapSet (bid, pbid) = Map.alter (alterfDelete bid) pbid mapSet

{------------------------------------------------------------------------------
  Comparing utilities
------------------------------------------------------------------------------}

maxSlotMap :: Map Word64 (Word64, BlockInfo blockId) -> Maybe (blockId, SlotNo)
maxSlotMap mp = f <$> safeMaximumBy (\a b -> compare (getSlot a) (getSlot b)) (Map.elems mp)
    where
        f (_, bInfo) = (bbid bInfo, bslot bInfo)
        getSlot (_, bInfo) = bslot bInfo

maxSlotList :: [(blockId, SlotNo)] -> Maybe (blockId, SlotNo)
maxSlotList = updateSlot Nothing

cmpMaybe :: Ord a => Maybe a -> a -> Bool
cmpMaybe Nothing _   = False
cmpMaybe (Just a) a' = a >= a'

updateSlot :: forall blockId. Maybe (blockId, SlotNo) -> [(blockId, SlotNo)] -> Maybe (blockId, SlotNo)
updateSlot msl ls = safeMaximumBy (\a b -> compare (snd a) (snd b))
    $ case msl of
        Nothing -> ls
        Just sl -> sl : ls

updateSlotNoBlockId :: Maybe SlotNo -> [SlotNo] -> Maybe SlotNo
updateSlotNoBlockId msl ls = safeMaximumBy compare $ case msl of
        Nothing -> ls
        Just sl -> sl : ls

safeMaximumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
safeMaximumBy _cmp [] = Nothing
safeMaximumBy cmp ls  = Just $ maximumBy cmp ls
