{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Storage.VolatileDB.Util where

import           Control.Monad
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Data.Int (Int64)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Text as T
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

{------------------------------------------------------------------------------
  Comparing utilities
------------------------------------------------------------------------------}

maxSlotMap :: Map Int64 (Int, blockId) -> (blockId -> Slot) -> Maybe (blockId, Slot)
maxSlotMap mp toSlot = maxSlotList toSlot $ snd <$> Map.elems mp

maxSlotList :: (blockId -> Slot) -> [blockId] -> Maybe (blockId, Slot)
maxSlotList toSlot = updateSlot toSlot Nothing

cmpMaybe :: Ord a => Maybe a -> a -> Bool
cmpMaybe Nothing _   = False
cmpMaybe (Just a) a' = a >= a'

updateSlot :: forall blockId. (blockId -> Slot) -> Maybe blockId -> [blockId] -> Maybe (blockId, Slot)
updateSlot toSlot mbid = foldl cmpr ((\b -> (b, toSlot b)) <$> mbid)
    where
        cmpr :: Maybe (blockId, Slot) -> blockId -> Maybe (blockId, Slot)
        cmpr Nothing bid = Just (bid, toSlot bid)
        cmpr (Just (bid, sl)) bid' =
            let sl' = toSlot bid'
            in Just $ if sl > sl' then (bid, sl) else (bid', sl')

updateSlotNoBlockId :: Maybe Slot -> [Slot] -> Maybe Slot
updateSlotNoBlockId = foldl cmpr
    where
        cmpr :: Maybe Slot -> Slot -> Maybe Slot
        cmpr Nothing sl'   = Just sl'
        cmpr (Just sl) sl' = Just $ max sl sl'
