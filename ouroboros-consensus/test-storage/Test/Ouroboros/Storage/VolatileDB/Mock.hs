{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Ouroboros.Storage.VolatileDB.Mock (openDBMock) where

import           Ouroboros.Consensus.Util ((.:))
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Storage.VolatileDB.API

import           Test.Ouroboros.Storage.VolatileDB.Model

openDBMock  :: forall m blockId. (IOLike m, Ord blockId)
            => BlocksPerFile
            -> m (DBModel blockId, VolatileDB blockId m)
openDBMock maxBlocksPerFile = do
    dbVar <- uncheckedNewTVarM dbModel
    return (dbModel, db dbVar)
  where
    dbModel = initDBModel maxBlocksPerFile

    db :: StrictTVar m (DBModel blockId) -> VolatileDB blockId m
    db dbVar = VolatileDB {
          closeDB             = update_   $ closeModel
        , getBlockComponent   = queryE   .: getBlockComponentModel
        , putBlock            = updateE_ .: putBlockModel
        , garbageCollect      = updateE_  . garbageCollectModel
        , filterByPredecessor = querySTME $ filterByPredecessorModel
        , getBlockInfo        = querySTME $ getBlockInfoModel
        , getMaxSlotNo        = querySTME $ getMaxSlotNoModel
        }
      where
        update_ :: (DBModel blockId -> DBModel blockId) -> m ()
        update_ f = atomically $ modifyTVar dbVar f

        updateE_ :: (DBModel blockId -> Either VolatileDBError (DBModel blockId)) -> m ()
        updateE_ f = atomically $ do
          (f <$> readTVar dbVar) >>= \case
            Left  e   -> throwM e
            Right db' -> writeTVar dbVar db'

        query :: (DBModel blockId -> a) -> m a
        query f = fmap f $ atomically $ readTVar dbVar

        queryE :: (DBModel blockId -> Either VolatileDBError a) -> m a
        queryE f = query f >>= \case
          Left  e -> throwM e
          Right a -> return a

        querySTME :: (DBModel blockId -> Either VolatileDBError a) -> STM m a
        querySTME f =
          (f <$> readTVar dbVar) >>= \case
            Left  e -> throwM e
            Right a -> return a
