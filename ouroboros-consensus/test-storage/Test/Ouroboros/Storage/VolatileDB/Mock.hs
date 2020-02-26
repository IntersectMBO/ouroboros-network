{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Ouroboros.Storage.VolatileDB.Mock (openDBMock) where

import           Control.Monad (join)

import           Ouroboros.Consensus.Util ((.:))
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Storage.Util.ErrorHandling (ErrorHandling,
                     ThrowCantCatch)
import qualified Ouroboros.Consensus.Storage.Util.ErrorHandling as EH
import           Ouroboros.Consensus.Storage.VolatileDB.API

import           Test.Ouroboros.Storage.VolatileDB.Model

openDBMock  :: forall m blockId. (IOLike m, Ord blockId)
            => ErrorHandling VolatileDBError m
            -> ThrowCantCatch VolatileDBError (STM m)
            -> BlocksPerFile
            -> m (DBModel blockId, VolatileDB blockId m)
openDBMock err errSTM maxBlocksPerFile = do
    dbVar <- uncheckedNewTVarM dbModel
    return (dbModel, db dbVar)
  where
    dbModel = initDBModel maxBlocksPerFile

    db :: StrictTVar m (DBModel blockId) -> VolatileDB blockId m
    db dbVar = VolatileDB {
          closeDB           = update_   $ closeModel
        , isOpenDB          = query     $ isOpenModel
        , reOpenDB          = update_   $ reOpenModel
        , getBlockComponent = queryE   .: getBlockComponentModel
        , putBlock          = updateE_ .: putBlockModel
        , garbageCollect    = updateE_  . garbageCollectModel
        , getSuccessors     = querySTME $ getSuccessorsModel
        , getBlockInfo      = querySTME $ getBlockInfoModel
        , getMaxSlotNo      = querySTME $ getMaxSlotNoModel
        }
      where
        update_ :: (DBModel blockId -> DBModel blockId) -> m ()
        update_ f = atomically $ modifyTVar dbVar f

        updateE_ :: (DBModel blockId -> Either VolatileDBError (DBModel blockId)) -> m ()
        updateE_ f = join $ atomically $ do
          (f <$> readTVar dbVar) >>= \case
            Left  e   -> return $ EH.throwError err e
            Right db' -> do
              writeTVar dbVar db'
              return $ return ()

        query :: (DBModel blockId -> a) -> m a
        query f = fmap f $ atomically $ readTVar dbVar

        queryE :: (DBModel blockId -> Either VolatileDBError a) -> m a
        queryE f = query f >>= \case
          Left  e -> EH.throwError err e
          Right a -> return a

        querySTME :: (DBModel blockId -> Either VolatileDBError a) -> STM m a
        querySTME f =
          (f <$> readTVar dbVar) >>= \case
            Left  e -> EH.throwError' errSTM e
            Right a -> return a
