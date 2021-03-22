{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Ouroboros.Storage.VolatileDB.Mock (openDBMock) where

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Util ((.:))
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Storage.Serialisation (EncodeDisk (..),
                     HasBinaryBlockInfo (..))
import           Ouroboros.Consensus.Storage.VolatileDB hiding
                     (VolatileDbArgs (..))

import           Test.Ouroboros.Storage.VolatileDB.Model

openDBMock ::
     forall m blk.
     ( IOLike m
     , GetPrevHash blk
     , HasBinaryBlockInfo blk
     , EncodeDisk blk blk
     , HasNestedContent Header blk
     )
  => BlocksPerFile
  -> CodecConfig blk
  -> m (DBModel blk, VolatileDB m blk)
openDBMock maxBlocksPerFile ccfg = do
    dbVar <- uncheckedNewTVarM dbModel
    return (dbModel, db dbVar)
  where
    dbModel = initDBModel maxBlocksPerFile ccfg

    db :: StrictTVar m (DBModel blk) -> VolatileDB m blk
    db dbVar = VolatileDB {
          closeDB             = update_   $ closeModel
        , getBlockComponent   = queryE   .: getBlockComponentModel
        , putBlock            = updateE_  . putBlockModel
        , garbageCollect      = updateE_  . garbageCollectModel
        , filterByPredecessor = querySTME $ filterByPredecessorModel
        , getBlockInfo        = querySTME $ getBlockInfoModel
        , getMaxSlotNo        = querySTME $ getMaxSlotNoModel
        }
      where
        update_ :: (DBModel blk -> DBModel blk) -> m ()
        update_ f = atomically $ modifyTVar dbVar f

        updateE_ :: (DBModel blk -> Either (VolatileDBError blk) (DBModel blk)) -> m ()
        updateE_ f = atomically $ do
          (f <$> readTVar dbVar) >>= \case
            Left  e   -> throwSTM e
            Right db' -> writeTVar dbVar db'

        query :: (DBModel blk -> a) -> m a
        query f = fmap f $ atomically $ readTVar dbVar

        queryE :: (DBModel blk -> Either (VolatileDBError blk) a) -> m a
        queryE f = query f >>= \case
          Left  e -> throwIO e
          Right a -> return a

        querySTME :: (DBModel blk -> Either (VolatileDBError blk) a) -> STM m a
        querySTME f =
          (f <$> readTVar dbVar) >>= \case
            Left  e -> throwSTM e
            Right a -> return a
