{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Ouroboros.Storage.VolatileDB.Mock (openDBMock) where

import           Control.Monad.State (StateT)

import           Ouroboros.Consensus.Util ((.:))
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.STM (simStateT)

import           Ouroboros.Storage.Common (castBlockComponent)
import           Ouroboros.Storage.Util.ErrorHandling (ThrowCantCatch)
import qualified Ouroboros.Storage.Util.ErrorHandling as EH
import           Ouroboros.Storage.VolatileDB.API

import           Test.Ouroboros.Storage.VolatileDB.Model

openDBMock  :: forall m blockId. IOLike m
            => (Ord blockId)
            => ThrowCantCatch VolatileDBError (STM m)
            -> Int
            -> m (DBModel blockId, VolatileDB blockId m)
openDBMock err maxNumPerFile = do
    dbVar <- uncheckedNewTVarM dbModel
    return (dbModel, db dbVar)
  where
    dbModel = initDBModel maxNumPerFile

    db :: StrictTVar m (DBModel blockId) -> VolatileDB blockId m
    db dbVar = VolatileDB {
          closeDB           = wrapModel' dbVar  $  closeModel
        , isOpenDB          = wrapModel' dbVar  $  isOpenModel
        , reOpenDB          = wrapModel' dbVar  $  reOpenModel            err'
        , getBlockComponent = wrapModel' dbVar .: (getBlockComponentModel err' . castBlockComponent)
        , putBlock          = wrapModel' dbVar .:  putBlockModel          err' Nothing
        , garbageCollect    = wrapModel' dbVar  .  garbageCollectModel    err' Nothing
        , getIsMember       = wrapModel  dbVar  $  getIsMemberModel       err'
        , getBlockIds       = wrapModel' dbVar  $  getBlockIdsModel       err'
        , getSuccessors     = wrapModel  dbVar  $  getSuccessorsModel     err'
        , getPredecessor    = wrapModel  dbVar  $  getPredecessorModel    err'
        , getMaxSlotNo      = wrapModel  dbVar  $  getMaxSlotNoModel      err'
        }

    err' :: ThrowCantCatch VolatileDBError
                           (StateT (DBModel blockId) (STM m))
    err' = EH.liftThrowT err

    wrapModel' :: StrictTVar m (DBModel blockId)
               -> StateT (DBModel blockId) (STM m) a -> m a
    wrapModel' dbVar = atomically . wrapModel dbVar

    wrapModel :: StrictTVar m (DBModel blockId)
              -> StateT (DBModel blockId) (STM m) a -> STM m a
    wrapModel dbVar = simStateT dbVar $ id
