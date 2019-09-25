{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Ouroboros.Storage.VolatileDB.Mock (openDBMock) where

import           Control.Monad.State (StateT)

import           Ouroboros.Storage.Util.ErrorHandling (ThrowCantCatch)
import qualified Ouroboros.Storage.Util.ErrorHandling as EH
import           Ouroboros.Storage.VolatileDB.API

import           Ouroboros.Consensus.Util ((.:))
import           Ouroboros.Consensus.Util.MonadSTM.NormalForm
import           Ouroboros.Consensus.Util.STM (simStateT)

import           Test.Ouroboros.Storage.VolatileDB.Model

openDBMock  :: forall m blockId.
               MonadSTM m
            => (Ord blockId)
            => ThrowCantCatch (VolatileDBError blockId) (STM m)
            -> Int
            -> m (DBModel blockId, VolatileDB blockId m)
openDBMock err maxNumPerFile = do
    dbVar <- atomically $ newTVar dbModel
    return (dbModel, db dbVar)
  where
    dbModel = initDBModel maxNumPerFile

    db :: StrictTVar m (DBModel blockId) -> VolatileDB blockId m
    db dbVar = VolatileDB {
          closeDB        = wrapModel' dbVar  $ closeModel
        , isOpenDB       = wrapModel' dbVar  $ isOpenModel
        , reOpenDB       = wrapModel' dbVar  $ reOpenModel         err'
        , getBlock       = wrapModel' dbVar  . getBlockModel       err'
        , putBlock       = wrapModel' dbVar .: putBlockModel       err' Nothing
        , garbageCollect = wrapModel' dbVar  . garbageCollectModel err' Nothing
        , getIsMember    = wrapModel  dbVar  $ getIsMemberModel    err'
        , getBlockIds    = wrapModel' dbVar  $ getBlockIdsModel    err'
        , getSuccessors  = wrapModel  dbVar  $ getSuccessorsModel  err'
        , getPredecessor = wrapModel  dbVar  $ getPredecessorModel err'
        }

    err' :: ThrowCantCatch (VolatileDBError blockId)
                           (StateT (DBModel blockId) (STM m))
    err' = EH.liftThrowT err

    wrapModel' :: StrictTVar m (DBModel blockId)
               -> StateT (DBModel blockId) (STM m) a -> m a
    wrapModel' dbVar = atomically . wrapModel dbVar

    wrapModel :: StrictTVar m (DBModel blockId)
              -> StateT (DBModel blockId) (STM m) a -> STM m a
    wrapModel dbVar = simStateT dbVar $ id
