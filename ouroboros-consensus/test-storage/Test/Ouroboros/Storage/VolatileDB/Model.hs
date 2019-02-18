{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}
-- | In-memory Model implementation of 'VolatileDB' for testing
module Test.Ouroboros.Storage.VolatileDB.Model
    (
      DBModel (..)
    , initDBModel
    , openDBModel
    ) where

import           Control.Monad.State (MonadState, get, put)
import           Data.ByteString (ByteString)
import           Data.ByteString.Builder
import           Data.ByteString.Lazy (toStrict)
import           Data.Map (Map)
import qualified Data.Map as Map

import           Ouroboros.Storage.VolatileDB.API
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling (..))
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

data DBModel blockId = DBModel {
      open           :: Bool
    , mp             :: Map blockId ByteString
    , latestGarbaged :: Maybe Slot
    } deriving (Show)

initDBModel :: DBModel blockId
initDBModel = DBModel {
      open = True
    , mp = Map.empty
    , latestGarbaged = Nothing
}

openDBModel :: MonadState (DBModel blockId) m
            => (Ord blockId, Show blockId)
            => ErrorHandling (VolatileDBError blockId) m
            -> (DBModel blockId, VolatileDB blockId m)
openDBModel err = (dbModel, db)
    where
        dbModel = initDBModel
        db = VolatileDB {
              closeDB  = closeDBModel
            , isOpenDB = isOpenModel
            , reOpenDB = reOpenModel
            , getBlock = getBlockModel err
            , putBlock = putBlockModel err
            , garbageCollect = garbageCollectModel err
        }

closeDBModel :: MonadState (DBModel blockId) m => m ()
closeDBModel = do
    dbm <- get
    put $ dbm {open = False}

isOpenModel :: MonadState (DBModel blockId) m => m Bool
isOpenModel = do
    DBModel {..} <- get
    return open

reOpenModel :: MonadState (DBModel blockId) m => m ()
reOpenModel = do
    dbm <- get
    put $ dbm {open = True}

getBlockModel :: forall m blockId. (MonadState (DBModel blockId) m, Ord blockId, Show blockId)
              => ErrorHandling (VolatileDBError blockId) m
              -> blockId
              -> m (Maybe ByteString)
getBlockModel err sl = do
    DBModel {..} <- get
    if not open then EH.throwError err ClosedDBError
    else return $ Map.lookup sl mp

putBlockModel :: MonadState (DBModel blockId) m
              => Ord blockId
              => ErrorHandling (VolatileDBError blockId) m
              -> blockId
              -> Builder
              -> m ()
putBlockModel err sl bs = do
    dbm@DBModel {..} <- get
    if not open then EH.throwError err ClosedDBError
    else case Map.lookup sl mp of
        Nothing -> put dbm{mp = Map.insert sl (toStrict $ toLazyByteString bs) mp}
        Just _bs -> return ()

garbageCollectModel :: MonadState (DBModel blockId) m
                    => Ord blockId
                    => ErrorHandling (VolatileDBError blockId) m
                    -> Slot
                    -> m ()
garbageCollectModel err sl = do
    dbm@DBModel {..} <- get
    if not open then EH.throwError err ClosedDBError
    else put dbm {latestGarbaged = Just $ maxMaybe latestGarbaged sl}

maxMaybe :: Ord slot => Maybe slot -> slot -> slot
maxMaybe Nothing sl = sl
maxMaybe (Just sl') sl = max sl' sl