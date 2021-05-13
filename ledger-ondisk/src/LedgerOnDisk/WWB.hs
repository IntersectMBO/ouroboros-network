{-# LANGUAGE LambdaCase #-}
-- |

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
module LedgerOnDisk.WWB where

import LedgerOnDisk.Class
import Data.Hashable
import Data.TreeDiff
import GHC.Generics
import Data.HashMap.Strict (HashMap)
import Control.Monad.IO.Class
import Control.Monad.RWS.Strict
import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue
import Control.Concurrent.Async
import Control.Applicative
import Data.PriorityQueue.FingerTree
import qualified Data.Semigroup as Semi
import qualified Data.HashMap.Strict as HashMap
import Data.FingerTree (FingerTree, Measured(..), (><))
import Data.Coerce
import qualified Data.HashSet as HashSet
import qualified Data.FingerTree as FingerTree
import Data.HashSet (HashSet)
import Control.Monad.Reader
import qualified Control.Monad.State.Strict as State
import Control.Monad.Except

newtype WWBT k v m a = WWBT { unWWBT :: ReaderT (WWBConfig k v) m a }
  deriving newtype (Functor, Applicative, Monad)

data InMemoryEntry k v
  = IME !k !(Maybe v)
  | LiveQuery !Int !(HashSet k)
  | CompletedQuery !Int

data InMemoryEntryMeasure k v = InMemoryEntryMeasure
  { imeMap :: !(HashMap k (Semi.Last (Maybe v)))
  , liveQueries :: !(HashMap Int (Semi.Last (Maybe (HashSet k))))
  }
  deriving stock (Show, Eq)

instance (Eq k, Hashable k) => Measured (InMemoryEntryMeasure k v) (InMemoryEntry k v) where
  measure = \case
    IME k v -> mempty { imeMap = HashMap.singleton k $ coerce v }
    LiveQuery i qs -> mempty { liveQueries = HashMap.singleton i . coerce . Just $ qs }
    CompletedQuery i -> mempty { liveQueries = HashMap.singleton i . Semi.Last $ Nothing }

instance (Eq k, Hashable k) => Semigroup (InMemoryEntryMeasure k v) where
  InMemoryEntryMeasure il ll  <> InMemoryEntryMeasure ir lr  = InMemoryEntryMeasure{..} where
    imeMap = coerce $ HashMap.unionWith (<>) il ir
    liveQueries = coerce $ HashMap.unionWith (<>) ll lr

instance (Eq k, Hashable k) => Monoid (InMemoryEntryMeasure k v) where
  mempty = InMemoryEntryMeasure mempty mempty

type InMemoryStore k v = FingerTree (InMemoryEntryMeasure k v) (InMemoryEntry k v)

data WWBConfig k v = WWBConfig
  { backingStore :: !(TVar (HashMap k v))
  , inMemoryStore :: !(TVar (InMemoryStore k v))
  , nextQueryId :: !(TVar Int)
  }
  deriving stock (Generic)

data WWBState k v = WWBState
  deriving stock (Generic)

data WWBResultSet k v = WWBResultSet
  { resultSetId :: !Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToExpr)

flushInMemoryStore :: (Eq k, Hashable k, MonadState (InMemoryStore k v) m) => HashMap k v -> m (HashMap k v)
flushInMemoryStore initial_map = do
  InMemoryEntryMeasure{imeMap} <- gets measure
  put mempty
  let go k v = Endo $ maybe (HashMap.delete k) (HashMap.insert k) v
  pure $ appEndo (HashMap.foldMapWithKey go $ coerce imeMap) initial_map

flush :: (MonadIO m, Eq k, Hashable k) => WWBT k v m ()
flush = WWBT $ do
  WWBConfig{..} <- ask
  liftIO . atomically $ do
    bs <- readTVar backingStore
    new_bs <- stateTVar inMemoryStore . State.runState $ flushInMemoryStore bs
    writeTVar backingStore new_bs



makeInMemoryStore :: (Eq k, Hashable k) => HashMap k v -> HashSet k -> InMemoryStore k v
makeInMemoryStore backing_store = coerce . FingerTree.fromList .
  HashSet.foldr go []
  where go k acc = IME k (k `HashMap.lookup` backing_store) : acc

prepare :: (MonadState (InMemoryStore k v) m, Eq k, Hashable k) => Int -> HashSet k -> HashMap k v -> m ()
prepare i qs backing_store = do
  InMemoryEntryMeasure{imeMap} <- gets measure
  -- TODO check this i is not already in liveQueries
  let
    qs_map = HashSet.toMap qs

    live_ft = FingerTree.singleton $ LiveQuery i qs

    relevant_existing_kvs = imeMap `HashMap.intersection` qs_map
    relevant_ft = FingerTree.fromList
        [ IME k (coerce v) | (k, v) <- HashMap.toList relevant_existing_kvs ]

    new_keys = qs `HashSet.difference` HashMap.keysSet relevant_existing_kvs
    new_ft = makeInMemoryStore backing_store new_keys

  -- the live marker is to the left of the values we've pulled in, so if the
  -- live marker is present it must be that all the values are present
  modify $ \l -> l >< live_ft >< relevant_ft >< new_ft

submit :: (Eq k, Hashable k, MonadState (InMemoryStore k v) m) => WWBResultSet k v -> (HashMap k (Maybe v) -> (OperationResult k v, a))  -> m (Either BaseError a)
submit WWBResultSet{resultSetId} f = runExceptT $ do
  InMemoryEntryMeasure{imeMap, liveQueries} <- gets measure
  qs <- case HashMap.lookup resultSetId liveQueries >>= Semi.getLast  of
    Nothing -> throwError BEBadResultSet
    Just qs -> pure qs

  let (updates, r) = f $ coerce imeMap `HashMap.intersection` HashSet.toMap qs
      go k v acc = IME k v' : acc where v' = case v of
                                          DIRemove -> Nothing
                                          DIUpdate x -> Just . coerce $ x
      updateFingerTree = FingerTree.fromList $ HashMap.foldrWithKey go
        [CompletedQuery resultSetId]
        updates
  modify $ \l -> l FingerTree.>< updateFingerTree
  pure $ r


runWWBT :: (MonadIO m, Eq k, Hashable k) => WWBT k v m a -> HashMap k v -> m (HashMap k v, a)
runWWBT (WWBT m) initial_map = do
  -- let
  --   prepare_queue_depth = 100
  --   submit_queue_depth = 100
  c@WWBConfig{..} <- wwbConfigIO initial_map

  a <- runReaderT m c
  liftIO $ atomically $ do
    bs <- readTVar backingStore
    final_map <- stateTVar inMemoryStore . State.runState $ flushInMemoryStore bs
    pure (final_map, a)

runWWBTWithConfig :: WWBT k v m a -> WWBConfig k v -> m a
runWWBTWithConfig (WWBT m) = runReaderT m

wwbConfigIO :: (Eq k, Hashable k, MonadIO m) => HashMap k v -> m (WWBConfig k v)
wwbConfigIO m = liftIO $ do
    (backingStore, inMemoryStore, nextQueryId) <- liftA3 (,,)
        (newTVarIO m)
        (newTVarIO mempty)
        (newTVarIO 0)
    pure WWBConfig{..}

resetWWBTIO  :: (Eq k, Hashable k, MonadIO m) => HashMap k v -> WWBConfig k v -> m ()
resetWWBTIO m WWBConfig{..} = liftIO . atomically $ do
        writeTVar backingStore m
        writeTVar inMemoryStore mempty

instance (Eq k, Hashable k, MonadIO m) => MonadKV k v (WWBT k v m) where
  newtype ResultSet (WWBT k v m) = WWBResultSet_ { unWWBResultSet_ :: WWBResultSet  k v }
    deriving newtype (Eq, Show, Generic, ToExpr)

  prepareOperation qs = WWBT $ do
    WWBConfig{..} <- ask
    resultSetId <- liftIO . atomically $ do
        bs <- readTVar backingStore
        qid <- stateTVar nextQueryId . State.runState $ get <* modify' (+1)
        stateTVar inMemoryStore . State.runState $ prepare qid (coerce qs) bs
        pure qid

    pure . coerce $ WWBResultSet {..}


  submitOperation rs f = WWBT $ do
    WWBConfig{..} <- ask
    liftIO . atomically $ stateTVar inMemoryStore . State.runState $ submit (coerce rs) f
