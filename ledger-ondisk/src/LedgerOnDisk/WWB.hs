{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
-- |

{-# LANGUAGE ScopedTypeVariables #-}
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
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE InstanceSigs #-}

{-# OPTIONS -fno-warn-unused-imports #-}
module LedgerOnDisk.WWB where

import LedgerOnDisk.Class
import Data.Hashable
import Data.TreeDiff
import GHC.Generics hiding (D)
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
import Data.FingerTree (FingerTree, Measured(..))
import Data.Coerce
import qualified Data.HashSet as HashSet
import qualified Data.FingerTree as FingerTree
import Data.HashSet (HashSet)
import Control.Monad.Reader
import qualified Control.Monad.State.Strict as State
import Control.Monad.Except
import Data.Functor
import Data.Function
import Data.Functor.Identity
import Data.Foldable
import Data.Either
import qualified Control.Monad.State as Strict

newtype WWBT k v m a = WWBT { unWWBT :: ReaderT (WWBConfig k v) m a }
  deriving newtype (Functor, Applicative, Monad)

-- TODO QSPreexecuted needs names, one is keys to fetch, one is keys to query. so fetch \subset query

data QueryState k v = QSPreexecuted
  { scope :: !(HashSet k)
  -- ^ All the keys that have been requested
  , fetchKeys :: !(HashSet k)
  -- ^ The keys that must be fetched from the backingStore
  } | QSExecuted !(Either (WWBErr k v) (HashSet k))
  -- ^ Either an error encountered while fetching, or the scope of the query
  | QSRetired
  -- ^ The query has been retired
  deriving (Show, Eq)

data InMemoryEntry k v
  = IME !k !(Maybe v)
  | QueryStateChange !Int !(QueryState k v)

type role MonoidalHashMap nominal representational
newtype MonoidalHashMap k v = MonoidalHashMap { unMonoidalHashMap :: HashMap k v }
  deriving stock (Show)
  deriving newtype (Eq)

instance (Eq k, Hashable k, Semigroup v) => Semigroup (MonoidalHashMap k v) where
  (MonoidalHashMap x) <> (MonoidalHashMap y) = MonoidalHashMap (HashMap.unionWith (<>) x y)

instance (Eq k, Hashable k, Semigroup v) => Monoid (MonoidalHashMap k v) where
  mempty = MonoidalHashMap mempty

data InMemoryEntryMeasure k v = InMemoryEntryMeasure
  { imeMap :: !(HashMap k (Maybe v))
  , liveQueries :: !(HashMap Int (QueryState k v))
  }
  deriving stock (Show, Eq)

measureInMemoryEntries :: (Eq k, Hashable k, Foldable f) => f (InMemoryEntry k v) -> InMemoryEntryMeasure k v
measureInMemoryEntries f = InMemoryEntryMeasure{..} where
  (flip appEndo mempty -> imeMap, flip appEndo mempty -> liveQueries) = foldMap go . toList $ f
  go = \case
    IME k v -> (Endo $ coerce $ HashMap.insert k v, mempty)
    QueryStateChange i qs -> (mempty, coerce $ HashMap.insert i qs)

instance (Eq k, Hashable k) => Measured (InMemoryEntryMeasure k v) (InMemoryEntry k v) where
  measure = measureInMemoryEntries . Identity

instance  (Eq k, Hashable k) => Semigroup (InMemoryEntryMeasure k v) where
  InMemoryEntryMeasure il ll <> InMemoryEntryMeasure ir lr  = InMemoryEntryMeasure
    { imeMap = coerce imeMap'
    , liveQueries = coerce liveQueries'}
    where
    imeMap' :: MonoidalHashMap k (Semi.Last (Maybe v))
    imeMap' = coerce il <> coerce ir
    liveQueries' :: MonoidalHashMap Int (Semi.Last (QueryState k v))
    liveQueries' = coerce ll <> coerce lr

instance (Eq k, Hashable k) => Monoid (InMemoryEntryMeasure k v) where
  mempty = InMemoryEntryMeasure mempty mempty

type InMemoryStore k v = FingerTree (InMemoryEntryMeasure k v) (InMemoryEntry k v)

data WWBFlushPolicy = FPNever | FPAll | FPMaxWidth !Int

data WWBConfig k v = WWBConfig
  { backingStore :: !(TVar (HashMap k v))
  , inMemoryStore :: !(TVar (InMemoryStore k v))
  , nextQueryId :: !(TVar Int)
  , queryOnPrepare :: !Bool
  , flushPolicy :: !WWBFlushPolicy
  }
  deriving stock (Generic)

data WWBState k v = WWBState
  deriving stock (Generic)

newtype WWBResultSet k v = WWBResultSet
  { resultSetId :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToExpr)

nextResultSet :: MonadState Int m => m (WWBResultSet k v)
nextResultSet = gets coerce <* modify (+1)

flushInMemoryStore :: (Eq k, Hashable k, MonadState (InMemoryStore k v) m) => WWBFlushPolicy -> Int -> m (HashMap k (D v))
flushInMemoryStore flushPolicy about_to_add = do
    to_flush_map <- do
      case flushPolicy of
        FPNever -> pure mempty
        FPAll -> gets (imeMap . measure) <* put mempty
        FPMaxWidth w -> do
          let go_split InMemoryEntryMeasure{imeMap} = length imeMap - about_to_add > w
          (to_split_ft, new_ims_ft) <- gets $ FingerTree.split go_split
          put new_ims_ft $> (imeMap . measure $ to_split_ft)

    pure $ HashMap.map (maybe DRemove DChangeTo) to_flush_map

flush :: (MonadIO m, Eq k, Hashable k) => WWBT k v m ()
flush = WWBT $ do
  WWBConfig{..} <- ask
  liftIO . atomically $ do
    bs_d <- wwbStateTVar inMemoryStore $ flushInMemoryStore flushPolicy 0
    wwbStateTVar backingStore $ modify' $ applyDtoHashMap bs_d

makeInMemoryStore :: (Eq k, Hashable k) => HashMap k v -> HashSet k -> InMemoryStore k v
makeInMemoryStore backing_store = coerce . FingerTree.fromList .
  HashSet.foldr go []
  where go k acc = IME k (k `HashMap.lookup` backing_store) : acc

-- Do this before
markLiveQuery :: (MonadState (InMemoryStore k v) m, Eq k, Hashable k) => WWBResultSet k v -> HashSet k -> m ()
markLiveQuery WWBResultSet{resultSetId} scope =  do
  InMemoryEntryMeasure{imeMap} <- gets measure
  let
    scope_map = HashSet.toMap scope
    relevant_existing_kvs = imeMap `HashMap.intersection` scope_map
    relevant_ft = FingerTree.fromList
        [ IME k v | (k, v) <- HashMap.toList relevant_existing_kvs ]

    fetchKeys = scope `HashSet.difference` HashMap.keysSet relevant_existing_kvs
    live_ft = FingerTree.singleton $ QueryStateChange resultSetId $ QSPreexecuted {..}

  -- the live marker is to the left of the values we've pulled in, so if the
  -- live marker is present it must be that all the values are present
  -- TODO we could remove the elements of  relevent_ft from the left of the tree
  modify' (<> (live_ft <> relevant_ft))

executeQuery :: (MonadState (InMemoryStore k v) m, Eq k, Hashable k) => WWBResultSet k v -> WWBFlushPolicy -> HashMap k v -> m ()
executeQuery WWBResultSet{resultSetId} _flush_policy backing_store = do
  InMemoryEntryMeasure{imeMap, liveQueries} <- gets measure
  -- TODO check this i is not already in liveQueries
  r <- runExceptT $ do
    -- TODO
    (scope, fetchKeys) <- case resultSetId `HashMap.lookup` liveQueries of
      Nothing -> throwError . WWBEBase $ BEBadResultSet
      Just qs -> case qs of
        QSPreexecuted {..} ->
          -- fetchKeys was correct when it was computed, but in the meantime we
          -- may have fetched some of these keys for other queries
          -- So we remove any that exist in the In Memory Srore
          pure (scope, fetchKeys `HashSet.difference` HashMap.keysSet imeMap)
        QSExecuted (Left e) -> throwError e
        QSExecuted Right{} -> throwError . WWBEBase $ BEBadResultSet-- TODO distinguish error
        QSRetired -> throwError . WWBEBase $ BEBadResultSet-- TODO distinguish error
    let
      go k acc = IME k (k `HashMap.lookup` backing_store) : acc

    -- TODO I think this is the right place to call flush. It's inconvenient
    -- right now, because backing_store is pure

    pure . FingerTree.fromList $ (QueryStateChange resultSetId . QSExecuted . Right $ scope) :
      foldr go [] fetchKeys

  let ft_update = either (FingerTree.singleton . QueryStateChange resultSetId . QSExecuted . Left) id r
  modify (<> ft_update)

submit :: (Eq k, Hashable k, MonadState (InMemoryStore k v) m) => WWBResultSet k v -> (HashMap k (Maybe v) -> (KVOperationResult k v, a))  -> m (Either (WWBErr k v) a)
submit WWBResultSet{resultSetId} f = runExceptT $ do
  InMemoryEntryMeasure{imeMap, liveQueries} <- gets measure
  op_arg <- do
    scope <- HashMap.lookup resultSetId liveQueries & \case
      Nothing -> throwError . WWBEBase $ BEBadResultSet
      Just QSPreexecuted {} -> throwError . WWBEBase $ BEBadResultSet -- TODO distinguish error
      Just (QSExecuted r) -> either throwError pure r
      Just QSRetired -> throwError . WWBEBase $ BEBadResultSet
    let op_arg = imeMap `HashMap.intersection` HashSet.toMap scope
    when (length op_arg /= length scope) $ do
      -- we have arranged for everything asked for in the scope to be in imeMap,
      -- and the state of the query, QSExecuted, checked above, should have ensured this
      throwError $ WWBEWeird "Scope unsatisfied"
    pure op_arg

  -- Do it, call the function!
  let (updates, r) = f op_arg
      go k d acc = case d of
        DRemove -> IME k Nothing : acc
        DChangeTo v -> IME k (Just v) : acc
        DNoChange -> acc
      updateFingerTree = FingerTree.fromList $ QueryStateChange resultSetId QSRetired : HashMap.foldrWithKey go [] updates
  modify $ \l -> l <> updateFingerTree
  pure r


runWWBT :: (MonadIO m, Eq k, Hashable k) => WWBT k v m a -> Bool -> WWBFlushPolicy -> HashMap k v -> m (HashMap k v, a)
runWWBT (WWBT m) query_on_prepare flush_policy initial_map = do
  -- let
  --   prepare_queue_depth = 100
  --   submit_queue_depth = 100
  c@WWBConfig{..} <- wwbConfigIO query_on_prepare flush_policy initial_map

  a <- runReaderT m c
  liftIO $ atomically $ do
    nearly_final_map <- readTVar backingStore
    final_flush_d <- stateTVar inMemoryStore . State.runState $ flushInMemoryStore FPAll 0
    pure (applyDtoHashMap final_flush_d nearly_final_map, a)

runWWBTWithConfig :: WWBT k v m a -> WWBConfig k v -> m a
runWWBTWithConfig (WWBT m) = runReaderT m

wwbConfigIO :: (Eq k, Hashable k, MonadIO m) => Bool -> WWBFlushPolicy -> HashMap k v -> m (WWBConfig k v)
wwbConfigIO queryOnPrepare flushPolicy m = liftIO $ do
    (backingStore, inMemoryStore, nextQueryId) <- liftA3 (,,)
        (newTVarIO m)
        (newTVarIO mempty)
        (newTVarIO 0)
    pure WWBConfig{..}

resetWWBTIO  :: (Eq k, Hashable k, MonadIO m) => HashMap k v -> WWBConfig k v -> m ()
resetWWBTIO m WWBConfig{..} = liftIO . atomically $ do
        writeTVar backingStore m
        writeTVar inMemoryStore mempty
        writeTVar nextQueryId 0


data WWBErr k v = WWBEBase BaseError | WWBEExpiredResultSet | WWBEWeird String
  deriving stock (Eq, Show, Generic)

wwbStateTVar :: TVar s -> Strict.State s a -> STM a
wwbStateTVar v = stateTVar v . Strict.runState

instance (Eq k, Hashable k, MonadIO m) => MonadKV k v (WWBT k v m) where
  -- This awkward thing is because most functions above don't use this m, they
  -- use various MonadState constraints. So they operate on WWBResultSet.
  -- Perhaps there is a better way?
  newtype ResultSet (WWBT k v m) = WWBResultSet_ { unWWBResultSet_ :: WWBResultSet  k v }
    deriving newtype (Eq, Show, Generic, ToExpr)
  type Err (WWBT k v m) = WWBErr k v
  fromKVBaseError _ = WWBEBase
  toKVBaseError _ = \case
    WWBEBase e -> Just e
    _ -> Nothing

  prepareOperation qs = WWBT $ do
    WWBConfig{..} <- ask
    liftIO . atomically $ do
        bs <- readTVar backingStore
        rs <- wwbStateTVar nextQueryId nextResultSet
        mb_bs_diff <- wwbStateTVar inMemoryStore $ do
          markLiveQuery rs $ coerce qs
          if queryOnPrepare
          then do
            executeQuery rs flushPolicy bs
            Just <$> flushInMemoryStore flushPolicy 0
          else pure Nothing
        for_ mb_bs_diff $ \d -> wwbStateTVar backingStore . modify' $ applyDtoHashMap d
        pure . coerce $ rs

  submitOperation (coerce -> rs@WWBResultSet{}) f = WWBT $ do
    WWBConfig{..} <- ask
    liftIO . atomically $ do
      unless queryOnPrepare $ do
        bs <- readTVar backingStore
        mb_bs_diff <- wwbStateTVar inMemoryStore $ do
          executeQuery rs flushPolicy bs
          Just <$> flushInMemoryStore flushPolicy 0
        for_ mb_bs_diff $ \d -> wwbStateTVar backingStore . modify' $ applyDtoHashMap d
      wwbStateTVar inMemoryStore $ submit rs f
