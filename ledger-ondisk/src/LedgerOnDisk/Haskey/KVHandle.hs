{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ViewPatterns #-}
module LedgerOnDisk.Haskey.KVHandle
  ( inMemoryBackend
  , filestoreBackend
  , HaskeyBackend
  , withKVHandle
  , KVHandle
  , proxyConstraint
  )

where

import Data.Kind
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Monad.State.Strict
import Control.Monad.Catch
import Data.Proxy
import Data.Hashable
import Control.Tracer

-- import Data.HashMap.Strict (HashMap)
-- import qualified Data.HashMap.Strict as HashMap

import LedgerOnDisk.KVHandle.Class
import LedgerOnDisk.Mapping.PTMap
import LedgerOnDisk.Diff

import qualified Database.Haskey.Alloc.Concurrent as Haskey
import qualified Data.BTree.Impure as Haskey
import qualified Database.Haskey.Store.InMemory as Haskey
import qualified Database.Haskey.Store.File as Haskey
import qualified Data.BTree.Alloc as Haskey
import qualified Data.BTree.Primitives as Haskey
import Control.Applicative
import Control.Concurrent
import qualified Data.Map.Strict as Map
import Data.Map (Map)

class (Haskey.Key k, Haskey.Value v, Hashable k) => HaskeyDBKVConstraint k v
instance (Haskey.Key k, Haskey.Value v, Hashable k) => HaskeyDBKVConstraint k v

proxyConstraint :: Proxy HaskeyDBKVConstraint
proxyConstraint = Proxy

type HaskeyOnDiskMappings state =
  ( HasConstrainedOnDiskMappings HaskeyDBKVConstraint state
  , Haskey.Root (OnDiskMappings state Haskey.Tree)
  )

runStateTVar :: TVar a -> State a b -> STM b
runStateTVar v = stateTVar v . runState

type QueryId = Int
data HaskeyTrace
  = HT_Prepare HaskeyTracePrepare
  | HT_Open HaskeyTraceOpen
  | HT_Submit HaskeyTraceSubmit
  deriving stock (Show)

data HaskeyBackend
  = HBMemory
    { memConfig :: !(Haskey.MemoryStoreConfig)
    , memFiles :: !(Haskey.MemoryFiles FilePath)
    }
  | HBFile
    { fileConfig :: !(Haskey.FileStoreConfig)
    }

data KVHandle (state :: (Type -> Type -> Type) -> Type) = KVHandle
  { tracer :: !(Tracer IO HaskeyTrace)
  , nextQueryIdTV :: !(TVar Int)
  , outstandingQueriesTV :: !(TVar Int)
  , numTickets :: !Int -- ^ maximum outstanding queries
  , dbRoot :: !(Haskey.ConcurrentDb (OnDiskMappings state Haskey.Tree))
  , haskeyBackend :: !HaskeyBackend
  , concurrentHandles :: !Haskey.ConcurrentHandles
  , queryTimeoutMS :: !Int
  }


data HaskeyReadSet (root :: (Type -> Type -> Type) -> Type) = HaskeyReadSet
  { rsFetchAsync :: !(Async (OnDiskMappings root Map))
  , rsId :: !QueryId
  }

queryOneTree :: forall m k v. (Haskey.AllocReaderM m, Haskey.Key k, Haskey.Value v, Hashable k)
  =>  Keys k v -> Haskey.Tree k v -> m (Map k v)
queryOneTree (Keys keyset) tree = foldr go (pure Map.empty) keyset
  where
    go k m_map = do
      m <- m_map
      mb_v <- Haskey.lookup k tree
      pure $ maybe id (Map.insert k) mb_v m

doQuery :: forall m state root.
  ( Haskey.AllocReaderM m
  , MonadMask m
  , HaskeyOnDiskMappings state
  , root ~ OnDiskMappings state)
  => root Keys
  -> root Haskey.Tree
  -> m (root Map)
doQuery keys tree = zipMappings proxyConstraint queryOneTree keys tree

haskeyQuery :: forall m root state.
  ( MonadIO m, MonadMask m, Haskey.ConcurrentMetaStoreM m
  , HaskeyOnDiskMappings state
  , root ~ OnDiskMappings state
  )
  => Haskey.ConcurrentDb (root Haskey.Tree) -> root Keys -> m (root Map)
haskeyQuery dbRoot keys = Haskey.transactReadOnly (doQuery keys) dbRoot


haskeyQuerySafeAsync ::
  ( HaskeyOnDiskMappings state
  , root ~ OnDiskMappings state
  )
  => Tracer IO HaskeyTracePrepare -> KVHandle state -> QueryId -> root Keys -> IO (Async (root Map))
haskeyQuerySafeAsync (traceWith -> trace) h@KVHandle{..} rsId keys = do
  started_tmvar <- newEmptyTMVarIO
  let timeout_action = do
        threadDelay $ 1000 * queryTimeoutMS
        trace $ HTP_Timedout rsId
      query_action = do
        trace $ HTP_QueryStarted rsId
        r <- runHaskeyBackend haskeyBackend $ haskeyQuery dbRoot keys
        trace $ HTP_QueryComplete rsId
        pure r

  r_async <- async $
    bracket_
      -- If the put succeeds then the decrement in the finally clause is guaranteed to run
      (atomically $ putTMVar started_tmvar ())
      (atomically $ decrementNumOutstandingQueriesSTM h) $ do
        x <- race timeout_action query_action
        case x of
          Left () -> throwM $ userError "timeout"
          Right y -> pure y

  r <- atomically
    $ (Right <$> takeTMVar started_tmvar)
    <|> waitCatchSTM (() <$ r_async)
  case r of
    Right () -> pure r_async -- Either we read from started_tmvar, and so the
                             -- decrement in the async is guaranteed to run,
                             -- or the async completed, in which case the
                             -- decrement has already happened
    Left e -> throwM e       -- async failed before it could write to started_tmvar, decrement hasn't happened

decrementNumOutstandingQueriesSTM :: KVHandle state -> STM ()
decrementNumOutstandingQueriesSTM KVHandle{outstandingQueriesTV} =
  runStateTVar outstandingQueriesTV $ modify (+ (-1))

data HaskeyTracePrepare
  = HTP_Prepare
  | HTP_Preparing !QueryId
  | HTP_Timedout !QueryId
  | HTP_QueryStarted !QueryId
  | HTP_QueryComplete !QueryId
  deriving stock (Show)

haskeyPrepare ::
  ( HaskeyOnDiskMappings state
  )
  => KVHandle state -> OnDiskMappings state Keys -> IO (HaskeyReadSet state)
haskeyPrepare h@KVHandle{..} qs = do
  let
    my_tracer = contramap HT_Prepare tracer
    trace = traceWith my_tracer
  trace HTP_Prepare
  let
    initQuery = atomically $ do
        num_outstanding_queries <- readTVar outstandingQueriesTV
        guard $ num_outstanding_queries < numTickets
        qId <- runStateTVar nextQueryIdTV $ get <* modify (+1)
        pure qId

  -- we are responsible for the decrement if and only if initQuery succeeds and
  -- haskeyQuerySafeAsync throws
  (rsId, rsFetchAsync) <- bracketOnError
    initQuery
    (\_ -> atomically $ decrementNumOutstandingQueriesSTM h)
    (\qId -> (qId,) <$> haskeyQuerySafeAsync my_tracer h qId qs)
  trace $ HTP_Preparing rsId
  pure HaskeyReadSet{..}

updateOneTree :: forall m k v. (Haskey.AllocM m, Haskey.Key k, Haskey.Value v)
  =>  DiffMap k v -> Haskey.Tree k v -> m (Haskey.Tree k v)
updateOneTree  (DiffMap diffmap) tree = Map.foldlWithKey' go (pure tree) diffmap
  where
    go mtree k v = case v of
      DNoChange -> mtree
      DRemove -> Haskey.delete k =<< mtree
      DChangeTo x -> Haskey.insert k x =<< mtree
      DMappend x -> do
        t <- mtree
        mb_v <- Haskey.lookup k t
        Haskey.insert k (maybe x (<> x) mb_v) t

doUpdate :: forall m state root.
  ( Haskey.AllocM m
  , MonadMask m
  , HaskeyOnDiskMappings state
  , root ~ OnDiskMappings state)
  => root DiffMap
  -> root Haskey.Tree
  -> m (root Haskey.Tree)
doUpdate diff tree = zipMappings proxyConstraint updateOneTree diff tree

data HaskeyTraceSubmit
  = HTS_Submit QueryId
  | HTS_Running QueryId
  | HTS_Complete QueryId
  deriving stock (Show)

haskeySubmit :: forall a state root.
  ( root ~ OnDiskMappings state
  , HaskeyOnDiskMappings state
  )
  => KVHandle state
  -> HaskeyReadSet state
  -> (OnDiskMappings state PTMap -> (a, OnDiskMappings state DiffMap))
  -> IO a
haskeySubmit KVHandle{..} HaskeyReadSet{..} op = do
  let trace = traceWith (contramap HT_Submit tracer)
  -- TODO need to catch and handle error here
  trace $ HTS_Submit rsId
  query_result0 <- wait rsFetchAsync
  trace $ HTS_Running rsId
  let
    query_result1 = mapMappings proxyConstraint ptMapFromMap query_result0
    (a, diff_map) = op query_result1
  runHaskeyBackend haskeyBackend $ Haskey.transact_ (\x -> doUpdate diff_map x >>= Haskey.commit_) dbRoot
  trace $ HTS_Complete rsId
  pure a

instance forall (state :: (Type -> Type -> Type) -> Type).
  ( HaskeyOnDiskMappings state
  , Haskey.Root (OnDiskMappings state Haskey.Tree))
  => DB state (KVHandle state) where
  type ReadSet state (KVHandle state) = HaskeyReadSet state
  type DBKVConstraint state = HaskeyDBKVConstraint

  prepare = haskeyPrepare
  submit = haskeySubmit

nullTree :: HaskeyOnDiskMappings state => OnDiskMappings state Haskey.Tree
nullTree = mapMappings proxyConstraint (\NullMap -> Haskey.empty) nullMap

runHaskeyBackend :: (MonadMask m, MonadIO m) =>
  HaskeyBackend
  -> (forall n. (MonadMask n, MonadIO n, Haskey.ConcurrentMetaStoreM n) => n a)
  -> m a
runHaskeyBackend be m = case be of
  HBMemory{..} -> Haskey.runMemoryStoreT m memConfig memFiles
  HBFile{..} -> Haskey.runFileStoreT m fileConfig

inMemoryBackend :: Haskey.MemoryStoreConfig  -> IO HaskeyBackend
inMemoryBackend memConfig = do
  memFiles <- Haskey.newEmptyMemoryStore
  pure HBMemory{..}

filestoreBackend :: Haskey.FileStoreConfig -> HaskeyBackend
filestoreBackend = HBFile


data HaskeyTraceOpen
  = HTO_Init FilePath
  | HTO_Created
  | HTO_Opened
  deriving stock (Show)

openKVHandle :: (MonadMask m, MonadIO m, HaskeyOnDiskMappings state)
  => Tracer IO HaskeyTrace -> Int -> HaskeyBackend -> FilePath -> m (KVHandle state)
openKVHandle tracer numTickets haskeyBackend fp = do
  let trace = liftIO . (traceWith $ contramap HT_Open tracer)
  let
    concurrentHandles = Haskey.concurrentHandles fp
    queryTimeoutMS = 100
  trace $ HTO_Init fp
  (dbRoot, created) <- runHaskeyBackend haskeyBackend $ do
    Haskey.openConcurrentDb concurrentHandles >>= \case
      Nothing -> do
        r <- Haskey.createConcurrentDb concurrentHandles nullTree
        pure (r, True)
      Just x -> pure (x, False)

  when created $ trace HTO_Created
  trace HTO_Opened
  (nextQueryIdTV, outstandingQueriesTV) <- liftIO $ liftA2 (,) (newTVarIO 0) (newTVarIO 0)
  pure KVHandle {..}

closeKVHandle :: (MonadMask m, MonadIO m) => KVHandle state -> m ()
closeKVHandle KVHandle{concurrentHandles, haskeyBackend} =
  runHaskeyBackend haskeyBackend $ Haskey.closeConcurrentHandles concurrentHandles

withKVHandle :: (MonadMask m, MonadIO m, HaskeyOnDiskMappings state) => Tracer IO HaskeyTrace -> Int -> HaskeyBackend -> FilePath -> (KVHandle state -> m a) -> m a
withKVHandle tracer n be f = bracket (openKVHandle tracer n be f) closeKVHandle
