{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS -fno-warn-unused-imports #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module LedgerOnDisk.WWB where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue
import Control.Concurrent.STM.TSem
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.RWS.Strict
import Control.Monad.Reader
import qualified Control.Monad.State as Strict
import qualified Control.Monad.State.Strict as State
import Control.Tracer
import Data.Bifunctor
import Data.Coerce
import Data.Either
import Data.FingerTree (FingerTree, Measured (..))
import qualified Data.FingerTree as FingerTree
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Functor.Identity
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable
import Data.IORef
import Data.Maybe
import Data.PriorityQueue.FingerTree
import qualified Data.Semigroup as Semi
import Data.Time
import Data.TreeDiff
import GHC.Generics hiding (D)
import LedgerOnDisk.Class
import LedgerOnDisk.Pure
import LedgerOnDisk.Util
import LedgerOnDisk.Util.RandomTime
import LedgerOnDisk.Util.Trace
import Test.QuickCheck
import Numeric.Natural

wwbStateTVar :: TVar s -> Strict.State s a -> STM a
wwbStateTVar v = stateTVar v . Strict.runState

data WWBTrace = WWBTPrepare WWBPrepareTrace | WWBTSubmit WWBSubmitTrace | WWBTLoop WWBLoopTrace

newtype WWBT k v m a = WWBT {unWWBT :: ReaderT (Tracer m WWBTrace, WWBConfig k v) m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadFail, MonadState s)

instance MonadTrans (WWBT k v) where
  lift = WWBT . lift

instance MonadReader r m => MonadReader r (WWBT k v m) where
  ask = lift ask
  local f (WWBT m) = WWBT $ ask >>= lift . local f . runReaderT m

type role MonoidalHashMap nominal representational

newtype MonoidalHashMap k v = MonoidalHashMap {unMonoidalHashMap :: HashMap k v}
  deriving stock (Show)
  deriving newtype (Eq, Arbitrary)

type QueryId = Int

instance (Eq k, Hashable k, Semigroup v) => Semigroup (MonoidalHashMap k v) where
  (MonoidalHashMap x) <> (MonoidalHashMap y) = MonoidalHashMap (HashMap.unionWith (<>) x y)

instance (Eq k, Hashable k, Semigroup v) => Monoid (MonoidalHashMap k v) where
  mempty = MonoidalHashMap HashMap.empty

data WriteBufferEntry k v
  = WBEQueryCompleted {qId :: !QueryId, qResult :: !(HashMap k (D v))}
  deriving stock (Eq, Show)

data WriteBufferMeasure k v = WriteBufferMeasure
  { wbmSummary :: !(HashMap k (D v))
  , wbmCount :: !Int
  }
  deriving stock (Eq, Show, Generic)
  -- deriving (Semigroup, Monoid) via MonoidalHashMap k (D v)

instance (Eq k, Hashable k) => Semigroup (WriteBufferMeasure k v) where
  WriteBufferMeasure s1 c1 <> WriteBufferMeasure s2 c2 = WriteBufferMeasure (coerce merged_maps) (coerce merged_counts)
    where
      merged_maps :: MonoidalHashMap k (D v)
      merged_maps = coerce s1 <> coerce s2
      merged_counts :: Sum Int
      merged_counts = coerce c1 <> coerce c2

emptyWriteBufferMeasure :: WriteBufferMeasure k v
emptyWriteBufferMeasure  = WriteBufferMeasure HashMap.empty 0

instance (Eq k, Hashable k) => Monoid (WriteBufferMeasure k v) where
  mempty = emptyWriteBufferMeasure

instance (Eq k, Hashable k, Arbitrary k, Arbitrary v) => Arbitrary (WriteBufferMeasure k v) where
  arbitrary = WriteBufferMeasure <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance (Eq k, Hashable k) => Measured (WriteBufferMeasure k v) (WriteBufferEntry k v) where
  measure WBEQueryCompleted {qResult} = WriteBufferMeasure { wbmSummary = qResult, wbmCount = 1}

type WriteBuffer k v = FingerTree (WriteBufferMeasure k v) (WriteBufferEntry k v)

data WWBReadSet k v = WWBReadSet
  { rsId :: !QueryId
  ,  rsFetchAsync :: !(Async (HashMap k (Maybe v)))
  -- , rsWriteBufferSnapshot :: !(WriteBufferMeasure k v)
  }
  deriving stock (Eq, Generic)

instance ToExpr (WWBReadSet k v) where
  toExpr WWBReadSet {rsId} = toExpr rsId

instance Show (WWBReadSet k v) where
  showsPrec d WWBReadSet {rsId} = showsPrec d rsId

data WWBSubmission k v = forall a.
  WWBSubmission
  { sReadSet :: !(WWBReadSet k v),
    sOp :: !(KVOperation k v a),
    sResultMV :: !(MVar (Either (WWBErr k v) a))
  }

data WWBConfig k v = WWBConfig
  { writeBufferTV :: !(TVar (WriteBuffer k v)),
    nextQueryIdTV :: !(TVar Int),
    lastSubmittedQueryIdTV :: !(TVar Int),
    submissionQueueRef :: !(TBQueue (WWBSubmission k v)),

    loopAsync :: !(Async ()),

    backingStoreMV :: !(MVar (HashMap k v)),

    randomQueryDelayRange :: !(NominalDiffTime, NominalDiffTime),
    numTickets :: !Natural
  }
  deriving stock (Generic)

data WWBLoopTrace
  = WWBLTReceivedSubmission QueryId Bool
  | WWBLTCommit
      { tId :: !QueryId,
        tNumMutations :: !Int,
        tNumFlushed :: !Int,
        tWriteBufferSize :: !Int
      }
  deriving stock (Show, Generic)

wwbLoop ::
  (MonadIO m, Eq k, Hashable k) =>
  Tracer IO WWBLoopTrace ->
  WWBConfig k v ->
  m ()
wwbLoop tracer WWBConfig
  { submissionQueueRef
  , writeBufferTV
  , backingStoreMV
  , numTickets
  , lastSubmittedQueryIdTV
  } = liftIO $
  forever $ do
    ( WWBSubmission
      { sOp, sReadSet = WWBReadSet
        { rsId
        -- , rsWriteBufferSnapshot
        , rsFetchAsync
        }
      , sResultMV
      }
     , wb
     , mb_result :: Either (WWBErr k v) (HashMap k (Maybe v))
     ) <- atomically $ do
        s@WWBSubmission{sReadSet = WWBReadSet{rsId, rsFetchAsync}} <- readTBQueue submissionQueueRef
        wb <- readTVar writeBufferTV
        lsqid <- readTVar lastSubmittedQueryIdTV
        mb_r <- runExceptT $ do
          when (lsqid >= rsId) $ throwError WWBEOutOfOrderSubmit
            { ooosLastSubmittedQueryId = lsqid
            , ooosThisQueryId = rsId
            }
          ExceptT . fmap (first $ WWBEReadErr . ReadSetException) $ waitCatchSTM rsFetchAsync
        wwbStateTVar lastSubmittedQueryIdTV $ modify' (rsId `max`) 
        pure (s, wb, mb_r)


    res <- runExceptT $ do
      let trace = lift . traceWith tracer
      trace $ WWBLTReceivedSubmission rsId (isRight mb_result)
      fetch_results <- liftEither mb_result `catchError`
        (\e -> liftIO (cancel rsFetchAsync) *> throwError e)
      let (!qResult, a) = sOp $ applyDtoHashMaybeMap wbmSummary fetch_results
            where
              WriteBufferMeasure{wbmSummary} = measure wb

      (to_flush, new_size) <- liftIO . atomically $ do
        wwbStateTVar writeBufferTV $ do
          modify' (<> FingerTree.singleton WBEQueryCompleted {qId = rsId, qResult})
          ft <- get
          let
            len_ft = length ft
            nt_i = fromIntegral numTickets
          if len_ft >= 2 * nt_i
          then do
            let
              num_to_flush = len_ft - nt_i
              (measure -> WriteBufferMeasure{wbmSummary}, new_ft) = FingerTree.split go ft
                where
                  go WriteBufferMeasure{wbmCount} = wbmCount < num_to_flush
            put new_ft
            pure (wbmSummary, length new_ft)
          else pure (mempty, len_ft)

      when (length to_flush > 0) $ do
        liftIO . modifyMVar_ backingStoreMV $ pure . applyDtoHashMap to_flush

      trace WWBLTCommit
            { tId = rsId,
              tNumMutations = length qResult,
              tNumFlushed = length to_flush,
              tWriteBufferSize = new_size
            }
      pure a
    putMVar sResultMV res

wwbConfigIO :: (MonadIO m, Eq k, Hashable k) => HashMap k v -> Natural -> Tracer IO WWBTrace -> (NominalDiffTime, NominalDiffTime) -> m (Tracer m WWBTrace, WWBConfig k v)
wwbConfigIO initial_map numTickets tracer0 randomQueryDelayRange = liftIO $ do
  let tracer = natTracer liftIO tracer0
  backingStoreMV <- newMVar initial_map
  writeBufferTV <- newTVarIO mempty
  nextQueryIdTV <- newTVarIO 0
  lastSubmittedQueryIdTV <- newTVarIO (-1)
  submissionQueueRef <- newTBQueueIO numTickets
  mdo
    let cfg = WWBConfig {..}
    loopAsync <- async $ wwbLoop (contramap WWBTLoop tracer0) cfg
    pure $ (tracer, cfg)

closeWWbConfig :: (MonadThrow m, MonadIO m) => WWBConfig k v -> m ()
closeWWbConfig WWBConfig {loopAsync} = liftIO $ do
  cancel loopAsync
  waitCatch loopAsync >>= \case
    Left (fromException -> Just AsyncCancelled) -> pure ()
    Left e -> throwM e
    Right () -> pure ()

withWWBConfig :: (MonadIO m, MonadMask m, Eq k, Hashable k) => HashMap k v -> Natural -> Tracer IO WWBTrace -> (NominalDiffTime, NominalDiffTime) -> ((Tracer m WWBTrace, WWBConfig k v) -> m a) -> m a
withWWBConfig hm num_tickets tracer range f = bracket (wwbConfigIO hm num_tickets tracer range) (closeWWbConfig . snd) f

runWWBT ::
  (Eq k, Hashable k, MonadIO m, MonadMask m) =>
  WWBT k v m a ->
  Tracer IO WWBTrace ->
  HashMap k v ->
  Natural ->
  (NominalDiffTime, NominalDiffTime) ->
  m a
runWWBT m tracer0 hm num_tickets range = do
  withWWBConfig hm num_tickets tracer0 range $ \(tracer, cfg) -> runWWBTWithConfig m tracer cfg

runWWBTWithConfig :: (Eq k, Hashable k, MonadMask m, MonadIO m) => WWBT k v m a -> Tracer m WWBTrace -> WWBConfig k v -> m a
runWWBTWithConfig (WWBT m) tracer cfg = runReaderT m (tracer, cfg)

data WWBPrepareTrace = WWBPPrepared
  { tId :: !QueryId,
    tQuerySize :: !Int
  }

prepare ::
  (MonadIO m, Eq k, Hashable k) =>
  Tracer m WWBPrepareTrace ->
  WWBConfig k v ->
  HashSet k ->
  m (WWBReadSet k v)
prepare
  tracer
  WWBConfig
    { backingStoreMV,
      nextQueryIdTV,
      writeBufferTV,
      randomQueryDelayRange,
      numTickets,
      lastSubmittedQueryIdTV
    }
  qs = do
    (rsId, _rsWriteBufferSnapshot) <- liftIO . atomically $ do
      qId <- wwbStateTVar nextQueryIdTV $ get <* modify' (+ 1)
      lastSubmittedId <- readTVar lastSubmittedQueryIdTV

      -- This guard ensures we can only ever have numTickets outstanding queries
      guard $ lastSubmittedId + fromIntegral numTickets > qId

      WriteBufferMeasure {wbmSummary} <- measure <$> readTVar writeBufferTV
      pure (qId, wbmSummary)

    rsFetchAsync <- liftIO . async $ simulateQuery randomQueryDelayRange qs (readMVar backingStoreMV)
    traceWith tracer $ WWBPPrepared rsId (length qs)
    pure WWBReadSet {..}

data WWBSubmitTrace = WWBSSubmitted !QueryId !Bool | WWBSComplete !QueryId !Bool

submit ::
  (MonadIO m) =>
  Tracer m WWBSubmitTrace ->
  WWBConfig k v ->
  WWBReadSet k v ->
  KVOperation k v a ->
  m (Either (WWBErr k v) a)
submit tracer WWBConfig{submissionQueueRef} sReadSet@WWBReadSet {rsId} sOp = do
    sResultMV <- liftIO newEmptyMVar
    liftIO . atomically . writeTBQueue submissionQueueRef $ WWBSubmission {..}
    traceWith tracer $ WWBSSubmitted rsId True
    r <- liftIO $ takeMVar sResultMV
    traceWith tracer $ WWBSComplete rsId (isRight r)
    pure r

data ReadSetError = ReadSetException SomeException
  deriving stock (Show)

instance Eq ReadSetError where
  ReadSetException x == ReadSetException y = show x == show y

cancelReadSet :: MonadIO m => WWBReadSet k v -> m ()
cancelReadSet WWBReadSet {rsFetchAsync} = liftIO $ cancel rsFetchAsync

waitReadSet :: WWBReadSet k v -> STM (Maybe ReadSetError)
waitReadSet WWBReadSet {rsFetchAsync} = waitCatchSTM rsFetchAsync <&> either (Just . ReadSetException) (const Nothing)

waitReadSetIO :: MonadIO m => WWBReadSet k v -> m (Maybe ReadSetError)
waitReadSetIO = liftIO . atomically . waitReadSet

pollReadSet :: WWBReadSet k v -> STM Bool
pollReadSet WWBReadSet {rsFetchAsync} = pollSTM rsFetchAsync <&> isJust

pollReadSetIO :: MonadIO m => WWBReadSet k v -> m Bool
pollReadSetIO = liftIO . atomically . pollReadSet

threadDelay' :: MonadIO m => NominalDiffTime -> m ()
threadDelay' = liftIO . threadDelay . round . (* 100000)

simulateQuery :: (Eq k, Hashable k) => (NominalDiffTime, NominalDiffTime) -> HashSet k -> IO (HashMap k v) -> IO (HashMap k (Maybe v))
simulateQuery range qs read_data = do
  RandomNominalDiffTime total_delay <- randomRIO . coerce $ range
  RandomNominalDiffTime pre_delay <- randomRIO (0, coerce total_delay)
  let post_delay = total_delay - pre_delay
  threadDelay' pre_delay
  s <- read_data
  let !result = HashMap.mapWithKey (\k _ -> k `HashMap.lookup` s) (HashSet.toMap qs)
  threadDelay' post_delay
  pure result

data WWBErr k v = WWBEBase BaseError | WWBEExpiredReadSet | WWBEWeird String | WWBEReadErr ReadSetError |
  WWBEOutOfOrderSubmit
    { ooosThisQueryId :: !QueryId
    , ooosLastSubmittedQueryId :: !QueryId
    }
  deriving stock (Eq, Show, Generic)

instance (Eq k, Hashable k, MonadIO m) => MonadKV k v (WWBT k v m) where
  -- This awkward thing is because most functions above don't use this m, they
  -- use various MonadState constraints. So they operate on WWBReadSet.
  -- Perhaps there is a better way?
  newtype ReadSet (WWBT k v m) = WWBReadSet_ {unWWBReadSet_ :: WWBReadSet k v}
    deriving newtype (Eq, Show, Generic, ToExpr)

  type Err (WWBT k v m) = WWBErr k v
  fromKVBaseError _ = WWBEBase
  toKVBaseError _ = \case
    WWBEBase e -> Just e
    WWBEOutOfOrderSubmit {} -> Just BEBadReadSet
    _ -> Nothing

  prepareOperation qs = WWBT . ReaderT $ \(tracer, cfg) -> do
    res <- prepare (contramap WWBTPrepare tracer) cfg . coerce $ qs
    pure $ coerce res

  submitOperation (coerce -> rs) op = WWBT . ReaderT $ \(tracer, cfg) -> do
    submit (contramap WWBTSubmit tracer) cfg rs op
