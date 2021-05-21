{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
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

{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -fno-warn-unused-imports #-}

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
module LedgerOnDisk.WWB where

import LedgerOnDisk.Util.RandomTime
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
import Test.QuickCheck
import Control.Concurrent
import Data.Time
import Control.Concurrent.STM.TSem
import Control.Monad.Catch
import Data.IORef
import LedgerOnDisk.Pure
import Data.Maybe


wwbStateTVar :: TVar s -> Strict.State s a -> STM a
wwbStateTVar v = stateTVar v . Strict.runState

newtype WWBT k v m a = WWBT { unWWBT :: ReaderT (WWBConfig k v) m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadFail, MonadTrans, MonadState s)

instance MonadReader r m => MonadReader r (WWBT k v m) where
  ask = lift ask
  local f (WWBT m) = WWBT $ ask >>= lift . local f . runReaderT m

-- data InMemoryEntry k v
--   = IME !k !(Maybe v)

type role MonoidalHashMap nominal representational

newtype MonoidalHashMap k v = MonoidalHashMap { unMonoidalHashMap :: HashMap k v }
  deriving stock (Show)
  deriving newtype (Eq, Arbitrary)

type QueryId = Int

instance (Eq k, Hashable k, Semigroup v) => Semigroup (MonoidalHashMap k v) where
  (MonoidalHashMap x) <> (MonoidalHashMap y) = MonoidalHashMap (HashMap.unionWith (<>) x y)

instance (Eq k, Hashable k, Semigroup v) => Monoid (MonoidalHashMap k v) where
  mempty = MonoidalHashMap mempty

data WriteBufferEntry k v
  = WBEQueryCompleted { qId :: !QueryId, qResult :: !(HashMap k (D v)) }
  | WBEQueryStarted { qId :: !QueryId, qInitDiff :: !(HashMap k (D v)) }
  deriving stock (Eq, Show)

data QueryState k v = QSStarted { qInitDiff :: HashMap k (D v) } | QSCompleted
  deriving stock (Eq, Show, Generic)

instance (Eq k, Hashable k, Arbitrary k, Arbitrary v) => Arbitrary (QueryState k v) where
  arbitrary = oneof [ QSStarted <$> arbitrary, pure QSCompleted ]

data WriteBufferMeasure k v = WriteBufferMeasure
  { wbmSummary :: !(HashMap k (D v))
  , wbmIsQueryPending :: !(HashMap QueryId (QueryState k v))
  } deriving stock (Eq, Show, Generic)

instance (Eq k, Hashable k, Arbitrary k, Arbitrary v) => Arbitrary (WriteBufferMeasure k v) where
  arbitrary = WriteBufferMeasure <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance (Eq k, Hashable k) => Semigroup (WriteBufferMeasure k v) where
  WriteBufferMeasure s1 p1 <> WriteBufferMeasure s2 p2 = let
    s3 :: MonoidalHashMap k (D v)
    s3 = coerce s1 <> coerce s2
    p3 :: MonoidalHashMap QueryId (Semi.Last (QueryState k v))
    p3 = coerce p1 <> coerce p2
    in WriteBufferMeasure (coerce s3) (coerce p3)

emptyWriteBufferMeasure :: (Eq k, Hashable k) => WriteBufferMeasure k v
emptyWriteBufferMeasure = WriteBufferMeasure HashMap.empty HashMap.empty

instance (Eq k, Hashable k) => Monoid (WriteBufferMeasure k v) where
  mempty = emptyWriteBufferMeasure

instance (Eq k, Hashable k) => Measured (WriteBufferMeasure k v) (WriteBufferEntry k v) where
  measure = \case
    WBEQueryCompleted{qId, qResult} -> WriteBufferMeasure
      { wbmSummary = qResult
      , wbmIsQueryPending = HashMap.singleton qId QSCompleted
      }
    WBEQueryStarted{qId, qInitDiff} -> WriteBufferMeasure
      { wbmSummary = HashMap.empty
      , wbmIsQueryPending = HashMap.singleton qId QSStarted {qInitDiff}
      }

type WriteBuffer k v = FingerTree (WriteBufferMeasure k v) (WriteBufferEntry k v)

-- TODO Hide the constructor
data SubmissionToken = SubmissionToken


data WWBReadSet k v = WWBReadSet
  { rsId :: !QueryId
  , rsFetchAsync :: !(Async (HashMap k (Maybe v)))
  , tokenMV :: !(MVar SubmissionToken)
  }
  deriving stock (Eq, Generic)

instance ToExpr (WWBReadSet k v) where
  toExpr WWBReadSet{rsId} = toExpr rsId

instance Show (WWBReadSet k v) where
  showsPrec d WWBReadSet{rsId} = showsPrec d rsId

data WWBSubmission k v = forall a. WWBSubmission
  { sReadSet :: !(WWBReadSet k v)
  , sOp :: !(KVOperation k v a)
  , sResultTMV :: !(TMVar (Either (WWBErr k v) a))
  }

data WWBConfig k v = WWBConfig
  { backingStoreMV :: !(MVar (HashMap k v))
  , writeBufferTV :: !(TVar (WriteBuffer k v))
  , nextQueryIdTV :: !(TVar Int)
  , submissionQueueRef :: !(TBQueue (WWBSubmission k v))
  -- , tickets :: !TSem
  , randomQueryDelayRange :: !(NominalDiffTime, NominalDiffTime)
  , loopAsync :: !(Async ())
  }
  deriving stock (Generic)

wwbLoop :: (MonadIO m, Eq k, Hashable k) => TVar (WriteBuffer k v) -> TBQueue (WWBSubmission k v) -> m ()
wwbLoop writeBufferTV submissionQueueRef = liftIO $ forever $ do
  (WWBSubmission{sOp, sReadSet = WWBReadSet{rsId}, sResultTMV}, fetch_results, wb) <- atomically $ do
    s@WWBSubmission{sReadSet = WWBReadSet{rsFetchAsync}} <- readTBQueue submissionQueueRef
    -- TODO think about exceptions
    fetch_results <- waitSTM rsFetchAsync
    wb <- readTVar writeBufferTV
    pure (s, fetch_results, wb)

  let
    WriteBufferMeasure
      { wbmSummary = coerce -> wbmSummary
      , wbmIsQueryPending = coerce -> iqp
      } = measure wb

    Just (Semi.Last (QSStarted{qInitDiff})) = rsId `HashMap.lookup` iqp
    (qResult, a) = sOp $ applyDtoHashMaybeMap (unMonoidalHashMap $ MonoidalHashMap qInitDiff <> wbmSummary) fetch_results

  -- traceM $ "qResult: " <> show qResult
  atomically $ do
    wwbStateTVar writeBufferTV $ do
      -- TODO this should never wait, perhaps check that it doesn't. Is TMVar the right primitive?
      -- sending result doesn't need to be atomic with updating the fingertree
      modify (<> FingerTree.singleton WBEQueryCompleted {qId = rsId, qResult})
    putTMVar sResultTMV (Right a)

wwbConfigIO :: (MonadIO m, Eq k, Hashable k) => HashMap k v -> (NominalDiffTime, NominalDiffTime) -> m (WWBConfig k v)
wwbConfigIO initial_map randomQueryDelayRange = liftIO $ do
  backingStoreMV <- newMVar initial_map
  writeBufferTV <- newTVarIO mempty
  nextQueryIdTV <- newTVarIO 0
  submissionQueueRef <- newTBQueueIO 10
  loopAsync <- async $ wwbLoop writeBufferTV submissionQueueRef
  pure $ WWBConfig{..}

closeWWbConfig :: (MonadThrow m, MonadIO m) => WWBConfig k v -> m ()
closeWWbConfig WWBConfig{loopAsync} = liftIO $ do
  cancel loopAsync
  waitCatch loopAsync >>= \case
    Left (fromException -> Just AsyncCancelled) -> pure ()
    Left e -> throwM  e
    Right () -> pure ()

withWWBConfig :: (MonadIO m, MonadMask m, Eq k, Hashable k) => HashMap k v -> (NominalDiffTime, NominalDiffTime) -> (WWBConfig k v -> m a) -> m a
withWWBConfig hm range f = bracket (wwbConfigIO hm range) closeWWbConfig f

runWWBT :: (Eq k, Hashable k, MonadIO m, MonadMask m) => WWBT k v m a -> HashMap k v -> (NominalDiffTime, NominalDiffTime) -> m a
runWWBT m hm range = do
  cfg <- wwbConfigIO hm range
  runWWBTWithConfig m cfg

runWWBTWithConfig :: (Eq k, Hashable k, MonadMask m, MonadIO m) => WWBT k v m a -> WWBConfig k v -> m a
runWWBTWithConfig (WWBT m) cfg = runReaderT m cfg

prepare :: (MonadIO m, MonadReader (WWBConfig k v) m, Eq k, Hashable k) => HashSet k -> m (WWBReadSet k v)
prepare qs = ask >>= \WWBConfig{backingStoreMV, nextQueryIdTV, writeBufferTV, randomQueryDelayRange} -> liftIO $ do
  rsId <- atomically $ do
    qId <- wwbStateTVar nextQueryIdTV $ get <* modify' (+1)
    wwbStateTVar writeBufferTV $ do
      WriteBufferMeasure{wbmSummary = coerce -> qInitDiff} <- gets measure
      modify' (<> FingerTree.singleton (WBEQueryStarted{..}))
    pure qId
  rsFetchAsync <- async $ simulateQuery randomQueryDelayRange qs (readMVar backingStoreMV)
  tokenMV <- newMVar SubmissionToken
  pure WWBReadSet{..}

submit :: (MonadReader (WWBConfig k v) m, MonadIO m) => WWBReadSet k v -> KVOperation k v a -> m (Either (WWBErr k v) a)
submit sReadSet@WWBReadSet{tokenMV} sOp = ask >>= \WWBConfig{submissionQueueRef} -> liftIO . runExceptT $ do
  lift (tryTakeMVar tokenMV) >>= \case
    Nothing -> throwError $ WWBEBase BEBadReadSet
    Just _ -> pure ()
  ExceptT $ do
    sResultTMV <- newEmptyTMVarIO
    atomically $ writeTBQueue submissionQueueRef $ WWBSubmission {..}
    atomically $ readTMVar sResultTMV

data ReadSetError = forall e. Exception e => ReadSetException e


cancelReadSet :: MonadIO m => WWBReadSet k v -> m ()
cancelReadSet WWBReadSet{rsFetchAsync} = liftIO $ cancel rsFetchAsync

waitReadSet :: WWBReadSet k v -> STM (Maybe ReadSetError)
waitReadSet WWBReadSet{rsFetchAsync} = waitCatchSTM rsFetchAsync <&> either (Just . ReadSetException) (const Nothing)

waitReadSetIO :: MonadIO m => WWBReadSet k v -> m (Maybe ReadSetError)
waitReadSetIO = liftIO . atomically . waitReadSet

pollReadSet :: WWBReadSet k v -> STM Bool
pollReadSet WWBReadSet{rsFetchAsync} = pollSTM rsFetchAsync <&> isJust

pollReadSetIO :: MonadIO m => WWBReadSet k v -> m Bool
pollReadSetIO = liftIO . atomically . pollReadSet

-- nextReadSetId :: MonadState Int m => m (Int)
-- nextReadSetId = gets <* modify (+1)

-- writeBufferSuffixForQuery :: QueryId -> WriteBuffer k v -> WriteBuffer k v
-- writeBufferSuffixForQuery qid = FingerTree.dropUntil pred where
--   -- TODO not sure about this, is it monotone, why doesn't it care about the value of the query state ?
--   -- we assume here that there is at most one "QueryStarted" event for the query
--   pred WriteBufferMeasure{wbmIsQueryPending} = HashMap.lookup (coerce wbmIsQueryPending) qid == Nothing

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

data WWBErr k v = WWBEBase BaseError | WWBEExpiredReadSet | WWBEWeird String
  deriving stock (Eq, Show, Generic)

-- | WARNING: This instance upholds no invariants
-- instance Arbitrary (WWBErr k v) where
--   arbitrary = oneof
--     [ WWBEBase <$> arbitrary
--     , pure WWBEExpiredReadSet
--     , pure $ WWBEWeird "weird"
--     ]
--   shrink = genericShrink


instance (Eq k, Hashable k, MonadIO m) => MonadKV k v (WWBT k v m) where
  -- This awkward thing is because most functions above don't use this m, they
  -- use various MonadState constraints. So they operate on WWBReadSet.
  -- Perhaps there is a better way?
  newtype ReadSet (WWBT k v m) = WWBReadSet_ { unWWBReadSet_ :: WWBReadSet  k v }
    deriving newtype (Eq, Show, Generic, ToExpr)

  type Err (WWBT k v m) = WWBErr k v
  fromKVBaseError _ = WWBEBase
  toKVBaseError _ = \case
    WWBEBase e -> Just e
    _ -> Nothing

  prepareOperation = WWBT . fmap coerce . prepare . coerce
  submitOperation (coerce -> rs@WWBReadSet{}) = WWBT . submit rs
