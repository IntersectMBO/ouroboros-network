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
import Data.Bifunctor
import Control.Tracer
import LedgerOnDisk.Util.Trace

wwbStateTVar :: TVar s -> Strict.State s a -> STM a
wwbStateTVar v = stateTVar v . Strict.runState

data WWBTrace = WWBTPrepare WWBPrepareTrace | WWBTSubmit WWBSubmitTrace | WWBTLoop WWBLoopTrace

newtype WWBT k v m a = WWBT { unWWBT :: ReaderT (WWBConfig k v m) m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadFail, MonadState s)

instance MonadTrans (WWBT k v) where
  lift = WWBT . lift

instance MonadReader r m => MonadReader r (WWBT k v m) where
  ask = lift ask
  local f (WWBT m) = WWBT $ ask >>= lift . local f . runReaderT m

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
  , sResultMV :: !(MVar (Either (WWBErr k v) a))
  }

data WWBConfig k v m = WWBConfig
  { backingStoreMV :: !(MVar (HashMap k v))
  , writeBufferTV :: !(TVar (WriteBuffer k v))
  , nextQueryIdTV :: !(TVar Int)
  , submissionQueueRef :: !(TBQueue (WWBSubmission k v))
  -- , tickets :: !TSem
  , randomQueryDelayRange :: !(NominalDiffTime, NominalDiffTime)
  , loopAsync :: !(Async ())
  , tracer :: !(Tracer m WWBTrace)
  }
  deriving stock (Generic)

data WWBLoopTrace =
  WWBLTReceivedSubmission QueryId Bool
  | WWBLTCommit
    { tId :: !QueryId
    , tNumMutations :: !Int
    , tNumFlushed :: !Int
    , tWriteBufferSize :: !Int
    }
  deriving stock (Show, Generic)

wwbLoop :: (MonadIO m, Eq k, Hashable k)
  => Tracer IO WWBLoopTrace
  -> MVar (HashMap k v)
  -> TVar (WriteBuffer k v)
  -> TBQueue (WWBSubmission k v)
  -> m ()
wwbLoop tracer backingStoreMV writeBufferTV submissionQueueRef = liftIO $ forever $ do
  (WWBSubmission{sOp, sReadSet = WWBReadSet{rsId}, sResultMV}, e_fetch_results, wb) <- atomically $ do
    s@WWBSubmission{sReadSet = WWBReadSet{rsFetchAsync}} <- readTBQueue submissionQueueRef
    -- TODO think about exceptions
    e_fetch_results <- waitCatchSTM rsFetchAsync
    wb <- readTVar writeBufferTV
    pure (s, first (WWBEReadErr . ReadSetException) e_fetch_results, wb)

  traceWith tracer $ WWBLTReceivedSubmission rsId (isLeft e_fetch_results)

  res <- runExceptT $ do
    fetch_results <- liftEither e_fetch_results
    let
      (!qResult, a) = sOp $ applyDtoHashMaybeMap diff fetch_results
        where
          diff = unMonoidalHashMap $ coerce qInitDiff <> coerce wbmSummary
          WriteBufferMeasure
            { wbmSummary
            , wbmIsQueryPending
            } = measure wb
          Just (QSStarted{qInitDiff}) = rsId `HashMap.lookup` wbmIsQueryPending

    (to_flush, new_size) <- liftIO . atomically $ do
      wwbStateTVar writeBufferTV $ do
        modify (<> FingerTree.singleton WBEQueryCompleted {qId = rsId, qResult})
        ft <- get
        let WriteBufferMeasure{wbmIsQueryPending = current_wiqp} = measure ft
            any_pending WriteBufferMeasure{wbmIsQueryPending} = getAny . HashMap.foldMapWithKey go $ wbmIsQueryPending
              where go k qs
                      | QSStarted {} <- qs
                      , Just (QSStarted {}) <- k `HashMap.lookup` current_wiqp
                      = Any True
                      | otherwise = Any False

            (measure -> WriteBufferMeasure{wbmSummary = to_flush}, new_ft) = FingerTree.split any_pending ft
        put new_ft
        pure (to_flush, length new_ft)
    lift $ traceWith tracer $ WWBLTCommit
      { tId = rsId
      , tNumMutations = length to_flush
      , tNumFlushed = length qResult
      , tWriteBufferSize = new_size
      }
    liftIO . modifyMVar_ backingStoreMV $ pure . applyDtoHashMap to_flush
    pure a
  putMVar sResultMV res


wwbConfigIO :: (MonadIO m, Eq k, Hashable k) => HashMap k v -> Tracer IO WWBTrace -> (NominalDiffTime, NominalDiffTime) -> m (WWBConfig k v m)
wwbConfigIO initial_map tracer0 randomQueryDelayRange = liftIO $ do
  let tracer = natTracer liftIO tracer0
  backingStoreMV <- newMVar initial_map
  writeBufferTV <- newTVarIO mempty
  nextQueryIdTV <- newTVarIO 0
  submissionQueueRef <- newTBQueueIO 10
  loopAsync <- async $ wwbLoop (contramap WWBTLoop tracer0) backingStoreMV writeBufferTV submissionQueueRef
  pure $ WWBConfig{..}

closeWWbConfig :: (MonadThrow m, MonadIO m) => WWBConfig k v m -> m ()
closeWWbConfig WWBConfig{loopAsync} = liftIO $ do
  cancel loopAsync
  waitCatch loopAsync >>= \case
    Left (fromException -> Just AsyncCancelled) -> pure ()
    Left e -> throwM  e
    Right () -> pure ()

withWWBConfig :: (MonadIO m, MonadMask m, Eq k, Hashable k) => HashMap k v -> Tracer IO WWBTrace -> (NominalDiffTime, NominalDiffTime) -> (WWBConfig k v m -> m a) -> m a
withWWBConfig hm tracer range f = bracket (wwbConfigIO hm tracer range) closeWWbConfig f

runWWBT :: (Eq k, Hashable k, MonadIO m, MonadMask m) => WWBT k v m a -> Tracer IO WWBTrace -> HashMap k v -> (NominalDiffTime, NominalDiffTime) -> m a
runWWBT m tracer hm range = do
  cfg <- wwbConfigIO hm tracer range
  runWWBTWithConfig m cfg

runWWBTWithConfig :: (Eq k, Hashable k, MonadMask m, MonadIO m) => WWBT k v m a -> WWBConfig k v m -> m a
runWWBTWithConfig (WWBT m) cfg = runReaderT m cfg

data WWBPrepareTrace

prepare :: (MonadIO m, Eq k, Hashable k)
  => Tracer m WWBPrepareTrace
  -> WWBConfig k v m
  -> HashSet k
  -> m (WWBReadSet k v)
prepare _tracer WWBConfig
    { backingStoreMV
    , nextQueryIdTV
    , writeBufferTV
    , randomQueryDelayRange
    }
    qs = liftIO $ do
  rsId <- atomically $ do
    qId <- wwbStateTVar nextQueryIdTV $ get <* modify' (+1)
    wwbStateTVar writeBufferTV $ do
      WriteBufferMeasure{wbmSummary = qInitDiff} <- gets measure
      modify' (<> FingerTree.singleton (WBEQueryStarted{..}))
    pure qId
  -- TODO if this async doesn't complete then it should write an event to the fingertree
  rsFetchAsync <- async $ simulateQuery randomQueryDelayRange qs (readMVar backingStoreMV)
  tokenMV <- newMVar SubmissionToken
  pure WWBReadSet{..}

data WWBSubmitTrace

submit :: (MonadIO m)
  => Tracer m WWBSubmitTrace
  -> TBQueue (WWBSubmission k v)
  -> WWBReadSet k v
  -> KVOperation k v a
  -> m (Either (WWBErr k v) a)
submit _tracer submissionQueueRef sReadSet@WWBReadSet{tokenMV} sOp = liftIO . runExceptT $ do
  lift (tryTakeMVar tokenMV) >>= \case
    Nothing -> throwError $ WWBEBase BEBadReadSet
    Just _ -> pure ()
  ExceptT $ do
    sResultMV <- newEmptyMVar
    atomically . writeTBQueue submissionQueueRef $ WWBSubmission {..}
    takeMVar sResultMV

data ReadSetError = ReadSetException SomeException
  deriving stock(Show)

instance Eq ReadSetError where
  ReadSetException x == ReadSetException y = show x == show y

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

data WWBErr k v = WWBEBase BaseError | WWBEExpiredReadSet | WWBEWeird String | WWBEReadErr ReadSetError
  deriving stock (Eq, Show, Generic)

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

  prepareOperation qs = WWBT . ReaderT $ \cfg@WWBConfig{tracer} -> do
    res <- prepare (contramap WWBTPrepare tracer) cfg . coerce $ qs
    pure $ coerce res

  submitOperation (coerce -> rs) op = WWBT . ReaderT $ \WWBConfig{tracer, submissionQueueRef} -> do
    submit (contramap WWBTSubmit tracer) submissionQueueRef rs op
