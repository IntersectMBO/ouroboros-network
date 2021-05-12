{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module LedgerOnDisk.Simple where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Coerce
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.IORef
import Data.Monoid
import LedgerOnDisk.Class
import LedgerOnDisk.Pure
import Debug.Trace
import Data.TreeDiff.Class
import GHC.Generics

type SimpleKey = Int

type SimpleValue = Int

type SimpleMap = HashMap SimpleKey SimpleValue

type SimpleSet = HashSet SimpleKey

type SimpleMonadKV = MonadKV SimpleKey SimpleValue


data SimpleState = SimpleState
  { backingStore :: !SimpleMap,
    activeQueries :: !(HashSet Int),
    nextQueryId :: !Int
  }

initialState :: SimpleMap -> SimpleState
initialState m = SimpleState m mempty 0

newtype SimpleT m a = SimpleT {unSimpleT :: ReaderT (IORef SimpleState) m a}
  deriving newtype (Functor, Applicative, Monad, MonadFail, MonadTrans)

hoistSimpleT :: (m a -> n b) -> SimpleT m a -> SimpleT n b
hoistSimpleT f (SimpleT m) = SimpleT $ mapReaderT f m

askSimpleT :: MonadIO m => SimpleT m (IORef SimpleState)
askSimpleT = SimpleT ask

runSimpleT :: MonadIO m => SimpleT m a -> SimpleMap -> m (SimpleMap, a)
runSimpleT m kv = do
  ref <- liftIO . newIORef $ initialState kv
  r <- runReaderT (unSimpleT m) ref
  kv' <- liftIO $ readIORef ref
  pure (backingStore kv', r)

runSimpleTWithIORef :: SimpleT m a -> IORef SimpleState -> m a
runSimpleTWithIORef = runReaderT . unSimpleT

withState :: MonadIO m => (SimpleState -> m (SimpleState, a)) -> SimpleT m a
withState f = SimpleT $ do
  ref <- ask
  (m', r) <- liftIO (readIORef ref) >>= lift . f
  liftIO $ writeIORef ref m'
  pure r

-- askMap :: MonadIO m => SimpleT m SimpleMap
-- askMap = SimpleT $ ask >>= liftIO . readIORef

instance MonadIO m => MonadKV SimpleKey SimpleValue (SimpleT m) where
  data ResultSet (SimpleT m) = SimpleResultSet
    { resultSetId :: !Int,
      resultSetQuery :: !(QueryScope SimpleKey)
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToExpr)

  prepareOperation q = withState $ \s@SimpleState {..} ->
    pure
      ( s
          { nextQueryId = nextQueryId + 1,
            activeQueries = nextQueryId `HashSet.insert`  activeQueries
          },
        SimpleResultSet nextQueryId q
      )

  submitOperation SimpleResultSet {..} f = hoistSimpleT runExceptT $ do
    withState $ \s@SimpleState {..} -> do
      unless (resultSetId `HashSet.member` activeQueries) $ throwError BEBadResultSet
      let (a, new_map) = pureApplyOperation (coerce resultSetQuery) f backingStore
      pure
        ( s
            { activeQueries = resultSetId `HashSet.delete` activeQueries,
              backingStore = new_map
            },
          a
        )
