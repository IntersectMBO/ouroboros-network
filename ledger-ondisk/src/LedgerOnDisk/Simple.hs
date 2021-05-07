{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module LedgerOnDisk.Simple where


import LedgerOnDisk.Class
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.IORef
import Control.Monad.Reader
import Data.Coerce
import Data.Monoid
import Control.Monad.Except

type SimpleKey = Int
type SimpleValue = Int
type SimpleMap = HashMap SimpleKey SimpleValue
type SimpleSet = HashSet SimpleKey
type SimpleMonadKV = MonadKV SimpleKey SimpleValue

data SimpleState = SimpleState
  { backingStore :: !SimpleMap
  , activeQueries :: !(HashSet Int)
  , nextQueryId :: !Int
  }

initialState :: SimpleMap -> SimpleState
initialState m = SimpleState m mempty 0

newtype SimpleT m a = SimpleT { unSimpleT :: ReaderT (IORef SimpleState) m a }
  deriving newtype (Functor, Applicative, Monad, MonadFail, MonadTrans)

hoistSimpleT :: (m a -> n b) -> SimpleT m a -> SimpleT n b
hoistSimpleT f (SimpleT m ) = SimpleT $ mapReaderT f m

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
      { resultSetId :: !Int
      , resultSetQuery :: !(QueryScope SimpleKey)
      }
    deriving stock (Show, Eq)

  prepareOperation q = withState $ \s@SimpleState{..} -> pure (s
    { nextQueryId = nextQueryId + 1
    , activeQueries = HashSet.insert nextQueryId activeQueries
    }, SimpleResultSet nextQueryId q)

  submitOperation SimpleResultSet {..} f = hoistSimpleT runExceptT $ do
    withState $ \s@SimpleState{..} ->  do
      unless (HashSet.member resultSetId activeQueries) $ throwError KVEBadResultSet
      let
        restricted_map = HashMap.mapWithKey (\k _ -> HashMap.lookup k backingStore) $ HashSet.toMap . coerce $ resultSetQuery
        -- TODO check size restricted_map == size set
        (updates, r) = f restricted_map
        go k = Endo . \case
          DIUpdate v' -> HashMap.insert k v'
          DIRemove -> HashMap.delete k
        new_map = appEndo (HashMap.foldMapWithKey go updates) backingStore
      pure (s
        { activeQueries = HashSet.delete resultSetId activeQueries
        , backingStore = new_map
        }, r)
