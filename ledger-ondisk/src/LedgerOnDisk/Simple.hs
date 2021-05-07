{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
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

type SimpleKey = Int
type SimpleValue = Int
type SimpleMap = HashMap SimpleKey SimpleValue
type SimpleSet = HashSet SimpleKey
type SimpleMonadKV = MonadKV SimpleKey SimpleValue

newtype SimpleT m a = SimpleT { unSimpleT :: ReaderT (IORef SimpleMap) m a }
  deriving newtype (Functor, Applicative, Monad, MonadFail)

runSimpleT :: MonadIO m => SimpleT m a -> SimpleMap -> m (SimpleMap, a)
runSimpleT m kv = do
  ref <- liftIO $ newIORef kv
  r <- runReaderT (unSimpleT m) ref
  kv' <- liftIO $ readIORef ref
  pure (kv', r)

withMap :: MonadIO m => (SimpleMap -> m (SimpleMap, a)) -> SimpleT m a
withMap f = SimpleT $ do
  ref <- ask
  (m', r) <- liftIO (readIORef ref) >>= lift . f
  liftIO $ writeIORef ref m'
  pure r

askMap :: MonadIO m => SimpleT m SimpleMap
askMap = SimpleT $ ask >>= liftIO . readIORef

instance MonadIO m => MonadKV SimpleKey SimpleValue (SimpleT m) where
  newtype ResultSet (SimpleT m) = SimpleResultSet { unSimpleResultSet :: SimpleSet }
  data Err (SimpleT m) = Err
  prepareOperation = pure . coerce
  submitOperation (SimpleResultSet set) f = do
    withMap $ \full_map -> let
      restricted_map = HashMap.mapWithKey (\k _ -> HashMap.lookup k full_map) $ HashSet.toMap set
    -- TODO check size restricted_map == size set
      (updates, r) = f restricted_map
      go k = Endo . \case
          DIUpdate v' -> HashMap.insert k v'
          DIRemove -> HashMap.delete k
      new_map = appEndo (HashMap.foldMapWithKey go updates) full_map
      in pure (new_map, pure r)
