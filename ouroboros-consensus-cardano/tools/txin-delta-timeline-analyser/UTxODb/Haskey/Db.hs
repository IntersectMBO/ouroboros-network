{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
-- |

module UTxODb.Haskey.Db where


import qualified Database.Haskey.Alloc.Concurrent as Haskey
import Control.Monad.IO.Class
import Control.Monad.Catch
import qualified Database.Haskey.Store.File as Haskey
import qualified Database.Haskey.Store.InMemory as Haskey
import Data.BTree.Impure (Tree)
import GHC.Generics (Generic)
import Data.Binary
import qualified Data.BTree.Primitives.Value as Haskey
import Data.Int
import qualified Data.BTree.Impure as Haskey
import qualified Data.Map.Strict as Map
import Data.Foldable
import Data.Traversable
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import Data.Coerce
import qualified Data.BTree.Alloc.Class as Haskey
import qualified Data.BTree.Primitives as Haskey
import Control.Monad
import Control.Monad.State.Strict
import qualified Control.Monad.State.Strict as State
import Type.Reflection
import qualified Options.Applicative as O

import UTxODb.Snapshots
import UTxODb.Haskey.Tree
import Control.Concurrent.MVar
import Data.Functor
import Data.Monoid

data HaskeySideTable state = HaskeySideTable
  { seqId :: !(SeqNo state)
  }
  deriving stock (Show, Generic)

deriving anyclass instance Binary (SeqNo state) => Binary (HaskeySideTable state)

instance (Typeable state, Binary (SeqNo state)) => Haskey.Value (HaskeySideTable state)

data HaskeyRoot state = HaskeyRoot
  { innerTables :: Tables (HaskeyState state) OnDiskTable
  , sideTree :: Tree () (HaskeySideTable state)
  }
  deriving stock (Show, Generic)
  deriving anyclass Binary

instance HasHaskeyOnDiskTables state => Haskey.Value (HaskeyRoot state)
instance HasHaskeyOnDiskTables state => Haskey.Root (HaskeyRoot state)

data HaskeyTrace

data HaskeyBackend
  = HBMemory
    { memConfig :: !Haskey.MemoryStoreConfig
    , memFiles :: !(Haskey.MemoryFiles FilePath)
    , filename :: !FilePath
    }
  | HBFile
    { fileConfig :: !Haskey.FileStoreConfig
    , filename :: !FilePath
    }

data HaskeyDb (state :: StateKind) = HaskeyDb
  { hdbBackend :: !HaskeyBackend
  , hdbRoot :: !(Haskey.ConcurrentDb (HaskeyRoot state))
  -- , hdbTrace :: Tracer IO HaskeyTrace
  }


readTree :: (Haskey.AllocReaderM m, Foldable f, Haskey.Key k, Haskey.Value v) => f k -> Tree k v -> m (Map k v)
readTree ks t = do
  list_of_maybe_kv <- for (toList ks) $ \k -> fmap (k,) <$> Haskey.lookup k t
  pure . Map.fromList . catMaybes $ list_of_maybe_kv

readTransaction :: forall state m a. (HasHaskeyOnDiskTables state, Haskey.AllocReaderM m, MonadMask m) =>  AnnTableKeySets state a -> HaskeyRoot state -> m (AnnTableReadSets state (a, SeqNo state))
readTransaction keysets HaskeyRoot{..} = do
  HaskeySideTable{seqId} <- Haskey.lookup () sideTree >>= maybe (throwM $ userError "no side tables") pure
  let
    go_lookup :: TableTag t v
      -> AnnTable TableKeySet a t k v
      -> OnDiskTable t k v
      -> m (AnnTable TableReadSet (a, SeqNo state) t k v)
    go_lookup tag (AnnTable TableQuery {tqKeySet} x) t =
      go_lookup tag (AnnTable (TableKeySet tqKeySet) x) t
    go_lookup tag (AnnTable (TableKeySet ks) x) (RO_ODT t) = do
      result <- if Set.null ks
        then pure Map.empty
        else readTree ks t
      let
        trs = emptyTableReadSetRO  { troPTMap = PMapRO (PMapRep result ks)}
      pure $ AnnTable trs (x, seqId)
    go_lookup tag (AnnTable (TableKeySet ks) x) (RW_ODT t) = do
      result <- if Set.null ks
        then pure Map.empty
        else readTree ks t
      let
        trs = TableReadSetRW (PMapRW (PMapRep result ks))
      pure $ AnnTable trs (x, seqId)

    go_lookup tag (AnnTable (TableKeySet ks) x) (RWU_ODT t) = do
      result <- if Set.null ks
        then pure Map.empty
        else readTree ks t
      let
        trs = TableReadSetRWU (PMapRWU (PMapRep result ks))
      pure $ AnnTable trs (x, seqId)
  HaskeyTables r <- traverse2Tables go_lookup (HaskeyTables keysets) innerTables
  pure r

data UnexpectedSeqId state = UnexpectedSeqId
  { expectedSeqId :: SeqNo state
  , foundSeqId :: SeqNo state
  }
  deriving stock (Show)
  deriving anyclass Exception

data NonMonotonicSeqId state = NonMonotonicSeqId
  { oldSeqId :: SeqNo state
  , newSeqId :: SeqNo state
  }
  deriving stock (Show)
  deriving anyclass Exception

writeTransaction :: forall state m a. (HasHaskeyOnDiskTables state, Haskey.AllocM m, MonadMask m)
  =>
  [Either (TableDiffs state) (TableSnapshots state)]
  -> SeqNo state
  -> SeqNo state
  -> HaskeyRoot state
  -> m (Haskey.Transaction (HaskeyRoot state) (Maybe SomeException))
writeTransaction changes old_seq new_seq hr@HaskeyRoot{..} = do
  hst@HaskeySideTable{seqId} <- Haskey.lookup () sideTree >>= maybe (throwM $ userError "no side table") pure
  when (old_seq /= seqId) $ throwM UnexpectedSeqId { expectedSeqId = old_seq, foundSeqId = seqId }
  when (old_seq >= new_seq) $ throwM NonMonotonicSeqId { oldSeqId = old_seq, newSeqId = new_seq }
  let
    go_change :: Tables (HaskeyState state) OnDiskTable -> Either (Tables state TableDiff) (TableSnapshots state) -> m (Tables (HaskeyState state) OnDiskTable)
    go_change tables (Right snapshot) = error "unimplemented"
    go_change tables (Left diff) = traverse2Tables go_diff (HaskeyTables diff :: Tables (HaskeyState state) TableDiff) tables where
      go_diff :: forall (t :: TableType) k v. TableTag t v -> TableDiff t k v -> OnDiskTable t k v -> m (OnDiskTable t k v)
      go_diff TableTagRO TableDiffRO x = pure x
      go_diff TableTagRW (TableDiffRW (DiffMapRW m)) (RW_ODT x) = fmap RW_ODT . flip State.execStateT x $ Map.traverseWithKey go m where
        go k d = do
          t <- State.get
          case d of
            MapRWElemDelete -> lift (Haskey.delete k t) >>= State.put
            MapRWElemInsert v -> lift (Haskey.insert k v t) >>= State.put
      go_diff TableTagRWU (TableDiffRWU (DiffMapRWU m)) (RWU_ODT x) = fmap RWU_ODT . flip State.execStateT x $ Map.traverseWithKey go m where
        go k d = do
          t <- State.get
          case d of
            MapRWUElemDelete -> lift (Haskey.delete k t) >>= State.put
            MapRWUElemInsert v -> lift (Haskey.insert k v t) >>= State.put
            MapRWUElemUpdate v -> do
              mb_old <- lift $ Haskey.lookup k t
              lift (Haskey.insert k (maybe v (<> v) mb_old) t) >>= State.put

  r <- try $ foldM go_change innerTables changes
  case r of
    Left e -> do
      Haskey.abort (pure e)
    Right x -> do
      st <- Haskey.insert () hst { seqId = new_seq } sideTree
      Haskey.commit Nothing hr { sideTree = st, innerTables = x }

keysetSize :: HasTables (Tables state) => AnnTableKeySets state a -> Int
keysetSize = getSum . foldMapTables go where
  go _ (AnnTable (TableKeySet ks) _) = Sum $ length ks
  go _ (AnnTable TableQuery{tqKeySet} _) = Sum $ length tqKeySet


-- isEmptyChanges :: [Either (TableDiffs state) (TableSnapshots state)] -> Bool
-- isEmptyChanges [] = True
-- isEmptyChanges (Left diffs : xs )= empty_diffs && isEmptyChanges xs where
--   empty_diffs = getSum . foldMapTables go where
--     go TableDiffRO = True
--     go (TableDiffRW (DiffMapRW m)) = Map.null m
--     go (TableDiffRWU (DiffMapRWU m)) = Map.null m
-- isEmptyChanges _ = False
instance HasHaskeyOnDiskTables state => DiskDb (HaskeyDb state) state where
  readDb HaskeyDb {hdbBackend, hdbRoot} keysets = runHaskeyBackend hdbBackend $ Haskey.transactReadOnly (readTransaction keysets) hdbRoot
  writeDb HaskeyDb {..} changes old_seq new_seq = do
    mb_e <- runHaskeyBackend hdbBackend $ Haskey.transact (writeTransaction changes old_seq new_seq) hdbRoot
    for_ mb_e throwM



openHaskeyDb :: forall m state. (HasHaskeyOnDiskTables state, MonadMask m, MonadIO m)
  =>
  -- Tracer IO HaskeyTrace -- ^
  SeqNo state
  -> HaskeyBackend -- ^
  -> m (HaskeyDb state)
openHaskeyDb init_seqno hdbBackend = do
  let fp = filename hdbBackend
      concurrentHandles = Haskey.concurrentHandles fp

  (hdbRoot, created) <- runHaskeyBackend hdbBackend  $ do
    Haskey.openConcurrentDb concurrentHandles >>= \case
      Nothing -> do
        db <- Haskey.createConcurrentDb concurrentHandles HaskeyRoot
          { innerTables = constTables  $ \case
              TableTagRO -> RO_ODT Haskey.empty
              TableTagRW -> RW_ODT Haskey.empty
              TableTagRWU -> RWU_ODT Haskey.empty
          , sideTree = Haskey.empty
          }
        let
          go :: forall m. (Haskey.AllocM m, MonadMask m) => HaskeyRoot state -> m (Haskey.Transaction (HaskeyRoot state) ())
          go hr0 = do
            st <- Haskey.insert () HaskeySideTable {seqId = init_seqno} (sideTree hr0)
            let hr = hr0 { sideTree = st }
            Haskey.commit_ hr
        Haskey.transact_ go db
        pure (db, True)
      Just x -> pure (x, False)
  pure HaskeyDb {..}

runHaskeyBackend :: (MonadMask m, MonadIO m)
  => HaskeyBackend
  -> (forall n. (MonadMask n, MonadIO n, Haskey.ConcurrentMetaStoreM n) => n a)
  -> m a
runHaskeyBackend hbe m = case hbe of
  HBMemory{..} -> Haskey.runMemoryStoreT m memConfig memFiles
  HBFile{..} -> Haskey.runFileStoreT m fileConfig

data HaskeyParams = HaskeyParams
  { haskeyPageSize :: !(Maybe Word32)
  , haskeyMaxKeySize :: !(Maybe Word64)
  , haskeyMaxValueSize :: !(Maybe Word64)
  , haskeyOnDisk :: !Bool
  , haskeyFile :: !FilePath
  } deriving stock (Show)

haskeyParamsParser :: O.Parser HaskeyParams
haskeyParamsParser = HaskeyParams
  <$> parsePageSize
  <*> parseMaxKeySize
  <*> parseMaxValueSize
  <*> parseInMemory
  <*> parseFile
  where
    parsePageSize = O.optional $ O.option O.auto (O.long "page-size")
    parseMaxKeySize = O.optional $ O.option O.auto (O.long "max-key-size")
    parseMaxValueSize = O.optional $ O.option O.auto (O.long "max-value-size")
    parseInMemory = O.flag False True (O.long "disk")
    parseFile = O.option O.str (O.long "db")

haskeyBackendParser :: O.Parser (IO HaskeyBackend)
haskeyBackendParser = haskeyParamsParser <&> \HaskeyParams{..} -> do
  if haskeyOnDisk then
    pure HBFile
      { fileConfig = Haskey.FileStoreConfig
        { Haskey.fileStoreConfigPageSize = maybe (Haskey.fileStoreConfigPageSize Haskey.defFileStoreConfig) fromIntegral haskeyPageSize
        , Haskey.fileStoreConfigMaxKeySize = fromMaybe (Haskey.fileStoreConfigMaxKeySize Haskey.defFileStoreConfig) haskeyMaxKeySize
        , Haskey.fileStoreConfigMaxValueSize = fromMaybe (Haskey.fileStoreConfigMaxValueSize Haskey.defFileStoreConfig) haskeyMaxValueSize
        }
      , filename = haskeyFile
      }
  else do
    memFiles <- newEmptyMVar
    pure HBMemory
      { memConfig = Haskey.MemoryStoreConfig
        { Haskey.memoryStoreConfigPageSize = maybe (Haskey.memoryStoreConfigPageSize Haskey.defMemoryStoreConfig) fromIntegral haskeyPageSize
        , Haskey.memoryStoreConfigMaxKeySize = fromMaybe (Haskey.memoryStoreConfigMaxKeySize Haskey.defMemoryStoreConfig) haskeyMaxKeySize
        , Haskey.memoryStoreConfigMaxValueSize = fromMaybe (Haskey.memoryStoreConfigMaxValueSize Haskey.defMemoryStoreConfig) haskeyMaxValueSize
        }
      , memFiles
      , filename = haskeyFile
      }
