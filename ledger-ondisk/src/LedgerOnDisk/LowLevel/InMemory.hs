-- |

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module LedgerOnDisk.LowLevel.InMemory where

import Data.Map.Strict (Map)
import Control.Concurrent
import Control.Concurrent.STM
import LedgerOnDisk.LowLevel.Class
import LedgerOnDisk.KVHandle.OnDiskMappings
import Data.Proxy
import LedgerOnDisk.KVHandle.RangeQuery
import qualified Data.Map.Strict as Map
import Data.Functor.Identity
import Data.Coerce
import LedgerOnDisk.Mapping.PTMap
import LedgerOnDisk.Diff

newtype InMemoryROHandle state = InMemoryROHandle (OnDiskMappings state Map)

proxyDBKVConstraint :: Proxy KeysAreOrd
proxyDBKVConstraint = Proxy

doQuery :: Ord k => Query k v -> Map k v -> QueryResult k v
doQuery q m0 = runIdentity $ runQuery doLookup doRangeQuery q
  where
    doLookup k = coerce $ Map.lookup k m0
    doRangeQuery mb_k n = coerce $ let
      m1
        | Just k <- mb_k = let
            (_, mb_r, r0) = Map.splitLookup k m0
            in maybe id (Map.insert k) mb_r r0
        | otherwise = m0
      (result, rest) = Map.splitAt (fromIntegral n) m1
      returnedRows = fromIntegral $ length result
      remaining_rows = fromIntegral $ length rest
      nextKey = (\(k, _) -> (k, remaining_rows)) <$> Map.lookupMin rest
      in (RangeQueryMetadata { returnedRows, nextKey }, Map.toList result)

applyDiffMap :: Ord k => DiffMap k v -> Map k v -> Map k v
applyDiffMap (DiffMap d) = applyDifftoMap d

instance
  (HasConstrainedOnDiskMappings KeysAreOrd state)
  => DBROLowLevel (InMemoryROHandle state) where
  type DBKVConstraint (InMemoryROHandle state) = KeysAreOrd
  type ROLLT (InMemoryROHandle state) = state

  readSnapshot (InMemoryROHandle odm) q = pure . runIdentity  $ zipMappings proxyDBKVConstraint (coerce doQuery) q odm
  closeHandle _ = pure ()

data InMemoryHandle state = InMemoryHandle
  { dataMV :: !(MVar (OnDiskMappings state Map))
  , seqIdTV :: !(TVar SeqId)
  }

instance
  (HasConstrainedOnDiskMappings KeysAreOrd state
  ) => DBLowLevel (InMemoryHandle state) where
  type LLT (InMemoryHandle state) = state
  type ROHandle (InMemoryHandle state) = InMemoryROHandle state

  read InMemoryHandle {..} q = withMVar dataMV $ \odm -> do
    seq_id <- readTVarIO seqIdTV
    x <- zipMappings proxyDBKVConstraint (\q' m -> pure $ doQuery q' m) q odm
    pure (seq_id, x)

  write InMemoryHandle {..} d = modifyMVar dataMV $ \odm -> do
    nid <- atomically $ stateTVar seqIdTV $ \i -> (i, i + 1)
    newodm <- zipMappings proxyDBKVConstraint (\d' m -> pure $ applyDiffMap d' m) d odm
    pure (newodm, nid)

  getSeqId InMemoryHandle{..} = readTVar seqIdTV

  snapshot InMemoryHandle{..} = do
    snapshotted <- readMVar dataMV
    pure $ InMemoryROHandle snapshotted
