{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The module should be imported qualified.
--
module Ouroboros.Network.TxSubmission.Mempool.Simple
  ( Mempool (..)
  , empty
  , new
  , read
  , getReader
  , getWriter
  ) where

import Prelude hiding (read, seq)

import Control.Concurrent.Class.MonadSTM.Strict

import Data.Foldable (toList)
import Data.Foldable qualified as Foldable
import Data.Function (on)
import Data.List (find, nubBy)
import Data.Maybe (isJust)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set qualified as Set

import Ouroboros.Network.SizeInBytes
import Ouroboros.Network.TxSubmission.Inbound.V2.Types
import Ouroboros.Network.TxSubmission.Mempool.Reader


-- | A simple in-memory mempool implementation.
--
newtype Mempool m tx = Mempool (StrictTVar m (Seq tx))


empty :: MonadSTM m => m (Mempool m tx)
empty = Mempool <$> newTVarIO Seq.empty


new :: MonadSTM m
    => [tx]
    -> m (Mempool m tx)
new = fmap Mempool
    . newTVarIO
    . Seq.fromList


read :: MonadSTM m => Mempool m tx -> m [tx]
read (Mempool mempool) = toList <$> readTVarIO mempool


getReader :: forall tx txid m.
             ( MonadSTM m
             , Eq txid
             )
          => (tx -> txid)
          -> (tx -> SizeInBytes)
          -> Mempool m tx
          -> TxSubmissionMempoolReader txid tx Int m
getReader getTxId getTxSize (Mempool mempool) =
    TxSubmissionMempoolReader { mempoolGetSnapshot, mempoolZeroIdx = 0 }
  where
    mempoolGetSnapshot :: STM m (MempoolSnapshot txid tx Int)
    mempoolGetSnapshot = getSnapshot <$> readTVar mempool

    getSnapshot :: Seq tx
                -> MempoolSnapshot txid tx Int
    getSnapshot seq =
      MempoolSnapshot {
          mempoolTxIdsAfter =
            \idx -> zipWith f [idx + 1 ..] (toList $ Seq.drop idx seq),
          -- why do I need to use `pred`?
          mempoolLookupTx   = flip Seq.lookup seq . pred,
          mempoolHasTx      = \txid -> isJust $ find (\tx -> getTxId tx == txid) seq
       }

    f :: Int -> tx -> (txid, Int, SizeInBytes)
    f idx tx = (getTxId tx, idx, getTxSize tx)


-- | A simple mempool writer.
--
getWriter :: forall tx txid m.
             ( MonadSTM m
             , Ord txid
             )
          => (tx -> txid)
          -> (tx -> Bool)
          -- ^ validate a tx
          -> Mempool m tx
          -> TxSubmissionMempoolWriter txid tx Int m
getWriter getTxId validateTx (Mempool mempool) =
    TxSubmissionMempoolWriter {
        txId = getTxId,

        mempoolAddTxs = \txs -> do
          atomically $ do
            mempoolTxs <- readTVar mempool
            let currentIds = Set.fromList (map getTxId (toList mempoolTxs))
                validTxs = nubBy (on (==) getTxId)
                         $ filter
                            (\tx -> validateTx tx
                                 && getTxId tx `Set.notMember` currentIds)
                           txs
                mempoolTxs' = Foldable.foldl' (Seq.|>) mempoolTxs validTxs
            writeTVar mempool mempoolTxs'
            return (map getTxId validTxs)
      }
