{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

-- | The module should be imported qualified.
--
module Ouroboros.Network.TxSubmission.Mempool.Simple
  ( Mempool (..)
  , MempoolSeq (..)
  , empty
  , new
  , read
  , getReader
  , getWriter
  ) where

import Prelude hiding (read, seq)

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad (when)
import Control.Monad.Class.MonadThrow

import Data.Bifunctor (bimap)
import Data.Either (partitionEithers)
import Data.Foldable (toList)
import Data.Foldable qualified as Foldable
import Data.Function (on)
import Data.List (find, nubBy)
import Data.Maybe (isJust)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable (Typeable)

import Ouroboros.Network.SizeInBytes
import Ouroboros.Network.TxSubmission.Inbound.V2.Types
import Ouroboros.Network.TxSubmission.Mempool.Reader


data MempoolSeq txid tx = MempoolSeq {
    mempoolSet :: !(Set txid),
    -- ^ cached set of `txid`s in the mempool
    mempoolSeq :: !(Seq tx)
    -- ^ sequence of all `tx`s
  }

-- | A simple in-memory mempool implementation.
--
newtype Mempool m txid tx = Mempool (StrictTVar m (MempoolSeq txid tx))


empty :: MonadSTM m => m (Mempool m txid tx)
empty = Mempool <$> newTVarIO (MempoolSeq Set.empty Seq.empty)


new :: ( MonadSTM m
       , Ord txid
       )
    => (tx -> txid)
    -> [tx]
    -> m (Mempool m txid tx)
new getTxId txs =
    fmap Mempool
  . newTVarIO
  $ MempoolSeq { mempoolSet = Set.fromList (getTxId <$> txs),
                 mempoolSeq = Seq.fromList txs
               }


read :: MonadSTM m => Mempool m txid tx -> m [tx]
read (Mempool mempool) = toList . mempoolSeq <$> readTVarIO mempool


getReader :: forall tx txid m.
             ( MonadSTM m
             , Eq txid
             )
          => (tx -> txid)
          -> (tx -> SizeInBytes)
          -> Mempool m txid tx
          -> TxSubmissionMempoolReader txid tx Int m
getReader getTxId getTxSize (Mempool mempool) =
    -- Using `0`-based index.  `mempoolZeroIdx = -1` so that
    -- `mempoolTxIdsAfter mempoolZeroIdx` returns all txs.
    TxSubmissionMempoolReader { mempoolGetSnapshot,
                                mempoolZeroIdx = -1
                              }
  where
    mempoolGetSnapshot :: STM m (MempoolSnapshot txid tx Int)
    mempoolGetSnapshot = getSnapshot . mempoolSeq <$> readTVar mempool

    getSnapshot :: Seq tx
                -> MempoolSnapshot txid tx Int
    getSnapshot seq =
      MempoolSnapshot {
          mempoolTxIdsAfter = \idx -> zipWith f [idx + 1..]
                                                (toList $ Seq.drop (idx + 1) seq),
          mempoolLookupTx   = \idx -> Seq.lookup idx seq,
          mempoolHasTx      = \txid -> isJust $ find (\tx -> getTxId tx == txid) seq
       }

    f :: Int -> tx -> (txid, Int, SizeInBytes)
    f idx tx = (getTxId tx, idx, getTxSize tx)


data InvalidTxsError where
    InvalidTxsError :: forall txid failure.
                       ( Typeable txid
                       , Typeable failure
                       , Show txid
                       , Show failure
                       )
                    => [(txid, failure)]
                    -> InvalidTxsError

deriving instance Show InvalidTxsError
instance Exception InvalidTxsError


-- | A simple mempool writer.
--
getWriter :: forall tx txid ctx failure m.
             ( MonadSTM m
             , MonadThrow m
             , Ord txid
             , Typeable txid
             , Typeable failure
             , Show txid
             , Show failure
             )
          => (tx -> txid)
          -- ^ get txid of a tx
          -> m ctx
          -- ^ monadic validation ctx
          -> (ctx -> tx -> Either failure ())
          -- ^ validate a tx, any failing `tx` throws an exception.
          -> (failure -> Bool)
          -- ^ return `True` when a failure should throw an exception
          -> Mempool m txid tx
          -> TxSubmissionMempoolWriter txid tx Int m
getWriter getTxId getValidationCtx validateTx failureFilterFn (Mempool mempool) =
    TxSubmissionMempoolWriter {
        txId = getTxId,

        mempoolAddTxs = \txs -> do
          ctx <- getValidationCtx
          (invalidTxIds, validTxs) <- atomically $ do
            MempoolSeq { mempoolSet, mempoolSeq } <- readTVar mempool
            let (invalidTxIds, validTxs) =
                    bimap (filter (failureFilterFn . snd))
                          (nubBy (on (==) getTxId))
                  . partitionEithers
                  . map (\tx -> case validateTx ctx tx of
                                 Left  e -> Left (getTxId tx, e)
                                 Right _ -> Right tx
                        )
                  . filter (\tx -> getTxId tx `Set.notMember` mempoolSet)
                  $ txs
                mempoolTxs' = MempoolSeq {
                    mempoolSet = Foldable.foldl' (\s tx -> getTxId tx `Set.insert` s)
                                                 mempoolSet
                                                 validTxs,
                    mempoolSeq = Foldable.foldl' (Seq.|>) mempoolSeq validTxs
                  }
            writeTVar mempool mempoolTxs'
            return (invalidTxIds, map getTxId validTxs)
          when (not (null invalidTxIds)) $
            throwIO (InvalidTxsError invalidTxIds)
          return validTxs
      }
