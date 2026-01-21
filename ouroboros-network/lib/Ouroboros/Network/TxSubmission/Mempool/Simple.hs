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
  , WithIndex (..)
  , empty
  , new
  , read
  , getReader
  , TxSubmissionMempoolReader (..)
  , getWriter
  , TxSubmissionMempoolWriter (..)
  ) where

import Prelude hiding (read, seq)

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI

import Data.Bifunctor (first)
import Data.Either (partitionEithers)
import Data.Foldable (toList)
import Data.Foldable qualified as Foldable
import Data.List (find)
import Data.List qualified as List
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable (Typeable)

import Ouroboros.Network.SizeInBytes
import Ouroboros.Network.TxSubmission.Inbound.V2.Types
import Ouroboros.Network.TxSubmission.Mempool.Reader

data WithIndex tx = WithIndex { getIdx :: !Integer,
                                getTx  :: !tx }

data MempoolSeq txid tx = MempoolSeq {
    mempoolSet :: !(Set txid),
    -- ^ cached set of `txid`s in the mempool
    mempoolSeq :: !(Seq (WithIndex tx)),
    -- ^ sequence of all `tx`s
    nextIdx    :: !Integer
    -- ^ next available index
    --
    -- Invariant: larger than the index of the last element of `mempoolSeq`.
  }

-- | A simple in-memory mempool implementation.
--
newtype Mempool m txid tx = Mempool (StrictTVar m (MempoolSeq txid tx))


empty :: MonadSTM m => m (Mempool m txid tx)
empty = Mempool <$> newTVarIO (MempoolSeq Set.empty Seq.empty (-1))


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
                   mempoolSeq,
                   nextIdx = fromIntegral (Seq.length mempoolSeq) + 1
                 }
  where
    mempoolSeq = Seq.fromList $ zipWith WithIndex [0..] txs


read :: MonadSTM m => Mempool m txid tx -> m [tx]
read (Mempool mempool) = map getTx . toList . mempoolSeq <$> readTVarIO mempool


getReader :: forall tx txid m.
             ( MonadSTM m
             , Ord txid
             )
          => (tx -> txid)
          -> (tx -> SizeInBytes)
          -> Mempool m txid tx
          -> TxSubmissionMempoolReader txid tx Integer m
getReader getTxId getTxSize (Mempool mempool) =
    -- Using `0`-based index.  `mempoolZeroIdx = -1` so that
    -- `mempoolTxIdsAfter mempoolZeroIdx` returns all txs.
    TxSubmissionMempoolReader { mempoolGetSnapshot,
                                mempoolZeroIdx = -1
                              }
  where
    mempoolGetSnapshot :: STM m (MempoolSnapshot txid tx Integer)
    mempoolGetSnapshot = getSnapshot <$> readTVar mempool

    getSnapshot :: MempoolSeq txid tx
                -> MempoolSnapshot txid tx Integer
    getSnapshot MempoolSeq { mempoolSeq = seq, mempoolSet } =
      MempoolSnapshot {
          mempoolTxIdsAfter =
            \idx ->
              foldr
                (\WithIndex {getIdx, getTx} acc ->
                  if getIdx > idx
                  then (getTxId getTx, getIdx, getTxSize getTx) : acc
                  else acc
                )
                []
                seq,
          mempoolLookupTx =
            \idx -> getTx <$> find (\WithIndex {getIdx} -> getIdx == idx) seq,
          mempoolHasTx =
            \txid -> txid `Set.member` mempoolSet
        }


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
getWriter :: forall tx txid failure m.
             ( MonadSTM m
             , MonadTime m
             , Ord txid
             )
          => failure
          -- ^ duplicate tx error
          -> (tx -> txid)
          -- ^ get transaction hash
          -> (UTCTime -> [tx] -> STM m [Either (txid, failure) tx])
          -- ^ validate a tx in an `STM` transaction, this allows acquiring and
          -- updating validation context. 
          -> ([(txid, failure)] -> m ())
          -- ^ handle invalid txs, e.g. logging, throwing exceptions, etc
          -> Mempool m txid tx
          -- ^ mempool
          -> TxSubmissionMempoolWriter txid tx Integer m failure
getWriter duplicateTxError getTxId validateTx handleInvalidTxs (Mempool mempoolVar) =
    TxSubmissionMempoolWriter {
        txId = getTxId,

        mempoolAddTxs = \txs -> do
          now <- getCurrentTime
          (rejectedTxIds, acceptedTxs) <- atomically $ do
            MempoolSeq { mempoolSet, mempoolSeq, nextIdx } <- readTVar mempoolVar

            -- remove txs that are already in the mempool
            -- so we don't validate txs which are already in the mempool
            let duplicateTxIds :: [txid]
                (duplicateTxIds, txs') =
                    first (map getTxId)
                  $ List.partition (\tx -> getTxId tx `Set.member` mempoolSet) txs

            -- validate `txs'`
            -- NOTE: we might have duplicates in the `txs'` list
            (invalidTxIds, validTxs) <-
                fmap partitionEithers
              . validateTx now
              $ txs'

            let acceptedTxs     :: [txid]
                -- duplicate txids in the submitted list `txs`
                duplicateTxIds' :: [txid]

                -- delta - set of newly accepted txids
                -- NOTE: `validTxs` are not in the mempool, we just need to
                -- check that we are not adding multiple copies of the same tx.
                (delta, mempoolSeq', nextIdx', acceptedTxs, duplicateTxIds') =
                  Foldable.foldl'
                    (\(set, seq, idx, as, rs) tx ->
                      if getTxId tx `Set.member` set
                      then ( set
                           , seq
                           , idx
                           , as
                           , getTxId tx : rs
                           )
                      else ( Set.insert (getTxId tx) set
                           , seq Seq.|> WithIndex idx tx
                           , succ idx
                           , getTxId tx : as
                           , rs
                           )
                    )
                    (Set.empty, mempoolSeq, nextIdx, [], [])
                  validTxs
            writeTVar mempoolVar MempoolSeq { mempoolSet = mempoolSet `Set.union` delta
                                            , mempoolSeq = mempoolSeq'
                                            , nextIdx    = nextIdx'
                                            }
            return ( invalidTxIds
                     ++
                     [ (txid, duplicateTxError)
                     | txid <- duplicateTxIds ++ duplicateTxIds'
                     ]
                   , acceptedTxs
                   )
          handleInvalidTxs rejectedTxIds
          return (acceptedTxs, rejectedTxIds)
      }
