{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE BlockArguments           #-}
{-# LANGUAGE DerivingStrategies       #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TupleSections            #-}
{-# LANGUAGE TypeFamilies             #-}

-- | The module should be imported qualified.
--
module Ouroboros.Network.TxSubmission.Mempool.Simple
  ( TxValidationFail
  , Mempool (..)
  , MempoolSeq (..)
  , MempoolWriter (..)
  , empty
  , new
  , read
  , getReader
  , getWriter
  , writerAdapter
  ) where

import Prelude hiding (read, seq)

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (assert)
import Control.Monad.Trans.Except
import Data.Bifunctor (bimap, first, second)
import Data.Either
import Data.Foldable (toList)
import Data.Foldable qualified as Foldable
import Data.List (find)
import Data.Maybe (isJust)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set

import Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult (..))
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


-- | type of mempool validation errors which are non-fatal
--
data family TxValidationFail tx

-- | A mempool writer which generalizes the tx submission mempool writer
-- TODO: We could replace TxSubmissionMempoolWriter with this at some point
--
data MempoolWriter txid tx idx m =
     MempoolWriter {

       -- | Compute the transaction id from a transaction.
       --
       -- This is used in the protocol handler to verify a full transaction
       -- matches a previously given transaction id.
       --
       txId          :: tx -> txid,

       -- | Supply a batch of transactions to the mempool. They are either
       -- accepted or rejected individually, but in the order supplied.
       --
       -- The 'txid's of all transactions that were added successfully are
       -- returned.
       mempoolAddTxs
         :: [tx]
         -> m (Either (txid, TxValidationFail tx) [(txid, SubmitResult (TxValidationFail tx))])
    }


-- | A mempool writer with validation harness
-- PRECONDITION: no duplicates given to mempoolAddTxs
--
getWriter :: forall tx txid ctx m.
             ( MonadSTM m
             -- , NFData txid
             -- , NFData tx
             -- , NFData (TxValidationFail tx)
             , Ord txid
             )
          => (tx -> txid)
          -- ^ get txid of a tx
          -> m ctx
          -- ^ acquire validation context
          -> (   [tx]
              -> ctx
              -> ExceptT (tx, TxValidationFail tx) m
                         [(tx, Either (TxValidationFail tx) ())])
          -- ^ validation function which should evaluate its result to normal form
          -- esp. if it is 'expensive'
          -> TxValidationFail tx
          -- ^ replace duplicates
          -> Mempool m txid tx
          -> MempoolWriter txid tx Int m
getWriter getTxId acquireCtx validateTxs duplicateFail (Mempool mempool) =
  MempoolWriter {
    txId = getTxId,

    mempoolAddTxs = \txs -> assert (not . null $ txs) $ do
      ctx <- acquireCtx
      first (first getTxId)
            <$> runExceptT do
        -- todo probably should force the results before entering the atomically block
        !vTxs <- zipWith ((,) . getTxId) txs <$> validateTxs txs ctx

        ExceptT . atomically $ do
          MempoolSeq { mempoolSet, mempoolSeq } <- readTVar mempool
          let result =
                [if duplicate then
                   Left (txid, duplicateFail)
                 else
                   bimap (txid,) (const (txid, tx)) eResult
                | (txid, (tx, eResult)) <- vTxs
                , let duplicate = txid `Set.member` mempoolSet
                ]
              (validIds, validTxs) = unzip . rights $ result
              mempoolTxs' = MempoolSeq {
                 mempoolSet = Set.union mempoolSet (Set.fromList validIds),
                 mempoolSeq = Foldable.foldl' (Seq.|>) mempoolSeq validTxs
              }
          writeTVar mempool mempoolTxs'
          return . Right $ either (second SubmitFail) (second (const SubmitSuccess)) <$> result
  }


-- | Takes the general mempool writer defined here
-- and adapts it to the API of the tx submission mempool writer
-- to avoid more breaking changes for now.
--
writerAdapter :: (Functor m)
              => MempoolWriter txid tx idx m
              -> TxSubmissionMempoolWriter txid tx idx m
writerAdapter MempoolWriter { txId, mempoolAddTxs } = undefined
  TxSubmissionMempoolWriter { txId, mempoolAddTxs = adapter }
  where
    adapter = fmap (either (const []) (fmap fst)) . mempoolAddTxs
