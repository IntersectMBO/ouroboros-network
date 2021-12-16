module Ouroboros.Network.TxSubmission.Mempool.Reader
  ( TxSubmissionMempoolReader (..)
  , MempoolSnapshot (..)
  , mapMempoolSnapshot
  , mapTxSubmissionMempoolReader
  ) where

import           Control.Monad.Class.MonadSTM (MonadSTM, STM)

import           Ouroboros.Network.Protocol.TxSubmission.Client (TxSizeInBytes)

-- | The consensus layer functionality that the inbound and outbound side of
-- the tx submission logic requires.
--
-- This is provided to the tx submission logic by the consensus layer.
--
data TxSubmissionMempoolReader txid tx idx m =
     TxSubmissionMempoolReader {

       -- | In STM, grab a snapshot of the contents of the mempool. This allows
       -- further pure queries on the snapshot.
       --
       mempoolGetSnapshot :: STM m (MempoolSnapshot txid tx idx),

       -- | 'mempoolTxIdsAfter' with 'mempoolZeroIdx' is expected to give all
       -- txs currently in the mempool.
       mempoolZeroIdx     :: idx
    }

mapTxSubmissionMempoolReader ::
     MonadSTM m
  => (tx -> tx')
  -> TxSubmissionMempoolReader txid tx  idx m
  -> TxSubmissionMempoolReader txid tx' idx m
mapTxSubmissionMempoolReader f rdr =
    rdr {
       mempoolGetSnapshot = mapMempoolSnapshot f <$> mempoolGetSnapshot rdr
    }

-- | A pure snapshot of the contents of the mempool. It allows fetching
-- information about transactions in the mempool, and fetching individual
-- transactions.
--
-- This uses a transaction sequence number type for identifying transactions
-- within the mempool sequence. The sequence number is local to this mempool,
-- unlike the transaction hash. This allows us to ask for all transactions
-- after a known sequence number, to get new transactions. It is also used to
-- look up individual transactions.
--
-- Note that it is expected that 'mempoolLookupTx' will often return 'Nothing'
-- even for tx sequence numbers returned in previous snapshots. This happens
-- when the transaction has been removed from the mempool between snapshots.
--
data MempoolSnapshot txid tx idx =
     MempoolSnapshot {
       mempoolTxIdsAfter :: idx -> [(txid, idx, TxSizeInBytes)],
       mempoolLookupTx   :: idx -> Maybe tx,
       mempoolHasTx      :: txid -> Bool
     }

mapMempoolSnapshot ::
     (tx -> tx')
  -> MempoolSnapshot txid tx  idx
  -> MempoolSnapshot txid tx' idx
mapMempoolSnapshot f snap =
     snap {
       mempoolLookupTx = fmap f . mempoolLookupTx snap
     }
