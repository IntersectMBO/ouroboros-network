module Ouroboros.Network.TxSubmission.Inbound.Types
  ( ProcessedTxCount (..)
  , TxSubmissionMempoolWriter (..)
  , TraceTxSubmissionInbound (..)
  , TxSubmissionProtocolError (..)
  ) where

import Control.Exception (Exception (..))

import Ouroboros.Network.TxSubmission.Inbound.Decision (TxDecision (..))


data ProcessedTxCount = ProcessedTxCount {
      -- | Just accepted this many transactions.
      ptxcAccepted :: Int
      -- | Just rejected this many transactions.
    , ptxcRejected :: Int
    }
  deriving (Eq, Show)


-- | The consensus layer functionality that the inbound side of the tx
-- submission logic requires.
--
-- This is provided to the tx submission logic by the consensus layer.
--
data TxSubmissionMempoolWriter txid tx idx m =
     TxSubmissionMempoolWriter {

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
       mempoolAddTxs :: [tx] -> m [txid]
    }


data TraceTxSubmissionInbound txid tx =
    -- | Number of transactions just about to be inserted.
    TraceTxSubmissionCollected Int
    -- | Just processed transaction pass/fail breakdown.
  | TraceTxSubmissionProcessed ProcessedTxCount
    -- | Server received 'MsgDone'
  | TraceTxInboundCanRequestMoreTxs Int
  | TraceTxInboundCannotRequestMoreTxs Int
  | TraceTxInboundAddedToMempool [txid]

  --
  -- messages emitted by the new implementation of the server in
  -- "Ouroboros.Network.TxSubmission.Inbound.Server"; some of them are also
  -- used in this module.
  --

  | TraceTxInboundTerminated
  | TraceTxInboundDecision (TxDecision txid tx)
  deriving (Eq, Show)


data TxSubmissionProtocolError =
       ProtocolErrorTxNotRequested
     | ProtocolErrorTxIdsNotRequested
  deriving Show

instance Exception TxSubmissionProtocolError where
  displayException ProtocolErrorTxNotRequested =
      "The peer replied with a transaction we did not ask for."
  displayException ProtocolErrorTxIdsNotRequested =
      "The peer replied with more txids than we asked for."
