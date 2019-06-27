{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE BangPatterns        #-}

module Ouroboros.Network.TxSubmission.Outbound (
    txSubmissionOutbound,
    TxSubmissionMempoolReader(..),
    TraceTxSubmissionOutbound(..),
    TxSubmissionProtocolError(..),
  ) where

import           Data.Word (Word16)
import           Data.Maybe (isNothing, catMaybes)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq)
import           Data.Foldable (foldl')
import qualified Data.Foldable as Foldable

import           Control.Monad (when, unless)
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Exception (Exception(..), assert)
import           Control.Tracer (Tracer)

import           Ouroboros.Network.Protocol.TxSubmission.Client


-- | The consensus layer functionality that the outbound side of the tx
-- submission logic requires.
--
-- This is provided to the tx submission logic by the consensus layer.
--
data TxSubmissionMempoolReader txid tx idx m =
     TxSubmissionMempoolReader {

       -- | In STM, grab a snapshot of the contents of the mempool. This allows
       -- further pure queries on the snapshot.
       --
       mempoolGetSnapshot  :: STM m (MempoolSnapshot txid tx idx),

       -- | 'mempoolTxIdsAfter' with 'mempoolZeroIdx' is expected to give all
       -- txs currently in the mempool.
       mempoolZeroIdx      :: idx
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
       mempoolLookupTx   :: idx -> Maybe tx
     }

data TraceTxSubmissionOutbound txid tx = TraceTxSubmissionOutbound --TODO
  deriving Show

data TxSubmissionProtocolError =
       ProtocolErrorAckedTooManyTxids
     | ProtocolErrorRequestedNothing
     | ProtocolErrorRequestedTooManyTxids Word16 Word16
     | ProtocolErrorRequestBlocking
     | ProtocolErrorRequestNonBlocking
     | ProtocolErrorRequestedUnavailableTx
  deriving Show

instance Exception TxSubmissionProtocolError where
  displayException ProtocolErrorAckedTooManyTxids =
      "The peer tried to acknowledged more txids than are available to do so."

  displayException (ProtocolErrorRequestedTooManyTxids reqNo maxUnacked) =
      "The peer requested " ++ show reqNo ++ " txids which would put the "
   ++ "total in flight over the limit of " ++ show maxUnacked

  displayException ProtocolErrorRequestedNothing =
      "The peer requested zero txids."

  displayException ProtocolErrorRequestBlocking =
      "The peer made a blocking request for more txids when there are still "
   ++ "unacknowledged txids. It should have used a non-blocking request."

  displayException ProtocolErrorRequestNonBlocking =
      "The peer made a non-blocking request for more txids when there are "
   ++ "no unacknowledged txids. It should have used a blocking request."

  displayException ProtocolErrorRequestedUnavailableTx =
      "The peer requested a transaction which is not available, either " 
   ++ "because it was never available or because it was previously requested."


txSubmissionOutbound
  :: forall txid tx idx m.
     (Ord txid, Ord idx, MonadSTM m, MonadThrow m)
  => Tracer m (TraceTxSubmissionOutbound txid tx)
  -> Word16         -- ^ Maximum number of unacknowledged txids allowed
  -> TxSubmissionMempoolReader txid tx idx m
  -> TxSubmissionClient txid tx m ()
txSubmissionOutbound _tracer maxUnacked TxSubmissionMempoolReader{..} =
    TxSubmissionClient (pure (client Seq.empty Map.empty mempoolZeroIdx))
  where
    client :: Seq txid -> Map txid idx -> idx -> ClientStIdle txid tx m ()
    client !unackedSeq !unackedMap !lastIdx =
        assert invariant
        ClientStIdle { recvMsgRequestTxIds, recvMsgRequestTxs }
      where
        invariant =
          Map.isSubmapOfBy
            (\_ _ -> True)
            unackedMap
            (Map.fromList [ (x, ()) | x <- Foldable.toList unackedSeq ])

        recvMsgRequestTxIds :: forall blocking.
                               TokBlockingStyle blocking
                            -> Word16
                            -> Word16
                            -> m (ClientStTxIds blocking txid tx m ())
        recvMsgRequestTxIds blocking ackNo reqNo = do

          when (ackNo > fromIntegral (Seq.length unackedSeq)) $
            throwM ProtocolErrorAckedTooManyTxids

          when (  fromIntegral (Seq.length unackedSeq)
                - ackNo
                + fromIntegral reqNo
                > maxUnacked) $
            throwM (ProtocolErrorRequestedTooManyTxids reqNo maxUnacked)

          -- Update our tracking state to remove the number of txids that the
          -- peer has acknowledged.
          let !unackedSeq' = Seq.drop (fromIntegral ackNo) unackedSeq
              !unackedMap' = foldl' (flip Map.delete) unackedMap
                                    (Seq.take (fromIntegral ackNo) unackedSeq)

          -- Grab info about any new txs after the last tx idx we've seen,
          -- up to  the number that the peer has requested.
          txs <- case blocking of
            TokBlocking -> do
              when (reqNo == 0) $
                throwM ProtocolErrorRequestedNothing
              unless (Seq.null unackedSeq') $
                throwM ProtocolErrorRequestBlocking

              atomically $ do
                MempoolSnapshot{mempoolTxIdsAfter} <- mempoolGetSnapshot
                let txs = mempoolTxIdsAfter lastIdx
                -- but block until there are some
                check (not (null txs))
                return (take (fromIntegral reqNo) txs)

            TokNonBlocking -> do
              when (reqNo == 0 && ackNo == 0) $
                throwM ProtocolErrorRequestedNothing
              when (Seq.null unackedSeq') $
                throwM ProtocolErrorRequestNonBlocking

              atomically $ do
                MempoolSnapshot{mempoolTxIdsAfter} <- mempoolGetSnapshot
                let txs = mempoolTxIdsAfter lastIdx
                return (take (fromIntegral reqNo) txs)

          -- These txs should all be fresh
          assert (all (\(_, idx, _) -> idx > lastIdx) txs) (return ())

          -- Update our tracking state with any extra txs available.
          let !unackedSeq'' = unackedSeq' <> Seq.fromList
                                [ txid | (txid, _, _) <- txs ]
              !unackedMap'' = unackedMap' <> Map.fromList
                                [ (txid, idx) | (txid, idx, _) <- txs ]
              !lastIdx'
                | null txs  = lastIdx
                | otherwise = idx where (_, idx, _) = last txs
              txs'         :: [(txid, TxSizeInBytes)]
              txs'          = [ (txid, size) | (txid, _, size) <- txs ]
              client'       = client unackedSeq'' unackedMap'' lastIdx'

          -- Our reply type is different in the blocking vs non-blocking cases
          return $! case blocking of
            TokNonBlocking -> SendMsgReplyTxIds (NonBlockingReply txs') client'
            TokBlocking    -> SendMsgReplyTxIds (BlockingReply   txs'') client'
              where
                Just txs'' = NonEmpty.nonEmpty txs'
                -- Assert txs is non-empty: we blocked until txs was non-null,
                -- and we know reqNo > 0, hence take reqNo txs is non-null.


        recvMsgRequestTxs :: [txid]
                          -> m (ClientStTxs txid tx m ())
        recvMsgRequestTxs txids = do
          MempoolSnapshot{mempoolLookupTx} <- atomically mempoolGetSnapshot

          let txidxs  = [ Map.lookup txid unackedMap | txid <- txids ]
              txidxs' = catMaybes txidxs

          when (any isNothing txidxs) $
            throwM ProtocolErrorRequestedUnavailableTx

          -- The 'mempoolLookupTx' will return nothing if the transaction is no
          -- longer in the mempool. This is good. Neither the sending nor
          -- receiving side wants to forward txs that are no longer of interest.
          let txs          = catMaybes (map mempoolLookupTx txidxs')
              !unackedMap' = foldl' (flip Map.delete) unackedMap txids
              client'      = client unackedSeq unackedMap' lastIdx

          return $ SendMsgReplyTxs txs client'

