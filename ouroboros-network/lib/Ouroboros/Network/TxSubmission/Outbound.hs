{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

module Ouroboros.Network.TxSubmission.Outbound
  ( txSubmissionOutbound
  , TraceTxSubmissionOutbound (..)
  , TxSubmissionProtocolError (..)
  ) where

import Data.Foldable qualified as Foldable
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (mapMaybe)
import Data.Sequence.Strict (StrictSeq)
import Data.Sequence.Strict qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set

import Control.Exception (assert)
import Control.Monad (unless, when)
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Control.Tracer (Tracer (..), traceWith)

import Ouroboros.Network.ControlMessage (ControlMessage, ControlMessageSTM,
           timeoutWithControlMessage)
import Ouroboros.Network.Protocol.TxSubmission2.Client
import Ouroboros.Network.Protocol.TxSubmission2.Type
import Ouroboros.Network.TxSubmission.Mempool.Reader (MempoolSnapshot (..),
           TxSubmissionMempoolReader (..))


data TraceTxSubmissionOutbound txid tx
  = TraceTxSubmissionOutboundRecvMsgRequestTxs
      [txid]
      -- ^ The IDs of the transactions requested.
  | TraceTxSubmissionOutboundSendMsgReplyTxs
      [tx]
      -- ^ The transactions to be sent in the response.
  | TraceControlMessage ControlMessage
  deriving Show

data TxSubmissionProtocolError =
       ProtocolErrorAckedTooManyTxids
     | ProtocolErrorRequestedNothing
     | ProtocolErrorRequestedTooManyTxids NumTxIdsToReq Int NumTxIdsToAck
     | ProtocolErrorRequestBlocking
     | ProtocolErrorRequestNonBlocking
     | ProtocolErrorRequestedUnavailableTx
     | forall txid. Show txid => ProtocolErrorDuplicateTxIds txid

deriving instance Show TxSubmissionProtocolError

instance Exception TxSubmissionProtocolError where
  displayException ProtocolErrorAckedTooManyTxids =
      "The peer tried to acknowledged more txids than are available to do so."

  displayException (ProtocolErrorRequestedTooManyTxids reqNo unackedNo maxUnacked) =
      "The peer requested " ++ show reqNo ++ " txids which would put the "
   ++ "total in flight over the limit of " ++ show maxUnacked ++ "."
   ++ " Number of unacked txids " ++ show unackedNo

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

  displayException (ProtocolErrorDuplicateTxIds txids) =
      "The peer requested duplicate txIds: " ++ show txids


txSubmissionOutbound
  :: forall version txid tx idx m.
     (Ord txid, Show txid, Ord idx, MonadSTM m, MonadThrow m)
  => Tracer m (TraceTxSubmissionOutbound txid tx)
  -> NumTxIdsToAck  -- ^ Maximum number of unacknowledged txids allowed
  -> TxSubmissionMempoolReader txid tx idx m
  -> version
  -> ControlMessageSTM m
  -> TxSubmissionClient txid tx m ()
txSubmissionOutbound tracer maxUnacked TxSubmissionMempoolReader{..} _version controlMessageSTM =
    TxSubmissionClient (pure (client Set.empty Seq.empty mempoolZeroIdx))
  where
    client :: Set idx
           -- ^ indices of the unacknowledged entries which were requested
           -> StrictSeq (txid, idx)
           -- ^ unacknowledged txids; a `txid` can have more than one entry
           -- if the tx was removed from the mempool and added again
           -> idx
           -> ClientStIdle txid tx m ()
    client !requestedIdxs !unackedSeq !lastIdx =
        ClientStIdle { recvMsgRequestTxIds, recvMsgRequestTxs }
      where
        recvMsgRequestTxIds :: forall blocking.
                               SingBlockingStyle blocking
                            -> NumTxIdsToAck
                            -> NumTxIdsToReq
                            -> m (ClientStTxIds blocking txid tx m ())
        recvMsgRequestTxIds blocking ackNo reqNo = do
          when (getNumTxIdsToAck ackNo > fromIntegral (Seq.length unackedSeq)) $
            throwIO ProtocolErrorAckedTooManyTxids

          let unackedNo :: Int
              unackedNo = Seq.length unackedSeq
          when (  unackedNo
                - fromIntegral ackNo
                + fromIntegral reqNo
                > fromIntegral maxUnacked) $
            throwIO (ProtocolErrorRequestedTooManyTxids reqNo unackedNo maxUnacked)

          -- Update our tracking state to remove the number of txids that the
          -- peer has acknowledged, and forget the acknowledged entries.  Both
          -- are forced here, so a blocking request does not retain the
          -- acknowledged window while it waits for the mempool.
          let (droppedSeq, !unackedSeq') = Seq.splitAt (fromIntegral ackNo) unackedSeq
              !requestedIdxs' = requestedIdxs
                                Set.\\ Set.fromList (snd <$> Foldable.toList droppedSeq)

          -- Update our tracking state with any extra txs available.
          let update txs =
                -- These txs should all be fresh
                assert (all (\(_, idx, _) -> idx > lastIdx) txs) $
                  let announced :: [(txid, idx)]
                      announced = [(txid, idx) | (txid, idx, _) <- txs]

                      unackedSeq'' :: StrictSeq (txid, idx)
                      !unackedSeq'' = unackedSeq' <> Seq.fromList announced

                      lastIdx' :: idx
                      !lastIdx'
                        | null txs  = lastIdx
                        | otherwise = idx where (_, idx, _) = last txs

                      txs'         :: [(txid, SizeInBytes)]
                      txs'          = [ (txid, size) | (txid, _, size) <- txs ]

                      -- announced entries have fresh indices, so they can
                      -- be requested
                      client'       = client requestedIdxs' unackedSeq'' lastIdx'
                  in  (txs', client')

          -- Grab info about any new txs after the last tx idx we've seen,
          -- up to the number that the peer has requested.
          case blocking of
            SingBlocking -> do
              when (reqNo == 0) $
                throwIO ProtocolErrorRequestedNothing
              unless (Seq.null unackedSeq') $
                throwIO ProtocolErrorRequestBlocking

              mbtxs <- timeoutWithControlMessage controlMessageSTM $
                do
                  MempoolSnapshot{mempoolTxIdsAfter} <- mempoolGetSnapshot
                  let txs = mempoolTxIdsAfter lastIdx
                  check (not $ null txs)
                  pure (take (fromIntegral reqNo) txs)

              case mbtxs of
                Nothing -> pure (SendMsgDone ())
                Just txs ->
                  let !(txs', client') = update txs
                      txs'' = case NonEmpty.nonEmpty txs' of
                        Just x -> x
                        -- Assert txs is non-empty: we blocked until txs was non-null,
                        -- and we know reqNo > 0, hence `take reqNo txs` is non-null.
                        Nothing -> error "txSubmissionOutbound: empty transaction's list"
                  in  pure (SendMsgReplyTxIds (BlockingReply txs'') client')

            SingNonBlocking -> do
              when (reqNo == 0 && ackNo == 0) $
                throwIO ProtocolErrorRequestedNothing
              when (Seq.null unackedSeq') $
                throwIO ProtocolErrorRequestNonBlocking

              txs <- atomically $ do
                MempoolSnapshot{mempoolTxIdsAfter} <- mempoolGetSnapshot
                let txs = mempoolTxIdsAfter lastIdx
                return (take (fromIntegral reqNo) txs)

              let !(txs', client') = update txs
              pure (SendMsgReplyTxIds (NonBlockingReply txs') client')

        recvMsgRequestTxs :: [txid]
                          -> m (ClientStTxs txid tx m ())
        recvMsgRequestTxs txids = do
          -- Trace the IDs of the transactions requested.
          traceWith tracer (TraceTxSubmissionOutboundRecvMsgRequestTxs txids)

          MempoolSnapshot{mempoolLookupTx} <- atomically mempoolGetSnapshot

          -- Look up the requested txids in 'unackedSeq', stopping at the first
          -- one which is unavailable or a duplicate.  Until then the txids are
          -- distinct elements of 'unackedSeq', so the fold stops within
          -- 'Seq.length unackedSeq + 1' steps, however long the request is.
          --
          -- Each entry of 'unackedSeq' can be requested once: a request for
          -- a `txid` takes its first entry which was not yet requested, so
          -- the entries of a `txid` are served in the order they were
          -- announced.  Requesting a `txid` all of whose entries were already
          -- requested is a protocol error.
          let lookupTxIds :: txid
                          -> Either TxSubmissionProtocolError ([idx], Set txid)
                          -> Either TxSubmissionProtocolError ([idx], Set txid)
              lookupTxIds _txid a@Left{} = a
              lookupTxIds txid (Right (idxs, !seen))
                | txid `Set.member` seen
                = Left (ProtocolErrorDuplicateTxIds txid)

                | Just (_, idx) <- r
                = Right (idx : idxs, seen')

                | any ((== txid) . fst) unackedSeq
                -- all entries of `txid` were already requested
                = Left (ProtocolErrorDuplicateTxIds txid)

                | otherwise
                = Left ProtocolErrorRequestedUnavailableTx
                where
                  -- find first not yet requested (txid, idx) pair
                  r = Foldable.find (\(t, i) -> t == txid
                                             && i `Set.notMember` requestedIdxs)
                                    unackedSeq
                  seen' = Set.insert txid seen

          (txidxs, _) <- either throwIO pure
                       $ foldr lookupTxIds
                               (Right ([], Set.empty))
                               txids

          let requestedIdxs' :: Set idx
              !requestedIdxs' = requestedIdxs <> Set.fromList txidxs

          -- The 'mempoolLookupTx' will return nothing if the transaction is no
          -- longer in the mempool. This is good. Neither the sending nor
          -- receiving side wants to forward txs that are no longer of interest.
          let txs          = mapMaybe mempoolLookupTx txidxs
              client'      = client requestedIdxs' unackedSeq lastIdx

          -- Trace the transactions to be sent in the response.
          traceWith tracer (TraceTxSubmissionOutboundSendMsgReplyTxs txs)

          return $ SendMsgReplyTxs txs client'
