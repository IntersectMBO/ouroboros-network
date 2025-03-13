{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.TxSubmission.Inbound.Server where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set qualified as Set

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (assert)
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Tracer (Tracer, traceWith)

import Network.TypedProtocol

import Control.Monad (unless, when)
import Ouroboros.Network.Protocol.TxSubmission2.Server
import Ouroboros.Network.TxSubmission.Inbound.Registry (PeerTxAPI (..))
import Ouroboros.Network.TxSubmission.Inbound.Types
import Ouroboros.Network.TxSubmission.Mempool.Reader

-- | Flag to enable/disable the usage of the new tx submission protocol
--
data EnableNewTxSubmissionProtocol =
      EnableNewTxSubmissionProtocol
    | DisableNewTxSubmissionProtocol
    deriving (Eq, Show)

-- | A tx-submission outbound side (server, sic!).
--
-- The server blocks on receiving `TxDecision` from the decision logic. If
-- there are tx's to download it pipelines two requests: first for tx's second
-- for txid's. If there are no tx's to download, it either sends a blocking or
-- non-blocking request for txid's.
--
txSubmissionInboundV2
  :: forall txid tx idx m.
     ( MonadSTM   m
     , MonadThrow m
     , MonadMonotonicTime m
     , Ord txid
     )
  => Tracer m (TraceTxSubmissionInbound txid tx)
  -> TxSubmissionMempoolReader txid tx idx m
  -> TxSubmissionMempoolWriter txid tx idx m
  -> PeerTxAPI m txid tx
  -> TxSubmissionServerPipelined txid tx m ()
txSubmissionInboundV2
    tracer
    TxSubmissionMempoolReader{
      mempoolGetSnapshot
    }
    TxSubmissionMempoolWriter {
      txId,
      mempoolAddTxs
    }
    PeerTxAPI {
      readTxDecision,
      handleReceivedTxIds,
      handleReceivedTxs,
      countRejectedTxs,
      withMempoolSem
    }
    =
    TxSubmissionServerPipelined serverIdle
  where
    serverIdle
      :: m (ServerStIdle Z txid tx m ())
    serverIdle = do
        -- Block on next decision.
        txd@TxDecision { txdTxsToRequest = txsToRequest,
                         txdTxsToMempool = TxsToMempool { listOfTxsToMempool } }
          <- readTxDecision
        traceWith tracer (TraceTxInboundDecision txd)

        let !collected = length listOfTxsToMempool

        -- Only attempt to add TXs if we have some work to do
        when (collected > 0) $ do
            mapM_ (withMempoolSem . addTx) listOfTxsToMempool

            traceWith tracer $
              TraceTxSubmissionCollected collected

        -- TODO:
        -- We can update the state so that other `tx-submission` servers will
        -- not try to add these txs to the mempool.
        if Set.null txsToRequest
          then serverReqTxIds Zero txd
          else serverReqTxs txd

    addTx :: (txid,tx) -> m (Either (txid, tx) (txid, tx))
    addTx (txid,tx) = do
      mpSnapshot <- atomically mempoolGetSnapshot

      -- Note that checking if the mempool contains a TX before
      -- spending several ms attempting to add it to the pool has
      -- been judged immoral.
      if mempoolHasTx mpSnapshot txid
         then do
           !now <- getMonotonicTime
           !s <- countRejectedTxs now 1
           traceWith tracer $ TraceTxSubmissionProcessed ProcessedTxCount {
                ptxcAccepted = 0
              , ptxcRejected = 1
              , ptxcScore    = s
              }
           return $ Left (txid, tx)
         else do
           !start <- getMonotonicTime
           acceptedTxs <- mempoolAddTxs [tx]
           !end <- getMonotonicTime
           let duration = diffTime end start

           traceWith tracer $
             TraceTxInboundAddedToMempool acceptedTxs duration
           if null acceptedTxs
              then do
                  !s <- countRejectedTxs end 1
                  traceWith tracer $ TraceTxSubmissionProcessed ProcessedTxCount {
                      ptxcAccepted = 0
                    , ptxcRejected = 1
                    , ptxcScore    = s
                    }
                  return $ Left (txid, tx)
              else do
                  !s <- countRejectedTxs end 0
                  traceWith tracer $ TraceTxSubmissionProcessed ProcessedTxCount {
                      ptxcAccepted = 1
                    , ptxcRejected = 0
                    , ptxcScore    = s
                    }
                  return $ Right (txid, tx)

    -- Pipelined request of txs
    serverReqTxs :: TxDecision txid tx
                 -> m (ServerStIdle Z txid tx m ())
    serverReqTxs txd@TxDecision { txdTxsToRequest = txdTxsToRequest } =
      pure $ SendMsgRequestTxsPipelined (Set.toList txdTxsToRequest)
                                        (serverReqTxIds (Succ Zero) txd)


    serverReqTxIds :: forall (n :: N).
                      Nat n
                   -> TxDecision txid tx
                   -> m (ServerStIdle n txid tx m ())
    serverReqTxIds
      n TxDecision { txdTxIdsToRequest = 0 }
      =
      case n of
        Zero   -> serverIdle
        Succ _ -> handleReplies n

    serverReqTxIds
      -- if there are no unacknowledged txids, the protocol requires sending
      -- a blocking `MsgRequestTxIds` request.  This is important, as otherwise
      -- the client side wouldn't have a chance to terminate the
      -- mini-protocol.
      Zero TxDecision { txdTxIdsToAcknowledge = txIdsToAck,
                        txdPipelineTxIds      = False,
                        txdTxIdsToRequest     = txIdsToReq
                      }
      =
      pure $ SendMsgRequestTxIdsBlocking
                txIdsToAck txIdsToReq
                -- Our result if the client terminates the protocol
                (traceWith tracer TraceTxInboundTerminated)
                (\txids -> do
                   let txids' = NonEmpty.toList txids
                       txidsSeq = StrictSeq.fromList $ fst <$> txids'
                       txidsMap = Map.fromList txids'
                   unless (StrictSeq.length txidsSeq <= fromIntegral txIdsToReq) $
                     throwIO ProtocolErrorTxIdsNotRequested
                   handleReceivedTxIds txIdsToReq txidsSeq txidsMap
                   serverIdle
                )

    serverReqTxIds
      n@Zero TxDecision { txdTxIdsToAcknowledge = txIdsToAck,
                          txdPipelineTxIds      = True,
                          txdTxIdsToRequest     = txIdsToReq
                        }
      =
      pure $ SendMsgRequestTxIdsPipelined
                txIdsToAck txIdsToReq
                (handleReplies (Succ n))

    serverReqTxIds
      n@Succ{} TxDecision { txdTxIdsToAcknowledge = txIdsToAck,
                            txdPipelineTxIds,
                            txdTxIdsToRequest     = txIdsToReq
                          }
      =
      -- it is impossible that we have had `tx`'s to request (Succ{} - is an
      -- evidence for that), but no unacknowledged `txid`s.
      assert txdPipelineTxIds $
      pure $ SendMsgRequestTxIdsPipelined
               txIdsToAck txIdsToReq
               (handleReplies (Succ n))


    handleReplies :: forall (n :: N).
                   Nat (S n)
                -> m (ServerStIdle (S n) txid tx m ())
    handleReplies (Succ n'@Succ{}) =
      pure $ CollectPipelined
                Nothing
                (handleReply (handleReplies n'))

    handleReplies (Succ Zero) =
      pure $ CollectPipelined
                Nothing
                (handleReply serverIdle)

    handleReply :: forall (n :: N).
                   m (ServerStIdle n txid tx m ())
                   -- continuation
                -> Collect txid tx
                -> m (ServerStIdle n txid tx m ())
    handleReply k = \case
      CollectTxIds txIdsToReq txids -> do
        let txidsSeq = StrictSeq.fromList $ fst <$> txids
            txidsMap = Map.fromList txids
        unless (StrictSeq.length txidsSeq <= fromIntegral txIdsToReq) $
          throwIO ProtocolErrorTxIdsNotRequested
        handleReceivedTxIds txIdsToReq txidsSeq txidsMap
        k
      CollectTxs txids txs -> do
        let requested = Set.fromList txids
            received  = Map.fromList [ (txId tx, tx) | tx <- txs ]

        unless (Map.keysSet received `Set.isSubsetOf` requested) $
          throwIO ProtocolErrorTxNotRequested

        mbe <- handleReceivedTxs requested received
        case mbe of
          -- one of `tx`s had a wrong size
          Just e  -> throwIO e
          Nothing -> k
