{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.TxSubmission.Inbound.V2
  ( -- * TxSubmision Inbound client
    txSubmissionInboundV2
    -- * PeerTxAPI
  , withPeer
  , PeerTxAPI
    -- * Supporting types
  , module V2
  , TxChannelsVar
  , newTxChannelsVar
  , TxMempoolSem
  , newTxMempoolSem
  , SharedTxStateVar
  , newSharedTxStateVar
  , TxDecisionPolicy (..)
  , defaultTxDecisionPolicy
  ) where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set qualified as Set

import Control.Exception (assert)
import Control.Monad (unless, when)
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer, traceWith)

import Network.TypedProtocol

import Ouroboros.Network.Protocol.Limits (longWait)
import Ouroboros.Network.Protocol.TxSubmission2.Server
import Ouroboros.Network.TxSubmission.Inbound.V2.Policy
import Ouroboros.Network.TxSubmission.Inbound.V2.Registry
import Ouroboros.Network.TxSubmission.Inbound.V2.State
import Ouroboros.Network.TxSubmission.Inbound.V2.Types as V2

-- | A tx-submission inbound side (server, sic!).
--
-- The server blocks on receiving `TxDecision` from the decision logic. If
-- there are tx's to download it pipelines two requests: first for tx's second
-- for txid's. If there are no tx's to download, it either sends a blocking or
-- non-blocking request for txid's.
--
txSubmissionInboundV2
  :: forall txid tx idx m.
     ( MonadDelay m
     , MonadThrow m
     , Ord txid
     )
  => Tracer m (TraceTxSubmissionInbound txid tx)
  -> TxSubmissionMempoolWriter txid tx idx m
  -> PeerTxAPI m txid tx
  -> TxSubmissionServerPipelined txid tx m ()
txSubmissionInboundV2
    tracer
    TxSubmissionMempoolWriter { txId }
    PeerTxAPI {
      readTxDecision,
      handleReceivedTxIds,
      handleReceivedTxs,
      submitTxToMempool
    }
    =
    TxSubmissionServerPipelined $ do
#ifdef TXSUBMISSION_DELAY
      -- make the client linger before asking for tx's and expending
      -- our resources as well, as he may disconnect for some reason
      threadDelay (fromMaybe (-1) longWait)
#endif
      serverIdle
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
          -- submitTxToMempool traces: `TraceTxSubmissionProcessed` and
          -- `TraceTxInboundAddedToMempool` events
          mapM_ (uncurry $ submitTxToMempool tracer) listOfTxsToMempool

        -- TODO:
        -- We can update the state so that other `tx-submission` servers will
        -- not try to add these txs to the mempool.
        if Set.null txsToRequest
          then serverReqTxIds Zero txd
          else serverReqTxs txd

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
        traceWith tracer $ TraceTxSubmissionCollected (txId `map` txs)
        case mbe of
          -- one of `tx`s had a wrong size
          Just e  -> throwIO e
          Nothing -> k
