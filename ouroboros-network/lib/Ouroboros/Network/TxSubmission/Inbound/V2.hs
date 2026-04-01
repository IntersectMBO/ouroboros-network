{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.TxSubmission.Inbound.V2
  ( -- * TxSubmission Inbound client
    txSubmissionInboundV2
    -- * Supporting types and APIs
  , module V2
  , TxDecisionPolicy (..)
  , defaultTxDecisionPolicy
  , TxSubmissionInitDelay (..)
  ) where

import Data.Functor (void)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set qualified as Set
import Data.Typeable (Typeable)

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad (unless)
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer, traceWith)

import Network.TypedProtocol

import Ouroboros.Network.Protocol.TxSubmission2.Server
import Ouroboros.Network.Protocol.TxSubmission2.Type (NumTxIdsToAck,
           SizeInBytes)
import Ouroboros.Network.TxSubmission.Inbound.V2.Policy
import Ouroboros.Network.TxSubmission.Inbound.V2.Registry as V2
import Ouroboros.Network.TxSubmission.Inbound.V2.Types as V2
import Ouroboros.Network.TxSubmission.Mempool.Reader


-- | A tx-submission inbound side (server, sic!).
--
-- The server blocks on receiving `TxDecision` from the decision logic. If
-- there are tx's to download it pipelines two requests: first for tx's second
-- for txid's. If there are no tx's to download, it either sends a blocking or
-- non-blocking request for txid's.
--
txSubmissionInboundV2
  :: forall txid tx idx m err.
     ( MonadDelay m
     , MonadSTM m
     , MonadThrow m
     , Ord txid
     , Show txid
     , Typeable txid
     )
  => Tracer m (TraceTxSubmissionInbound txid tx)
  -> TxSubmissionInitDelay
  -> TxSubmissionMempoolReader txid tx idx m
  -> TxSubmissionMempoolWriter txid tx idx m err
  -> (tx -> SizeInBytes)
  -> PeerTxAPI m txid tx
  -> TxSubmissionServerPipelined txid tx m ()
txSubmissionInboundV2
    tracer
    initDelay
    TxSubmissionMempoolReader { mempoolGetSnapshot }
    TxSubmissionMempoolWriter { txId, mempoolAddTxs }
    txSize
    PeerTxAPI {
      awaitSharedChange,
      runNextPeerAction,
      runNextPeerActionPipelined,
      applyReceivedTxIds,
      applyReceivedTxs,
      applySubmittedTxs,
      countRejectedTxs,
      resolveTxRequest,
      resolveBufferedTxs,
      startSubmittingTxs,
      addCounters
    } =
    TxSubmissionServerPipelined $ do
      -- The pipelined server API does not thread a user state parameter through
      -- `ServerStIdle`. Multiple continuations here resume after network IO and
      -- must all access and update the latest peer-local state, so a plain
      -- `PeerTxLocalState` captured in closures would go stale.
      --
      -- No other threads access the peer's peerStateVar.
      peerStateVar <- newTVarIO emptyPeerTxLocalState
      case initDelay of
        TxSubmissionInitDelay delay -> threadDelay delay
        NoTxSubmissionInitDelay     -> return ()
      serverIdle peerStateVar
  where

    -- Entry point and reset state for the non-pipelined server loop.
    --
    -- This function is called when:
    --   1. The server first starts
    --   2. All pipelined requests have completed and the counter returns to zero
    --   3. An idle peer wakes up after a @PeerDoNothing@ wait
    serverIdle :: StrictTVar m (PeerTxLocalState tx)
               -> m (ServerStIdle Z txid tx m ())
    serverIdle peerStateVar = do
      peerState <- readTVarIO peerStateVar

      now <- getMonotonicTime
      (peerAction, peerState') <- runNextPeerAction now peerState
      atomically $ writeTVar peerStateVar peerState'
      case peerAction of
           PeerDoNothing generation mDelay -> do
             awaitSharedChange generation mDelay
             serverIdle peerStateVar
           PeerSubmitTxs txKeys ->
             submitBufferedTxs peerStateVar txKeys (serverIdle peerStateVar)
           PeerRequestTxs txKeys ->
             requestTxBodies peerStateVar Zero txKeys
           PeerRequestTxIds txIdsToAck txIdsToReq -> do
                serverReqTxIds peerStateVar Zero txIdsToAck txIdsToReq

    -- | Submit buffered transaction bodies to the mempool.
    submitBufferedTxs :: forall (n :: N).
                         StrictTVar m (PeerTxLocalState tx)
                      -> [TxKey]
                      -> m (ServerStIdle n txid tx m ())
                      -> m (ServerStIdle n txid tx m ())
    submitBufferedTxs peerStateVar txKeys k = do

      peerState <- readTVarIO peerStateVar
      bufferedTxs <- resolveBufferedTxs peerState txKeys

      -- Flags the txs as on the way to the mempool, which temporarily blocks further
      -- download attempts.
      startSubmittingTxs txKeys

      start <- getMonotonicTime
      MempoolSnapshot { mempoolHasTx } <- atomically mempoolGetSnapshot

      -- Note that checking if the mempool contains a TX before
      -- spending several ms attempting to add it to the pool has
      -- been judged immoral.
      let (alreadyInMempool, pendingSubmit) = partitionBufferedTxs mempoolHasTx bufferedTxs
          submitted = [ (txKey, txid') | (txKey, txid', _) <- pendingSubmit ]
          toSubmit  = [ tx | (_, _, tx) <- pendingSubmit ]

      (acceptedTxIds, _) <- if null toSubmit
                               then pure ([], [])
                               else mempoolAddTxs toSubmit
      end <- getMonotonicTime

      let (acceptedTxs, rejectedTxs) =
            classifySubmittedTxs submitted (Set.fromList acceptedTxIds)
          resolvedTxKeys   = [ txKey | (txKey, _, _) <- alreadyInMempool ] <> fmap fst acceptedTxs
          rejectedForTrace = [ txid' | (_, txid', _) <- alreadyInMempool ] <> fmap snd rejectedTxs
          rejectedCount    = length rejectedForTrace
          delta = end `diffTime` start

      score <- countRejectedTxs end rejectedCount
      traceWith tracer $
        TraceTxSubmissionProcessed ProcessedTxCount {
            ptxcAccepted = length acceptedTxs,
            ptxcRejected = rejectedCount,
            ptxcScore    = score
          }
      unless (null acceptedTxs) $
        traceWith tracer (TraceTxInboundAddedToMempool (snd <$> acceptedTxs) delta)
      unless (null rejectedForTrace) $
        traceWith tracer (TraceTxInboundRejectedFromMempool rejectedForTrace delta)

      peerState' <- applySubmittedTxs end resolvedTxKeys (fmap fst rejectedTxs) peerState
      atomically $ writeTVar peerStateVar peerState'
      k

    -- Request transaction bodies from the peer.
    requestTxBodies :: forall (n :: N).
                       StrictTVar m (PeerTxLocalState tx)
                    -> Nat n
                    -> [TxKey]
                    -> m (ServerStIdle n txid tx m ())
    requestTxBodies peerStateVar n txKeys = do
      peerState <- readTVarIO peerStateVar
      txsToRequest <- resolveTxRequest peerState txKeys
      traceWith tracer (TraceTxInboundRequestTxs (Map.keys txsToRequest))
      addCounters mempty { txMessagesSent = 1
                         , txsRequested = fromIntegral (Map.size txsToRequest) }
      pure $ SendMsgRequestTxsPipelined txsToRequest
               (continueAfterBodyRequests peerStateVar (Succ n))

    -- Continue processing after receiving replies from the peer in pipelined mode.
    continueAfterReplies :: forall (n :: N).
                            StrictTVar m (PeerTxLocalState tx)
                         -> Nat n
                         -> m (ServerStIdle n txid tx m ())
    continueAfterReplies peerStateVar Zero = serverIdle peerStateVar
    continueAfterReplies peerStateVar n@Succ{} = do
      peerState <- readTVarIO peerStateVar
      now <- getMonotonicTime
      (peerAction, peerState') <- runNextPeerActionPipelined now peerState
      atomically $ writeTVar peerStateVar peerState'
      case peerAction of
        PeerSubmitTxs txKeys ->
          submitBufferedTxs peerStateVar txKeys (continueAfterReplies peerStateVar n)
        PeerRequestTxs txKeys ->
          requestTxBodies peerStateVar n txKeys
        PeerRequestTxIds txIdsToAck txIdsToReq ->
          serverReqTxIds peerStateVar n txIdsToAck txIdsToReq
        PeerDoNothing {} ->
          handleReplies peerStateVar n

    -- Continue processing after receiving transaction body replies in pipelined mode.
    continueAfterBodyRequests :: forall (n :: N).
                                 StrictTVar m (PeerTxLocalState tx)
                              -> Nat (S n)
                              -> m (ServerStIdle (S n) txid tx m ())
    continueAfterBodyRequests peerStateVar n = do
      peerState <- readTVarIO peerStateVar
      now <- getMonotonicTime
      (peerAction, peerState') <- runNextPeerActionPipelined now peerState
      atomically $ writeTVar peerStateVar peerState'
      case peerAction of
        PeerSubmitTxs txKeys ->
          submitBufferedTxs peerStateVar txKeys (continueAfterReplies peerStateVar n)
        PeerRequestTxs txKeys ->
          requestTxBodies peerStateVar n txKeys
        PeerRequestTxIds txIdsToAck txIdsToReq -> do
          serverReqTxIds peerStateVar n txIdsToAck txIdsToReq
        PeerDoNothing {} ->
          handleReplies peerStateVar n

    -- Construct and send a txid request message to the peer.
    serverReqTxIds :: forall (n :: N).
                      StrictTVar m (PeerTxLocalState tx)
                   -> Nat n
                   -> NumTxIdsToAck
                   -> NumTxIdsToReq
                   -> m (ServerStIdle n txid tx m ())
    -- No requests pending; transitions back to @serverIdle@
    serverReqTxIds peerStateVar Zero 0 0 = serverIdle peerStateVar

    -- Requests complete but pipeline not empty, continues to
    -- @handleReplies@ to process remaining in-flight replies
    serverReqTxIds peerStateVar n@Succ{} 0 0 = handleReplies peerStateVar n

    -- Non-pipelined request, may send a blocking request
    serverReqTxIds peerStateVar Zero txIdsToAck txIdsToReq = do
      peerState <- readTVarIO peerStateVar
      addCounters mempty { txIdMessagesSent = 1
                         , txIdsRequested = fromIntegral txIdsToReq }
      if StrictSeq.null (peerUnacknowledgedTxIds peerState)
         then
           pure $ SendMsgRequestTxIdsBlocking
                    txIdsToAck
                    txIdsToReq
                    (traceWith tracer TraceTxInboundTerminated)
                    (\txids -> do
                        now <- getMonotonicTime
                        let txids' = NonEmpty.toList txids
                        unless (length txids' <= fromIntegral txIdsToReq) $
                          throwIO ProtocolErrorTxIdsNotRequested
                        addCounters mempty { txIdRepliesReceived = 1
                                           , txIdsReceived = fromIntegral (length txids') }
                        peerStateCurrent <- readTVarIO peerStateVar
                        peerState' <- applyReceivedTxIds now txIdsToReq txids' peerStateCurrent
                        atomically $ writeTVar peerStateVar peerState'
                        serverIdle peerStateVar)
         else
           pure $ SendMsgRequestTxIdsPipelined
                    txIdsToAck
                    txIdsToReq
                    (handleReplies peerStateVar (Succ Zero))

    -- Pipelined request at depth > 0. Sends a pipelined message and continues
    -- to @handleReplies@.
    serverReqTxIds peerStateVar n@Succ{} txIdsToAck txIdsToReq = do
      addCounters mempty { txIdMessagesSent = 1
                         , txIdsRequested = fromIntegral txIdsToReq }
      pure $ SendMsgRequestTxIdsPipelined
               txIdsToAck
               txIdsToReq
               (handleReplies peerStateVar (Succ n))

    -- Prepare to collect pipelined replies from the peer.
    handleReplies :: forall (n :: N).
                     StrictTVar m (PeerTxLocalState tx)
                  -> Nat (S n)
                  -> m (ServerStIdle (S n) txid tx m ())
    handleReplies peerStateVar (Succ Zero) =
      pure $ CollectPipelined Nothing (handleReply peerStateVar Zero)

    handleReplies peerStateVar (Succ n'@Succ{}) =
      pure $ CollectPipelined Nothing (handleReply peerStateVar n')

    -- Process a single pipelined reply from the peer.
    handleReply :: forall (n :: N).
                   StrictTVar m (PeerTxLocalState tx)
                -> Nat n
                -> Collect txid tx
                -> m (ServerStIdle n txid tx m ())
    handleReply peerStateVar n = \case
      CollectTxIds txIdsToReq txids -> do
        unless (length txids <= fromIntegral txIdsToReq) $
          throwIO ProtocolErrorTxIdsNotRequested
        addCounters mempty { txIdRepliesReceived = 1
                           , txIdsReceived = fromIntegral (length txids) }
        peerState <- readTVarIO peerStateVar
        now <- getMonotonicTime
        peerState' <- applyReceivedTxIds now txIdsToReq txids peerState
        atomically $ writeTVar peerStateVar peerState'
        continueAfterReplies peerStateVar n

      CollectTxs requested txs -> do
        let received = Map.fromList [ (txId tx, tx) | tx <- txs ]
            wrongSizedTxs = collectWrongSizedTxs requested received
        unless (Map.keysSet received `Set.isSubsetOf` Map.keysSet requested) $
          throwIO ProtocolErrorTxNotRequested
        traceWith tracer $ TraceTxSubmissionCollected (txId <$> txs)
        unless (null wrongSizedTxs) $ do
          let protocolError = ProtocolErrorTxSizeError wrongSizedTxs
          traceWith tracer (TraceTxInboundError protocolError)
          throwIO protocolError
        peerState <- readTVarIO peerStateVar
        now <- getMonotonicTime
        (penaltyCount, peerState') <- applyReceivedTxs now [ (txId tx, tx) | tx <- txs ] peerState
        atomically $ writeTVar peerStateVar peerState'
        unless (penaltyCount == 0) $
          void $ countRejectedTxs now penaltyCount
        continueAfterReplies peerStateVar n

    -- Partition submitted transactions into accepted and rejected groups
    classifySubmittedTxs :: [(TxKey, txid)]
                         -> Set.Set txid
                         -> ([(TxKey, txid)], [(TxKey, txid)])
    classifySubmittedTxs submitted accepted =
      foldr classify ([], []) submitted
      where
        classify entry@(_, txid') (acceptedTxs, rejectedTxs)
          | Set.member txid' accepted = (entry : acceptedTxs, rejectedTxs)
          | otherwise                 = (acceptedTxs, entry : rejectedTxs)

    -- Partition buffered transactions by mempool presence.
    partitionBufferedTxs :: (txid -> Bool)
                         -> [(TxKey, txid, tx)]
                         -> ([(TxKey, txid, tx)], [(TxKey, txid, tx)])
    partitionBufferedTxs mempoolHasTx =
      foldr step ([], [])
      where
        step entry@(_, txid', _) (alreadyInMempool, pendingSubmit)
          | mempoolHasTx txid' = (entry : alreadyInMempool, pendingSubmit)
          | otherwise          = (alreadyInMempool, entry : pendingSubmit)

    -- Collect transactions with size mismatches between advertised and actual.
    collectWrongSizedTxs :: Map.Map txid SizeInBytes
                         -> Map.Map txid tx
                         -> [(txid, SizeInBytes, SizeInBytes)]
    collectWrongSizedTxs requestedTxIds receivedTxs =
      [ (txid', receivedSize, advertisedSize)
      | (txid', tx) <- Map.toList receivedTxs
      , let receivedSize = txSize tx
      , Just advertisedSize <- [Map.lookup txid' requestedTxIds]
      , not (checkTxSize receivedSize advertisedSize)
      ]

    -- Fuzzy size comparison that allows for +/- const_MAX_TX_SIZE_DISCREPANCY.
    checkTxSize :: SizeInBytes
                -> SizeInBytes
                -> Bool
    checkTxSize received advertised
      | received > advertised =
          received - advertised <= const_MAX_TX_SIZE_DISCREPANCY
      | otherwise =
          advertised - received <= const_MAX_TX_SIZE_DISCREPANCY
