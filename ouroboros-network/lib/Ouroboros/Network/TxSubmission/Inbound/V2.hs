{-# LANGUAGE BangPatterns        #-}
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
import Ouroboros.Network.TxSubmission.Inbound.V2.State qualified as State
import Ouroboros.Network.TxSubmission.Inbound.V2.Types as V2
import Ouroboros.Network.TxSubmission.Mempool.Reader

-- The same Stateful types as V1 uses.
newtype Stateful s n txid tx m = Stateful (s -> ServerStIdle n txid tx m ())

newtype StatefulM s n txid tx m
  = StatefulM (s -> m (ServerStIdle n txid tx m ()))

newtype StatefulCollect s n txid tx m
  = StatefulCollect (s -> Collect txid tx -> m (ServerStIdle n txid tx m ()))

continueWithState :: Stateful s n txid tx m
                  -> s
                  -> ServerStIdle n txid tx m ()
continueWithState (Stateful f) !st =
    f st

continueWithStateM :: StatefulM s n txid tx m
                   -> s
                   -> m (ServerStIdle n txid tx m ())
continueWithStateM (StatefulM f) !st =
    f st
{-# NOINLINE continueWithStateM #-}

collectAndContinueWithState :: StatefulCollect s n txid tx m
                            -> s
                            -> Collect txid tx
                            -> m (ServerStIdle n txid tx m ())
collectAndContinueWithState (StatefulCollect f) !st c =
    f st c
{-# NOINLINE collectAndContinueWithState #-}

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
  -> TxDecisionPolicy
  -> TxSubmissionMempoolReader txid tx idx m
  -> TxSubmissionMempoolWriter txid tx idx m err
  -> (tx -> SizeInBytes)
  -> PeerTxAPI m txid tx
  -> TxSubmissionServerPipelined txid tx m ()
txSubmissionInboundV2
    tracer
    initDelay
    policy
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
      resolveTxRequest,
      resolveBufferedTxs,
      startSubmittingTxs,
      addCounters
    } =
    TxSubmissionServerPipelined $ do
      case initDelay of
        TxSubmissionInitDelay delay -> threadDelay delay
        NoTxSubmissionInitDelay     -> return ()
      continueWithStateM serverIdle emptyPeerTxLocalState
  where

    -- Entry point and reset state for the non-pipelined server loop.
    --
    -- This function is called when:
    --   1. The server first starts
    --   2. All pipelined requests have completed and the counter returns to zero
    --   3. An idle peer wakes up after a @PeerDoNothing@ wait
    serverIdle :: StatefulM (PeerTxLocalState tx) Z txid tx m
    serverIdle = StatefulM $ \peerState -> do
      now <- getMonotonicTime
      -- When the pipeline fully drains, emit the body-download episode
      -- duration (covers all overlapping body and txid pipelined requests).
      peerState' <- case peerDownloadStartTime peerState of
                         Nothing        -> pure peerState
                         Just startTime -> do
                           addCounters mempty { txPipelineWaitMs =
                                                  diffTimeToMillis (now `diffTime` startTime) }
                           pure $ peerState { peerDownloadStartTime = Nothing }
      (peerAction, peerState'') <- runNextPeerAction now (State.drainPeerScore policy now peerState')
      case peerAction of
           PeerDoNothing generation mDelay -> do
             awaitSharedChange generation mDelay
             continueWithStateM serverIdle peerState''
           PeerSubmitTxs txKeys ->
             continueWithStateM (submitBufferedTxs txKeys serverIdle) peerState''
           PeerRequestTxs txKeys ->
             continueWithStateM (requestTxBodies Zero txKeys) peerState''
           PeerRequestTxIds _flavour txIdsToAck txIdsToReq ->
             continueWithStateM (serverReqTxIds Zero txIdsToAck txIdsToReq) peerState''

    -- | Submit buffered transaction bodies to the mempool.
    submitBufferedTxs :: forall (n :: N).
                         [TxKey]
                      -> StatefulM (PeerTxLocalState tx) n txid tx m
                      -> StatefulM (PeerTxLocalState tx) n txid tx m
    submitBufferedTxs txKeys k = StatefulM $ \peerState -> do
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

      addCounters mempty { txSubmissionWaitMs = diffTimeToMillis delta }
      peerState' <- applySubmittedTxs end resolvedTxKeys (fmap fst rejectedTxs) peerState
      let (score, peerState'') = State.applyPeerRejections policy end rejectedCount peerState'
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
      continueWithStateM k peerState''

    -- Request transaction bodies from the peer.
    requestTxBodies :: forall (n :: N).
                       Nat n
                    -> [TxKey]
                    -> StatefulM (PeerTxLocalState tx) n txid tx m
    requestTxBodies n txKeys = StatefulM $ \peerState -> do
      txsToRequest <- resolveTxRequest peerState txKeys
      traceWith tracer (TraceTxInboundRequestTxs (Map.keys txsToRequest))

      -- Record the start of the download episode on the first outstanding
      -- body request.  Subsequent pipelined requests leave the start time
      -- unchanged so we measure from first-send to last-receive.
      sendTime <- getMonotonicTime
      let peerState' = case peerDownloadStartTime peerState of
                            Nothing -> peerState { peerDownloadStartTime = Just sendTime }
                            Just _  -> peerState
      pure $ SendMsgRequestTxsPipelined txsToRequest
               (continueWithStateM (continueAfterBodyRequests (Succ n)) peerState')

    -- Continue processing after receiving replies from the peer in pipelined mode.
    continueAfterReplies :: forall (n :: N).
                            Nat n
                         -> StatefulM (PeerTxLocalState tx) n txid tx m
    continueAfterReplies Zero = serverIdle
    continueAfterReplies n@Succ{} = StatefulM $ \peerState -> do
      now <- getMonotonicTime
      (peerAction, peerState') <- runNextPeerActionPipelined now (State.drainPeerScore policy now peerState)
      case peerAction of
        PeerSubmitTxs txKeys ->
          continueWithStateM (submitBufferedTxs txKeys (continueAfterReplies n)) peerState'
        PeerRequestTxs txKeys ->
          continueWithStateM (requestTxBodies n txKeys) peerState'
        PeerRequestTxIds _flavour txIdsToAck txIdsToReq ->
          continueWithStateM (serverReqTxIds n txIdsToAck txIdsToReq) peerState'
        PeerDoNothing {} ->
          pure $ continueWithState (handleReplies n) peerState'

    -- Continue processing after receiving transaction body replies in pipelined mode.
    continueAfterBodyRequests :: forall (n :: N).
                                 Nat (S n)
                              -> StatefulM (PeerTxLocalState tx) (S n) txid tx m
    continueAfterBodyRequests n = StatefulM $ \peerState -> do
      now <- getMonotonicTime
      (peerAction, peerState') <- runNextPeerActionPipelined now (State.drainPeerScore policy now peerState)
      case peerAction of
        PeerSubmitTxs txKeys ->
          continueWithStateM (submitBufferedTxs txKeys (continueAfterReplies n)) peerState'
        PeerRequestTxs txKeys ->
          continueWithStateM (requestTxBodies n txKeys) peerState'
        PeerRequestTxIds _flavour txIdsToAck txIdsToReq ->
          continueWithStateM (serverReqTxIds n txIdsToAck txIdsToReq) peerState'
        PeerDoNothing {} ->
          pure $ continueWithState (handleReplies n) peerState'

    -- Construct and send a txid request message to the peer.
    serverReqTxIds :: forall (n :: N).
                      Nat n
                   -> NumTxIdsToAck
                   -> NumTxIdsToReq
                   -> StatefulM (PeerTxLocalState tx) n txid tx m
    -- No requests pending; transitions back to @serverIdle@
    serverReqTxIds Zero 0 0 = serverIdle

    -- Requests complete but pipeline not empty, continues to
    -- @handleReplies@ to process remaining in-flight replies
    serverReqTxIds n@Succ{} 0 0 = StatefulM $ \peerState ->
      pure $ continueWithState (handleReplies n) peerState

    -- Non-pipelined request, may send a blocking request
    serverReqTxIds Zero txIdsToAck txIdsToReq = StatefulM $ \peerState ->
      if StrictSeq.null (peerUnacknowledgedTxIds peerState)
         then do
           sendTime <- getMonotonicTime
           pure $ SendMsgRequestTxIdsBlocking
                    txIdsToAck
                    txIdsToReq
                    (traceWith tracer TraceTxInboundTerminated)
                    (\txids -> do
                        now <- getMonotonicTime
                        addCounters mempty { txIdBlockingWaitMs = diffTimeToMillis (now `diffTime` sendTime) }
                        let txids' = NonEmpty.toList txids
                        unless (length txids' <= fromIntegral txIdsToReq) $
                          throwIO ProtocolErrorTxIdsNotRequested
                        peerState' <- applyReceivedTxIds now txIdsToReq txids' peerState
                        continueWithStateM serverIdle peerState')
         else
           pure $ SendMsgRequestTxIdsPipelined
                    txIdsToAck
                    txIdsToReq
                    (pure $ continueWithState (handleReplies (Succ Zero)) peerState)

    -- Pipelined request at depth > 0. Sends a pipelined message and continues
    -- to @handleReplies@.
    serverReqTxIds n@Succ{} txIdsToAck txIdsToReq = StatefulM $ \peerState ->
      pure $ SendMsgRequestTxIdsPipelined
               txIdsToAck
               txIdsToReq
               (pure $ continueWithState (handleReplies (Succ n)) peerState)

    -- Prepare to collect pipelined replies from the peer.
    handleReplies :: forall (n :: N).
                     Nat (S n)
                  -> Stateful (PeerTxLocalState tx) (S n) txid tx m
    handleReplies (Succ Zero) = Stateful $ \peerState ->
      CollectPipelined Nothing (collectAndContinueWithState (handleReply Zero) peerState)

    handleReplies (Succ n'@Succ{}) = Stateful $ \peerState ->
      CollectPipelined Nothing (collectAndContinueWithState (handleReply n') peerState)

    -- Process a single pipelined reply from the peer.
    handleReply :: forall (n :: N).
                   Nat n
                -> StatefulCollect (PeerTxLocalState tx) n txid tx m
    handleReply n = StatefulCollect $ \peerState -> \case
      CollectTxIds txIdsToReq txids -> do
        unless (length txids <= fromIntegral txIdsToReq) $
          throwIO ProtocolErrorTxIdsNotRequested
        now <- getMonotonicTime
        peerState' <- applyReceivedTxIds now txIdsToReq txids peerState
        continueWithStateM (continueAfterReplies n) peerState'

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
        now <- getMonotonicTime
        (penaltyCount, peerState') <- applyReceivedTxs now [ (txId tx, tx) | tx <- txs ] peerState
        let peerState'' | penaltyCount == 0 = peerState'
                        | otherwise         = snd (State.applyPeerRejections policy now penaltyCount peerState')
        continueWithStateM (continueAfterReplies n) peerState''

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


