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
import Data.Maybe (mapMaybe)
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
import Ouroboros.Network.RegisteredDelay qualified as RegisteredDelay
import Ouroboros.Network.TxSubmission.Inbound.V2.Policy
import Ouroboros.Network.TxSubmission.Inbound.V2.Registry as V2
import Ouroboros.Network.TxSubmission.Inbound.V2.State qualified as State
import Ouroboros.Network.TxSubmission.Inbound.V2.Types as V2

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
{-# INLINE continueWithState #-}

continueWithStateM :: StatefulM s n txid tx m
                   -> s
                   -> m (ServerStIdle n txid tx m ())
continueWithStateM (StatefulM f) !st =
    f st
{-# INLINE continueWithStateM #-}

collectAndContinueWithState :: StatefulCollect s n txid tx m
                            -> s
                            -> Collect txid tx
                            -> m (ServerStIdle n txid tx m ())
collectAndContinueWithState (StatefulCollect f) !st =
    f st
{-# INLINE collectAndContinueWithState #-}

-- | A tx-submission inbound side (server, sic!).
--
-- Each call to 'runNextPeerAction' atomically inspects the shared state and
-- this peer's local protocol state and returns one of: submit buffered
-- bodies to the mempool, request bodies, request txids (blocking or
-- pipelined), or do nothing.  When idle, the server parks on
-- 'awaitSharedChange' until the shared-state generation moves or an
-- optional wake delay expires, then re-evaluates.  Body requests are
-- pipelined; in pipelined mode txid requests become non-blocking and the
-- server collects replies via 'handleReplies'.
--
-- V2 server state machine.
--
-- Depth 'n' tracks outstanding pipelined replies (type-level Nat).
--
-- States (non-pipelined, n = 0):
--   serverIdle           - park on awaitSharedChange or pick next action
--   serverReqTxIds 0     - send a txid request (blocking or pipelined)
--   submitBufferedTxs    - submit buffered bodies, run continuation
--   requestTxBodies      - send a pipelined body request, depth + 1
--
-- States (pipelined, n > 0):
--   continueAfterReplies      - post-reply pick; dispatch action or DoNothing
--   continueAfterBodyRequests - post-body-request pick; same dispatch
--   handleReplies n           - block on CollectPipelined for the next reply
--   handleReply  (n-1)        - apply received txids or bodies, then
--                               continueAfterReplies (n-1)
--
-- Transitions:
--
--   serverIdle
--     [PeerDoNothing]       --> awaitSharedChange   --> serverIdle
--     [PeerSubmitTxs ks]    --> submitBufferedTxs ks serverIdle
--     [PeerRequestTxs ks]   --> requestTxBodies 0 ks
--                                 --> continueAfterBodyRequests 1
--     [PeerRequestTxIds] when unack queue empty --> blocking wire request,
--                                                    wait for reply --> serverIdle
--     [PeerRequestTxIds] otherwise              --> pipelined wire request
--                                                    --> handleReplies 1
--
--   handleReplies n --> handleReply (n-1)
--   handleReply
--     [CollectTxIds] --> applyReceivedTxIds --> continueAfterReplies (n-1)
--     [CollectTxs]   --> applyReceivedTxs   --> continueAfterReplies (n-1)
--
--   continueAfterReplies 0 = serverIdle
--   continueAfterReplies n@(>0), continueAfterBodyRequests n
--     [PeerSubmitTxs ks]    --> submitBufferedTxs ks (continueAfterReplies n)
--     [PeerRequestTxs ks]   --> requestTxBodies n ks
--     [PeerRequestTxIds]    --> serverReqTxIds n
--     [PeerDoNothing]       --> handleReplies n
txSubmissionInboundV2
  :: forall txid tx idx m err.
     ( MonadDelay m
     , MonadTimer m
     , MonadThrow m
     , Ord txid
     , Show txid
     , Typeable txid
     )
  => Tracer m (TraceTxSubmissionInbound txid tx)
  -> TxSubmissionInitDelay
  -> TxDecisionPolicy
  -> TxSubmissionMempoolWriter txid tx idx m err
  -> (tx -> SizeInBytes)
  -> PeerTxAPI m txid tx
  -> TxSubmissionServerPipelined txid tx m ()
txSubmissionInboundV2
    tracer
    initDelay
    policy
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
      addCounters
    } =
    TxSubmissionServerPipelined $ do
      case initDelay of
        TxSubmissionInitDelay delay -> threadDelay delay
        NoTxSubmissionInitDelay     -> return ()
      continueWithStateM serverIdle emptyPeerTxLocalState
  where

    -- Mirror V1's TraceTxInboundCan/CannotRequestMoreTxs: emit once per
    -- scheduler iteration based on whether the peer currently advertises any
    -- txids whose bodies we could request.
    traceCanRequest :: forall (n :: N). Nat n -> PeerTxLocalState tx -> m ()
    traceCanRequest n st
      | null (peerAvailableTxIds st) =
          traceWith tracer (TraceTxInboundCannotRequestMoreTxs (natToInt n))
      | otherwise =
          traceWith tracer (TraceTxInboundCanRequestMoreTxs (natToInt n))

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
                                                  diffTimeToMilliseconds (now `diffTime` startTime) }
                           pure $ peerState { peerDownloadStartTime = Nothing }
      traceCanRequest Zero peerState'
      (peerAction, peerState'') <- runNextPeerAction now (State.drainPeerScore policy now peerState')
      case peerAction of
           PeerDoNothing generation mDelay -> do
             -- An Active->Idle transition means this peer has just become
             -- eligible for actions it could not take before (e.g. claiming
             -- expired leases as an idle claimant). Re-run the scheduler
             -- immediately rather than parking on a wake condition that may
             -- not fire.
             let cameToIdle = peerPhase peerState' /= PeerIdle
                           && peerPhase peerState'' == PeerIdle
             if cameToIdle
                then continueWithStateM serverIdle peerState''
                else do
                  mRegisteredDelay <- traverse RegisteredDelay.new mDelay
                  atomically $ awaitSharedChange generation mRegisteredDelay
                  continueWithStateM serverIdle peerState''
           PeerSubmitTxs txKeys ->
             continueWithStateM (submitBufferedTxs txKeys serverIdle) peerState''
           PeerRequestTxs txKeys ->
             continueWithStateM (requestTxBodies Zero txKeys) peerState''
           PeerRequestTxIds txIdsToAck txIdsToReq ->
             continueWithStateM (serverReqTxIds Zero txIdsToAck txIdsToReq) peerState''

    -- | Submit buffered transaction bodies to the mempool.
    submitBufferedTxs :: forall (n :: N).
                         [TxKey]
                      -> StatefulM (PeerTxLocalState tx) n txid tx m
                      -- ^ a continuation
                      -> StatefulM (PeerTxLocalState tx) n txid tx m
    submitBufferedTxs txKeys k = StatefulM $ \peerState -> do
      bufferedTxs <- resolveBufferedTxs peerState txKeys

      start <- getMonotonicTime
      let submitted = [ (txKey, txid') | (txKey, txid', _) <- bufferedTxs ]
          toSubmit  = [ tx | (_, _, tx) <- bufferedTxs ]

      (acceptedTxIds, rejectedTxs) <- if null toSubmit
                                         then pure ([], [])
                                         else mempoolAddTxs toSubmit
      end <- getMonotonicTime

      -- 'mempoolAddTxs' partitions the batch into accepted and
      -- rejected (see 'TxSubmissionMempoolWriter'); map each verdict's
      -- txids back to our 'TxKey's via the submitted batch.
      let submittedKeyOf   = Map.fromList [ (txid', txKey) | (txKey, txid') <- submitted ]
          resolvedTxKeys   = mapMaybe (`Map.lookup` submittedKeyOf) acceptedTxIds
          rejectedKeys     = mapMaybe ((`Map.lookup` submittedKeyOf) . fst) rejectedTxs
          rejectedForTrace = fmap fst rejectedTxs
          acceptedCount    = length acceptedTxIds
          rejectedCount    = length rejectedTxs
          delta            = end `diffTime` start

      addCounters mempty { txSubmissionWaitMs = diffTimeToMilliseconds delta }
      peerState' <- applySubmittedTxs end resolvedTxKeys rejectedKeys peerState
      let (score, peerState'') =
            State.applyPeerEvents policy end acceptedCount rejectedCount peerState'
      traceWith tracer $
        TraceTxSubmissionProcessed ProcessedTxCount {
            ptxcAccepted = acceptedCount,
            ptxcRejected = rejectedCount,
            ptxcScore    = score
          }
      unless (null acceptedTxIds) $
        traceWith tracer (TraceTxInboundAddedToMempool acceptedTxIds delta)
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
      traceCanRequest n peerState
      (peerAction, peerState') <- runNextPeerActionPipelined now (State.drainPeerScore policy now peerState)
      case peerAction of
        PeerSubmitTxs txKeys ->
          continueWithStateM (submitBufferedTxs txKeys (continueAfterReplies n)) peerState'
        PeerRequestTxs txKeys ->
          continueWithStateM (requestTxBodies n txKeys) peerState'
        PeerRequestTxIds txIdsToAck txIdsToReq ->
          continueWithStateM (serverReqTxIds n txIdsToAck txIdsToReq) peerState'
        PeerDoNothing {} ->
          pure $ continueWithState (handleReplies n) peerState'

    -- Continue processing after receiving transaction body replies in pipelined mode.
    continueAfterBodyRequests :: forall (n :: N).
                                 Nat (S n)
                              -> StatefulM (PeerTxLocalState tx) (S n) txid tx m
    continueAfterBodyRequests n = StatefulM $ \peerState -> do
      now <- getMonotonicTime
      traceCanRequest n peerState
      (peerAction, peerState') <- runNextPeerActionPipelined now (State.drainPeerScore policy now peerState)
      case peerAction of
        PeerSubmitTxs txKeys ->
          continueWithStateM (submitBufferedTxs txKeys (continueAfterReplies n)) peerState'
        PeerRequestTxs txKeys ->
          continueWithStateM (requestTxBodies n txKeys) peerState'
        PeerRequestTxIds txIdsToAck txIdsToReq ->
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
           addCounters mempty { txIdBlockingReqsSent = 1 }
           pure $ SendMsgRequestTxIdsBlocking
                    txIdsToAck
                    txIdsToReq
                    (traceWith tracer TraceTxInboundTerminated)
                    (\txids -> do
                        now <- getMonotonicTime
                        addCounters mempty { txIdBlockingWaitMs = diffTimeToMilliseconds (now `diffTime` sendTime) }
                        let txids' = NonEmpty.toList txids
                        unless (length txids' <= fromIntegral txIdsToReq) $
                          throwIO ProtocolErrorTxIdsNotRequested
                        peerState' <- applyReceivedTxIds now txIdsToReq txids' peerState
                        continueWithStateM serverIdle peerState')
         else do
           addCounters mempty { txIdPipelinedReqsSent = 1 }
           pure $ SendMsgRequestTxIdsPipelined
                    txIdsToAck
                    txIdsToReq
                    (pure $ continueWithState (handleReplies (Succ Zero)) peerState)

    -- Pipelined request at depth > 0. Sends a pipelined message and continues
    -- to @handleReplies@.
    serverReqTxIds n@Succ{} txIdsToAck txIdsToReq = StatefulM $ \peerState -> do
      addCounters mempty { txIdPipelinedReqsSent = 1 }
      pure $ SendMsgRequestTxIdsPipelined
               txIdsToAck
               txIdsToReq
               (pure $ continueWithState (handleReplies (Succ n)) peerState)

    -- Prepare to collect pipelined replies from the peer.
    handleReplies :: forall (n :: N).
                     Nat (S n)
                  -> Stateful (PeerTxLocalState tx) (S n) txid tx m
    handleReplies (Succ n) = Stateful $ \peerState ->
      CollectPipelined Nothing (collectAndContinueWithState (handleReply n) peerState)

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
        peerState'' <-
          if penaltyCount == 0
             then pure peerState'
             else do
               let (score, ps) = State.applyPeerEvents policy now 0 penaltyCount peerState'
               traceWith tracer $
                 TraceTxSubmissionProcessed ProcessedTxCount {
                     ptxcAccepted = 0,
                     ptxcRejected = penaltyCount,
                     ptxcScore    = score
                   }
               pure ps
        continueWithStateM (continueAfterReplies n) peerState''

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


