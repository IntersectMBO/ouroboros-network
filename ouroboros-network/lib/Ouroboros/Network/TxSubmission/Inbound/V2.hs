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
  , mkTxDecisionPolicy
  , TxConfigurationError (..)
  , TxSubmissionInitDelay (..)
  , TxSubmissionConfig (..)
  , defaultTxSubmissionConfigV2
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
-- After the initial delay the server puts the peer in the fetch tiers'
-- order ('joinTiers'), once.  Each call to 'runNextPeerAction' then
-- atomically inspects the shared state and
-- this peer's local protocol state and returns one of: submit buffered
-- bodies to the mempool, request bodies, request txids (blocking or
-- pipelined), or do nothing, with the bodies or the txids to send
-- resolved in that same transaction.  When idle, the server parks on
-- 'awaitPark' until the counter the decision chose, if one, moves or an
-- optional wake delay expires, then re-evaluates.  Body requests are
-- pipelined; in pipelined mode txid requests become non-blocking and the
-- server collects replies via 'handleReplies'.
--
-- V2 server state machine.
--
-- Depth 'n' tracks outstanding pipelined replies (type-level Nat).
--
-- States (non-pipelined, n = 0):
--   serverIdle           - park on awaitPark or pick next action
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
--     [ResolvedDoNothing]       --> awaitPark           --> serverIdle
--     [ResolvedSubmitTxs txs]    --> submitBufferedTxs txs serverIdle
--     [ResolvedRequestTxs txs]   --> requestTxBodies 0 txs
--                                 --> continueAfterBodyRequests 1
--     [ResolvedRequestTxIds] when unack queue empty --> blocking wire request,
--                                                    wait for reply --> serverIdle
--     [ResolvedRequestTxIds] otherwise              --> pipelined wire request
--                                                    --> handleReplies 1
--
--   handleReplies n --> handleReply (n-1)
--   handleReply
--     [CollectTxIds] --> applyReceivedTxIds --> continueAfterReplies (n-1)
--     [CollectTxs]   --> applyReceivedTxs   --> continueAfterReplies (n-1)
--
--   continueAfterReplies 0 = serverIdle
--   continueAfterReplies n@(>0), continueAfterBodyRequests n
--     [ResolvedSubmitTxs txs]    --> submitBufferedTxs txs (continueAfterReplies n)
--     [ResolvedRequestTxs txs]   --> requestTxBodies n txs
--     [ResolvedRequestTxIds]    --> serverReqTxIds n
--     [ResolvedDoNothing]       --> handleReplies n
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
      apiJitterSeed,
      joinTiers,
      awaitPark,
      runNextPeerAction,
      runNextPeerActionPipelined,
      applyReceivedTxIds,
      applyReceivedTxs,
      applySubmittedTxs
    } =
    TxSubmissionServerPipelined $ do
      case initDelay of
        TxSubmissionInitDelay delay -> threadDelay delay
        NoTxSubmissionInitDelay     -> return ()
      -- in the order from here on: a peer not polling yet holds no fast slot
      joinTiers
      continueWithStateM (serverIdle Nothing)
        emptyPeerTxLocalState { peerJitterSeed = apiJitterSeed }
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
    --   3. An idle peer wakes up after a @ResolvedDoNothing@ wait, with the
    --      timer of the park it wakes from and that park's deadline
    serverIdle :: Maybe (Time, RegisteredDelay m)
               -> StatefulM (PeerTxLocalState tx) Z txid tx m
    serverIdle held = StatefulM $ \peerState -> do
      now <- getMonotonicTime
      -- When the pipeline fully drains, report the body-download episode
      -- duration (covers all overlapping body and txid pipelined requests).
      let (mDownloadTime, peerState') =
            case peerDownloadStartTime peerState of
              Nothing -> (Nothing, peerState)
              Just startTime ->
                ( Just (now `diffTime` startTime)
                , peerState { peerDownloadStartTime = Nothing } )
      traceCanRequest Zero peerState'
      (peerAction, peerState'') <-
        runNextPeerAction now mDownloadTime
          (State.drainPeerScore policy now peerState')
      case peerAction of
           ResolvedDoNothing mDelay counter seen -> do
             -- Park at once: a second decision could find nothing this
             -- one did not, since the counter's value was read in its
             -- transaction and its delay is the earliest timed wake.  On
             -- the counter the decision chose: the claimable one when the
             -- slow tier's gate alone held a txid request, so that the
             -- peer waits for the gate or for an entry becoming claimable,
             -- not for every move of the wake counter; none when nothing
             -- it advertises is live, the gate alone.  With the timer of
             -- the park woken from when the deadline is the same, as it
             -- is for a gate a wake left unchanged: a wake registers no
             -- timer of its own then.
             timer <- parkTimer now held ((`addTime` now) <$> mDelay)
             atomically $ awaitPark counter seen (snd <$> timer)
             continueWithStateM (serverIdle timer) peerState''
           ResolvedSubmitTxs bufferedTxs -> leaving $
             continueWithStateM (submitBufferedTxs bufferedTxs (serverIdle Nothing)) peerState''
           ResolvedRequestTxs txsToRequest -> leaving $
             continueWithStateM (requestTxBodies Zero txsToRequest) peerState''
           ResolvedRequestTxIds txIdsToAck txIdsToReq -> leaving $
             continueWithStateM (serverReqTxIds Zero txIdsToAck txIdsToReq) peerState''
      where
        -- the park's timer is of no use to a peer leaving the idle loop
        leaving k = mapM_ (RegisteredDelay.cancel . snd) held >> k

    -- | A park's timer: the one held when its deadline is the park's,
    -- else a new one for the deadline, the held one cancelled; none for
    -- a park without a deadline.
    parkTimer :: Time
              -> Maybe (Time, RegisteredDelay m)
              -> Maybe Time
              -> m (Maybe (Time, RegisteredDelay m))
    parkTimer now held deadline_m = case (held, deadline_m) of
      (Just (d, timer), Just d') | d == d' -> pure (Just (d, timer))
      _ -> do
        mapM_ (RegisteredDelay.cancel . snd) held
        traverse (\d -> (,) d <$> RegisteredDelay.new (d `diffTime` now)) deadline_m

    -- | Submit buffered transaction bodies to the mempool.
    submitBufferedTxs :: forall (n :: N).
                         [(TxKey, txid, tx)]
                      -> StatefulM (PeerTxLocalState tx) n txid tx m
                      -- ^ a continuation
                      -> StatefulM (PeerTxLocalState tx) n txid tx m
    submitBufferedTxs bufferedTxs k = StatefulM $ \peerState -> do
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
          -- the accepted keys with their txids, for the state to retain a
          -- tx by txid when it has forgotten the key meanwhile
          acceptedKeyed    = [ (txKey, txid') | txid' <- acceptedTxIds
                                              , Just txKey <- [Map.lookup txid' submittedKeyOf] ]
          rejectedKeys     = mapMaybe ((`Map.lookup` submittedKeyOf) . fst) rejectedTxs
          rejectedForTrace = fmap fst rejectedTxs
          acceptedCount    = length acceptedTxIds
          rejectedCount    = length rejectedTxs
          delta            = end `diffTime` start

      peerState' <- applySubmittedTxs end delta acceptedKeyed rejectedKeys
                                      peerState
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
                    -> Map.Map txid SizeInBytes
                    -> StatefulM (PeerTxLocalState tx) n txid tx m
    requestTxBodies n txsToRequest = StatefulM $ \peerState -> do
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
    continueAfterReplies Zero     = serverIdle Nothing
    continueAfterReplies n@Succ{} = continueAfterBodyRequests n

    -- Continue processing after receiving transaction body replies in pipelined mode.
    continueAfterBodyRequests :: forall (n :: N).
                                 Nat (S n)
                              -> StatefulM (PeerTxLocalState tx) (S n) txid tx m
    continueAfterBodyRequests n = StatefulM $ \peerState -> do
      now <- getMonotonicTime
      traceCanRequest n peerState
      (peerAction, peerState') <- runNextPeerActionPipelined now (State.drainPeerScore policy now peerState)
      case peerAction of
        ResolvedSubmitTxs bufferedTxs ->
          continueWithStateM (submitBufferedTxs bufferedTxs (continueAfterReplies n)) peerState'
        ResolvedRequestTxs txsToRequest ->
          continueWithStateM (requestTxBodies n txsToRequest) peerState'
        ResolvedRequestTxIds txIdsToAck txIdsToReq ->
          continueWithStateM (serverReqTxIds n txIdsToAck txIdsToReq) peerState'
        ResolvedDoNothing {} ->
          pure $ continueWithState (handleReplies n) peerState'

    -- Construct and send a txid request message to the peer.
    serverReqTxIds :: forall (n :: N).
                      Nat n
                   -> NumTxIdsToAck
                   -> NumTxIdsToReq
                   -> StatefulM (PeerTxLocalState tx) n txid tx m
    -- No requests pending; transitions back to @serverIdle@
    serverReqTxIds Zero 0 0 = serverIdle Nothing

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
                        let txids' = NonEmpty.toList txids
                        -- protocol checks
                        checkReceivedTxIds txIdsToReq txids'
                        peerState' <- applyReceivedTxIds now
                                        (Just (now `diffTime` sendTime))
                                        txIdsToReq txids' peerState
                        continueWithStateM (serverIdle Nothing) peerState')
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
    handleReplies (Succ n) = Stateful $ \peerState ->
      CollectPipelined Nothing (collectAndContinueWithState (handleReply n) peerState)

    -- Process a single pipelined reply from the peer.
    handleReply :: forall (n :: N).
                   Nat n
                -> StatefulCollect (PeerTxLocalState tx) n txid tx m
    handleReply n = StatefulCollect $ \peerState -> \case
      CollectTxIds txIdsToReq txids -> do
        -- protocol checks
        checkReceivedTxIds txIdsToReq txids
        now <- getMonotonicTime
        peerState' <- applyReceivedTxIds now Nothing txIdsToReq txids peerState
        continueWithStateM (continueAfterReplies n) peerState'

      CollectTxs requested txs -> do
        let received = Map.fromList [ (txId tx, tx) | tx <- txs ]
            wrongSizedTxs = collectWrongSizedTxs requested received
        -- every body answers a requested txid, and no txid twice: a second
        -- copy would answer no pending key and pass for a late body,
        -- cancelling the omission of a key the peer left out
        unless (Map.keysSet received `Set.isSubsetOf` Map.keysSet requested
                && Map.size received == length txs) $ do
          traceWith tracer (TraceTxInboundError ProtocolErrorTxNotRequested)
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

    -- Validate a txid reply: the peer must not send more txids than we asked
    -- for, nor advertise a tx above `max_TX_SIZE`.  The latter guarantees
    -- that the reply to a body batch with a single tx, which may exceed
    -- `txsSizePerBatch`, fits within the `StTxs` size limit.
    checkReceivedTxIds :: NumTxIdsToReq
                       -> [(txid, SizeInBytes)]
                       -> m ()
    checkReceivedTxIds txIdsToReq txids = do
      unless (length txids <= fromIntegral txIdsToReq) $
        throwIO ProtocolErrorTxIdsNotRequested
      let tooLarge = filter ((> max_TX_SIZE) . snd) txids
      unless (null tooLarge) $ do
        let protocolError = ProtocolErrorTxSizeTooLarge tooLarge
        traceWith tracer (TraceTxInboundError protocolError)
        throwIO protocolError

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


