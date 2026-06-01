{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.TxSubmission.Inbound.V2.State
  ( handleReceivedTxIds
  , handleReceivedTxs
  , handleSubmittedTxs
  , markSubmittingTxs
  , nextPeerAction
  , nextPeerActionPipelined
  , currentPeerScore
  , drainPeerScore
  , peerClaimDelay
  , applyPeerEvents
  , sweepSharedState
  ) where

import Control.Monad.Class.MonadTime.SI (DiffTime, Time, addTime, diffTime)
import Data.Foldable (toList)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List as List (foldl')
import Data.Map.Strict qualified as Map
import Data.Sequence.Strict qualified as StrictSeq
import Data.Word (Word64)
import GHC.Stack (HasCallStack)

import Ouroboros.Network.Protocol.TxSubmission2.Type (NumTxIdsToAck,
           SizeInBytes)
import Ouroboros.Network.Tx (HasRawTxId (..))
import Ouroboros.Network.TxSubmission.Inbound.V2.Policy
import Ouroboros.Network.TxSubmission.Inbound.V2.Types

data TxIdRequestMode = AllowAnyTxIdRequests | AllowPipelinedTxIdRequests
  deriving Eq

-- | Precomputed context for selecting the next action for one peer.
--
data PeerActionContext peeraddr txid tx = PeerActionContext {
    -- | Current time used for lease expiry and score decay decisions.
    pacNow          :: !Time,
    -- | Decision policy that governs request, retry, and scoring limits.
    pacPolicy       :: !TxDecisionPolicy,
    -- | Address of the peer whose next action is being chosen.
    pacPeerAddr     :: !peeraddr,
    -- | Current peer-local state after local pruning has been applied.
    pacPeerState    :: !(PeerTxLocalState tx),
    -- | This peer's contribution counters mirroring shared-state writes.
    pacPeerInFlight :: !PeerTxInFlight,
    -- | Shared tx-submission state after shared pruning has been applied.
    pacSharedState  :: !(SharedTxState peeraddr txid),
    -- | Score-derived delay this peer must wait after a tx becomes claimable.
    pacClaimDelay   :: !DiffTime
  }

-- | Decision produced by 'pickPeerActionChoice'; consumed by
-- 'applyPeerActionChoice' to compute the resulting state updates.
--
data PeerActionChoice peeraddr =
    -- | Submit the buffered bodies for the given tx keys to the mempool.
    ChooseSubmit ![TxKey]
    -- | Request tx bodies: keys to request, total expected size, and the
    -- tx table updated with the new lease assignments.
  | ChooseRequestTxs ![TxKey] !SizeInBytes !(IntMap.IntMap (TxEntry peeraddr))
    -- | Send a txid request: acknowledged keys, ack count, request count,
    -- and the updated unacknowledged queue.  The wire send site decides
    -- whether the message is blocking or pipelined based on the
    -- (post-ack) unack queue.
  | ChooseRequestTxIds ![TxKey] !NumTxIdsToAck !NumTxIdsToReq !(StrictSeq.StrictSeq TxKey)
    -- | Park until shared state advances or the wake delay elapses: the
    -- observed shared-state generation and an optional wake delay.
  | ChooseDoNothing !Word64 !(Maybe DiffTime)

-- | Build a precomputed context for selecting the next action for a peer.
--
mkPeerActionContext :: Time
                    -> TxDecisionPolicy
                    -> peeraddr
                    -> PeerTxLocalState tx
                    -> PeerTxInFlight
                    -> SharedTxState peeraddr txid
                    -> PeerActionContext peeraddr txid tx
mkPeerActionContext now policy peeraddr peerState peerInFlight sharedState =
  PeerActionContext {
    pacNow = now,
    pacPolicy = policy,
    pacPeerAddr = peeraddr,
    pacPeerState = peerState',
    pacPeerInFlight = peerInFlight',
    pacSharedState = sharedState,
    pacClaimDelay = peerClaimDelay policy now (peerScore peerState')
    }
  where
    -- Drop locally buffered bodies whose 'sharedTxTable' entry is gone:
    -- another peer submitted the body (entry moved to 'sharedRetainedTxs',
    -- and possibly swept afterwards) or the sweep dropped the entry as an
    -- orphan after this peer's attempt was released.
    --
    -- The body can never be submitted now (no active entry to mark
    -- 'txInSubmission' on), so dropping it both frees the local buffer
    -- and unblocks 'txIdAckable', which otherwise refuses to ack while a
    -- body sits in 'peerDownloadedTxs'.
    --
    -- The same orphaned keys must also be removed from 'pifLeased' and
    -- 'pifAttempting': leaving them stale would block re-claim on
    -- re-advertisement (via 'txPeerHasAttempt') and cause the bracket
    -- finalizer to over-decrement 'txAttempt' on a freshly re-interned
    -- entry.  The other in-flight sets are unaffected: 'pifAdvertised'
    -- and 'pifAcksPending' are managed by the txid path and must stay
    -- in lockstep with the peer's queue; 'pifSubmitting' is only set
    -- via 'markSubmittingTxs', which cannot fire for an entry that's
    -- already been pruned.
    downloaded  = peerDownloadedTxs peerState
    downloaded' = IntMap.intersection downloaded (sharedTxTable sharedState)
    orphaned    = IntMap.keysSet downloaded
                  `IntSet.difference` IntMap.keysSet downloaded'

    peerState'
      | IntMap.null downloaded = peerState
      | otherwise              = peerState { peerDownloadedTxs = downloaded' }

    peerInFlight'
      | IntSet.null orphaned = peerInFlight
      | otherwise            = peerInFlight {
            pifLeased     = pifLeased     peerInFlight `IntSet.difference` orphaned,
            pifAttempting = pifAttempting peerInFlight `IntSet.difference` orphaned
          }

-- | Compute the next peer-local action.
nextPeerAction :: Ord peeraddr
               => Time
               -> TxDecisionPolicy
               -> peeraddr
               -> PeerTxLocalState tx
               -> PeerTxInFlight
               -> SharedTxState peeraddr txid
               -> (PeerAction, PeerTxLocalState tx, PeerTxInFlight, SharedTxState peeraddr txid)
nextPeerAction = nextPeerActionWithMode AllowAnyTxIdRequests
{-# INLINABLE nextPeerAction #-}

-- | Pipelined version of nextPeerAction
nextPeerActionPipelined :: Ord peeraddr
                        => Time
                        -> TxDecisionPolicy
                        -> peeraddr
                        -> PeerTxLocalState tx
                        -> PeerTxInFlight
                        -> SharedTxState peeraddr txid
                        -> (PeerAction, PeerTxLocalState tx, PeerTxInFlight, SharedTxState peeraddr txid)
nextPeerActionPipelined = nextPeerActionWithMode AllowPipelinedTxIdRequests
{-# INLINABLE nextPeerActionPipelined #-}

-- | V2 peer-thread scheduler
--
-- nextPeerActionWithMode handles body requests for txs this peer may currently
-- fetch, tx submission for bodies buffered locally by this peer, and txid ack/request
-- messages.  Before picking an action it runs 'bumpStuckEntries' over this
-- peer's buffered bodies to raise the inflight cap on entries whose
-- leaseholder has stalled, letting other peers claim them on their next pass.
-- Updates 'peerPhase' on the returned 'PeerTxLocalState' to reflect the
-- chosen action and threads the per-peer 'PeerTxInFlight' counters.
nextPeerActionWithMode :: Ord peeraddr
                       => TxIdRequestMode
                       -> Time
                       -> TxDecisionPolicy
                       -> peeraddr
                       -> PeerTxLocalState tx
                       -> PeerTxInFlight
                       -> SharedTxState peeraddr txid
                       -> (PeerAction, PeerTxLocalState tx, PeerTxInFlight, SharedTxState peeraddr txid)
nextPeerActionWithMode txIdRequestMode now policy peeraddr peerState peerInFlight sharedState =
    let (action, peerState', peerInFlight', sharedState'') =
          applyPeerActionChoice ctx (pickPeerActionChoice txIdRequestMode ctx)
        peerState'' = peerState' {
            peerPhase = phaseForAction txIdRequestMode (peerPhase peerState) action
          }
    in (action, peerState'', peerInFlight', sharedState'')
  where
    sharedState' = bumpStuckEntries now policy (peerDownloadedTxs peerState) sharedState
    ctx = mkPeerActionContext now policy peeraddr peerState peerInFlight sharedState'

-- | Compute the new 'PeerPhase' for the chosen 'PeerAction'.
--
-- In pipelined mode a 'PeerDoNothing' keeps the current phase (the peer
-- is still mid-pipeline, just waiting for replies). In non-pipelined mode
-- a 'PeerDoNothing' transitions to 'PeerIdle'.
phaseForAction :: TxIdRequestMode -> PeerPhase -> PeerAction -> PeerPhase
phaseForAction txIdRequestMode currentPhase action = case action of
    PeerDoNothing {}    -> case txIdRequestMode of
                             AllowPipelinedTxIdRequests -> currentPhase
                             AllowAnyTxIdRequests       -> PeerIdle
    PeerSubmitTxs {}    -> PeerSubmittingToMempool
    PeerRequestTxs {}   -> PeerWaitingTxs
    PeerRequestTxIds {} -> PeerWaitingTxIds

-- | Pick which action to perform next.
--
pickPeerActionChoice :: Ord peeraddr
                     => TxIdRequestMode
                     -> PeerActionContext peeraddr txid tx
                     -> PeerActionChoice peeraddr
pickPeerActionChoice txIdRequestMode ctx
  -- Pick TXs to submit to the mempool
  | Just txsToSubmit <- pickSubmitAction ctx =
      ChooseSubmit txsToSubmit
  -- Pick TXs to fetch
  | Just (txsToRequest, txsToRequestSize, txTable') <- pickRequestTxsAction ctx =
      ChooseRequestTxs txsToRequest txsToRequestSize txTable'
  -- Pick TXids to ack and/or request more TXids.
  | Just (acknowledgedTxIds, txIdsToAcknowledge, txIdsToRequest, unacknowledgedTxIds') <-
      pickRequestTxIdsAction txIdRequestMode ctx =
      ChooseRequestTxIds acknowledgedTxIds txIdsToAcknowledge txIdsToRequest unacknowledgedTxIds'
  -- Do nothing
  | otherwise =
      ChooseDoNothing (sharedGeneration (pacSharedState ctx)) (nextWakeDelay ctx)

-- | Execute a chosen peer action and compute resulting state updates
applyPeerActionChoice :: PeerActionContext peeraddr txid tx
                      -> PeerActionChoice peeraddr
                      -> (PeerAction, PeerTxLocalState tx, PeerTxInFlight, SharedTxState peeraddr txid)
applyPeerActionChoice ctx choice =
  case choice of
       ChooseSubmit txsToSubmit ->
         applySubmitChoice ctx txsToSubmit
       ChooseRequestTxs txsToRequest txsToRequestSize txTable' ->
         applyRequestTxsChoice ctx txsToRequest txsToRequestSize txTable'
       ChooseRequestTxIds acknowledgedTxIds txIdsToAcknowledge txIdsToRequest
                          unacknowledgedTxIds' ->
         applyRequestTxIdsChoice ctx acknowledgedTxIds txIdsToAcknowledge txIdsToRequest
                                 unacknowledgedTxIds'
       ChooseDoNothing generation wakeDelay ->
         applyDoNothingChoice ctx generation wakeDelay

-- | Construct a 'PeerSubmitTxs' action for buffered transactions.
--
-- Marks the selected txs as in-submission on this peer.  Other peers
-- skip them via 'txSubmittingByOther'.  STM serialisation guarantees
-- only one peer can win the @ChooseSubmit@ race for a given key.
applySubmitChoice :: PeerActionContext peeraddr txid tx
                  -> [TxKey]
                  -> (PeerAction, PeerTxLocalState tx, PeerTxInFlight, SharedTxState peeraddr txid)
applySubmitChoice ctx txsToSubmit =
  let keys = IntSet.fromList (unTxKey <$> txsToSubmit)
      pif  = pacPeerInFlight ctx
      pif' = pif {
          pifAttempting = pifAttempting pif `IntSet.difference` keys,
          pifSubmitting = pifSubmitting pif `IntSet.union`      keys
        }
  in ( PeerSubmitTxs txsToSubmit
     , pacPeerState ctx
     , pif'
     , markSubmittingTxs txsToSubmit (pacSharedState ctx)
     )

-- | Construct a 'PeerRequestTxs' action and update local and shared tx state.
applyRequestTxsChoice :: PeerActionContext peeraddr txid tx
                      -> [TxKey]
                      -> SizeInBytes
                      -> IntMap.IntMap (TxEntry peeraddr)
                      -> (PeerAction, PeerTxLocalState tx, PeerTxInFlight, SharedTxState peeraddr txid)
applyRequestTxsChoice ctx txsToRequest txsToRequestSize txTable =
  ( PeerRequestTxs txsToRequest
  , peerState'
  , peerInFlight'
  , sharedState'
  )
  where
    requestedKeys = IntSet.fromList (unTxKey <$> txsToRequest)
    peerState' =
      (pacPeerState ctx) {
        peerRequestedTxs =
          peerRequestedTxs (pacPeerState ctx) `IntSet.union` requestedKeys,
        peerRequestedTxBatches =
          peerRequestedTxBatches (pacPeerState ctx) StrictSeq.|> RequestedTxBatch {
            requestedTxBatchSet = requestedKeys,
            requestedTxBatchSize = txsToRequestSize
          },
        peerRequestedTxsSize = peerRequestedTxsSize (pacPeerState ctx) + txsToRequestSize
      }
    pif = pacPeerInFlight ctx
    peerInFlight' = pif {
        pifLeased     = pifLeased     pif `IntSet.union` requestedKeys,
        pifAttempting = pifAttempting pif `IntSet.union` requestedKeys
      }
    -- Bump only 'sharedRevision' (the structural dirty bit), not
    -- 'sharedGeneration' (the wake counter).  Claiming a lease grants no
    -- new option to other advertisers: they couldn't claim before this
    -- commit, and they still can't.  Waking them here would cost a full
    -- 'nextPeerAction' pass per peer with nothing to show for it; the
    -- wake they actually care about is on submit / lease release.
    sharedState' =
      (pacSharedState ctx) {
        sharedTxTable = txTable,
        sharedRevision = sharedRevision (pacSharedState ctx) + 1
      }

-- | Construct a 'PeerRequestTxIds' action and update local and shared txid state.
applyRequestTxIdsChoice
  :: PeerActionContext peeraddr txid tx
  -> [TxKey]
  -> NumTxIdsToAck
  -> NumTxIdsToReq
  -> StrictSeq.StrictSeq TxKey
  -> (PeerAction, PeerTxLocalState tx, PeerTxInFlight, SharedTxState peeraddr txid)
applyRequestTxIdsChoice ctx acknowledgedTxIds txIdsToAcknowledge txIdsToRequest unacknowledgedTxIds' =
  ( PeerRequestTxIds txIdsToAcknowledge txIdsToRequest
  , peerState''
  , peerInFlight''
  , pacSharedState ctx
  )
  where
    peerState0 = pacPeerState ctx
    acknowledgedKeys = IntSet.fromList (unTxKey <$> acknowledgedTxIds)
    peerState'' =
      peerState0 {
        peerAvailableTxIds =
          IntMap.withoutKeys (peerAvailableTxIds peerState0) acknowledgedKeys,
        peerUnacknowledgedTxIds = unacknowledgedTxIds',
        peerRequestedTxIds = peerRequestedTxIds peerState0 + txIdsToRequest
      }
    pif = pacPeerInFlight ctx
    peerInFlight'' = pif {
        pifAdvertised  = pifAdvertised  pif `IntSet.difference` acknowledgedKeys,
        pifAcksPending = pifAcksPending pif `IntSet.difference` acknowledgedKeys
      }

-- | Construct a 'PeerDoNothing' action.
applyDoNothingChoice
  :: PeerActionContext peeraddr txid tx
  -> Word64
  -> Maybe DiffTime
  -> (PeerAction, PeerTxLocalState tx, PeerTxInFlight, SharedTxState peeraddr txid)
applyDoNothingChoice ctx generation wakeDelay =
  ( PeerDoNothing generation wakeDelay
  , pacPeerState ctx
  , pacPeerInFlight ctx
  , pacSharedState ctx
  )

-- | Select downloaded transactions that this peer may submit to the mempool.
pickSubmitAction
  :: PeerActionContext peeraddr txid tx
  -> Maybe [TxKey]
pickSubmitAction PeerActionContext { pacPeerState, pacPeerInFlight, pacSharedState } =
  let txsToSubmit = pickBufferedTxsToSubmit in
  if null txsToSubmit
     then Nothing
     else Just txsToSubmit
  where

    -- Walk the unacknowledged txid queue in peer advertisement order,
    -- picking bodies buffered by this peer for immediate submission.
    -- Classification of each entry:
    --
    -- * 'sharedTxTable' has 'Nothing' for the key: the tx was resolved at
    --   some point (a peer submitted it to the mempool) and the entry was
    --   deleted.  Orphan sweep cannot drop an entry while any peer still
    --   tracks the key, so 'Nothing' here implies resolution; safe to skip
    --   and continue past, regardless of whether the retained marker has
    --   since expired or the mempool has since evicted.
    -- * 'sharedTxTable' has 'Just' but we don't have the body buffered, or
    --   another peer is already submitting: the tx is in flight elsewhere.
    --   Stop, otherwise later txs in our stream might run ahead of an
    --   unresolved earlier tx they depend on.
    -- * 'sharedTxTable' has 'Just' and we have the body buffered: submit.
    --
    -- The same 'TxKey' may legitimately appear more than once in
    -- 'peerUnacknowledgedTxIds' (e.g. a fork switch on the sender re-adds
    -- a tx to the mempool under a new idx, see 'txSubmissionOutbound').
    -- Track already-picked keys in 'seen' so each key is submitted at
    -- most once per scheduler invocation; the duplicates remain in the
    -- queue so the ack-by-count handshake stays in sync with the sender.
    pickBufferedTxsToSubmit =
        go IntSet.empty [] (toList (peerUnacknowledgedTxIds pacPeerState))
      where
        go _    acc [] = reverse acc
        go seen acc (txKey@(TxKey k) : rest)
          | IntSet.member k seen = go seen acc rest
          | otherwise =
              case IntMap.lookup k (sharedTxTable pacSharedState) of
                Just txEntry
                  | txBufferedByPeer pacPeerState k
                  , not (txSubmittingByOther pacPeerInFlight k txEntry) ->
                      go (IntSet.insert k seen) (txKey : acc) rest
                Just _  -> reverse acc
                Nothing -> go seen acc rest

-- | Select transactions to request from the peer, if within policy limits.
--
-- Returns a triple of:
-- Tx keys to request (in the peer's advertisement order)
-- Total serialized size of the requested txs
-- Updated shared state with new lease ownership for selected txs
pickRequestTxsAction :: Eq peeraddr
                     => PeerActionContext peeraddr txid tx
                     -> Maybe ([TxKey], SizeInBytes, IntMap.IntMap (TxEntry peeraddr))
pickRequestTxsAction ctx@PeerActionContext { pacNow, pacPolicy, pacPeerState, pacSharedState } =
  let (txsToRequest, txsToRequestSize, sharedState') = pickTxsToRequest in
  if null txsToRequest
     then Nothing
     else Just (txsToRequest, txsToRequestSize, sharedState')
  where

    -- Picks txs from the peer's available set that are not yet requested or
    -- downloaded, assigning leases with expiry timestamps.
    -- Respects 'maxOutstandingTxBatchesPerPeer' and 'txsSizeInflightPerPeer' policy
    -- constraints.
    pickTxsToRequest =
      if StrictSeq.length (peerRequestedTxBatches pacPeerState) >=
           maxOutstandingTxBatchesPerPeer pacPolicy
         then ([], 0, sharedTxTable pacSharedState)
         else go IntSet.empty [] 0 (sharedTxTable pacSharedState) candidates
      where
        -- Remaining bytes available for requesting new tx, based on the
        -- per-peer inflight size limit.
        sizeBudget =
          if peerRequestedTxsSize pacPeerState >= txsSizeInflightPerPeer pacPolicy
             then 0
             else txsSizeInflightPerPeer pacPolicy - peerRequestedTxsSize pacPeerState

        leaseUntil = addTime (interTxSpace pacPolicy) pacNow

        -- Iterate the peer's unacknowledged queue, which preserves the order
        -- in which the peer sent us the txids.  Walking the queue in that
        -- order aligns our fetch order with the peer's advertisement order,
        -- which preserves whatever submission-validity ordering the peer
        -- intended within the batch.
        candidates =
          [ (k, txSize)
          | TxKey k <- toList (peerUnacknowledgedTxIds pacPeerState)
          , IntSet.notMember k (peerRequestedTxs pacPeerState)
          , IntMap.notMember k (peerDownloadedTxs pacPeerState)
          , Just txSize <- [IntMap.lookup k (peerAvailableTxIds pacPeerState)]
          ]

        -- Select transactions to request by iterating through candidates in
        -- peer advertisement order until the size budget is consumed.
        --
        -- The same key may legitimately appear more than once in the
        -- queue (see 'pickBufferedTxsToSubmit' for the fork-switch
        -- scenario).  Track keys already picked this round in 'seen' so
        -- 'claimTx' fires at most once per key; the wire-level request
        -- 'Map txid SizeInBytes' already deduplicates, and we must not
        -- inflate 'txAttempt' or 'peerRequestedTxsSize' with phantom
        -- second claims.
        go _    selectedRev selectedSize txTable [] = (reverse selectedRev, selectedSize, txTable)
        go seen selectedRev selectedSize txTable ((k, txSize) : rest)
          | IntSet.member k seen = go seen selectedRev selectedSize txTable rest
          | exceedsBudget selectedSize txSize =
              (reverse selectedRev, selectedSize, txTable)
          | otherwise =
              case IntMap.lookup k txTable of
                  Just txEntry ->
                    if txSelectable ctx (TxKey k) txEntry
                       then
                         go (IntSet.insert k seen)
                            (TxKey k : selectedRev)
                            (selectedSize + txSize)
                            (IntMap.insert k (claimTx (pacPeerAddr ctx) leaseUntil txEntry)
                                           txTable)
                            rest
                       else go seen selectedRev selectedSize txTable rest
                  -- Entry resolved or swept since this peer's local state last synced; skip.
                  Nothing -> go seen selectedRev selectedSize txTable rest

        exceedsBudget selectedSize txSize
          | selectedSize + txSize <= sizeBudget = False
          | selectedSize /= 0 = True
          -- The inflight size limit is soft by up to one tx size, so a peer with
          -- spare capacity may still request its first tx in a batch even when
          -- that single tx exceeds the remaining byte budget.
          | otherwise = peerRequestedTxsSize pacPeerState >= txsSizeInflightPerPeer pacPolicy

-- | Determine txid acknowledgment and request counts, if any work is available.
pickRequestTxIdsAction :: TxIdRequestMode
                       -> PeerActionContext peeraddr txid tx
                       -> Maybe ([TxKey], NumTxIdsToAck, NumTxIdsToReq, StrictSeq.StrictSeq TxKey)
pickRequestTxIdsAction txIdRequestMode ctx@PeerActionContext { pacPolicy, pacPeerState }
  | txIdsToAcknowledge <= 0 && txIdsToRequest <= 0 = Nothing

  -- Benchmark hook: when 'disablePipelinedTxIdRequests' is set, never
  -- fire a request that would be sent as pipelined.  Pipelined fires
  -- whenever the post-ack queue is non-empty (or we're already in
  -- pipelined mode), so this guard suppresses both.  The peer then
  -- parks until the queue can drain via acks, yielding pure blocking
  -- request behaviour at the wire.
  | disablePipelinedTxIdRequests pacPolicy
  , txIdRequestMode == AllowPipelinedTxIdRequests
    || not (StrictSeq.null unacknowledgedTxIds') = Nothing

  -- A pure-ack pipelined message would burn a pipeline slot for an
  -- empty reply ('req=0' forces the response empty by construction) and
  -- shrink our window without growing it.  Defer the ack until we can
  -- also request more txids.
  | txIdRequestMode == AllowPipelinedTxIdRequests
  , txIdsToRequest <= 0 = Nothing

  -- Spec: a pipelined (non-blocking) request requires the post-ack
  -- queue to be non-empty.  When the peer's unacked queue is empty to
  -- begin with we can't satisfy that, and there's no blocking option
  -- in pipelined mode, so wait for a later wake.
  | txIdRequestMode == AllowPipelinedTxIdRequests
  , StrictSeq.null (peerUnacknowledgedTxIds pacPeerState) = Nothing

  -- Backoff after an empty txid reply: if our last request came back
  -- with zero txids and we still have nothing to ack, suppress the
  -- next 'ack=0, req=N' pipelined wire message (V2.hs sends pipelined
  -- whenever the unacked queue is non-empty).  The flag is cleared by
  -- a non-empty txid reply or by the mempool accepting one of this
  -- peer's txs - both signs the peer may have new work for us.
  | peerLastTxIdReplyWasEmpty pacPeerState
  , txIdsToAcknowledge <= 0
  , not (StrictSeq.null (peerUnacknowledgedTxIds pacPeerState)) = Nothing

  | otherwise = Just (acknowledgedTxIds, txIdsToAcknowledge, txIdsToRequest, unacknowledgedTxIds')
  where

    -- Split the unacknowledged txid queue into acknowledged and remaining portions.
    --
    -- acknowledgedTxIds is the longest prefix of "ackable" txids.
    -- unacknowledgedTxIds is the remaining txids
    (acknowledgedTxIds, txIdsToAcknowledge, txIdsToRequest, unacknowledgedTxIds') =
      ( toList acknowledgedTxIdsSeq
      , fromIntegral numOfAcked
      , txIdsToRequest'
      , unacknowledgedTxIds
      )
      where
        ackablePrefix = StrictSeq.takeWhileL (txIdAckable ctx) (peerUnacknowledgedTxIds
                                             pacPeerState)

        numOfUnacked = StrictSeq.length (peerUnacknowledgedTxIds pacPeerState)
        numOfRequested = fromIntegral (peerRequestedTxIds pacPeerState) :: Int
        hasOutstandingBodyReplies =
          not (StrictSeq.null (peerRequestedTxBatches pacPeerState))
        keepOneUnackedForPipelinedRequest =
          txIdRequestMode == AllowPipelinedTxIdRequests
            && (numOfRequested > 0 || hasOutstandingBodyReplies)
        numOfAcked0 = StrictSeq.length ackablePrefix
        numOfAcked
          -- A pipelined txid request becomes a non-blocking protocol message
          -- while any txid or body reply is still in flight. The outbound side
          -- requires at least one txid to remain unacknowledged in that case.
          | keepOneUnackedForPipelinedRequest =
              min numOfAcked0 (max 0 (numOfUnacked - 1))
          | otherwise = numOfAcked0

        acknowledgedTxIdsSeq = StrictSeq.take numOfAcked ackablePrefix
        unacknowledgedTxIds = StrictSeq.drop numOfAcked (peerUnacknowledgedTxIds pacPeerState)
        unackedAndRequested = numOfUnacked + numOfRequested

        -- How many new txids we can request: capped by the unack-window
        -- room left after this round's ack ('maxUnacknowledgedTxIds -
        -- unackedAndRequested + numOfAcked') and by the per-message
        -- request limit ('maxNumTxIdsToRequest - numOfRequested').
        txIdsToRequest' =
          fromIntegral $ max 0 $ min
            (fromIntegral (maxUnacknowledgedTxIds pacPolicy) - unackedAndRequested + numOfAcked)
            (fromIntegral (maxNumTxIdsToRequest pacPolicy) - numOfRequested)

-- | Compute the time delay until the peer should next wake to check for work.
nextWakeDelay :: PeerActionContext peeraddr txid tx
              -> Maybe DiffTime
nextWakeDelay PeerActionContext { pacNow, pacPolicy, pacClaimDelay
                                , pacPeerState, pacPeerInFlight, pacSharedState } =
    (`diffTime` pacNow) <$> minMaybe nextClaimWake nextBumpWake
  where
    -- Wake at the earliest claim-ready time among txs this peer advertises.
    nextClaimWake =
      IntSet.foldl' stepClaim Nothing (pifAdvertised pacPeerInFlight)

    -- Wake at the earliest bump-ready time among txs this peer holds buffered.
    -- Scoped to 'peerDownloadedTxs' to match 'bumpStuckEntries': only peers
    -- with the body buffered can issue a bump, so non-buffering peers don't
    -- schedule wakes for entries they can't touch.
    nextBumpWake =
      IntMap.foldlWithKey' stepBump Nothing (peerDownloadedTxs pacPeerState)

    stepClaim acc k =
      case IntMap.lookup k (sharedTxTable pacSharedState) of
        Just txEntry -> minMaybe acc (futureClaimWake txEntry)
        Nothing      -> acc

    stepBump acc k _tx =
      case IntMap.lookup k (sharedTxTable pacSharedState) of
        Just txEntry -> minMaybe acc (nextStuckBumpWake pacNow pacPolicy txEntry)
        Nothing      -> acc

    futureClaimWake txEntry =
      let claimWake = txClaimReadyAt pacClaimDelay txEntry in
      if claimWake > pacNow then Just claimWake
                            else Nothing

    minMaybe :: Ord a => Maybe a -> Maybe a -> Maybe a
    minMaybe Nothing y         = y
    minMaybe x Nothing         = x
    minMaybe (Just x) (Just y) = Just (min x y)

-- | Assign a tx lease to a peer and increment the attempt count.
claimTx :: peeraddr
        -> Time
        -> TxEntry peeraddr
        -> TxEntry peeraddr
claimTx peeraddr leaseUntil txEntry@TxEntry { txAttempt } =
  txEntry {
    txLease   = TxLeased peeraddr leaseUntil,
    txAttempt = txAttempt + 1
  }

-- | Time at which a leased entry becomes eligible for an inflight cap bump.
--
-- The leaseholder's claim time is recovered from the lease deadline. If the
-- entry has been re-claimed since the original peer's attempt, this reflects
-- the latest claim instead. That is intentional: each successful claim earns
-- its own 'inflightTimeout' grace period before the next bump, so cap growth
-- is rate-limited at one bump per 'inflightTimeout' per claim.
--
-- The offset cancels the 'interTxSpace' baked into 'leaseUntil', yielding
-- 'claimTime + inflightTimeout'.
stuckBumpReadyAt :: TxDecisionPolicy -> Time -> Time
stuckBumpReadyAt policy =
    addTime (inflightTimeout policy - interTxSpace policy)

-- | The time at which an entry would become eligible for an inflight cap
-- bump, or 'Nothing' if the entry is not bump-eligible at all (e.g. the
-- entry is unleased, the cap is not the bottleneck, or submission is
-- already underway).
stuckBumpEligibleAt :: TxDecisionPolicy -> TxEntry peeraddr -> Maybe Time
stuckBumpEligibleAt policy
                    entry@TxEntry { txLease = TxLeased _ leaseUntil
                                  , currentMaxInflightMultiplicity = cap }
  | txAttempt entry >= cap
  , not (txInSubmission entry)
  = Just (stuckBumpReadyAt policy leaseUntil)
stuckBumpEligibleAt _ _ = Nothing

-- | Future wake time at which an entry would become eligible for a cap bump.
--
-- Returns 'Just' for any bump-eligible entry whose 'bumpAt' is at or after
-- 'now'. Using '>=' (rather than '>') means a peer that runs 'nextWakeDelay'
-- without 'bumpStuckEntries' having already fired still schedules a wake.
nextStuckBumpWake :: Time -> TxDecisionPolicy -> TxEntry peeraddr -> Maybe Time
nextStuckBumpWake now policy entry
  | Just bumpAt <- stuckBumpEligibleAt policy entry
  , bumpAt >= now
  = Just bumpAt
nextStuckBumpWake _ _ _ = Nothing

-- | Sweep the txs this peer has buffered locally and bump any whose lease
-- has been held past 'inflightTimeout'. The leaseholder is in the best
-- position to detect that it is holding others up: its 'peerDownloadedTxs'
-- is small (usually empty) so the sweep is cheap, and any tx it has buffered
-- is one it has at some point claimed itself.  Note that bumps will only
-- happen for downloaded TXs.
--
-- 'sharedGeneration' is bumped when entries change so other peers wake out
-- of 'awaitSharedChange' and re-evaluate eligibility under the new cap.
bumpStuckEntries :: forall peeraddr txid tx.
                    Time
                 -> TxDecisionPolicy
                 -> IntMap.IntMap tx
                 -> SharedTxState peeraddr txid
                 -> SharedTxState peeraddr txid
bumpStuckEntries now policy downloadedTxs st =
    if anyBumped
       then st { sharedTxTable    = txTable',
                 sharedGeneration = sharedGeneration st + 1 }
       else st
  where
    (anyBumped, txTable') =
      IntMap.foldlWithKey' bumpOne (False, sharedTxTable st) downloadedTxs
    bumpOne (!changed, !tbl) k _tx
      | Just entry  <- IntMap.lookup k tbl
      , Just entry' <- bumpCurrentMaxIfStuck entry
      = (True, IntMap.insert k entry' tbl)
      | otherwise = (changed, tbl)

    -- Bump 'currentMaxInflightMultiplicity' by one when the leaseholder
    -- has held the lease past 'inflightTimeout' without anyone reaching
    -- submission, and the cap is the bottleneck preventing another peer
    -- from joining.  Returns 'Nothing' when no bump was warranted.
    bumpCurrentMaxIfStuck :: TxEntry peeraddr -> Maybe (TxEntry peeraddr)
    bumpCurrentMaxIfStuck entry@TxEntry { currentMaxInflightMultiplicity = cap }
      | Just bumpAt <- stuckBumpEligibleAt policy entry
      , now >= bumpAt
      = Just entry { currentMaxInflightMultiplicity = cap + 1 }
    bumpCurrentMaxIfStuck _ = Nothing

-- | Determine if a tx is eligible for this peer to request.
--
-- A tx is selectable if it can be claimed or is already owned by this peer
-- and this peer's score-derived claim delay has elapsed.
--
-- Callers iterate candidates from the peer's own 'peerAvailableTxIds', so
-- the "this peer advertises @txKey@" precondition is established by the
-- caller.
txSelectable :: forall peeraddr txid tx.
                Eq peeraddr
             => PeerActionContext peeraddr txid tx
             -> TxKey
             -> TxEntry peeraddr
             -> Bool
txSelectable PeerActionContext { pacNow, pacPeerAddr, pacClaimDelay
                               , pacPeerInFlight }
             (TxKey k)
             txEntry
  | txInSubmission txEntry = False
  | txPeerHasAttempt = False
  | txAttempt txEntry >= currentMaxInflightMultiplicity txEntry = False
  | txOwnedByPeer txEntry = True
  | otherwise = txClaimReadyAt pacClaimDelay txEntry <= pacNow
  where
    txOwnedByPeer :: TxEntry peeraddr -> Bool
    txOwnedByPeer TxEntry { txLease = TxLeased owner _ } = owner == pacPeerAddr
    txOwnedByPeer TxEntry { txLease = TxClaimable _ }    = False

    txPeerHasAttempt :: Bool
    txPeerHasAttempt =
         IntSet.member k (pifAttempting pacPeerInFlight)
      || IntSet.member k (pifSubmitting pacPeerInFlight)

-- | Does the peer have the TX entry buffered locally?
--
-- The peer's own 'peerDownloadedTxs' is the source of truth for "buffered
-- body present", so this is a peer-local lookup.
txBufferedByPeer :: PeerTxLocalState tx -> Int -> Bool
txBufferedByPeer peerState k = IntMap.member k (peerDownloadedTxs peerState)

-- | Check whether some other peer is already submitting this tx.
--
-- True iff the entry's @txInSubmission@ flag is set and this peer is not
-- the submitter.
txSubmittingByOther :: PeerTxInFlight -> Int -> TxEntry peeraddr -> Bool
txSubmittingByOther pif k txEntry =
     txInSubmission txEntry
  && IntSet.notMember k (pifSubmitting pif)

-- | Compute the current usefulness score for a peer after time-based decay.
--
-- Scores drain at 'scoreRate' (txs/second) from the last update timestamp.
-- Returns zero for peers whose accumulated rejections have fully decayed.
currentPeerScore :: TxDecisionPolicy
                 -> Time
                 -> PeerScore
                 -> Double
currentPeerScore TxDecisionPolicy { scoreRate } currentTime
                 PeerScore { peerScoreValue, peerScoreTs }
    | peerScoreValue == 0 = 0
    | currentTime <= peerScoreTs = peerScoreValue
    | otherwise = max 0 $ peerScoreValue - realToFrac (diffTime currentTime peerScoreTs) * scoreRate

-- | Compute the peer's score-derived claim delay.
--
-- A peer with a fully drained score has no delay.  Any non-zero score
-- maps to a delay in @[10 ms, maxPeerClaimDelay]@: the linear
-- interpolation saturates when the score reaches 'scoreMax', and the
-- 10 ms floor ensures even a small accumulated score meaningfully
-- nudges race outcomes against the peer rather than being swallowed
-- by jitter.
peerClaimDelay :: TxDecisionPolicy
               -> Time
               -> PeerScore
               -> DiffTime
peerClaimDelay policy@TxDecisionPolicy { maxPeerClaimDelay, scoreMax } currentTime peerScore
    | s == 0   = 0
    | otherwise = max 0.010 . realToFrac $ s / scoreMax * realToFrac maxPeerClaimDelay
  where
    s = currentPeerScore policy currentTime peerScore

-- | Decay the peer's score to @now@, updating the timestamp.
--
-- Fast path: a score that is already 0 stays 0, and the stale
-- 'peerScoreTs' is harmless because 'currentPeerScore' short-circuits
-- on @peerScoreValue == 0@ without reading the timestamp, while any
-- later 'applyPeerEvents' transition to a positive score overwrites
-- 'peerScoreTs' with the current 'now'.  Return the state unchanged.
drainPeerScore :: TxDecisionPolicy
               -> Time
               -> PeerTxLocalState tx
               -> PeerTxLocalState tx
drainPeerScore policy now peerState@PeerTxLocalState { peerScore }
  | peerScoreValue peerScore == 0 = peerState
  | otherwise =
      let drained = currentPeerScore policy now peerScore in
      peerState { peerScore = PeerScore { peerScoreValue = drained, peerScoreTs = now } }
{-# INLINE drainPeerScore #-}

-- | Apply a batch of accept/reject events to the peer's local score.
-- Drains the score by elapsed time at 'scoreRate', then adds the
-- rejection count and subtracts 'scoreAcceptDecrement' per accept;
-- clamps to @[0, scoreMax]@. Returns the new score value (for
-- tracing) and the updated local state.
applyPeerEvents :: TxDecisionPolicy
                -> Time
                -> Int  -- ^ accepted count (decrements score)
                -> Int  -- ^ rejected/penalty count (increments score)
                -> PeerTxLocalState tx
                -> (Double, PeerTxLocalState tx)
applyPeerEvents policy@TxDecisionPolicy { scoreMax, scoreAcceptDecrement }
                now acceptedCount rejectedCount
                peerState@PeerTxLocalState { peerScore }
  -- Fast path: score is already 0 and there's no penalty to add.
  -- Accepts can only decrement (clamped at 0), so they can't move
  -- the score. Return the state unchanged.
  | rejectedCount == 0
  , peerScoreValue peerScore == 0
  = (0, peerState)

  | otherwise
  = (peerScoreValue peerScore', peerState { peerScore = peerScore' })
  where
    rejGain = fromIntegral rejectedCount :: Double
    accDec  = fromIntegral acceptedCount * scoreAcceptDecrement
    peerScore' =
      let !drained = currentPeerScore policy now peerScore
          !final   = min scoreMax (max 0 (drained + rejGain - accDec)) in
      peerScore { peerScoreValue = final, peerScoreTs = now }
{-# INLINE applyPeerEvents #-}

txClaimReadyAt :: DiffTime -> TxEntry peeraddr -> Time
txClaimReadyAt claimDelay TxEntry { txLease } =
    addTime claimDelay claimableAt
  where
    claimableAt =
      case txLease of
        TxLeased _ leaseUntil -> leaseUntil
        TxClaimable readyAt   -> readyAt

-- | Determine if an unacknowledged txid is ready to be acknowledged.
--
-- A txid remains ackable after it has been resolved and removed from shared
-- state. The wire protocol only needs an ack count for the peer-local
-- unacknowledged prefix, so a late ack is still safe even after the active
-- entry and retained marker have been pruned.
txIdAckable :: PeerActionContext peeraddr txid tx
            -> TxKey
            -> Bool
txIdAckable PeerActionContext { pacPeerState, pacPeerInFlight, pacSharedState }
            (TxKey k)
  | retainedMember k (sharedRetainedTxs pacSharedState) = True
    -- Resolved and still within the retention window; ack is safe.
  | IntMap.member k (peerDownloadedTxs pacPeerState) = False
    -- We hold the body in our local buffer; we must submit it before
    -- acking the txid, otherwise the body would orphan in
    -- 'peerDownloadedTxs' if 'pickSubmitAction' is blocked from
    -- reaching this entry by an earlier in-flight tx.
  | not (IntSet.member k (pifAdvertised pacPeerInFlight)) = True
    -- The peer no longer tracks the txid as advertised. This covers
    -- mempool/retained txids that 'handleReceivedTxIds' kept out of the
    -- advertised set, as well as txids the peer has already attempted
    -- and submitted (or that another peer resolved).
  | otherwise =
    -- Peer still advertises the key and doesn't have the body.  Ack iff
    -- the active entry has been pruned from 'sharedTxTable' (safe late
    -- ack); otherwise the txid is still in flight and must stay
    -- unacknowledged.
    IntMap.notMember k (sharedTxTable pacSharedState)

-- | Shared-state cleanup
--
-- Drops three kinds of dead entries in one pass:
--
-- * Retained entries whose retention deadline has passed.  Only the
--   'sharedRetainedTxs' membership is removed; the txid lookup tables
--   ('sharedTxIdToKey', 'sharedKeyToTxId') are preserved here because
--   peers may still hold the key in 'peerUnacknowledgedTxIds' until
--   they ack it.
-- * Orphaned 'sharedTxTable' entries: entries with a released lease,
--   no in-flight attempt, and no live peer still tracking the key.
--   These are safe to fully tear down (lookup tables included): by
--   definition no peer references them.
-- * Stale lookup-table entries: keys present only in 'sharedTxIdToKey'
--   / 'sharedKeyToTxId' with no peer still referencing them.  Bounds
--   the lookup tables so they don't grow unboundedly.
--
-- The @liveReferences@ set is the union of every active peer's
-- 'pifAdvertised' and 'pifAcksPending', snapshotted by the caller in
-- the same STM transaction as this function so it is coherent with the
-- 'sharedTxTable' read.
--
-- Bumps only 'sharedRevision' (the structural dirty bit), not
-- 'sharedGeneration' (the wake counter).  None of the three cleanup
-- categories grant other peers new options: expired-retained drops
-- and orphan removals never affect a referenced key, and stale-lookup
-- removal doesn't touch any peer-visible state.  Waking parked peers
-- here would cost a full 'nextPeerAction' pass per peer for nothing.
sweepSharedState :: HasRawTxId txid
                 => Time
                 -> IntSet
                 -> SharedTxState peeraddr txid
                 -> SharedTxState peeraddr txid
sweepSharedState now liveReferences st
    | IntSet.null orphans
   && IntSet.null expiredRetained
   && IntSet.null staleLookups = st
    | otherwise =
        ( dropLookupOnly staleLookups
        . dropTxKeys orphans
        $ st { sharedRetainedTxs =
                 retainedDeleteKeys expiredRetained (sharedRetainedTxs st) }
        ) { sharedRevision = sharedRevision st + 1 }
  where
    expiredRetained = retainedExpiredKeys now (sharedRetainedTxs st)
    orphans =
      IntMap.keysSet
        (IntMap.filterWithKey isOrphan (sharedTxTable st))

    retainedAfter  = retainedKeysSet (sharedRetainedTxs st)
                      `IntSet.difference` expiredRetained
    referencedKeys = IntMap.keysSet (sharedTxTable st)
                      `IntSet.union` retainedAfter
                      `IntSet.union` liveReferences
    staleLookups   = IntMap.keysSet (sharedKeyToTxId st)
                      `IntSet.difference` referencedKeys

    isOrphan _ TxEntry { txLease = TxLeased {} } = False
    isOrphan k TxEntry { txAttempt, txInSubmission }
      | txAttempt > 0                     = False
      | txInSubmission                    = False
      | IntSet.member k liveReferences    = False
      | otherwise                         = True

    -- Remove transaction entries from all shared state maps by key.
    dropTxKeys keys s
      | IntSet.null keys = s
      | otherwise =
          s {
            sharedTxTable     = IntMap.withoutKeys (sharedTxTable s) keys,
            sharedRetainedTxs = retainedDeleteKeys keys (sharedRetainedTxs s),
            sharedTxIdToKey   = IntSet.foldl' (deleteTxId (sharedKeyToTxId s))
                                              (sharedTxIdToKey s) keys,
            sharedKeyToTxId   = IntMap.withoutKeys (sharedKeyToTxId s) keys
          }

    -- Remove only the txid <-> key lookup entries.  Used for keys that
    -- have outlived their 'sharedTxTable' and retained residence but had
    -- their lookup entries kept while peers still carried the txid in
    -- 'peerUnacknowledgedTxIds'.
    dropLookupOnly keys s
      | IntSet.null keys = s
      | otherwise =
          s {
            sharedTxIdToKey = IntSet.foldl' (deleteTxId (sharedKeyToTxId s))
                                            (sharedTxIdToKey s) keys,
            sharedKeyToTxId = IntMap.withoutKeys (sharedKeyToTxId s) keys
          }

    deleteTxId keyToTxId txIdToKey k =
      case IntMap.lookup k keyToTxId of
        Just txid -> Map.delete (getRawTxId txid) txIdToKey
        Nothing   -> txIdToKey
{-# INLINABLE sweepSharedState #-}

-- | Handle a batch of tx bodies received from one peer.
--
-- Received bodies are buffered locally in the peer state. Bodies that are
-- already retained or already in the mempool are counted as late and dropped.
-- Any requested tx omitted from the reply releases this peer's ownership.
-- Late TXs contributes to the returned penalty count.
handleReceivedTxs :: HasCallStack
                  => (Eq peeraddr, Show peeraddr, HasRawTxId txid)
                  => (txid -> Bool)
                  -> Time
                  -> TxDecisionPolicy
                  -> peeraddr
                  -> [(txid, tx)]
                  -> PeerTxLocalState tx
                  -> PeerTxInFlight
                  -> SharedTxState peeraddr txid
                  -> (Int, Int, PeerTxLocalState tx, PeerTxInFlight, SharedTxState peeraddr txid)
handleReceivedTxs mempoolHasTx now policy peeraddr txs peerState peerInFlight sharedState =
    (omittedCount, lateCount, peerState', peerInFlight', sharedState')
  where
    txidToKey = sharedTxIdToKey sharedState

    requestedKeys = requestedTxBatchSet requestedBatch
    retainUntil   = addTime (bufferedTxsMinLifetime policy) now

    -- Dequeue the next requested tx batch to process.
    (requestedBatch, remainingRequestedBatches) =
      case peerRequestedTxBatches peerState of
           StrictSeq.Empty ->
             error $ "TxSubmission.Inbound.V2.handleReceivedTxs: "
                  ++ "body reply received with no outstanding tx-body "
                  ++ "request from peer " ++ show peeraddr
                  ++ " (received " ++ show (length txs) ++ " txs)"
           batch StrictSeq.:<| batches ->
             (batch, batches)

    -- Process each received tx, collecting late counts, pending
    -- requests, the keys still buffered by this peer, and updated
    -- shared state.  @bufferedKeys@ is the subset of @requestedKeys@
    -- that ended up buffered locally; the rest are released below.
    --
    -- @wakeChangeFromHandle@ is set only when the mempool branch fires,
    -- since its retained-insert can enable a late ack on another peer
    -- that already advertised this txid.  All other branches either
    -- leave the shared state untouched or perform a structural no-op
    -- (defensive 'IntMap.adjust' under the active/retained disjointness
    -- invariant).
    ( lateCount
      , pendingRequestedKeys
      , bufferedKeys
      , sharedStateHandled
      , peerDownloadedTxs'
      , wakeChangeFromHandle
      ) =
      List.foldl'
        handleOne
        ( 0
        , requestedKeys
        , IntSet.empty
        , sharedState
        , peerDownloadedTxs peerState
        , False
        )
        txs

    -- Process omitted (not received) txs: count a penalty for every omitted
    -- request and release this peer's lease where it still held one.
    --
    -- @wakeChangeFromOmit@ is set only when a lease is actually released
    -- (other advertisers can now claim).
    (omittedCount, sharedStateReleased, wakeChangeFromOmit) =
      IntSet.foldl' handleOmitted (0, sharedStateHandled, False) pendingRequestedKeys

    -- Keys this peer is no longer attempting (everything in the batch
    -- except still-buffered keys).  Used to update both the per-peer
    -- in-flight set and the shared 'txAttempt' counters via 'releaseLease'.
    releasedKeys = requestedKeys `IntSet.difference` bufferedKeys

    -- Bump both counters only when something happened that other peers
    -- need to react to.  In the common case (all bodies received, no
    -- mempool race) no bump fires and the structural-but-pointer-different
    -- 'sharedStateReleased' is discarded by 'writeSharedStateIfChanged'.
    sharedState'
      | wakeChangeFromHandle || wakeChangeFromOmit =
          sharedStateReleased {
            sharedGeneration = sharedGeneration sharedState + 1,
            sharedRevision   = sharedRevision   sharedState + 1
          }
      | otherwise =
          sharedState

    -- Update peer state: remove processed keys, update batch tracking,
    -- and record downloaded txs.
    peerState' = peerState {
        peerAvailableTxIds =
          IntMap.withoutKeys (peerAvailableTxIds peerState) requestedKeys
      , peerRequestedTxs = peerRequestedTxs peerState `IntSet.difference` requestedKeys
      , peerRequestedTxBatches = remainingRequestedBatches
      , peerRequestedTxsSize = peerRequestedTxsSize peerState - requestedTxBatchSize requestedBatch
      , peerDownloadedTxs = peerDownloadedTxs'
      }

    peerInFlight' = peerInFlight {
        pifLeased     = pifLeased     peerInFlight `IntSet.difference` releasedKeys,
        pifAttempting = pifAttempting peerInFlight `IntSet.difference` releasedKeys,
        pifAdvertised = pifAdvertised peerInFlight `IntSet.difference` releasedKeys
      }

    keyWasLive k =
         IntMap.member k (sharedTxTable sharedState)
      || retainedMember k (sharedRetainedTxs sharedState)

    -- Fold function over received txs: classify as late, already in mempool, or buffer for
    -- download.
    handleOne
      ( !lateCountAcc
      , !pendingKeysAcc
      , !bufferedAcc
      , !sharedAcc
      , !downloadedAcc
      , !wakeAcc
      )
      (txid, tx) =
        case Map.lookup (getRawTxId txid) txidToKey of
             Nothing ->
               ( lateCountAcc + 1
               , pendingKeysAcc
               , bufferedAcc
               , sharedAcc
               , downloadedAcc
               , wakeAcc
               )
             Just (TxKey k)
               | retainedMember k (sharedRetainedTxs sharedAcc) ->
                   let sharedAcc' =
                         sharedAcc {
                           sharedTxTable =
                             IntMap.adjust decAttempt k (sharedTxTable sharedAcc)
                         }
                   in ( lateCountAcc + 1
                      , IntSet.delete k pendingKeysAcc
                      , bufferedAcc
                      , sharedAcc'
                      , downloadedAcc
                      , wakeAcc
                      )
               | mempoolHasTx txid ->
                   let sharedAcc' =
                         sharedAcc {
                           sharedTxTable = IntMap.delete k (sharedTxTable sharedAcc),
                           sharedRetainedTxs =
                             retainedInsertMax k retainUntil (sharedRetainedTxs sharedAcc)
                         }
                   in ( lateCountAcc + 1
                      , IntSet.delete k pendingKeysAcc
                      , bufferedAcc
                      , sharedAcc'
                      , downloadedAcc
                      , True
                      )
               | otherwise ->
                   case IntMap.lookup k (sharedTxTable sharedAcc) of
                        Just _txEntry
                          | IntSet.member k (pifAttempting peerInFlight) ->
                              ( lateCountAcc
                              , IntSet.delete k pendingKeysAcc
                              , IntSet.insert k bufferedAcc
                              , sharedAcc
                              , IntMap.insert k tx downloadedAcc
                              , wakeAcc
                              )
                        _ ->
                              ( lateCountAcc + 1
                              , IntSet.delete k pendingKeysAcc
                              , bufferedAcc
                              , sharedAcc
                              , downloadedAcc
                              , wakeAcc
                              )

    -- Handle omitted (not received) txs: release this peer's lease so
    -- another advertiser can claim the tx, decrement 'txAttempt'.
    handleOmitted (!omittedCountAcc, !sharedAcc, !wakeAcc) k
      | keyWasLive k =
          case IntMap.lookup k (sharedTxTable sharedAcc) of
            Just txEntry ->
              ( omittedCountAcc + 1
              , sharedAcc {
                  sharedTxTable =
                    IntMap.insert k (releaseLease txEntry)
                      (sharedTxTable sharedAcc)
                }
              , True
              )
            Nothing ->
              (omittedCountAcc + 1, sharedAcc, wakeAcc)
      | otherwise =
          (omittedCountAcc + 1, sharedAcc, wakeAcc)

    -- Decrement the entry's attempt counter (e.g. when its body arrived
    -- but the tx had been retained meanwhile, so the peer's attempt is
    -- effectively over).  Lease unaffected: if this peer didn't hold it,
    -- nothing to do; if it did, the decrement still leaves @txAttempt@
    -- non-negative because every increment had a paired peer.
    decAttempt entry@TxEntry { txAttempt } =
      entry { txAttempt = max 0 (txAttempt - 1) }

    releaseLease txEntry@TxEntry { txLease, txAttempt } =
      txEntry {
        txLease = case txLease of
                       TxLeased owner _ | owner == peeraddr -> TxClaimable now
                       _                                    -> txLease,
        txAttempt = max 0 (txAttempt - 1)
      }
{-# INLINABLE handleReceivedTxs #-}


-- | Handle the result of submitting buffered txs to the mempool.
--
-- Accepted txs leave the active table and move into the retained set so later
-- txid advertisements can be acknowledged without re-requesting the body.
-- Txs rejected by the mempool release this peer's lease and clear
-- 'txInSubmission' so another advertiser may try later.
handleSubmittedTxs :: Eq peeraddr
                   => Time
                   -> TxDecisionPolicy
                   -> peeraddr
                   -> [TxKey]
                   -> [TxKey]
                   -> PeerTxLocalState tx
                   -> PeerTxInFlight
                   -> SharedTxState peeraddr txid
                   -> (PeerTxLocalState tx, PeerTxInFlight, SharedTxState peeraddr txid)
handleSubmittedTxs now policy peeraddr acceptedTxs rejectedTxs peerState peerInFlight sharedState =
  (peerState', peerInFlight', sharedState')
  where
    acceptedKeys = IntSet.fromList (unTxKey <$> acceptedTxs)
    rejectedKeys = IntSet.fromList (unTxKey <$> rejectedTxs)
    submittedKeys = acceptedKeys `IntSet.union` rejectedKeys

    peerState' = peerState {
        peerDownloadedTxs =
          IntMap.withoutKeys (peerDownloadedTxs peerState) submittedKeys,
        peerAvailableTxIds =
          IntMap.withoutKeys (peerAvailableTxIds peerState) submittedKeys,
        -- An accepted tx from this peer is a demonstrable sign of
        -- progress: clear the empty-reply backoff flag so the txid
        -- picker may resume issuing pipelined 'ack=0, req=N' requests.
        peerLastTxIdReplyWasEmpty =
          null acceptedTxs && peerLastTxIdReplyWasEmpty peerState
      }

    -- Submission outcomes clear the peer's submission and advertised
    -- contributions for these keys; the peer is done with them.  The
    -- attempt was already taken off 'pifAttempting' (and 'txAttempt')
    -- when 'markSubmittingTxs' fired.
    peerInFlight' = peerInFlight {
        pifLeased     = pifLeased     peerInFlight `IntSet.difference` submittedKeys,
        pifSubmitting = pifSubmitting peerInFlight `IntSet.difference` submittedKeys,
        pifAdvertised = pifAdvertised peerInFlight `IntSet.difference` submittedKeys
      }

    sharedStateAfterAccepted =
      List.foldl' acceptSubmittedTx sharedState acceptedTxs

    sharedStateAfterRejected =
      IntSet.foldl' updateRejected sharedStateAfterAccepted rejectedKeys

    -- Both branches grant new options to other peers, so bump
    -- 'sharedGeneration': accept moves the key into 'sharedRetainedTxs'
    -- (other advertisers can ack via the retained-membership check),
    -- reject releases the lease (other advertisers can claim).
    sharedState' =
      sharedStateAfterRejected {
        sharedGeneration = sharedGeneration sharedState + 1
      }

    retainedUntil = addTime (bufferedTxsMinLifetime policy) now

    acceptSubmittedTx sharedAcc (TxKey k) =
      sharedAcc {
        sharedTxTable = IntMap.delete k (sharedTxTable sharedAcc),
        sharedRetainedTxs =
          retainedInsertMax k retainedUntil (sharedRetainedTxs sharedAcc)
      }

    updateRejected sharedAcc k =
      case IntMap.lookup k (sharedTxTable sharedAcc) of
        Just txEntry ->
          sharedAcc {
            sharedTxTable =
              IntMap.insert k (markRejected txEntry) (sharedTxTable sharedAcc)
          }
        Nothing ->
          sharedAcc

    markRejected txEntry@TxEntry { txLease } =
      txEntry {
        txLease = case txLease of
          TxLeased owner _ | owner == peeraddr -> TxClaimable now
          _                                    -> txLease,
        txInSubmission = False
      }
{-# INLINABLE handleSubmittedTxs #-}


-- | Mark buffered txs as entering mempool submission.
--
-- Decrements 'txAttempt' (the peer is leaving the @attempting@ state)
-- and sets 'txInSubmission'.  STM serialisation around the
-- @ChooseSubmit@ choice guarantees only one peer ever flips
-- 'txInSubmission' from @False@ to @True@ for a given key.
--
-- Bumps only 'sharedRevision' (the structural dirty bit), not
-- 'sharedGeneration' (the wake counter).  This transition only removes
-- options for other peers (they may no longer claim or submit the
-- key) and does not enable any new ack - that happens on the
-- subsequent 'handleSubmittedTxs' acceptance, which bumps the wake
-- counter itself.
markSubmittingTxs :: [TxKey]
                  -> SharedTxState peeraddr txid
                  -> SharedTxState peeraddr txid
markSubmittingTxs [] st = st
markSubmittingTxs txKeys st =
  st {
    sharedTxTable  = List.foldl' markOne (sharedTxTable st) txKeys,
    sharedRevision = sharedRevision st + 1
  }
  where
    markOne txTable (TxKey k) = IntMap.adjust markSubmitting k txTable

    markSubmitting txEntry@TxEntry { txAttempt } =
      txEntry {
        txAttempt      = max 0 (txAttempt - 1),
        txInSubmission = True
      }


-- | Handle a batch of txids received from one peer.
--
-- Newly seen txids are interned, appended to the peer's unacknowledged queue,
-- and entered into the shared tx table as claimable work. Any peer that later
-- advertises the txid may claim it once its score-derived delay has elapsed,
-- which avoids pinning fresh work to the first peer that happened to announce
-- it.
handleReceivedTxIds :: forall peeraddr txid tx. HasRawTxId txid
                    => (txid -> Bool)
                    -> Time
                    -> TxDecisionPolicy
                    -> NumTxIdsToReq
                    -> [(txid, SizeInBytes)]
                    -> PeerTxLocalState tx
                    -> PeerTxInFlight
                    -> SharedTxState peeraddr txid
                    -> (PeerTxLocalState tx, PeerTxInFlight, SharedTxState peeraddr txid)
handleReceivedTxIds mempoolHasTx now policy requestedTxIds txidsAndSizes
                    peerState peerInFlight sharedState =
    (peerState'', peerInFlight'', sharedState'')
  where
    peerAdvertisedKeys0 = pifAdvertised peerInFlight
    peerAcksPending0    = pifAcksPending peerInFlight

    -- Fold over received txids: build unacknowledged list, update tables.
    --
    -- Two flags are accumulated:
    --   wakeChange         - mempool branch fired, whose retained-insert may
    --                        enable a late ack on another peer that already
    --                        advertised the same txid.  Bumps the wake
    --                        counter ('sharedGeneration').
    --   revisionOnlyChange - any other structural change (new entry, new
    --                        txKey interning) that other peers cannot act
    --                        on yet.  Bumps only the dirty bit
    --                        ('sharedRevision').
    ( receivedTxKeysRev
      , peerAvailableTxIds'
      , sharedStateHandled
      , peerAdvertisedKeys'
      , peerAcksPending'
      , wakeChange
      , revisionOnlyChange
      ) =
      List.foldl'
        step
        ( []
        , peerAvailableTxIds peerState
        , sharedState
        , peerAdvertisedKeys0
        , peerAcksPending0
        , False
        , False
        )
        txidsAndSizes

    peerUnacknowledgedTxIds' =
      peerUnacknowledgedTxIds peerState <> StrictSeq.fromList (reverse receivedTxKeysRev)

    peerState'' = peerState {
        peerUnacknowledgedTxIds = peerUnacknowledgedTxIds',
        peerRequestedTxIds = fromIntegral $
            max 0 ( fromIntegral (peerRequestedTxIds peerState) -
                    fromIntegral requestedTxIds :: Int ),
        peerAvailableTxIds = peerAvailableTxIds',
        -- Track empty-reply state so the txid picker can back off
        -- 'ack=0, req=N' pipelined requests until the peer makes
        -- progress.
        peerLastTxIdReplyWasEmpty = null txidsAndSizes
      }

    peerInFlight'' = peerInFlight {
        pifAdvertised  = peerAdvertisedKeys',
        pifAcksPending = peerAcksPending'
      }

    sharedState''
      | wakeChange =
          sharedStateHandled {
            sharedGeneration = sharedGeneration sharedState + 1,
            sharedRevision   = sharedRevision   sharedState + 1
          }
      | revisionOnlyChange =
          sharedStateHandled {
            sharedRevision = sharedRevision sharedState + 1
          }
      | otherwise =
          sharedState

    retainUntil = addTime (bufferedTxsMinLifetime policy) now

    -- Process each received txid: classify as retained, in mempool, or new entry.
    step
      :: ( [TxKey]
         , IntMap.IntMap SizeInBytes
         , SharedTxState peeraddr txid
         , IntSet.IntSet
         , IntSet.IntSet
         , Bool                     -- wakeChange
         , Bool                     -- revisionOnlyChange
         )
      -> (txid, SizeInBytes)
      -> ( [TxKey]
         , IntMap.IntMap SizeInBytes
         , SharedTxState peeraddr txid
         , IntSet.IntSet
         , IntSet.IntSet
         , Bool
         , Bool
         )
    step
      ( !unacknowledgedAcc
      , !availableAcc
      , !sharedAcc
      , !peerAdvertisedKeysAcc
      , !peerAcksPendingAcc
      , !wakeAcc
      , !revOnlyAcc
      )
      (txid, txSize)
      | retainedMember k retainedAcc =
          -- No 'sharedTxTable' change.  Only revision-relevant if interning
          -- added a new key.
          ( txKey : unacknowledgedAcc
          , IntMap.delete k availableAcc
          , sharedAcc'
          , IntSet.delete k peerAdvertisedKeysAcc
          , IntSet.insert k peerAcksPendingAcc
          , wakeAcc
          , revOnlyAcc'
          )
      | mempoolHasTx txid =
          -- Retained-insert may enable a late ack on another peer that
          -- already advertised this txid: wake-relevant.
          ( txKey : unacknowledgedAcc
          , IntMap.delete k availableAcc
          , sharedAcc' {
              sharedTxTable = IntMap.delete k (sharedTxTable sharedAcc'),
              sharedRetainedTxs =
                retainedInsertMax k retainUntil (sharedRetainedTxs sharedAcc')
            }
          , IntSet.delete k peerAdvertisedKeysAcc
          , IntSet.insert k peerAcksPendingAcc
          , True
          , True
          )
      | otherwise =
          case IntMap.lookup k (sharedTxTable sharedAcc') of
            Nothing ->
              -- New 'TxClaimable' entry.  Only this peer knows about it,
              -- so no wake; revision tracks the structural change.
              let txEntry = TxEntry {
                              txLease = TxClaimable now,
                              txAttempt = 0,
                              txInSubmission = False,
                              currentMaxInflightMultiplicity =
                                txInflightMultiplicity policy
                            }
              in ( txKey : unacknowledgedAcc
                 , IntMap.insert k txSize availableAcc
                 , sharedAcc' {
                     sharedTxTable = IntMap.insert k txEntry (sharedTxTable sharedAcc')
                   }
                 , IntSet.insert k peerAdvertisedKeysAcc
                 , IntSet.insert k peerAcksPendingAcc
                 , wakeAcc
                 , True
                 )
            Just _ ->
              -- Existing entry; another peer already advertised.  No
              -- 'sharedTxTable' change beyond optional interning.
              ( txKey : unacknowledgedAcc
              , IntMap.insert k txSize availableAcc
              , sharedAcc'
              , IntSet.insert k peerAdvertisedKeysAcc
              , IntSet.insert k peerAcksPendingAcc
              , wakeAcc
              , revOnlyAcc'
              )
      where
        retainedAcc = sharedRetainedTxs sharedAcc'
        revOnlyAcc' = revOnlyAcc || txKeyWasNew
        (txKey@(TxKey k), txKeyWasNew, sharedAcc') = lookupOrInternTxId txid sharedAcc

    lookupOrInternTxId txid st =
      let (_, key, st') = internTxId txid st
      in (key, sharedNextTxKey st' /= sharedNextTxKey st, st')
{-# INLINABLE handleReceivedTxIds #-}
