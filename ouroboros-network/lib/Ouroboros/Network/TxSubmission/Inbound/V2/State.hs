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
  , sweepSharedStateUpTo
  , expireSharedStateUpTo
  , reviewDoomed
  , doomedKeys
    -- * For tests and the benchmark
  , sweepSharedState
  , walkFrom
  ) where

import Control.Exception (assert)
import Control.Monad.Class.MonadTime.SI (DiffTime, Time, addTime, diffTime)
import Data.Foldable (toList)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List as List (foldl')
import Data.Map.Strict qualified as Map
import Data.Sequence.Strict qualified as StrictSeq
import GHC.Stack (HasCallStack)

import Ouroboros.Network.Protocol.TxSubmission2.Type (NumTxIdsToAck,
           SizeInBytes)
import Ouroboros.Network.Tx (HasRawTxId (..))
import Ouroboros.Network.TxSubmission.Inbound.V2.Policy
import Ouroboros.Network.TxSubmission.Inbound.V2.Tier (effectiveTier,
           nextJitter, recordNovel, slowWait)
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
    -- | Configuration options
    pacConfig       :: !TxSubmissionConfig,
    -- | Address of the peer whose next action is being chosen.
    pacPeerAddr     :: !peeraddr,
    -- | Current peer-local state after local pruning has been applied.
    pacPeerState    :: !(PeerTxLocalState tx),
    -- | This peer's contribution counters mirroring shared-state writes.
    pacPeerInFlight :: !PeerTxInFlight,
    -- | Shared tx-submission state after shared pruning has been applied.
    pacSharedState  :: !(SharedTxState peeraddr txid),
    -- | Score-derived delay this peer must wait after a tx becomes claimable.
    pacClaimDelay   :: !DiffTime,
    -- | The tier the peer is served on now ('effectiveTier').
    pacTier         :: !PeerTier
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
    -- | Park until the counter moves or the wake delay elapses: an
    -- optional wake delay and the counter, the claimable one for the slow
    -- tier's wait while its gate holds a txid request, the wake counter
    -- otherwise.
  | ChooseDoNothing !(Maybe DiffTime) !ParkCounter

-- | Build a precomputed context for selecting the next action for a peer.
--
mkPeerActionContext :: Time
                    -> TxDecisionPolicy
                    -> TxSubmissionConfig
                    -> peeraddr
                    -> PeerTxLocalState tx
                    -> PeerTxInFlight
                    -> SharedTxState peeraddr txid
                    -> PeerActionContext peeraddr txid tx
mkPeerActionContext now policy config peeraddr peerState peerInFlight sharedState =
  PeerActionContext {
    pacNow = now,
    pacPolicy = policy,
    pacConfig = config,
    pacPeerAddr = peeraddr,
    pacPeerState = peerState',
    pacPeerInFlight = peerInFlight',
    pacSharedState = sharedState,
    pacClaimDelay = peerClaimDelay policy now (peerScore peerState'),
    pacTier = effectiveTier policy now peerState'
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
    -- is managed by the txid path and must stay in lockstep with the
    -- peer's queue; 'pifSubmitting' is only set
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
               -> TxSubmissionConfig
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
                        -> TxSubmissionConfig
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
                       -> TxSubmissionConfig
                       -> peeraddr
                       -> PeerTxLocalState tx
                       -> PeerTxInFlight
                       -> SharedTxState peeraddr txid
                       -> (PeerAction, PeerTxLocalState tx, PeerTxInFlight, SharedTxState peeraddr txid)
nextPeerActionWithMode txIdRequestMode now policy config peeraddr peerState peerInFlight sharedState =
    let (action, peerState', peerInFlight', sharedState'') =
          applyPeerActionChoice ctx (pickPeerActionChoice txIdRequestMode ctx)
        newPhase = phaseForAction txIdRequestMode (peerPhase peerState) action
        peerState'' = if newPhase == peerPhase peerState'
                         then peerState'
                         else peerState' { peerPhase = newPhase }
    in (action, peerState'', peerInFlight', sharedState'')
  where
    sharedState' = bumpStuckEntries now policy (peerDownloadedTxs peerState) sharedState
    ctx = mkPeerActionContext now policy config peeraddr peerState peerInFlight sharedState'

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
  -- Pick TXids to ack and/or request more TXids, unless the slow tier's
  -- gate holds the request.  Acknowledgements that free a buffered body
  -- pass the gate, in a request for no txids, which leaves the gate as
  -- it is; not pipelined, where a request for nothing burns a slot.
  | Just (acknowledgedTxIds, txIdsToAcknowledge, txIdsToRequest, unacknowledgedTxIds') <-
      pickRequestTxIdsAction txIdRequestMode ctx =
      case slowGate ctx of
        Nothing        -> ChooseRequestTxIds acknowledgedTxIds txIdsToAcknowledge
                                             txIdsToRequest unacknowledgedTxIds'
        Just gateUntil
          | txIdRequestMode == AllowAnyTxIdRequests
          , txIdsToAcknowledge > 0
          , Just _ <- pickSubmitFrom ctx unacknowledgedTxIds' ->
              ChooseRequestTxIds acknowledgedTxIds txIdsToAcknowledge 0 unacknowledgedTxIds'
          | otherwise -> waitGate gateUntil
  -- Nothing to do now.  A slow peer whose gate holds waits for the gate
  -- as for a gated request, unless it holds a buffered body; anyone else
  -- parks on the generation.
  | Just gateUntil <- slowGate ctx
  , noBuffered =
      waitGate gateUntil
  | otherwise =
      ChooseDoNothing wakeDelay WakeCounter
  where
    noBuffered = IntMap.null (peerDownloadedTxs (pacPeerState ctx))

    -- one pass over the advertised keys, for the wake and the gate's park
    (anyLive, claimWake) = advertisedOutlook ctx
    wakeDelay = nextWakeDelay ctx claimWake

    -- Wait for the gate, or less for a timed claim or bump, parked on the
    -- claimable counter rather than the generation, which moves on every
    -- accept: a slow peer re-evaluating at each of those would cost what
    -- the tier saves, while a lease released elsewhere still wakes it; on
    -- no counter when no key it advertises has a live entry, since the
    -- keys it holds stay dead until it acknowledges them, after the gate,
    -- and nothing the claimable counter announces is its to claim.  A
    -- peer holding a buffered body keeps the generation, since another
    -- peer's accept may be what lets it submit that body, and the gate is
    -- only meant to space txid requests.
    waitGate gateUntil =
        ChooseDoNothing (Just delay) park
      where
        gateDelay = gateUntil `diffTime` pacNow ctx
        delay     = maybe gateDelay (min gateDelay) wakeDelay
        park | not noBuffered = WakeCounter
             | anyLive        = ClaimableCounter
             | otherwise      = NoCounter

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
       ChooseDoNothing wakeDelay park ->
         applyDoNothingChoice ctx wakeDelay park

-- | The slow tier's gate on a txid request: the time it opens, when the
-- peer is slow and the gate set by the previous request for txids has
-- not passed.  It holds blocking requests too: a peer with a stream
-- answers one at once, so leaving them open would leave the tier nothing
-- to slow; a peer without one waits for a tx either way, so the gate
-- costs it at most one cadence on its first.  A buffered body behind a
-- key dead for the peer, a body it omitted or an entry resolved since,
-- waits for that key's acknowledgement, which goes out at once in a
-- request for no txids ('pickPeerActionChoice').
slowGate :: PeerActionContext peeraddr txid tx -> Maybe Time
slowGate PeerActionContext { pacNow, pacPeerState, pacTier }
  | pacTier == FastTier = Nothing
  | Just gateUntil <- peerSlowGate pacPeerState
  , gateUntil > pacNow  = Just gateUntil
  | otherwise           = Nothing

-- | Construct a 'PeerSubmitTxs' action for buffered transactions.
--
-- Marks the selected txs as in-submission on this peer.  Other peers
-- skip them while the 'txInSubmission' flag is set.  STM serialisation
-- guarantees only one peer can win the @ChooseSubmit@ race for a given key.
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
    -- the request is recorded behind the earlier outstanding ones: for
    -- txids on this decision's tier, or for acknowledgements alone
    request | txIdsToRequest > 0 = TxIdsOn (pacTier ctx)
            | otherwise          = AcksOnly
    peerState' =
      recordTxIdRequest request peerState0 {
        peerAvailableTxIds =
          IntMap.withoutKeys (peerAvailableTxIds peerState0) acknowledgedKeys,
        peerUnacknowledgedTxIds = unacknowledgedTxIds',
        peerRequestedTxIds = peerRequestedTxIds peerState0 + txIdsToRequest
      }
    -- on the slow tier a request for txids sets the gate for the next
    -- one, at the cadence jittered by a draw from the peer's own
    -- generator; one for acknowledgements alone leaves the gate
    peerState''
      | pacTier ctx == SlowTier
      , txIdsToRequest > 0 =
          let (u, seed') = nextJitter (peerJitterSeed peerState0)
              !gateUntil = slowWait (pacPolicy ctx) u `addTime` pacNow ctx
          in peerState' {
            peerSlowGate   = Just gateUntil,
            peerJitterSeed = seed'
          }
      | otherwise = peerState'
    pif = pacPeerInFlight ctx
    peerInFlight'' = pif {
        pifAdvertised = pifAdvertised pif `IntSet.difference` acknowledgedKeys
      }

-- | Construct a 'PeerDoNothing' action.
applyDoNothingChoice
  :: PeerActionContext peeraddr txid tx
  -> Maybe DiffTime
  -> ParkCounter
  -> (PeerAction, PeerTxLocalState tx, PeerTxInFlight, SharedTxState peeraddr txid)
applyDoNothingChoice ctx wakeDelay park =
  ( PeerDoNothing wakeDelay park
  , pacPeerState ctx
  , pacPeerInFlight ctx
  , pacSharedState ctx
  )

-- | Select downloaded transactions that this peer may submit to the mempool.
pickSubmitAction
  :: PeerActionContext peeraddr txid tx
  -> Maybe [TxKey]
pickSubmitAction ctx@PeerActionContext { pacPeerState } =
    pickSubmitFrom ctx (peerUnacknowledgedTxIds pacPeerState)

-- | 'pickSubmitAction' over a given unacknowledged queue: the one an
-- acknowledgement leaves, for whether it frees a buffered body.
pickSubmitFrom
  :: PeerActionContext peeraddr txid tx
  -> StrictSeq.StrictSeq TxKey
  -> Maybe [TxKey]
pickSubmitFrom PeerActionContext { pacPeerState, pacSharedState } unacknowledgedTxIds =
  if null pickBufferedTxsToSubmit
     then Nothing
     else Just pickBufferedTxsToSubmit
  where

    -- Walk the unacknowledged txid queue in peer advertisement order,
    -- picking bodies buffered by this peer for immediate submission.
    -- Classification of each entry:
    --
    -- * 'sharedTxTable' has 'Nothing' for the key: the tx was resolved at
    --   some point (a peer submitted it to the mempool) and the entry was
    --   deleted, or this peer omitted the body or had it rejected and so
    --   stopped advertising it.  The sweep drops no entry a peer still
    --   advertises, so 'Nothing' here means one of those; safe to skip
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
        go IntSet.empty [] (toList unacknowledgedTxIds)
      where
        go _    acc [] = reverse acc
        go seen acc (txKey@(TxKey k) : rest)
          | IntSet.member k seen = go seen acc rest
          | otherwise =
              case IntMap.lookup k (sharedTxTable pacSharedState) of
                Just txEntry
                  | txBufferedByPeer pacPeerState k
                  , not (txInSubmission txEntry) ->
                      go (IntSet.insert k seen) (txKey : acc) rest
                Just _  -> reverse acc
                Nothing -> go seen acc rest

-- | Select transactions to request from the peer, if within policy limits.
--
-- Returns a triple of:
-- Tx keys to request (in the peer's advertisement order)
-- Total serialized size of the requested txs
-- Updated shared state with new lease ownership for selected txs
pickRequestTxsAction :: forall peeraddr txid tx.
                        Eq peeraddr
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
    -- Respects 'maxOutstandingTxBatchesPerPeer', 'txsSizeInflightPerPeer' and
    -- 'txsSizePerBatch' policy constraints.
    pickTxsToRequest =
      if StrictSeq.length (peerRequestedTxBatches pacPeerState) >=
           maxOutstandingTxBatchesPerPeer pacPolicy
         then ([], 0, sharedTxTable pacSharedState)
         else go IntSet.empty [] 0 (sharedTxTable pacSharedState) candidates
      where
        leaseUntil = addTime (interTxSpace pacPolicy) pacNow

        -- Iterate the peer's unacknowledged queue, which preserves the order
        -- in which the peer sent us the txids.  Walking the queue in that
        -- order aligns our fetch order with the peer's advertisement order,
        -- which preserves whatever submission-validity ordering the peer
        -- intended within the batch.
        candidates :: [(Int, SizeInBytes)]
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
        go :: IntSet
           -> [TxKey]
           -- ^ selected `TxKey`s to request
           -> SizeInBytes
           -- ^ overall size of the requested txs
           -> IntMap.IntMap (TxEntry peeraddr)
           -> [(Int, SizeInBytes)]
           -- ^ candidates
           -> ([TxKey], SizeInBytes, IntMap.IntMap (TxEntry peeraddr))
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

        -- Check if the size is within the budget.
        --
        -- The inflight size limit is soft by up to one tx size, so a peer with
        -- spare capacity may still request its first tx in a batch even when
        -- that single tx exceeds the remaining byte budget.  The per-batch
        -- limit is hard, except for the first transaction in a batch, so
        -- that a tx larger than `txsSizePerBatch` does not block the peer.
        --
        -- An advertised txid with size above `max_TX_SIZE` is a protocol
        -- error, so a batch which exceeds `txsSizePerBatch` holds a single tx
        -- of at most `max_TX_SIZE`.  `saneTxDecisionPolicy` accounts for it
        -- when checking that a reply fits within the `StTxs` size limit.
        exceedsBudget selectedSize txSize
            | selectedSize /= 0
            , selectedSize + txSize > txsSizePerBatch pacPolicy = True
            | selectedSize + txSize <= sizeBudget = False
            | selectedSize /= 0 = True
            | otherwise = peerRequestedTxsSize pacPeerState >= txsSizeInflightPerPeer pacPolicy

        -- Remaining bytes available for requesting new tx, based on the
        -- per-peer inflight size limit.
        sizeBudget =
          if peerRequestedTxsSize pacPeerState >= txsSizeInflightPerPeer pacPolicy
             then 0
             else txsSizeInflightPerPeer pacPolicy - peerRequestedTxsSize pacPeerState


-- | Determine txid acknowledgment and request counts, if any work is available.
pickRequestTxIdsAction :: TxIdRequestMode
                       -> PeerActionContext peeraddr txid tx
                       -> Maybe ([TxKey], NumTxIdsToAck, NumTxIdsToReq, StrictSeq.StrictSeq TxKey)
pickRequestTxIdsAction txIdRequestMode ctx@PeerActionContext { pacConfig, pacPeerState }
  | txIdsToAcknowledge <= 0 && txIdsToRequest <= 0 = Nothing

  -- A pipelined txid request must both ack and request: with nothing to
  -- ack the peer waits for its unacked txids to resolve rather than poll
  -- with 'ack=0, req=N' messages.  Stricter than V1, which pipelines
  -- such a poll after a body request while the window has room.
  -- (An empty unacked queue has nothing to ack, so this also keeps the
  -- post-ack queue non-empty, as a pipelined request requires.)
  | txIdRequestMode == AllowPipelinedTxIdRequests
  , txIdsToAcknowledge <= 0 || txIdsToRequest <= 0 = Nothing

  | otherwise = Just (acknowledgedTxIds, txIdsToAcknowledge, txIdsToRequest, unacknowledgedTxIds)
  where

    -- Split the unacknowledged txid queue into acknowledged and remaining portions.
    --
    -- acknowledgedTxIds is the longest prefix of "ackable" txids.
    -- unacknowledgedTxIds is the remaining txids
    acknowledgedTxIds = toList acknowledgedTxIdsSeq
    txIdsToAcknowledge = fromIntegral numOfAcked

    ackablePrefix =
      StrictSeq.takeWhileL (txIdAckable ctx)
                           (peerUnacknowledgedTxIds pacPeerState)

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

    (acknowledgedTxIdsSeq, unacknowledgedTxIds) = StrictSeq.splitAt numOfAcked
      (peerUnacknowledgedTxIds pacPeerState)
    unackedAndRequested = numOfUnacked + numOfRequested

    -- How many new txids we can request: zero while txids sit unacked
    -- with nothing ackable yet, otherwise capped by the unack-window
    -- room left after this round's ack ('maxNumUnacknowledgedTxIds -
    -- unackedAndRequested + numOfAcked') and by the per-message
    -- request limit ('maxNumTxIdsToRequest - numOfRequested').
    --
    -- Window mismatch (design decision): with 'TxOutboundV_2' the window
    -- is a local setting of each side and it is not negotiated, so we size
    -- requests from our own window.  If the peer's window is smaller, it
    -- clamps our request to the room its window has once our ack is
    -- applied.  We accept that rather than announce the window through
    -- the handshake.  A pipelined request always acks something, so that
    -- room is at least the acked number and the reply is empty only when
    -- the peer has nothing new; with equal windows the clamp never binds.
    txIdsToRequest
      | numOfAcked == 0 && numOfUnacked > 0 = 0
      | otherwise =
          fromIntegral $ max 0 $ min
            (fromIntegral (maxNumUnacknowledgedTxIds pacConfig) - unackedAndRequested + numOfAcked)
            (fromIntegral (maxNumTxIdsToRequest pacConfig) - numOfRequested)

-- | What the keys this peer advertises hold for it, in one pass over
-- them: whether any has a live entry, and the earliest claim-ready time
-- after now among those entries.
advertisedOutlook :: PeerActionContext peeraddr txid tx
                  -> (Bool, Maybe Time)
advertisedOutlook PeerActionContext { pacNow, pacClaimDelay, pacPeerInFlight, pacSharedState } =
    IntSet.foldl' step (False, Nothing) (pifAdvertised pacPeerInFlight)
  where
    step (!live, !acc) k =
      case IntMap.lookup k (sharedTxTable pacSharedState) of
        Just txEntry -> (True, minMaybe acc (futureClaimWake txEntry))
        Nothing      -> (live, acc)

    futureClaimWake txEntry =
      let claimWake = txClaimReadyAt pacClaimDelay txEntry in
      if claimWake > pacNow then Just claimWake
                            else Nothing

-- | Compute the time delay until the peer should next wake to check for
-- work, from the outlook's claim wake ('advertisedOutlook'): the earlier
-- of that and the earliest bump-ready time among the txs this peer holds
-- buffered.  Scoped to 'peerDownloadedTxs' to match 'bumpStuckEntries':
-- only peers with the body buffered can issue a bump, so non-buffering
-- peers don't schedule wakes for entries they can't touch.
nextWakeDelay :: PeerActionContext peeraddr txid tx
              -> Maybe Time
              -> Maybe DiffTime
nextWakeDelay PeerActionContext { pacNow, pacPolicy, pacPeerState, pacSharedState } nextClaimWake =
    (`diffTime` pacNow) <$> minMaybe nextClaimWake nextBumpWake
  where
    nextBumpWake =
      IntMap.foldlWithKey' stepBump Nothing (peerDownloadedTxs pacPeerState)

    stepBump acc k _tx =
      case IntMap.lookup k (sharedTxTable pacSharedState) of
        Just txEntry -> minMaybe acc (nextStuckBumpWake pacNow pacPolicy txEntry)
        Nothing      -> acc

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
    txAttempt = txAttempt + 1,
    -- a claim ends a review: the lease is a write anyway
    txDoom    = Undoomed
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
-- of 'awaitPark' and re-evaluate eligibility under the new cap.
bumpStuckEntries :: forall peeraddr txid tx.
                    Time
                 -> TxDecisionPolicy
                 -> IntMap.IntMap tx
                 -> SharedTxState peeraddr txid
                 -> SharedTxState peeraddr txid
bumpStuckEntries now policy downloadedTxs st =
    if anyBumped
       then st { sharedTxTable    = txTable',
                 sharedGeneration = sharedGeneration st + 1,
                 sharedRevision   = sharedRevision st + 1,
                 sharedClaimable  = sharedClaimable st + 1 }
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
-- An ack is only a count of txids to drop from the front of this peer's
-- 'peerUnacknowledgedTxIds' queue; it never refers to shared state.  So
-- once a txid is resolved it stays ackable, even after the sweep has
-- pruned both its 'sharedTxTable' entry and its retained marker.
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

-- | 'sweepSharedStateUpTo' with no limit: what repeated bounded sweeps
-- round the table amount to; the tests' and the benchmark's oracle.
sweepSharedState :: HasRawTxId txid
                 => DiffTime
                 -> Time
                 -> SharedTxState peeraddr txid
                 -> (SharedTxState peeraddr txid, IntSet)
sweepSharedState grace now st =
    case sweepSharedStateUpTo maxBound grace now sweepStart st of
      (st', _, _, doomed) -> (st', doomed)
{-# INLINABLE sweepSharedState #-}

-- | Shared-state cleanup, at most @limit@ units of work per sweep.
--
-- Two kinds of dead entries go:
--
-- * Retained entries whose retention deadline has passed, with their
--   lookup entries ('sharedTxIdToKey', 'sharedKeyToTxId'): a lookup entry
--   lives with its residence.  A peer still holding the key in its
--   unacknowledged queue acknowledges it as resolved ('txIdAckable'), and
--   a body arriving for it counts as late.  The sweep drops these itself.
-- * Abandoned 'sharedTxTable' entries: claimable for a grace with no
--   attempt and no submission, and advertised by no live peer, their
--   advertisers having left or omitted the body.  The sweep dooms the
--   candidates among the entries it examines ('TxDoom'); the counters
--   thread then asks every live peer's advertised set, one peer at a time,
--   and 'reviewDoomed' drops the doomed entries no peer holds, with their
--   lookup entries, and spares the rest.  An advertisement or a claim of a
--   doomed entry meanwhile clears the doom, so no review drops an entry a
--   peer came to advertise after the doom: its reply committed before it,
--   and the peer's set shows the key, or after, and cleared it.
--
-- The sweep reads nothing but the state, and the peers' sets are read
-- outside its transaction, one per transaction.  Advertising an existing
-- entry writes nothing shared unless the entry is under review.
--
-- Bumps only 'sharedRevision' (the structural dirty bit), not
-- 'sharedGeneration' (the wake counter): neither removal grants another
-- peer an option.  Waking parked peers here would cost a full
-- 'nextPeerAction' pass per peer for nothing.
--
-- A sweep expires up to half of the limit, rounded up, as retained
-- entries, the earliest first, the rest waiting, and examines what the
-- expiry left of the limit as entries of 'sharedTxTable' for candidates
-- to doom, from its cursor on and round the table, so that repeated
-- sweeps cover the whole table.  Also whether any expired entry remains,
-- so the caller can go on with 'expireSharedStateUpTo' at once, the
-- cursor to go on from, and the keys doomed, for the review.  A limit of
-- zero or less does nothing but still reports the backlog.
sweepSharedStateUpTo :: HasRawTxId txid
                     => Int
                     -> DiffTime
                     -- ^ the grace: how long an entry is claimable, or
                     -- spared, before it is a candidate
                     -> Time
                     -> SweepCursor
                     -> SharedTxState peeraddr txid
                     -> (SharedTxState peeraddr txid, Bool, SweepCursor, IntSet)
sweepSharedStateUpTo limit grace now cursor st
    | expired == 0 && IntSet.null doomed = (st, more, cursor', IntSet.empty)
    | otherwise =
        ( doomedSt { sharedRevision = sharedRevision st + 1 }
        , more
        , cursor'
        , doomed )
  where
    -- The expiry takes at most half the limit, rounded up, so that a
    -- backlog of expired entries never starves the walk: the drain between
    -- sweeps is where a backlog goes.
    (expiredSt, more, expired) = expireUpTo (limit - limit `div` 2) now st

    -- The candidates among the next table entries are doomed; the review
    -- drops or spares them.  The walk takes what the expiry left of the
    -- limit: one transaction does at most the limit of work in all.
    (tableEntries, from') =
      walkFrom (limit - expired) (sweepFrom cursor) (sharedTxTable expiredSt)
    doomed =
      IntSet.fromList [ k | (k, entry) <- tableEntries, candidate entry ]
    doomedSt =
      expiredSt {
        sharedTxTable =
          IntSet.foldl' (\tbl k -> IntMap.adjust doom k tbl)
                        (sharedTxTable expiredSt) doomed
      }
    doom entry = entry { txDoom = Doomed }
    cursor' = SweepCursor from'

    -- A candidate: claimable since the cutoff, a grace ago, or longer,
    -- nobody attempting or submitting it, not under review already; a
    -- spared entry once its grace has passed.  The cutoff is computed
    -- once: a 'Time' is a 'Fixed' of picoseconds, a big integer past 106
    -- days of uptime, and a sum per entry would allocate one each.
    cutoff = addTime (negate grace) now
    candidate TxEntry { txLease = TxLeased {} } = False
    candidate TxEntry { txLease = TxClaimable since, txAttempt, txInSubmission, txDoom }
      | txAttempt > 0  = False
      | txInSubmission = False
      | otherwise      =
          case txDoom of
            Undoomed  -> since <= cutoff
            Doomed    -> False
            Spared at -> at <= cutoff
{-# INLINABLE sweepSharedStateUpTo #-}

-- | The review of the entries a sweep doomed, given the keys among them
-- that some live peer advertises: those still doomed that nobody holds
-- are dropped with their lookup entries, the rest spared, candidates
-- again a grace later.  A doomed entry is claimable with no attempt and
-- no submission, as the sweep found it: a claim clears the doom, and
-- nothing else leases, attempts or submits an entry.  A key no longer
-- doomed, its doom cleared by an advertisement or a claim meanwhile, or
-- gone, is left alone.  Also the keys dropped.  Bumps the revision when
-- anything changed, never the wake counter: a drop grants nobody an
-- option.
reviewDoomed :: HasRawTxId txid
             => Time
             -> IntSet
             -- ^ the doomed keys some live peer advertises
             -> IntSet
             -- ^ the keys the sweep doomed
             -> SharedTxState peeraddr txid
             -> (SharedTxState peeraddr txid, IntSet)
reviewDoomed now held doomed st
    | IntSet.null reviewed = (st, IntSet.empty)
    | otherwise =
        -- the dropped are table entries, never retained: no retained
        -- delete
        ( (dropLookupEntries dropped
             st { sharedTxTable = IntMap.withoutKeys sparedTable dropped })
            { sharedRevision = sharedRevision st + 1 }
        , dropped )
  where
    reviewed = IntSet.filter stillDoomed doomed
    stillDoomed k =
      maybe False ((== Doomed) . txDoom) (IntMap.lookup k (sharedTxTable st))
    (spared, dropped) = IntSet.partition (`IntSet.member` held) reviewed
    sparedTable =
      IntSet.foldl' (\tbl k -> IntMap.adjust (\e -> e { txDoom = Spared now }) k tbl)
                    (sharedTxTable st) spared
{-# INLINABLE reviewDoomed #-}

-- | The keys of the entries under review ('Doomed'): what a counters
-- thread starting over an existing state reviews at its first tick.
doomedKeys :: SharedTxState peeraddr txid -> IntSet
doomedKeys = IntMap.keysSet . IntMap.filter ((== Doomed) . txDoom) . sharedTxTable

-- | At most @limit@ entries of the map in key order from the key on, going
-- on from the start once the end is reached, so that a walk takes its
-- limit or the whole map; and the key to go on from, the one after the
-- last entry taken.  Keys are non-negative and far from 'maxBound'.
walkFrom :: Int -> Int -> IntMap.IntMap a -> ([(Int, a)], Int)
walkFrom limit from m
    | limit <= 0 = ([], from)
    | otherwise  = go limit [] 0 (IntMap.toAscList onward ++ IntMap.toAscList before)
  where
    -- The entries from the key on, then those before it.
    (before, onward) = IntMap.spanAntitone (< from) m

    -- The first @n@ entries in order, and the key after the last of them:
    -- zero when there is none, so that a walk of an empty table starts
    -- over.
    go :: Int -> [(Int, a)] -> Int -> [(Int, a)] -> ([(Int, a)], Int)
    go 0 acc next _                 = (reverse acc, next)
    go _ acc next []                = (reverse acc, next)
    go n acc _    (e@(k, _) : rest) = go (n - 1) (e : acc) (k + 1) rest
{-# INLINABLE walkFrom #-}

-- | Expiry alone: 'sweepSharedStateUpTo' without the walks, for draining a
-- backlog.
expireSharedStateUpTo :: HasRawTxId txid
                      => Int
                      -> Time
                      -> SharedTxState peeraddr txid
                      -> (SharedTxState peeraddr txid, Bool)
expireSharedStateUpTo limit now st
    | expired > 0 = (bumped, more)
    | otherwise   = (st, more)
  where
    (expiredSt, more, expired) = expireUpTo limit now st
    bumped = expiredSt { sharedRevision = sharedRevision st + 1 }
{-# INLINABLE expireSharedStateUpTo #-}

-- | Expire at most @limit@ retained entries, the earliest first, with
-- their lookup entries.  Also whether any expired entry remains, and
-- how many expired; the revision is the caller's to bump.
expireUpTo :: HasRawTxId txid
           => Int
           -> Time
           -> SharedTxState peeraddr txid
           -> (SharedTxState peeraddr txid, Bool, Int)
expireUpTo limit now st
    | IntSet.null expiredRetained = (st, more, 0)
    | otherwise =
        ( dropLookupEntries expiredRetained st { sharedRetainedTxs = retained' }
        , more
        , IntSet.size expiredRetained )
  where
    now' = retainNow now
    (expiredRetained, retained') =
      retainedExpireUpTo limit now' (sharedRetainedTxs st)
    -- a backlog is a fact about the state, whatever the limit allowed:
    -- a sweep left no budget still asks for the drain
    more = retainedDueBy now' retained'
{-# INLINABLE expireUpTo #-}

-- | Whether a key is in the tx table or retained.
residentKey :: SharedTxState peeraddr txid -> Int -> Bool
residentKey st k =
  IntMap.member k (sharedTxTable st) || retainedMember k (sharedRetainedTxs st)

-- | Remove the txid <-> key lookup entries of keys that lost their
-- residence: a lookup entry lives with its residence and goes with it
-- ('expireUpTo', 'reviewDoomed').
dropLookupEntries :: HasRawTxId txid
                  => IntSet -> SharedTxState peeraddr txid -> SharedTxState peeraddr txid
dropLookupEntries keys s
  | IntSet.null keys = s
  | otherwise =
      s {
        sharedTxIdToKey = IntSet.foldl' (deleteTxId (sharedKeyToTxId s))
                                        (sharedTxIdToKey s) keys,
        sharedKeyToTxId = IntMap.withoutKeys (sharedKeyToTxId s) keys
      }
  where
    deleteTxId keyToTxId txIdToKey k =
      case IntMap.lookup k keyToTxId of
        Just txid -> Map.delete (getRawTxId txid) txIdToKey
        Nothing   -> txIdToKey
{-# INLINABLE dropLookupEntries #-}

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
    retainUntil   = retainDeadline (addTime (bufferedTxsMinLifetime policy) now)

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

    -- Process each received tx, collecting the pending requests, the
    -- keys still buffered by this peer, and updated shared state.
    -- @bufferedKeys@ is the subset of @requestedKeys@ that ended up
    -- buffered locally; the rest are released below.
    --
    -- @wakeChangeFromHandle@ is set only when the mempool branch fires,
    -- since its retained-insert can enable a late ack on another peer
    -- that already advertised this txid.  All other branches either
    -- leave the shared state untouched or perform a structural no-op
    -- (defensive 'IntMap.adjust' under the active/retained disjointness
    -- invariant).
    ( pendingRequestedKeys
      , bufferedKeys
      , sharedStateHandled
      , peerDownloadedTxs'
      , wakeChangeFromHandle
      ) =
      List.foldl'
        handleOne
        ( requestedKeys
        , IntSet.empty
        , sharedState
        , peerDownloadedTxs peerState
        , False
        )
        txs

    -- Every body not buffered is late: for a tx retained or in the
    -- mempool already, for a key resolved or re-interned since the
    -- request, or for none.
    lateCount = length txs - IntSet.size bufferedKeys

    -- Release this peer's lease on every pending key whose entry lives,
    -- where it still held one, and count the omitted: the requested keys
    -- no body answered, every body answering a distinct txid of this
    -- batch, as the client rejects a reply naming one it did not request
    -- or naming one twice.  A pending key whose residence ended
    -- meanwhile, resolved by another peer and expired, has no lookup
    -- entry any more, so a body for it arrives under an unknown txid, or
    -- under a fresh key if the txid was advertised again since, and
    -- answers one such key: late rather than omitted, and the live
    -- pending keys never outnumber the requested keys left unanswered.
    --
    -- @wakeChangeFromOmit@ is set only when a lease is actually released
    -- (other advertisers can now claim).
    livePending = IntSet.filter keyWasLive pendingRequestedKeys
    (sharedStateReleased, wakeChangeFromOmit) =
      IntSet.foldl' handleOmitted (sharedStateHandled, False) livePending
    omittedCount = IntSet.size requestedKeys - length txs

    -- Keys this peer is no longer attempting (everything in the batch
    -- except still-buffered keys).  Used to update both the per-peer
    -- in-flight set and the shared 'txAttempt' counters via 'releaseLease'.
    releasedKeys = requestedKeys `IntSet.difference` bufferedKeys

    -- Bump the counters only when something happened that other peers
    -- need to react to.  In the common case (all bodies received, no
    -- mempool race) no bump fires and the structural-but-pointer-different
    -- 'sharedStateReleased' is discarded by 'writeSharedStateIfChanged'.
    -- The released keys this peer advertised cost nothing shared: the
    -- sweep's review finds the ones left abandoned.
    sharedState'
      | wakeChangeFromHandle || wakeChangeFromOmit =
          sharedStateReleased {
            sharedGeneration = sharedGeneration sharedState + 1,
            sharedRevision   = sharedRevision   sharedState + 1,
            -- an omitted body released a lease: claimable by another
            sharedClaimable  = sharedClaimable sharedState
                             + (if wakeChangeFromOmit then 1 else 0)
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

    keyWasLive = residentKey sharedState

    -- Fold function over received txs: a body answering a pending key
    -- this peer attempts is buffered; one for a tx retained or in the
    -- mempool already, for a key answered already or re-interned since
    -- the request, or for no key at all, is dropped, late.
    handleOne
      ( !pendingKeysAcc
      , !bufferedAcc
      , !sharedAcc
      , !downloadedAcc
      , !wakeAcc
      )
      (txid, tx) =
        case Map.lookup (getRawTxId txid) txidToKey of
             Nothing ->
               ( pendingKeysAcc
               , bufferedAcc
               , sharedAcc
               , downloadedAcc
               , wakeAcc
               )
             Just (TxKey k)
               | retainedMember k (sharedRetainedTxs sharedAcc) ->
                   -- Active and retained are disjoint by construction:
                   -- 'acceptSubmittedTx' deletes from 'sharedTxTable' and
                   -- inserts into 'sharedRetainedTxs' in one step, and
                   -- 'handleReceivedTxIds' leaves the active table alone
                   -- when classifying a txid as retained.  So the key is
                   -- not in 'sharedTxTable' here and there is nothing to
                   -- decrement; skip the record update.
                   ( IntSet.delete k pendingKeysAcc
                   , bufferedAcc
                   , sharedAcc
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
                   in ( IntSet.delete k pendingKeysAcc
                      , bufferedAcc
                      , sharedAcc'
                      , downloadedAcc
                      , True
                      )
               | otherwise ->
                   case IntMap.lookup k (sharedTxTable sharedAcc) of
                        Just _txEntry
                          -- The body answers this batch.  One for a key this
                          -- peer requested in a later batch, the txid interned
                          -- afresh since, is late here and comes again with
                          -- that batch's reply.
                          | IntSet.member k (pifAttempting peerInFlight)
                          , IntSet.member k pendingKeysAcc ->
                              ( IntSet.delete k pendingKeysAcc
                              , IntSet.insert k bufferedAcc
                              , sharedAcc
                              , IntMap.insert k tx downloadedAcc
                              , wakeAcc
                              )
                        _ ->
                              ( IntSet.delete k pendingKeysAcc
                              , bufferedAcc
                              , sharedAcc
                              , downloadedAcc
                              , wakeAcc
                              )

    -- Handle an omitted (not received) tx: release this peer's lease so
    -- another advertiser can claim the tx, decrement 'txAttempt'.
    handleOmitted (!sharedAcc, !wakeAcc) k =
      case IntMap.lookup k (sharedTxTable sharedAcc) of
        Just txEntry ->
          ( sharedAcc {
              sharedTxTable =
                IntMap.insert k (releaseLease txEntry)
                  (sharedTxTable sharedAcc)
            }
          , True
          )
        Nothing ->
          (sharedAcc, wakeAcc)

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
-- txid advertisements can be acknowledged without re-requesting the body;
-- one whose key is gone meanwhile is retained under a fresh key, which
-- is why the accepted txids come along.
-- Txs rejected by the mempool release this peer's lease and clear
-- 'txInSubmission' so another advertiser may try later.
handleSubmittedTxs :: (Eq peeraddr, HasRawTxId txid)
                   => Time
                   -> TxDecisionPolicy
                   -> peeraddr
                   -> [(TxKey, txid)]
                   -- ^ the accepted txs, by the key they were submitted under
                   -> [TxKey]
                   -> PeerTxLocalState tx
                   -> PeerTxInFlight
                   -> SharedTxState peeraddr txid
                   -> (PeerTxLocalState tx, PeerTxInFlight, SharedTxState peeraddr txid)
handleSubmittedTxs now policy peeraddr acceptedTxs rejectedTxs peerState peerInFlight sharedState =
  (peerState', peerInFlight', sharedState')
  where
    acceptedKeys = IntSet.fromList (unTxKey . fst <$> acceptedTxs)
    rejectedKeys = IntSet.fromList (unTxKey <$> rejectedTxs)
    submittedKeys = acceptedKeys `IntSet.union` rejectedKeys

    peerState' = peerState {
        peerDownloadedTxs =
          IntMap.withoutKeys (peerDownloadedTxs peerState) submittedKeys,
        peerAvailableTxIds =
          IntMap.withoutKeys (peerAvailableTxIds peerState) submittedKeys,
        -- what the mempool accepted from this peer it had from no one
        -- else: the peer is that much of a source.  Not while the coin or
        -- its standing has it fast: a fast peer wins the races and would
        -- make a source of itself, keeping the standing once the coin
        -- drops it.  A peer producing at the slow cadence is the source the
        -- rule is after, and one fast by that standing keeps producing to
        -- keep it.
        peerNovelAt =
          if peerTier peerState == FastTier
             then peerNovelAt peerState
             else recordNovel policy now (length acceptedTxs) (peerNovelAt peerState)
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
        sharedGeneration = sharedGeneration sharedState + 1,
        sharedRevision   = sharedRevision   sharedState + 1,
        -- a rejection reopens its entry: out of submission, and this
        -- peer's lease, where it held one, released ('markRejected'), so
        -- another peer may claim it, at once or once the lease it still
        -- carries lapses.  Not a rejected key whose entry is gone:
        -- nothing of it opens to anyone.
        sharedClaimable  = sharedClaimable sharedState
                         + (if reopenedAny then 1 else 0)
      }

    -- whether a rejected key still has an entry, the one 'markRejected'
    -- reopens
    reopenedAny =
      any (`IntMap.member` sharedTxTable sharedStateAfterAccepted) (IntSet.toList rejectedKeys)

    retainedUntil = retainDeadline (addTime (bufferedTxsMinLifetime policy) now)

    acceptSubmittedTx sharedAcc (TxKey k, txid)
      | IntMap.member k (sharedKeyToTxId sharedAcc) = retainUnder k sharedAcc
      -- The key's lookup entries are gone: another peer found the tx in
      -- the mempool and retained it meanwhile, and that retention expired
      -- while this submission was blocked.  The txid is retained under
      -- the key it has now, or gets now: afresh, so that a lagging peer's
      -- advertisement of it is acknowledged rather than fetched and
      -- rejected; retained again, the retention is extended; active again
      -- under another key, by such an advertisement, that entry is
      -- retained as after a mempool hit, since the mempool holds the tx
      -- now, so that nobody fetches it and is penalised for it (a peer
      -- with its body in flight sees a late body).
      | otherwise =
          case internTxId txid sharedAcc of
            (_, TxKey k', sharedAcc') -> retainUnder k' sharedAcc'

    -- The accepted tx retained under a key: out of the table, where an
    -- active entry held it, and retained until the deadline, or the later
    -- one it already has.
    retainUnder k st =
      st { sharedTxTable     = IntMap.delete k (sharedTxTable st),
           sharedRetainedTxs = retainedInsertMax k retainedUntil (sharedRetainedTxs st) }

    -- A rejected key stays active for its other advertisers; this peer is
    -- done with it.
    updateRejected sharedAcc k =
      sharedAcc {
        sharedTxTable = IntMap.adjust markRejected k (sharedTxTable sharedAcc)
      }

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
      , wakeChange
      , revisionOnlyChange
      ) =
      List.foldl'
        step
        ( []
        , peerAvailableTxIds peerState
        , sharedState
        , peerAdvertisedKeys0
        , False
        , False
        )
        txidsAndSizes

    peerUnacknowledgedTxIds' =
      peerUnacknowledgedTxIds peerState <> StrictSeq.fromList (reverse receivedTxKeysRev)

    -- the reply answers the oldest outstanding request
    peerState'' = (snd (takeTxIdReply peerState)) {
        peerUnacknowledgedTxIds = peerUnacknowledgedTxIds',
        peerRequestedTxIds =
          assert (requestedTxIds <= peerRequestedTxIds peerState)
                 (peerRequestedTxIds peerState - requestedTxIds),
        peerAvailableTxIds = peerAvailableTxIds'
      }

    -- An empty reply leaves the advertised set unchanged (the fold doesn't
    -- fire), so skip the record rebuild.
    peerInFlight'' =
        if null txidsAndSizes
           then peerInFlight
           else peerInFlight { pifAdvertised = peerAdvertisedKeys' }

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

    retainUntil = retainDeadline (addTime (bufferedTxsMinLifetime policy) now)

    -- Process each received txid: classify as retained, in mempool, or new entry.
    step
      :: ( [TxKey]
         , IntMap.IntMap SizeInBytes
         , SharedTxState peeraddr txid
         , IntSet.IntSet
         , Bool                     -- wakeChange
         , Bool                     -- revisionOnlyChange
         )
      -> (txid, SizeInBytes)
      -> ( [TxKey]
         , IntMap.IntMap SizeInBytes
         , SharedTxState peeraddr txid
         , IntSet.IntSet
         , Bool
         , Bool
         )
    step
      ( !unacknowledgedAcc
      , !availableAcc
      , !sharedAcc
      , !peerAdvertisedKeysAcc
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
                              txDoom = Undoomed,
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
                 , wakeAcc
                 , True
                 )
            Just entry ->
              -- Existing entry: this peer advertises it too, which only
              -- its own sets record, unless the entry is under review for
              -- abandonment, which the advertisement ends: spared from now,
              -- so the next walk does not doom it again at once on its old
              -- claimable time.
              ( txKey : unacknowledgedAcc
              , IntMap.insert k txSize availableAcc
              , case txDoom entry of
                  Doomed ->
                    sharedAcc' {
                      sharedTxTable =
                        IntMap.insert k entry { txDoom = Spared now }
                                      (sharedTxTable sharedAcc')
                    }
                  _ -> sharedAcc'
              , IntSet.insert k peerAdvertisedKeysAcc
              , wakeAcc
              , revOnlyAcc' || txDoom entry == Doomed
              )
      where
        retainedAcc = sharedRetainedTxs sharedAcc'
        revOnlyAcc' = revOnlyAcc || txKeyWasNew
        (txKey@(TxKey k), txKeyWasNew, sharedAcc') = lookupOrInternTxId txid sharedAcc

    lookupOrInternTxId txid st =
      let (_, key, st') = internTxId txid st
      in (key, sharedNextTxKey st' /= sharedNextTxKey st, st')
{-# INLINABLE handleReceivedTxIds #-}
