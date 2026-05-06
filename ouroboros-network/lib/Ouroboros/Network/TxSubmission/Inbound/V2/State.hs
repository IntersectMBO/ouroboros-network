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
  , applyPeerRejections
  , sweepSharedState
  ) where

import Control.Monad.Class.MonadTime.SI (DiffTime, Time, addTime, diffTime)
import Data.Foldable (foldl', toList)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Sequence.Strict qualified as StrictSeq
import Data.Word (Word64)

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

data PeerActionChoice peeraddr =
    ChooseSubmit ![TxKey]
  | ChooseRequestTxs ![TxKey] !SizeInBytes !(IntMap.IntMap (TxEntry peeraddr))
  | ChooseRequestTxIds !TxIdsReqFlavour ![TxKey] !NumTxIdsToAck !NumTxIdsToReq !(StrictSeq.StrictSeq TxKey)
  | ChooseDoNothing !Word64 !(Maybe DiffTime)

-- | Build a precomputed context for selecting the next action for a peer.
--
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
    pacPeerInFlight = peerInFlight,
    pacSharedState = sharedState,
    pacClaimDelay = peerClaimDelay policy now (peerScore peerState')
    }
  where
    -- Remove downloaded tx bodies that are no longer in the shared state.
    peerState' =
      let downloaded = peerDownloadedTxs peerState
      in if IntMap.null downloaded
            then peerState
            else peerState {
                   peerDownloadedTxs = IntMap.intersection downloaded (sharedTxTable sharedState)
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
-- messages.  Updates 'peerPhase' on the returned 'PeerTxLocalState' to
-- reflect the chosen action and threads the per-peer 'PeerTxInFlight'
-- counters.
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
    sharedState' = bumpStuckEntries now policy peerState sharedState
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
      let flavour
            | txIdRequestMode == AllowAnyTxIdRequests
            , StrictSeq.null unacknowledgedTxIds' = TxIdsBlockingReq
            | otherwise                            = TxIdsPipelinedReq
      in ChooseRequestTxIds flavour acknowledgedTxIds txIdsToAcknowledge txIdsToRequest unacknowledgedTxIds'
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
       ChooseRequestTxIds flavour acknowledgedTxIds txIdsToAcknowledge txIdsToRequest
                          unacknowledgedTxIds' ->
         applyRequestTxIdsChoice ctx flavour acknowledgedTxIds txIdsToAcknowledge txIdsToRequest
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
  , peerState''
  , peerInFlight''
  , sharedState''
  )
  where
    requestedKeys = IntSet.fromList (unTxKey <$> txsToRequest)
    peerState'' =
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
    peerInFlight'' = pif {
        pifLeased     = pifLeased     pif `IntSet.union` requestedKeys,
        pifAttempting = pifAttempting pif `IntSet.union` requestedKeys
      }
    -- Take the lease without waking other advertisers: claiming doesn't
    -- give them a new option (they couldn't claim before this commit, and
    -- they still can't), and they'll be bumped on submit / lease release.
    sharedState'' =
      (pacSharedState ctx) {
        sharedTxTable = txTable,
        sharedGeneration = sharedGeneration (pacSharedState ctx) + 1
      }

-- | Construct a 'PeerRequestTxIds' action and update local and shared txid state.
applyRequestTxIdsChoice
  :: PeerActionContext peeraddr txid tx
  -> TxIdsReqFlavour
  -> [TxKey]
  -> NumTxIdsToAck
  -> NumTxIdsToReq
  -> StrictSeq.StrictSeq TxKey
  -> (PeerAction, PeerTxLocalState tx, PeerTxInFlight, SharedTxState peeraddr txid)
applyRequestTxIdsChoice ctx flavour acknowledgedTxIds txIdsToAcknowledge txIdsToRequest unacknowledgedTxIds' =
  ( PeerRequestTxIds flavour txIdsToAcknowledge txIdsToRequest
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
    pickBufferedTxsToSubmit = go [] (toList (peerUnacknowledgedTxIds pacPeerState))
      where
        go acc [] = reverse acc
        go acc (txKey@(TxKey k) : rest) =
          case IntMap.lookup k (sharedTxTable pacSharedState) of
            Just txEntry
              | txBufferedByPeer pacPeerState k
              , not (txSubmittingByOther pacPeerInFlight k txEntry) ->
                  go (txKey : acc) rest
            Just _  -> reverse acc
            Nothing -> go acc rest

-- | Select transactions to request from the peer, if within policy limits.
--
-- Returns a triple of:
-- Tx keys to request (in ascending key order for deterministic selection)
-- Total serialized size of the requested txs
-- Updated shared state with new lease ownership for selected txs
pickRequestTxsAction :: Ord peeraddr
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
         else go [] 0 (sharedTxTable pacSharedState) candidates
      where
        -- Remaining bytes available for requesting new tx, based on the
        -- per-peer inflight size limit.
        sizeBudget =
          if peerRequestedTxsSize pacPeerState >= txsSizeInflightPerPeer pacPolicy
             then 0
             else txsSizeInflightPerPeer pacPolicy - peerRequestedTxsSize pacPeerState

        leaseUntil = addTime (interTxSpace pacPolicy) pacNow

        -- Iterate the peer's unacknowledged queue, which preserves the peer's
        -- advertisement order. Peers are expected to advertise in chain-
        -- topological order (parents before children), so walking in that
        -- order aligns fetch order with submission-validity order and a
        -- child is never requested ahead of its parent when the same peer
        -- carries both.
        candidates =
          [ (k, txSize)
          | TxKey k <- toList (peerUnacknowledgedTxIds pacPeerState)
          , IntSet.notMember k (peerRequestedTxs pacPeerState)
          , IntMap.notMember k (peerDownloadedTxs pacPeerState)
          , Just txSize <- [IntMap.lookup k (peerAvailableTxIds pacPeerState)]
          ]

        -- Select transactions to request by iterating through candidates in
        -- peer advertisement order until the size budget is consumed.
        go selectedRev selectedSize txTable [] = (reverse selectedRev, selectedSize, txTable)
        go selectedRev selectedSize txTable ((k, txSize) : rest) =
          if exceedsBudget selectedSize txSize
             then (reverse selectedRev, selectedSize, txTable)
             else
               case IntMap.lookup k txTable of
                   Just txEntry ->
                     if txSelectable ctx (TxKey k) txEntry
                        then
                          go (TxKey k : selectedRev)
                             (selectedSize + txSize)
                             (IntMap.insert k (claimTx (pacPeerAddr ctx) leaseUntil txEntry)
                                            txTable)
                             rest
                        else go selectedRev selectedSize txTable rest
                   Nothing -> go selectedRev selectedSize txTable rest

        exceedsBudget selectedSize txSize
          | selectedSize + txSize <= sizeBudget = False
          | selectedSize /= 0 = True
          -- The inflight size limit is soft by up to one tx size, so a peer with
          -- spare capacity may still request its first tx in a batch even when
          -- that single tx exceeds the remaining byte budget.
          | otherwise = peerRequestedTxsSize pacPeerState >= txsSizeInflightPerPeer pacPolicy

-- | Determine txid acknowledgment and request counts, if any work is available.
pickRequestTxIdsAction :: Ord peeraddr
                       => TxIdRequestMode
                       -> PeerActionContext peeraddr txid tx
                       -> Maybe ([TxKey], NumTxIdsToAck, NumTxIdsToReq, StrictSeq.StrictSeq TxKey)
pickRequestTxIdsAction txIdRequestMode ctx@PeerActionContext { pacPolicy, pacPeerState }
  | txIdsToAcknowledge <= 0 && txIdsToRequest <= 0 = Nothing
  | txIdRequestMode == AllowPipelinedTxIdRequests
  , txIdsToAcknowledge <= 0 || txIdsToRequest <= 0 = Nothing
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

        txIdsToRequest'
          | numOfAcked == 0 && numOfUnacked > 0 = 0
          | otherwise =
              fromIntegral $ max 0 $ min
                (fromIntegral (maxUnacknowledgedTxIds pacPolicy) - unackedAndRequested + numOfAcked)
                (fromIntegral (maxNumTxIdsToRequest pacPolicy) - numOfRequested)

-- | Compute the time delay until the peer should next wake to check for work.
nextWakeDelay :: PeerActionContext peeraddr txid tx -> Maybe DiffTime
nextWakeDelay PeerActionContext { pacNow, pacPolicy, pacClaimDelay
                                , pacPeerState, pacPeerInFlight, pacSharedState } =
    (`diffTime` pacNow) <$> minMaybe (minMaybe nextClaimWake nextBumpWake) nextRetainWake
  where
    -- Wake at the earliest claim-ready time among txs this peer advertises.
    nextClaimWake =
      IntSet.foldl' stepClaim Nothing (pifAdvertised pacPeerInFlight)

    -- Wake at the earliest bump-ready time among txs this peer holds buffered.
    -- Scoped to 'peerDownloadedTxs' to match 'bumpStuckEntries': only the
    -- leaseholder schedules a bump-wake, so non-leaseholders don't busy-loop
    -- at exact bumpAt with no entry to bump.
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

    nextRetainWake = retainedNextWake pacNow (sharedRetainedTxs pacSharedState)

    futureClaimWake txEntry =
      let claimWake = txClaimReadyAt pacClaimDelay txEntry
      in if claimWake > pacNow then Just claimWake else Nothing

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
stuckBumpReadyAt :: TxDecisionPolicy -> Time -> Time
stuckBumpReadyAt policy =
    addTime (inflightTimeout policy - interTxSpace policy)

-- | Bump 'currentMaxInflightMultiplicity' by one when the leaseholder has
-- held the lease past 'inflightTimeout' without anyone reaching submission,
-- and the cap is the bottleneck preventing another peer from joining.
bumpCurrentMaxIfStuck :: Time -> TxDecisionPolicy -> TxEntry peeraddr -> TxEntry peeraddr
bumpCurrentMaxIfStuck now policy
                      entry@TxEntry { txLease = TxLeased _ leaseUntil
                                    , currentMaxInflightMultiplicity = cap }
  | txAttempt entry >= cap
  , now >= stuckBumpReadyAt policy leaseUntil
  , not (txInSubmission entry)
  = entry { currentMaxInflightMultiplicity = cap + 1 }
bumpCurrentMaxIfStuck _ _ entry = entry

-- | Future wake time at which an entry would become eligible for a cap bump.
--
-- Returns 'Just' for any bump-eligible entry whose 'bumpAt' is at or after
-- 'now'. Using '>=' (rather than '>') means a peer that runs 'nextWakeDelay'
-- without 'bumpStuckEntries' having already fired still schedules a wake.
nextStuckBumpWake :: Time -> TxDecisionPolicy -> TxEntry peeraddr -> Maybe Time
nextStuckBumpWake now policy
                  entry@TxEntry { txLease = TxLeased _ leaseUntil
                                , currentMaxInflightMultiplicity = cap }
  | txAttempt entry >= cap
  , not (txInSubmission entry)
  , let bumpAt = stuckBumpReadyAt policy leaseUntil
  , bumpAt >= now
  = Just bumpAt
nextStuckBumpWake _ _ _ = Nothing

-- | Sweep the txs this peer has buffered locally and bump any whose lease
-- has been held past 'inflightTimeout'. The leaseholder is in the best
-- position to detect that it is holding others up: its 'peerDownloadedTxs'
-- is small (usually empty) so the sweep is cheap, and any tx it has buffered
-- is one it has at some point claimed itself.
--
-- 'sharedGeneration' is bumped when entries change so other peers wake out
-- of 'awaitSharedChange' and re-evaluate eligibility under the new cap.
bumpStuckEntries :: Time
                 -> TxDecisionPolicy
                 -> PeerTxLocalState tx
                 -> SharedTxState peeraddr txid
                 -> SharedTxState peeraddr txid
bumpStuckEntries now policy peerState st =
    if IntSet.null bumpedKeys
       then st
       else st { sharedTxTable    = txTable',
                 sharedGeneration = sharedGeneration st + 1 }
  where
    (bumpedKeys, txTable') =
      IntMap.foldlWithKey' bumpOne (IntSet.empty, sharedTxTable st)
                                   (peerDownloadedTxs peerState)
    bumpOne (bumpedAcc, tbl) k _tx =
      case IntMap.lookup k tbl of
        Nothing -> (bumpedAcc, tbl)
        Just entry ->
          let entry' = bumpCurrentMaxIfStuck now policy entry in
          if currentMaxInflightMultiplicity entry'
               /= currentMaxInflightMultiplicity entry
             then (IntSet.insert k bumpedAcc, IntMap.insert k entry' tbl)
             else (bumpedAcc, tbl)

-- | Determine if a tx is eligible for this peer to request.
--
-- A tx is selectable if it can be claimed or is already owned by this peer
-- and this peer's score-derived claim delay has elapsed.
--
-- Callers iterate candidates from the peer's own 'peerAvailableTxIds', so
-- the "this peer advertises @txKey@" precondition is established by the
-- caller.
txSelectable :: Eq peeraddr
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
    -- txOwnedByPeer :: TxEntry peeraddr -> Bool
    txOwnedByPeer TxEntry { txLease = TxLeased owner _ } = owner == pacPeerAddr
    txOwnedByPeer TxEntry { txLease = TxClaimable _ }    = False

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

peerClaimDelay :: TxDecisionPolicy
               -> Time
               -> PeerScore
               -> DiffTime
peerClaimDelay policy currentTime peerScore
    | peerScoreValue peerScore == 0 = 0
    | otherwise =
        -- Delay contribution in milliseconds is peerScore / 20, then converted to seconds.
        realToFrac . (/ 20000) $ currentPeerScore policy currentTime peerScore

-- | Decay the peer's score to @now@, updating the timestamp.
drainPeerScore :: TxDecisionPolicy
               -> Time
               -> PeerTxLocalState tx
               -> PeerTxLocalState tx
drainPeerScore policy now peerState@PeerTxLocalState { peerScore }
  | peerScoreValue peerScore == 0 =
      peerState { peerScore = peerScore { peerScoreTs = now } }
  | otherwise =
      let drained = currentPeerScore policy now peerScore in
      peerState { peerScore = PeerScore { peerScoreValue = drained, peerScoreTs = now } }
{-# INLINE drainPeerScore #-}

-- | Apply a rejection penalty to the peer's local score.
-- Returns the new score value (for tracing) and the updated local state.
applyPeerRejections :: TxDecisionPolicy
                    -> Time
                    -> Int
                    -> PeerTxLocalState tx
                    -> (Double, PeerTxLocalState tx)
applyPeerRejections TxDecisionPolicy { scoreRate, scoreMax } now rejectedCount
                    peerState@PeerTxLocalState { peerScore } =
  (peerScoreValue peerScore', peerState { peerScore = peerScore' })
  where
    n = fromIntegral rejectedCount :: Double
    peerScore' = applyRejects n peerScore
    applyRejects 0 ps@PeerScore { peerScoreValue = 0 } = ps { peerScoreTs = now }
    applyRejects n' ps@PeerScore { peerScoreValue, peerScoreTs } =
        let duration = diffTime now peerScoreTs
            !drain   = realToFrac duration * scoreRate
            !drained = max 0 (peerScoreValue - drain) in
        ps { peerScoreValue = min scoreMax (drained + n'), peerScoreTs = now }
{-# INLINE applyPeerRejections #-}

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
txIdAckable :: Eq peeraddr
            => PeerActionContext peeraddr txid tx
            -> TxKey
            -> Bool
txIdAckable PeerActionContext { pacPeerAddr, pacPeerState, pacPeerInFlight, pacSharedState }
            (TxKey k)
  | retainedMember k (sharedRetainedTxs pacSharedState) = True
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
      case IntMap.lookup k (sharedTxTable pacSharedState) of
           Just txEntry ->
             let peerHasStake =
                      IntSet.member k (pifAttempting pacPeerInFlight)
                   || IntSet.member k (pifSubmitting pacPeerInFlight)
                 ackWhenBuffered =
                   case txLease txEntry of
                     TxLeased owner _ -> owner == pacPeerAddr || peerHasStake
                     TxClaimable _    -> peerHasStake
             in
             -- Ack the txid if we downloaded it and no other
             -- peer is in the process of submitting it to the
             -- mempool.
             ackWhenBuffered
               && IntMap.member k (peerDownloadedTxs pacPeerState)
               && not (txBufferedByPeer pacPeerState k
                        && txSubmittingByOther pacPeerInFlight k txEntry)
           Nothing -> True -- Safe late ack after the resolved tx was pruned from shared state.

-- | Remove transaction entries from all shared state maps by key.
dropTxKeys :: HasRawTxId txid
           => IntSet.IntSet
           -> SharedTxState peeraddr txid
           -> SharedTxState peeraddr txid
dropTxKeys keys st@SharedTxState { sharedTxTable, sharedRetainedTxs, sharedTxIdToKey
                                 , sharedKeyToTxId }
  | IntSet.null keys = st
  | otherwise =
      st {
        sharedTxTable = IntMap.withoutKeys sharedTxTable keys,
        sharedRetainedTxs = retainedDeleteKeys keys sharedRetainedTxs,
        sharedTxIdToKey = IntSet.foldl' deleteTxId sharedTxIdToKey keys,
        sharedKeyToTxId = IntMap.withoutKeys sharedKeyToTxId keys
      }
  where
    deleteTxId txIdToKey k =
      case IntMap.lookup k sharedKeyToTxId of
           Just txid -> Map.delete (getRawTxId txid) txIdToKey
           Nothing   -> txIdToKey

-- | Remove only the txid <-> key lookup entries for the given keys.
-- Used by the sweep for keys that have outlived their 'sharedTxTable'
-- and retained residence but had their lookup entries kept while peers
-- still carried the txid in 'peerUnacknowledgedTxIds'.
dropLookupOnly :: HasRawTxId txid
               => IntSet.IntSet
               -> SharedTxState peeraddr txid
               -> SharedTxState peeraddr txid
dropLookupOnly keys st@SharedTxState { sharedTxIdToKey, sharedKeyToTxId }
  | IntSet.null keys = st
  | otherwise =
      st {
        sharedTxIdToKey = IntSet.foldl' deleteTxId sharedTxIdToKey keys,
        sharedKeyToTxId = IntMap.withoutKeys sharedKeyToTxId keys
      }
  where
    deleteTxId txIdToKey k =
      case IntMap.lookup k sharedKeyToTxId of
           Just txid -> Map.delete (getRawTxId txid) txIdToKey
           Nothing   -> txIdToKey

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
-- 'pifAdvertised' and 'pifAcksPending'.  Caller (the sweep thread)
-- snapshots all per-peer TVars in the same STM transaction that runs
-- this function so the snapshot is coherent with the sharedTxTable
-- read.
--
-- Bumps 'sharedGeneration' if anything changed so sleeping peer workers wake
-- and re-evaluate.
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
        ) { sharedGeneration = sharedGeneration st + 1 }
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
                      `IntSet.difference` orphans

    isOrphan _ TxEntry { txLease = TxLeased {} } = False
    isOrphan k TxEntry { txAttempt, txInSubmission }
      | txAttempt > 0                     = False
      | txInSubmission                    = False
      | IntSet.member k liveReferences    = False
      | otherwise                         = True
{-# INLINABLE sweepSharedState #-}

-- | Handle a batch of tx bodies received from one peer.
--
-- Received bodies are buffered locally in the peer state. Bodies that are
-- already retained or already in the mempool are counted as late and dropped.
-- Any requested tx omitted from the reply releases this peer's ownership.
-- Late TXs contributes to the returned penalty count.
handleReceivedTxs :: (Eq peeraddr, HasRawTxId txid)
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
             error "TxSubmission.Inbound.V2.handleReceivedTxs null requestedBatches"
           batch StrictSeq.:<| batches ->
             (batch, batches)

    -- Process each received tx, collecting late counts, pending
    -- requests, the keys still buffered by this peer, and updated
    -- shared state.  @bufferedKeys@ is the subset of @requestedKeys@
    -- that ended up buffered locally; the rest are released below.
    ( lateCount
      , pendingRequestedKeys
      , bufferedKeys
      , sharedStateHandled
      , peerDownloadedTxs'
      ) =
      foldl'
        handleOne
        ( 0
        , requestedKeys
        , IntSet.empty
        , sharedState
        , peerDownloadedTxs peerState
        )
        txs

    -- Process omitted (not received) txs: count a penalty for every omitted
    -- request and release this peer's lease where it still held one.
    (omittedCount, sharedStateReleased) =
      IntSet.foldl' handleOmitted (0, sharedStateHandled) pendingRequestedKeys

    -- Keys this peer is no longer attempting (everything in the batch
    -- except still-buffered keys).  Used to update both the per-peer
    -- in-flight set and the shared 'txAttempt' counters via 'releaseLease'.
    releasedKeys = requestedKeys `IntSet.difference` bufferedKeys

    sharedState' =
      sharedStateReleased {
        sharedGeneration = sharedGeneration sharedState + 1
      }

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
      ( lateCountAcc
      , pendingKeysAcc
      , bufferedAcc
      , sharedAcc
      , downloadedAcc
      )
      (txid, tx) =
        case Map.lookup (getRawTxId txid) txidToKey of
             Nothing ->
               ( lateCountAcc + 1
               , pendingKeysAcc
               , bufferedAcc
               , sharedAcc
               , downloadedAcc
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
                              )
                        _ ->
                              ( lateCountAcc + 1
                              , IntSet.delete k pendingKeysAcc
                              , bufferedAcc
                              , sharedAcc
                              , downloadedAcc
                              )

    -- Handle omitted (not received) txs: release this peer's lease so
    -- another advertiser can claim the tx, decrement 'txAttempt'.
    handleOmitted (omittedCountAcc, sharedAcc) k
      | keyWasLive k =
          let sharedAcc' =
                case IntMap.lookup k (sharedTxTable sharedAcc) of
                  Just txEntry ->
                    sharedAcc {
                      sharedTxTable =
                        IntMap.insert k (releaseLease txEntry)
                          (sharedTxTable sharedAcc)
                    }
                  Nothing ->
                    sharedAcc
          in (omittedCountAcc + 1, sharedAcc')
      | otherwise =
          (omittedCountAcc + 1, sharedAcc)

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
          IntMap.withoutKeys (peerAvailableTxIds peerState) submittedKeys
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
      foldl' acceptSubmittedTx sharedState acceptedTxs

    sharedStateAfterRejected =
      IntSet.foldl' updateRejected sharedStateAfterAccepted rejectedKeys

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
markSubmittingTxs :: [TxKey]
                  -> SharedTxState peeraddr txid
                  -> SharedTxState peeraddr txid
markSubmittingTxs [] st = st
markSubmittingTxs txKeys st =
  st {
    sharedTxTable    = foldl' markOne (sharedTxTable st) txKeys,
    sharedGeneration = sharedGeneration st + 1
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
    ( receivedTxKeysRev
      , peerAvailableTxIds'
      , sharedStateHandled
      , peerAdvertisedKeys'
      , peerAcksPending'
      , sharedChanged
      ) =
      foldl'
        step
        ( []
        , peerAvailableTxIds peerState
        , sharedState
        , peerAdvertisedKeys0
        , peerAcksPending0
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
        peerAvailableTxIds = peerAvailableTxIds'
      }

    peerInFlight'' = peerInFlight {
        pifAdvertised  = peerAdvertisedKeys',
        pifAcksPending = peerAcksPending'
      }

    sharedState''
      | sharedChanged =
          sharedStateHandled {
            sharedGeneration = sharedGeneration sharedState + 1
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
         , Bool
         )
      -> (txid, SizeInBytes)
      -> ( [TxKey]
         , IntMap.IntMap SizeInBytes
         , SharedTxState peeraddr txid
         , IntSet.IntSet
         , IntSet.IntSet
         , Bool
         )
    step
      ( !unacknowledgedAcc
      , !availableAcc
      , !sharedAcc
      , !peerAdvertisedKeysAcc
      , !peerAcksPendingAcc
      , !sharedChangedAcc
      )
      (txid, txSize)
      | retainedMember k retainedAcc =
          ( txKey : unacknowledgedAcc
          , IntMap.delete k availableAcc
          , sharedAcc'
          , IntSet.delete k peerAdvertisedKeysAcc
          , IntSet.insert k peerAcksPendingAcc
          , sharedChangedAcc'
          )
      | mempoolHasTx txid =
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
          )
      | otherwise =
          case IntMap.lookup k (sharedTxTable sharedAcc') of
            Nothing ->
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
                 , True
                 )
            Just _ ->
              ( txKey : unacknowledgedAcc
              , IntMap.insert k txSize availableAcc
              , sharedAcc'
              , IntSet.insert k peerAdvertisedKeysAcc
              , IntSet.insert k peerAcksPendingAcc
              , sharedChangedAcc'
              )
      where
        retainedAcc = sharedRetainedTxs sharedAcc'
        sharedChangedAcc' = sharedChangedAcc || txKeyWasNew
        (txKey@(TxKey k), txKeyWasNew, sharedAcc') = lookupOrInternTxId txid sharedAcc

    lookupOrInternTxId txid st@SharedTxState { sharedTxIdToKey, sharedKeyToTxId, sharedNextTxKey }
      | Just key <- Map.lookup rawId sharedTxIdToKey = (key, False, st)
      | otherwise =
          let key = TxKey sharedNextTxKey
          in ( key
             , True
             , st {
                 sharedTxIdToKey = Map.insert rawId key sharedTxIdToKey,
                 sharedKeyToTxId = IntMap.insert sharedNextTxKey txid sharedKeyToTxId,
                 sharedNextTxKey = sharedNextTxKey + 1
               }
             )
      where rawId = getRawTxId txid
{-# INLINABLE handleReceivedTxIds #-}
