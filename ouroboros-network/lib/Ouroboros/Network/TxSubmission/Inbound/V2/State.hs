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
  , advertisingPeersForTxKeysExcept
  , advertisingPeersForTxExcept
  , removeAdvertisingPeersForResolvedTx
  , drainPeerScore
  , applyPeerRejections
  ) where

import Control.Monad.Class.MonadTime.SI (DiffTime, Time, addTime, diffTime)
import Data.Foldable (foldl', toList)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set qualified as Set
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
    pacNow             :: !Time,
    -- | Decision policy that governs request, retry, and scoring limits.
    pacPolicy          :: !TxDecisionPolicy,
    -- | Address of the peer whose next action is being chosen.
    pacPeerAddr        :: !peeraddr,
    -- | Current peer-local state after local pruning has been applied.
    pacPeerState       :: !(PeerTxLocalState tx),
    -- | Shared tx-submission state after shared pruning has been applied.
    pacSharedState     :: !(SharedTxState peeraddr txid),
    -- | This peer's shared state after pruning.
    pacSharedPeerState :: !SharedPeerState,
    -- | Score-derived delay this peer must wait after a tx becomes claimable.
    pacClaimDelay      :: !DiffTime
  }

data PeerActionChoice peeraddr =
    ChooseSubmit ![TxKey]
  | ChooseRequestTxs ![TxKey] !SizeInBytes !(IntMap.IntMap (TxEntry peeraddr))
  | ChooseRequestTxIds !TxIdsReqFlavour ![TxKey] !NumTxIdsToAck !NumTxIdsToReq !(StrictSeq.StrictSeq TxKey)
  | ChooseDoNothing !Word64 !(Maybe DiffTime)

-- | Build a precomputed context for selecting the next action for a peer.
--
--
mkPeerActionContext :: (Ord peeraddr, HasRawTxId txid)
                    => Time
                    -> TxDecisionPolicy
                    -> peeraddr
                    -> PeerTxLocalState tx
                    -> SharedTxState peeraddr txid
                    -> PeerActionContext peeraddr txid tx
mkPeerActionContext now policy peeraddr peerState sharedState =
  PeerActionContext {
    pacNow = now,
    pacPolicy = policy,
    pacPeerAddr = peeraddr,
    pacPeerState = peerState',
    pacSharedState = sharedState',
    pacSharedPeerState = sharedPeerState',
    pacClaimDelay = peerClaimDelay policy now (peerScore peerState')
    }
  where
    -- Remove expired retained TX keys from all shared state tables.
    -- When the retain timer expires, the peer gives up waiting for this txid
    -- and will acknowledge it. We remove from all tables so the tx can be
    -- re-advertised if needed.
    sharedState' =
      let expiredRetainedKeys = retainedExpiredKeys now (sharedRetainedTxs sharedState)
          prunedSharedState = dropTxKeys expiredRetainedKeys sharedState in
      if IntSet.null expiredRetainedKeys
         then sharedState
         else prunedSharedState {
                sharedGeneration = sharedGeneration sharedState + 1
              }

    -- Remove downloaded tx bodies that are no longer in the shared state.
    peerState' =
      let downloaded = peerDownloadedTxs peerState
      in if IntMap.null downloaded
            then peerState
            else peerState {
                   peerDownloadedTxs = IntMap.intersection downloaded (sharedTxTable sharedState')
                 }

    sharedPeerState' =
      case Map.lookup peeraddr (sharedPeers sharedState') of
        Just sharedPeerState -> sharedPeerState
        Nothing ->
          error "TxSubmission.V2.mkPeerActionContext: missing peer"

-- | Compute the next peer-local action.
nextPeerAction :: (Ord peeraddr, HasRawTxId txid)
               => Time
               -> TxDecisionPolicy
               -> peeraddr
               -> PeerTxLocalState tx
               -> SharedTxState peeraddr txid
               -> (PeerAction, PeerTxLocalState tx, SharedTxState peeraddr txid)
nextPeerAction = nextPeerActionWithMode AllowAnyTxIdRequests

-- | Pipelined version of nextPeerAction
nextPeerActionPipelined :: (Ord peeraddr, HasRawTxId txid)
                        => Time
                        -> TxDecisionPolicy
                        -> peeraddr
                        -> PeerTxLocalState tx
                        -> SharedTxState peeraddr txid
                        -> (PeerAction, PeerTxLocalState tx, SharedTxState peeraddr txid)
nextPeerActionPipelined = nextPeerActionWithMode AllowPipelinedTxIdRequests

-- | V2 peer-thread scheduler
--
-- nextPeerActionWithMode handles body requests for txs this peer may currently
-- fetch, tx submission for bodies buffered locally by this peer, and txid ack/request
-- messages.
nextPeerActionWithMode :: (Ord peeraddr, HasRawTxId txid)
                       => TxIdRequestMode
                       -> Time
                       -> TxDecisionPolicy
                       -> peeraddr
                       -> PeerTxLocalState tx
                       -> SharedTxState peeraddr txid
                       -> (PeerAction, PeerTxLocalState tx, SharedTxState peeraddr txid)
nextPeerActionWithMode txIdRequestMode now policy peeraddr peerState sharedState =
  applyPeerActionChoice ctx (pickPeerActionChoice txIdRequestMode ctx)
  where
    ctx = mkPeerActionContext now policy peeraddr peerState sharedState

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
      ChooseDoNothing (peerGenerationOf (pacPeerAddr ctx) (pacSharedState ctx)) (nextWakeDelay ctx)

-- | Execute a chosen peer action and compute resulting state updates
applyPeerActionChoice :: (Ord peeraddr, HasRawTxId txid)
                      => PeerActionContext peeraddr txid tx
                      -> PeerActionChoice peeraddr
                      -> (PeerAction, PeerTxLocalState tx, SharedTxState peeraddr txid)
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
applySubmitChoice :: PeerActionContext peeraddr txid tx
                  -> [TxKey]
                  -> (PeerAction, PeerTxLocalState tx, SharedTxState peeraddr txid)
applySubmitChoice ctx txsToSubmit =
  ( PeerSubmitTxs txsToSubmit
  , pacPeerState ctx
  , pacSharedState ctx
  )

-- | Construct a 'PeerRequestTxs' action and update local and shared tx state.
applyRequestTxsChoice :: Ord peeraddr
                      => PeerActionContext peeraddr txid tx
                      -> [TxKey]
                      -> SizeInBytes
                      -> IntMap.IntMap (TxEntry peeraddr)
                      -> (PeerAction, PeerTxLocalState tx, SharedTxState peeraddr txid)
applyRequestTxsChoice ctx txsToRequest txsToRequestSize txTable =
  ( PeerRequestTxs txsToRequest
  , peerState''
  , sharedState''
  )
  where
    requestedKeys = IntSet.fromList (unTxKey <$> txsToRequest)

    peerState'' =
      (pacPeerState ctx) {
        peerRequestedTxs =
          foldl' (flip IntSet.insert) (peerRequestedTxs (pacPeerState ctx)) (unTxKey <$> txsToRequest),
        peerRequestedTxBatches =
          peerRequestedTxBatches (pacPeerState ctx) StrictSeq.|> RequestedTxBatch {
            requestedTxBatchSet = IntSet.fromList (unTxKey <$> txsToRequest),
            requestedTxBatchSize = txsToRequestSize
          },
        peerRequestedTxsSize = peerRequestedTxsSize (pacPeerState ctx) + txsToRequestSize
      }
    sharedState'' =
      bumpIdlePeerGenerations
        (advertisingPeersForTxKeysExcept (pacPeerAddr ctx) requestedKeys (pacSharedState ctx))
        ((pacSharedState ctx) {
          sharedTxTable = txTable,
          sharedGeneration = sharedGeneration (pacSharedState ctx) + 1
        })

-- | Construct a 'PeerRequestTxIds' action and update local and shared txid state.
applyRequestTxIdsChoice
  :: (Ord peeraddr, HasRawTxId txid)
  => PeerActionContext peeraddr txid tx
  -> TxIdsReqFlavour
  -> [TxKey]
  -> NumTxIdsToAck
  -> NumTxIdsToReq
  -> StrictSeq.StrictSeq TxKey
  -> (PeerAction, PeerTxLocalState tx, SharedTxState peeraddr txid)
applyRequestTxIdsChoice ctx flavour acknowledgedTxIds txIdsToAcknowledge txIdsToRequest unacknowledgedTxIds' =
  ( PeerRequestTxIds flavour txIdsToAcknowledge txIdsToRequest
  , peerState''
  , sharedState''
  )
  where
    peerState'' =
      (pacPeerState ctx) {
        peerAvailableTxIds =
          IntMap.withoutKeys (peerAvailableTxIds (pacPeerState ctx)) (IntSet.fromList $ unTxKey <$> acknowledgedTxIds),
        peerUnacknowledgedTxIds = unacknowledgedTxIds',
        peerRequestedTxIds = peerRequestedTxIds (pacPeerState ctx) + txIdsToRequest
      }
    sharedState'' =
      acknowledgeTxIds (pacPeerAddr ctx) acknowledgedTxIds (pacSharedState ctx)

-- | Construct a 'PeerDoNothing' action.
applyDoNothingChoice
  :: PeerActionContext peeraddr txid tx
  -> Word64
  -> Maybe DiffTime
  -> (PeerAction, PeerTxLocalState tx, SharedTxState peeraddr txid)
applyDoNothingChoice ctx generation wakeDelay =
  ( PeerDoNothing generation wakeDelay
  , pacPeerState ctx
  , pacSharedState ctx
  )

-- | Select downloaded transactions that this peer may submit to the mempool.
pickSubmitAction
  :: Ord peeraddr
  => PeerActionContext peeraddr txid tx
  -> Maybe [TxKey]
pickSubmitAction PeerActionContext { pacPeerAddr, pacPeerState, pacSharedState } =
  let txsToSubmit = pickBufferedTxsToSubmit in
  if null txsToSubmit
     then Nothing
     else Just txsToSubmit
  where

    -- Filters the unacknowledged txid queue for bodies buffered by this peer
    -- that are not currently being submitted by another advertiser.
    -- Returns the list of tx keys ready for immediate submission in the order they
    -- were originally advertised by the peer.
    pickBufferedTxsToSubmit =
      [ txKey
      | txKey@(TxKey k) <- toList (peerUnacknowledgedTxIds pacPeerState)
      , IntMap.member k (peerDownloadedTxs pacPeerState)
      , Just txEntry <- [IntMap.lookup k (sharedTxTable pacSharedState)]
      , txBufferedByPeer pacPeerAddr txEntry
      , not (txSubmittingByOther pacPeerAddr txEntry)
      ]

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

        -- We pick which TXs to download based on TxKey in ascending order.
        -- This makes it likely (but not guaranteed) that we end up downloading
        -- TXs in the order the peer presented them to us.
        candidates =
          [ (k, txSize)
          | (k, txSize) <- IntMap.toAscList (peerAvailableTxIds pacPeerState)
          , IntSet.notMember k (peerRequestedTxs pacPeerState)
          , IntMap.notMember k (peerDownloadedTxs pacPeerState)
          ]

        -- Select transactions to request by iterating through candidates in ascending
        -- key order until the size budget is consumed.
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
nextWakeDelay PeerActionContext { pacNow, pacClaimDelay, pacSharedPeerState, pacSharedState } =
    (`diffTime` pacNow) <$> minMaybe nextLeaseWake nextRetainWake
  where
    nextLeaseWake =
      IntSet.foldl' stepLease Nothing (sharedPeerAdvertisedTxKeys pacSharedPeerState)

    stepLease acc k =
      case IntMap.lookup k (sharedTxTable pacSharedState) of
        Just txEntry ->
          minMaybe acc (futureClaimWake txEntry)
        Nothing ->
          acc

    nextRetainWake = retainedNextWake pacNow (sharedRetainedTxs pacSharedState)

    futureClaimWake txEntry =
      let claimWake = txClaimReadyAt pacClaimDelay txEntry
      in if claimWake > pacNow then Just claimWake else Nothing

    minMaybe :: Ord a => Maybe a -> Maybe a -> Maybe a
    minMaybe Nothing y         = y
    minMaybe x Nothing         = x
    minMaybe (Just x) (Just y) = Just (min x y)

-- | Assign a tx lease to a peer and mark it as downloading.
claimTx :: Ord peeraddr
        => peeraddr
        -> Time
        -> TxEntry peeraddr
        -> TxEntry peeraddr
claimTx peeraddr leaseUntil txEntry@TxEntry { txAttempts } =
  txEntry {
    txLease = TxLeased peeraddr leaseUntil,
    txAttempts = Map.insert peeraddr TxDownloading txAttempts
  }

-- | Determine if a tx is eligible for this peer to request.
--
-- A tx is selectable if it can be claimed or is already owned by this peer
-- and this peer's score-derived claim delay has elapsed.
txSelectable :: Ord peeraddr
             => PeerActionContext peeraddr txid tx
             -> TxKey
             -> TxEntry peeraddr
             -> Bool
txSelectable PeerActionContext { pacNow, pacPeerAddr, pacPolicy, pacSharedPeerState
                               , pacClaimDelay }
             txKey
             txEntry
  | txSubmittingAnywhere txEntry = False
  | txPeerHasAttempt = False
  | txActiveAttemptCount txEntry >= txInflightMultiplicity pacPolicy = False
  | not peerAdvertisesTx = False
  | txOwnedByPeer txEntry = True
  | otherwise = txClaimReadyAt pacClaimDelay txEntry <= pacNow
  where
    peerAdvertisesTx =
      IntSet.member (unTxKey txKey) (sharedPeerAdvertisedTxKeys pacSharedPeerState)

    -- txOwnedByPeer :: TxEntry peeraddr -> Bool
    txOwnedByPeer TxEntry { txLease = TxLeased owner _ } = owner == pacPeerAddr
    txOwnedByPeer TxEntry { txLease = TxClaimable _ }    = False

    txPeerHasAttempt = Map.member pacPeerAddr (txAttempts txEntry)

    -- Safe to use Map.size here: by the time this guard is reached,
    -- txSubmittingAnywhere has already returned False, so the map contains
    -- only TxDownloading and TxBuffered entries.
    txActiveAttemptCount :: TxEntry peeraddr -> Int
    txActiveAttemptCount TxEntry { txAttempts } = Map.size txAttempts


-- | Extract the peer's TxAttemptState for the TX entry, if it exists.
txAttemptOfPeer :: Ord peeraddr => peeraddr -> TxEntry peeraddr -> Maybe TxAttemptState
txAttemptOfPeer peeraddr TxEntry { txAttempts } = Map.lookup peeraddr txAttempts

-- | Does the peer have the TX entry buffered?
txBufferedByPeer :: Ord peeraddr => peeraddr -> TxEntry peeraddr -> Bool
txBufferedByPeer peeraddr txEntry =
  txAttemptOfPeer peeraddr txEntry == Just TxBuffered

-- | Check whether some other peer is already submitting this tx.
--
-- Uses a single fold over 'txAttempts' and short-circuits at the first
-- matching 'TxSubmitting'.
txSubmittingByOther :: Eq peeraddr => peeraddr -> TxEntry peeraddr -> Bool
txSubmittingByOther peeraddr TxEntry { txAttempts } =
  Map.foldrWithKey
    (\owner attempt acc -> (owner /= peeraddr && attempt == TxSubmitting) || acc)
    False
    txAttempts

-- | Check whether any peer is currently submitting this tx.
--
-- Like 'txSubmittingByOther', this short-circuits at the first 'TxSubmitting'.
txSubmittingAnywhere :: TxEntry peeraddr -> Bool
txSubmittingAnywhere TxEntry { txAttempts } =
  Map.foldr (\attempt acc -> attempt == TxSubmitting || acc) False txAttempts


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

updatePeerAdvertisedTxKeys
  :: Ord peeraddr
  => peeraddr
  -> (IntSet.IntSet -> (a, IntSet.IntSet))
  -> SharedTxState peeraddr txid
  -> (a, SharedTxState peeraddr txid)
updatePeerAdvertisedTxKeys peeraddr updateKeys st@SharedTxState { sharedPeers } =
    case Map.lookup peeraddr sharedPeers of
      Just sharedPeerState ->
        let oldKeys = sharedPeerAdvertisedTxKeys sharedPeerState
            (result, newKeys) = updateKeys oldKeys
        in if newKeys == oldKeys
              then (result, st)
              else
                ( result
                , st {
                    sharedPeers =
                      Map.insert
                        peeraddr
                        (sharedPeerState { sharedPeerAdvertisedTxKeys = newKeys })
                        sharedPeers
                  }
                )
      Nothing ->
        error "TxSubmission.V2.updatePeerAdvertisedTxKeys: missing peer"

-- | Acknowledge txids from a peer and update shared state.
acknowledgeTxIds :: (Ord peeraddr, HasRawTxId txid)
                 => peeraddr
                 -> [TxKey]
                 -> SharedTxState peeraddr txid
                 -> SharedTxState peeraddr txid
acknowledgeTxIds _ [] st = st
acknowledgeTxIds peeraddr acknowledgedTxIds st =
  if IntSet.null removedKeys
    then st
    else
      let st'' = IntSet.foldl' acknowledgeOne st' removedKeys
      in st'' { sharedGeneration = sharedGeneration st + 1 }
  where
    acknowledgedKeys = IntSet.fromList (unTxKey <$> acknowledgedTxIds)
    (removedKeys, st') =
      updatePeerAdvertisedTxKeys peeraddr removeAdvertisedKeys st

    removeAdvertisedKeys advertisedKeys =
      let removed = IntSet.intersection acknowledgedKeys advertisedKeys
      in (removed, advertisedKeys `IntSet.difference` removed)

    acknowledgeOne acc k =
      case IntMap.lookup k (sharedTxTable acc) of
        Just txEntry ->
          let txEntry' = txEntry { txAdvertiserCount = txAdvertiserCount txEntry - 1 }
          in if activeTxLive txEntry'
                then acc { sharedTxTable = IntMap.insert k txEntry' (sharedTxTable acc) }
                else dropTxKey k acc
        Nothing ->
          acc

-- | Determine if an unacknowledged txid is ready to be acknowledged.
--
-- A txid remains ackable after it has been resolved and removed from shared
-- state. The wire protocol only needs an ack count for the peer-local
-- unacknowledged prefix, so a late ack is still safe even after the active
-- entry and retained marker have been pruned.
txIdAckable :: Ord peeraddr
            => PeerActionContext peeraddr txid tx
            -> TxKey
            -> Bool
txIdAckable PeerActionContext { pacPeerAddr, pacPeerState, pacSharedPeerState, pacSharedState }
            (TxKey k)
  | retainedMember k (sharedRetainedTxs pacSharedState) = True
  | otherwise =
      case IntMap.lookup k (sharedTxTable pacSharedState) of
           Just txEntry@TxEntry { txLease, txAttempts } ->
             not (IntSet.member k (sharedPeerAdvertisedTxKeys pacSharedPeerState))
             ||
               let ackWhenBuffered =
                     case txLease of
                       TxLeased owner _ -> owner == pacPeerAddr || Map.member pacPeerAddr txAttempts
                       TxClaimable _    -> Map.member pacPeerAddr txAttempts
               in
               -- Ack the txid if we downloaded it and no other
               -- peer is in the process of submitting it to the
               -- mempool.
               ackWhenBuffered
                 && IntMap.member k (peerDownloadedTxs pacPeerState)
                 && not (txBufferedByPeer pacPeerAddr txEntry
                          && txSubmittingByOther pacPeerAddr txEntry)
           Nothing -> True -- Safe late ack after the resolved tx was pruned from shared state.

-- | Remove one transaction entry from all shared state maps by key.
dropTxKey :: HasRawTxId txid
          => Int
          -> SharedTxState peeraddr txid
          -> SharedTxState peeraddr txid
dropTxKey k st@SharedTxState { sharedTxTable, sharedRetainedTxs, sharedTxIdToKey
                             , sharedKeyToTxId } =
    st {
      sharedTxTable = IntMap.delete k sharedTxTable,
      sharedRetainedTxs = retainedDeleteKeys (IntSet.singleton k) sharedRetainedTxs,
      sharedTxIdToKey = deleteTxId sharedTxIdToKey,
      sharedKeyToTxId = IntMap.delete k sharedKeyToTxId
    }
  where
    deleteTxId txIdToKey =
      case IntMap.lookup k sharedKeyToTxId of
           Just txid -> Map.delete (getRawTxId txid) txIdToKey
           Nothing   -> txIdToKey

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

-- | Remove transaction keys that are no longer active from the shared state.
dropDeadActiveKeys :: HasRawTxId txid
                   => IntSet.IntSet
                   -> SharedTxState peeraddr txid
                   -> SharedTxState peeraddr txid
dropDeadActiveKeys keys st@SharedTxState { sharedTxTable } =
  let deadKeys = IntSet.filter isDead keys in
  dropTxKeys deadKeys st
  where
    isDead k =
      case IntMap.lookup k sharedTxTable of
           Just txEntry -> not (activeTxLive txEntry)
           Nothing      -> False

-- | Is the TX entry alive?
--
-- A TX entry is alive if there is a lease, there are advertisers for it or there are
-- download attempts for it.
activeTxLive :: TxEntry peeraddr -> Bool
activeTxLive TxEntry { txLease, txAdvertiserCount, txAttempts } =
     leaseLive txLease
  || txAdvertiserCount > 0
  || not (Map.null txAttempts)
  where
    leaseLive TxClaimable {} = False
    leaseLive TxLeased {}    = True


peerAdvertisesTxKey :: Int -> SharedPeerState -> Bool
peerAdvertisesTxKey k SharedPeerState { sharedPeerAdvertisedTxKeys } =
    IntSet.member k sharedPeerAdvertisedTxKeys

peerAdvertisesAnyTxKey
  :: IntSet.IntSet
  -> SharedPeerState
  -> Bool
peerAdvertisesAnyTxKey targetKeys SharedPeerState { sharedPeerAdvertisedTxKeys } =
    not (IntSet.disjoint targetKeys sharedPeerAdvertisedTxKeys)

advertisingPeersForTxKeysExcept
  :: Ord peeraddr
  => peeraddr
  -> IntSet.IntSet
  -> SharedTxState peeraddr txid
  -> Set.Set peeraddr
advertisingPeersForTxKeysExcept _ targetKeys _
  | IntSet.null targetKeys = Set.empty
advertisingPeersForTxKeysExcept currentPeer targetKeys SharedTxState { sharedPeers } =
    Map.foldlWithKey' collect Set.empty sharedPeers
  where
    collect acc peeraddr sharedPeerState
      | peeraddr == currentPeer = acc
      | peerAdvertisesAnyTxKey targetKeys sharedPeerState = Set.insert peeraddr acc
      | otherwise = acc

advertisingPeersForTx
  :: Ord peeraddr
  => TxKey
  -> Map.Map peeraddr SharedPeerState
  -> Set.Set peeraddr
advertisingPeersForTx (TxKey k) =
    Map.foldlWithKey' collect Set.empty
  where
    collect acc peeraddr sharedPeerState
      | peerAdvertisesTxKey k sharedPeerState = Set.insert peeraddr acc
      | otherwise = acc

advertisingPeersForTxExcept
  :: Ord peeraddr
  => peeraddr
  -> TxKey
  -> SharedTxState peeraddr txid
  -> Set.Set peeraddr
advertisingPeersForTxExcept currentPeer txKey =
    advertisingPeersForTxKeysExcept currentPeer (IntSet.singleton (unTxKey txKey))

removeAdvertisingPeersForResolvedTx
  :: Ord peeraddr
  => TxKey
  -> SharedTxState peeraddr txid
  -> (SharedTxState peeraddr txid, Set.Set peeraddr)
removeAdvertisingPeersForResolvedTx txKey@(TxKey k) st@SharedTxState { sharedPeers }
  | Set.null advertisers = (st, advertisers)
  | otherwise =
      ( st {
          sharedPeers =
            Set.foldl' clearAdvertisedKey sharedPeers advertisers
        }
      , advertisers
      )
  where
    advertisers = advertisingPeersForTx txKey sharedPeers

    clearAdvertisedKey peers peeraddr =
      Map.adjust
        (\sharedPeerState ->
           sharedPeerState {
             sharedPeerAdvertisedTxKeys =
               IntSet.delete k (sharedPeerAdvertisedTxKeys sharedPeerState)
           })
        peeraddr
        peers

removeAdvertisingPeersForResolvedTxExcept
  :: Ord peeraddr
  => peeraddr
  -> TxKey
  -> SharedTxState peeraddr txid
  -> (SharedTxState peeraddr txid, Set.Set peeraddr)
removeAdvertisingPeersForResolvedTxExcept currentPeer txKey@(TxKey k) st@SharedTxState { sharedPeers }
  | Set.null advertisers = (st, advertisers)
  | otherwise =
      ( st {
          sharedPeers =
            Set.foldl' clearAdvertisedKey sharedPeers advertisers
        }
      , advertisers
      )
  where
    advertisers = advertisingPeersForTxExcept currentPeer txKey st

    clearAdvertisedKey peers peeraddr =
      Map.adjust
        (\sharedPeerState ->
           sharedPeerState {
             sharedPeerAdvertisedTxKeys =
               IntSet.delete k (sharedPeerAdvertisedTxKeys sharedPeerState)
           })
        peeraddr
        peers


-- | Handle a batch of tx bodies received from one peer.
--
-- Received bodies are buffered locally in the peer state. Bodies that are
-- already retained or already in the mempool are counted as late and dropped.
-- Any requested tx omitted from the reply releases this peer's ownership.
-- Late TXs contributes to the returned penalty count.
handleReceivedTxs :: (Ord peeraddr, HasRawTxId txid)
                  => (txid -> Bool)
                  -> Time
                  -> TxDecisionPolicy
                  -> peeraddr
                  -> [(txid, tx)]
                  -> PeerTxLocalState tx
                  -> SharedTxState peeraddr txid
                  -> (Int, Int, PeerTxLocalState tx, SharedTxState peeraddr txid)
handleReceivedTxs mempoolHasTx now policy peeraddr txs peerState sharedState =
    (omittedCount, lateCount, peerState', sharedState')
  where
    txidToKey = sharedTxIdToKey sharedState

    requestedKeys = requestedTxBatchSet requestedBatch
    retainUntil = addTime (bufferedTxsMinLifetime policy) now

    -- Dequeue the next requested tx batch to process.
    (requestedBatch, remainingRequestedBatches) =
      case peerRequestedTxBatches peerState of
           StrictSeq.Empty ->
             error "TxSubmission.Inbound.V2.handleReceivedTxs null requestedBatches"
           batch StrictSeq.:<| batches ->
             (batch, batches)

    -- Process each received tx, collecting late counts, pending requests,
    -- updated shared state, and peers to wake up.
    ( lateCount
      , pendingRequestedKeys
      , sharedStateHandled
      , receivedWakePeers
      , peerDownloadedTxs'
      ) =
      foldl'
        handleOne
        ( 0
        , requestedKeys
        , sharedState
        , Set.empty
        , peerDownloadedTxs peerState
        )
        txs

    (omittedAdvertisedKeys, sharedStateReleased0) =
      updatePeerAdvertisedTxKeys peeraddr removeOmittedAdvertisedKeys sharedStateHandled

    -- Process omitted (not received) txs: count a penalty for every omitted
    -- request, release ownership for keys that are still live, and collect
    -- peers to wake up.
    (omittedCount, sharedStateReleased, omittedWakePeers) =
      IntSet.foldl' handleOmitted (0, sharedStateReleased0, Set.empty) pendingRequestedKeys

    -- Build the final shared state with updated tables and cleaned-up dead entries.
    sharedState'' =
      dropDeadActiveKeys pendingRequestedKeys $
        sharedStateReleased {
          sharedGeneration = sharedGeneration sharedState + 1
        }

    -- Update peer state: remove processed keys, update batch tracking, and record
    -- downloaded txs.
    peerState' = peerState {
        peerAvailableTxIds =
          IntSet.foldl' (flip IntMap.delete) (peerAvailableTxIds peerState) requestedKeys
      , peerRequestedTxs = peerRequestedTxs peerState `IntSet.difference` requestedKeys
      , peerRequestedTxBatches = remainingRequestedBatches
      , peerRequestedTxsSize = peerRequestedTxsSize peerState - requestedTxBatchSize requestedBatch
      , peerDownloadedTxs = peerDownloadedTxs'
      }

    -- Flag peers that may now have work available after processing txs.
    sharedState' =
      bumpIdlePeerGenerations
        (Set.union receivedWakePeers omittedWakePeers)
        sharedState''

    keyWasLive k =
         IntMap.member k (sharedTxTable sharedState)
      || retainedMember k (sharedRetainedTxs sharedState)

    removeOmittedAdvertisedKeys advertisedKeys =
      let removed = IntSet.intersection pendingRequestedKeys advertisedKeys
      in (removed, advertisedKeys `IntSet.difference` removed)

    -- Fold function over received txs: classify as late, already in mempool, or buffer for
    -- download.
    handleOne
      ( lateCountAcc
      , pendingKeysAcc
      , sharedAcc
      , wakePeersAcc
      , downloadedAcc
      )
      (txid, tx) =
        case Map.lookup (getRawTxId txid) txidToKey of
             Nothing ->
               ( lateCountAcc + 1
               , pendingKeysAcc
               , sharedAcc
               , wakePeersAcc
               , downloadedAcc
               )
             Just txKey@(TxKey k)
               | retainedMember k (sharedRetainedTxs sharedAcc) ->
                   let sharedAcc' =
                         sharedAcc {
                           sharedTxTable = IntMap.delete k (sharedTxTable sharedAcc)
                         }
                   in ( lateCountAcc + 1
                      , IntSet.delete k pendingKeysAcc
                      , sharedAcc'
                      , wakePeersAcc
                      , downloadedAcc
                      )
               | mempoolHasTx txid ->
                   let (sharedAcc', advertisers) =
                         case IntMap.lookup k (sharedTxTable sharedAcc) of
                           Just _ ->
                             removeAdvertisingPeersForResolvedTx txKey sharedAcc
                           Nothing ->
                             (sharedAcc, Set.empty)
                       wakePeers =
                         Set.union wakePeersAcc (Set.delete peeraddr advertisers)
                       sharedAcc'' =
                         sharedAcc' {
                           sharedTxTable = IntMap.delete k (sharedTxTable sharedAcc'),
                           sharedRetainedTxs =
                             retainedInsertMax k retainUntil (sharedRetainedTxs sharedAcc')
                         }
                   in ( lateCountAcc + 1
                      , IntSet.delete k pendingKeysAcc
                      , sharedAcc''
                      , wakePeers
                      , downloadedAcc
                      )
               | otherwise ->
                   case IntMap.lookup k (sharedTxTable sharedAcc) of
                        Just txEntry
                          | peerHasAttempt txEntry ->
                              ( lateCountAcc
                              , IntSet.delete k pendingKeysAcc
                              , sharedAcc {
                                  sharedTxTable =
                                    IntMap.insert k (markBuffered txEntry)
                                      (sharedTxTable sharedAcc)
                                }
                              , wakePeersAcc
                              , IntMap.insert k tx downloadedAcc
                              )
                        _ ->
                              ( lateCountAcc + 1
                              , IntSet.delete k pendingKeysAcc
                              , sharedAcc
                              , wakePeersAcc
                              , downloadedAcc
                              )

    -- Handle omitted (not received) txs: release ownership, count penalties,
    -- and wake up other advertisers if the tx is still active.
    handleOmitted (omittedCountAcc, sharedAcc, wakePeersAcc) k
      | keyWasLive k =
          let sharedAcc' =
                case IntMap.lookup k (sharedTxTable sharedAcc) of
                  Just txEntry ->
                    sharedAcc {
                      sharedTxTable =
                        IntMap.insert k (releaseLease (IntSet.member k omittedAdvertisedKeys) txEntry)
                          (sharedTxTable sharedAcc)
                    }
                  Nothing ->
                    sharedAcc
              wakePeersAcc' =
                case IntMap.lookup k (sharedTxTable sharedAcc') of
                     Just txEntry
                       | activeTxLive txEntry ->
                           Set.union
                             (advertisingPeersForTxExcept peeraddr (TxKey k) sharedAcc')
                             wakePeersAcc
                     _ -> wakePeersAcc in
          (omittedCountAcc + 1, sharedAcc', wakePeersAcc')
      | otherwise =
          (omittedCountAcc + 1, sharedAcc, wakePeersAcc)

    peerHasAttempt TxEntry { txAttempts } =
      Map.member peeraddr txAttempts

    markBuffered txEntry@TxEntry { txAttempts } =
      txEntry { txAttempts = Map.insert peeraddr TxBuffered txAttempts }

    releaseLease wasAdvertised txEntry@TxEntry { txLease, txAdvertiserCount, txAttempts } =
      txEntry {
        txLease = case txLease of
                       TxLeased owner _ | owner == peeraddr -> TxClaimable now
                       _                                    -> txLease,
        txAdvertiserCount =
          if wasAdvertised
             then txAdvertiserCount - 1
             else txAdvertiserCount,
        txAttempts = Map.delete peeraddr txAttempts
      }


-- | Handle the result of submitting buffered txs to the mempool.
--
-- Accepted txs leave the active table and move into the retained set so later
-- txid advertisements can be acknowledged without re-requesting the body.
-- Txs rejected by the mempool release this peer's attempt state and advertiser
-- slot so another advertiser may try later.
handleSubmittedTxs :: (Ord peeraddr, HasRawTxId txid)
                   => Time
                   -> TxDecisionPolicy
                   -> peeraddr
                   -> [TxKey]
                   -> [TxKey]
                   -> PeerTxLocalState tx
                   -> SharedTxState peeraddr txid
                   -> (PeerTxLocalState tx, SharedTxState peeraddr txid)
handleSubmittedTxs now policy peeraddr acceptedTxs rejectedTxs peerState sharedState =
  (peerState', sharedState')
  where
    acceptedKeys = IntSet.fromList (unTxKey <$> acceptedTxs)
    rejectedKeys = IntSet.fromList (unTxKey <$> rejectedTxs)
    submittedKeys = acceptedKeys `IntSet.union` rejectedKeys

    peerState' = peerState {
        peerDownloadedTxs =
          IntSet.foldl' (flip IntMap.delete) (peerDownloadedTxs peerState) submittedKeys
      }

    (sharedStateAfterAccepted, acceptedAdvertisers) =
      foldl' acceptSubmittedTx (sharedState, Set.empty) acceptedTxs

    (rejectedAdvertisedKeys, sharedStateAfterRejectedPeer) =
      updatePeerAdvertisedTxKeys peeraddr removeRejectedAdvertisedKeys sharedStateAfterAccepted

    (sharedStateAfterRejected, rejectedWakePeers) =
      IntSet.foldl' updateRejected (sharedStateAfterRejectedPeer, Set.empty) rejectedKeys

    sharedState' =
      bumpIdlePeerGenerations
        (Set.union acceptedAdvertisers rejectedWakePeers)
        sharedState''

    sharedState'' =
      dropDeadActiveKeys rejectedKeys $
        sharedStateAfterRejected {
          sharedGeneration = sharedGeneration sharedState + 1
        }

    retainedUntil = addTime (bufferedTxsMinLifetime policy) now

    removeRejectedAdvertisedKeys advertisedKeys =
      let removed = IntSet.intersection rejectedKeys advertisedKeys
      in (removed, advertisedKeys `IntSet.difference` removed)

    acceptSubmittedTx (sharedAcc, wakePeersAcc) txKey@(TxKey k) =
      let (sharedAcc', advertisers) =
            case IntMap.lookup k (sharedTxTable sharedAcc) of
              Just _ ->
                removeAdvertisingPeersForResolvedTx txKey sharedAcc
              Nothing ->
                (sharedAcc, Set.empty)
          sharedAcc'' =
            sharedAcc' {
              sharedTxTable = IntMap.delete k (sharedTxTable sharedAcc'),
              sharedRetainedTxs =
                retainedInsertMax k retainedUntil (sharedRetainedTxs sharedAcc')
            }
      in (sharedAcc'', Set.union wakePeersAcc (Set.delete peeraddr advertisers))

    updateRejected (sharedAcc, wakePeersAcc) k =
      let sharedAcc' =
            case IntMap.lookup k (sharedTxTable sharedAcc) of
              Just txEntry ->
                sharedAcc {
                  sharedTxTable =
                    IntMap.insert k
                      (markRejected (IntSet.member k rejectedAdvertisedKeys) txEntry)
                      (sharedTxTable sharedAcc)
                }
              Nothing ->
                sharedAcc
          wakePeersAcc' =
            case IntMap.lookup k (sharedTxTable sharedAcc') of
              Just txEntry
                | activeTxLive txEntry ->
                    Set.union
                      (advertisingPeersForTxExcept peeraddr (TxKey k) sharedAcc')
                      wakePeersAcc
              _ ->
                wakePeersAcc
      in (sharedAcc', wakePeersAcc')

    markRejected wasAdvertised txEntry@TxEntry { txLease, txAdvertiserCount, txAttempts } =
      txEntry {
        txLease = case txLease of
          TxLeased owner _ | owner == peeraddr -> TxClaimable now
          _                                    -> txLease,
        txAdvertiserCount =
          if wasAdvertised
             then txAdvertiserCount - 1
             else txAdvertiserCount,
        txAttempts = Map.delete peeraddr txAttempts
      }


-- | Mark buffered txs as entering mempool submission.
markSubmittingTxs :: Ord peeraddr
                  => peeraddr
                  -> [TxKey]
                  -> SharedTxState peeraddr txid
                  -> SharedTxState peeraddr txid
markSubmittingTxs _ [] st = st
markSubmittingTxs peeraddr txKeys st =
  st {
    sharedTxTable = foldl' markOne (sharedTxTable st) txKeys,
    sharedGeneration = sharedGeneration st + 1
  }
  where
    markOne txTable (TxKey k) = IntMap.adjust markSubmitting k txTable

    markSubmitting txEntry@TxEntry { txAttempts } =
      txEntry {
        txAttempts = Map.adjust toSubmitting peeraddr txAttempts
      }

    toSubmitting TxBuffered = TxSubmitting
    toSubmitting attempt    = attempt


-- | Handle a batch of txids received from one peer.
--
-- Newly seen txids are interned, appended to the peer's unacknowledged queue,
-- and entered into the shared tx table as claimable work. Any peer that later
-- advertises the txid may claim it once its score-derived delay has elapsed,
-- which avoids pinning fresh work to the first peer that happened to announce
-- it.
handleReceivedTxIds :: forall peeraddr txid tx. (Ord peeraddr, HasRawTxId txid)
                    => (txid -> Bool)
                    -> Time
                    -> TxDecisionPolicy
                    -> peeraddr
                    -> NumTxIdsToReq
                    -> [(txid, SizeInBytes)]
                    -> PeerTxLocalState tx
                    -> SharedTxState peeraddr txid
                    -> (PeerTxLocalState tx, SharedTxState peeraddr txid)
handleReceivedTxIds mempoolHasTx now policy peeraddr requestedTxIds txidsAndSizes
                    peerState sharedState =
    (peerState'', sharedState'')
  where
    sharedPeerState0 =
      case Map.lookup peeraddr (sharedPeers sharedState) of
        Just sharedPeerState -> sharedPeerState
        Nothing ->
          error "TxSubmission.V2.handleReceivedTxIds: missing peer"

    peerAdvertisedKeys0 = sharedPeerAdvertisedTxKeys sharedPeerState0

    -- Fold over received txids: build unacknowledged list, update tables,
    -- and track peers to wake based on tx state (retained/mempool/new).
    ( receivedTxKeysRev
      , peerAvailableTxIds'
      , sharedStateHandled
      , peerAdvertisedKeys'
      , peersToWake
      , sharedChanged
      ) =
      foldl'
        step
        ( []
        , peerAvailableTxIds peerState
        , sharedState
        , peerAdvertisedKeys0
        , Set.empty
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

    sharedState''
      | sharedChanged || peerAdvertisedKeys' /= peerAdvertisedKeys0 =
          bumpIdlePeerGenerations peersToWake $
            sharedStateHandled {
              sharedPeers =
                if peerAdvertisedKeys' == peerAdvertisedKeys0
                  then sharedPeers sharedStateHandled
                  else
                    Map.insert
                      peeraddr
                      (sharedPeerState0 { sharedPeerAdvertisedTxKeys = peerAdvertisedKeys' })
                      (sharedPeers sharedStateHandled),
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
         , Set.Set peeraddr
         , Bool
         )
      -> (txid, SizeInBytes)
      -> ( [TxKey]
         , IntMap.IntMap SizeInBytes
         , SharedTxState peeraddr txid
         , IntSet.IntSet
         , Set.Set peeraddr
         , Bool
         )
    step
      ( !unacknowledgedAcc
      , !availableAcc
      , !sharedAcc
      , !peerAdvertisedKeysAcc
      , !peersAcc
      , !sharedChangedAcc
      )
      (txid, txSize)
      | retainedMember k retainedAcc =
          ( txKey : unacknowledgedAcc
          , IntMap.delete k availableAcc
          , sharedAcc'
          , IntSet.delete k peerAdvertisedKeysAcc
          , peersAcc
          , sharedChangedAcc'
          )
      | mempoolHasTx txid =
          let (sharedAcc'', advertisers) =
                case IntMap.lookup k (sharedTxTable sharedAcc') of
                  Just _ ->
                    removeAdvertisingPeersForResolvedTxExcept peeraddr txKey sharedAcc'
                  Nothing ->
                    (sharedAcc', Set.empty)
              wakePeers =
                Set.union peersAcc (Set.delete peeraddr advertisers)
          in ( txKey : unacknowledgedAcc
             , IntMap.delete k availableAcc
             , sharedAcc'' {
                 sharedTxTable = IntMap.delete k (sharedTxTable sharedAcc''),
                 sharedRetainedTxs =
                   retainedInsertMax k retainUntil (sharedRetainedTxs sharedAcc'')
               }
             , IntSet.delete k peerAdvertisedKeysAcc
             , wakePeers
             , True
             )
      | otherwise =
          case IntMap.lookup k (sharedTxTable sharedAcc') of
            Nothing ->
              let txEntry = TxEntry {
                              txLease = TxClaimable now,
                              txAdvertiserCount = 1,
                              txAttempts = Map.empty
                            }
              in ( txKey : unacknowledgedAcc
                 , IntMap.insert k txSize availableAcc
                 , sharedAcc' {
                     sharedTxTable = IntMap.insert k txEntry (sharedTxTable sharedAcc')
                   }
                 , IntSet.insert k peerAdvertisedKeysAcc
                 , peersAcc
                 , True
                 )
            Just txEntry ->
              let (entryChanged, txEntry', peerAdvertisedKeysAcc') =
                    addAdvertiser k peerAdvertisedKeysAcc txEntry
                  availableAcc' = IntMap.insert k txSize availableAcc
                  sharedAcc'' =
                    if entryChanged
                      then
                        sharedAcc' {
                          sharedTxTable = IntMap.insert k txEntry' (sharedTxTable sharedAcc')
                        }
                      else
                        sharedAcc'
              in ( txKey : unacknowledgedAcc
                 , availableAcc'
                 , sharedAcc''
                 , peerAdvertisedKeysAcc'
                 , peersAcc
                 , sharedChangedAcc' || entryChanged
                 )
      where
        retainedAcc = sharedRetainedTxs sharedAcc'
        sharedChangedAcc' = sharedChangedAcc || txKeyWasNew
        (txKey@(TxKey k), txKeyWasNew, sharedAcc') = lookupOrInternTxId txid sharedAcc

    addAdvertiser k peerAdvertisedKeysAcc txEntry@TxEntry { txAdvertiserCount } =
      if IntSet.member k peerAdvertisedKeysAcc
        then
          (False, txEntry, peerAdvertisedKeysAcc)
        else
          ( True
          , txEntry { txAdvertiserCount = txAdvertiserCount + 1 }
          , IntSet.insert k peerAdvertisedKeysAcc
          )

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
