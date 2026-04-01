{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.TxSubmission.Inbound.V2.State
  ( handleReceivedTxIds
  , handleReceivedTxs
  , handleSubmittedTxs
  , markSubmittingTxs
  , nextPeerAction
  , nextPeerActionPipelined
  ) where

import Control.Exception (assert)
import Control.Monad.Class.MonadTime.SI (DiffTime, Time, addTime, diffTime)
import Data.Foldable (foldl', toList)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set qualified as Set
import Data.Word (Word64)

import Ouroboros.Network.Protocol.TxSubmission2.Type (NumTxIdsToAck,
           SizeInBytes)
import Ouroboros.Network.TxSubmission.Inbound.V2.Policy
import Ouroboros.Network.TxSubmission.Inbound.V2.Types

data TxIdRequestMode = AllowAnyTxIdRequests | AllowPipelinedTxIdRequests
  deriving Eq

-- | Precomputed context for selecting the next action for one peer.
--
data PeerActionContext peeraddr txid tx = PeerActionContext {
    -- | Current time used for lease expiry and score decay decisions.
    pacNow            :: !Time,
    -- | Decision policy that governs request, retry, and scoring limits.
    pacPolicy         :: !TxDecisionPolicy,
    -- | Address of the peer whose next action is being chosen.
    pacPeerAddr       :: !peeraddr,
    -- | Current peer-local state after local pruning has been applied.
    pacPeerState      :: !(PeerTxLocalState tx),
    -- | Shared tx-submission state after shared pruning has been applied.
    pacSharedState    :: !(SharedTxState peeraddr txid),
    -- | Decayed scores for peers that are currently idle and eligible to claim work.
    pacIdlePeerScores :: !(Map.Map peeraddr Double)
  }

data PeerActionChoice peeraddr =
    ChooseSubmit ![TxKey]
  | ChooseRequestTxs ![TxKey] !SizeInBytes !(IntMap.IntMap (TxEntry peeraddr))
  | ChooseRequestTxIds ![TxKey] !NumTxIdsToAck !NumTxIdsToReq !(StrictSeq.StrictSeq TxKey)
  | ChooseDoNothing !Word64 !(Maybe DiffTime)

-- | Build a precomputed context for selecting the next action for a peer.
--
--
mkPeerActionContext :: Ord txid
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
    pacIdlePeerScores = idlePeerScores
    }
  where
    -- Remove expireds TX keys from the shared state
    sharedState' =
      let expiredRetainedKeys = retainedExpiredKeys now (sharedRetainedTxs sharedState) in
      dropTxKeys expiredRetainedKeys sharedState

    -- Remove downloaded tx bodies that are no longer in the shared state.
    peerState' =
      peerState {
        peerDownloadedTxs = IntMap.restrictKeys (peerDownloadedTxs peerState) (IntMap.keysSet (sharedTxTable sharedState'))
      }

    idlePeerScores =
      Map.mapMaybe toIdleScore (sharedPeers sharedState')
      where
        toIdleScore SharedPeerState { sharedPeerPhase, sharedPeerScore }
          | sharedPeerPhase == PeerIdle = Just (currentPeerScore policy now sharedPeerScore)
          | otherwise = Nothing

-- | Compute the next peer-local action.
nextPeerAction :: (Ord peeraddr, Ord txid)
               => Time
               -> TxDecisionPolicy
               -> peeraddr
               -> PeerTxLocalState tx
               -> SharedTxState peeraddr txid
               -> (PeerAction, PeerTxLocalState tx, SharedTxState peeraddr txid)
nextPeerAction = nextPeerActionWithMode AllowAnyTxIdRequests

-- | Pipelined version of nextPeerAction
nextPeerActionPipelined :: (Ord peeraddr, Ord txid)
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
nextPeerActionWithMode :: (Ord peeraddr, Ord txid)
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
      ChooseRequestTxIds acknowledgedTxIds txIdsToAcknowledge txIdsToRequest unacknowledgedTxIds'
  -- Do nothing
  | otherwise =
      ChooseDoNothing (peerGenerationOf (pacPeerAddr ctx) (pacSharedState ctx)) (nextWakeDelay ctx)

-- | Execute a chosen peer action and compute resulting state updates
applyPeerActionChoice :: (Ord peeraddr, Ord txid)
                      => PeerActionContext peeraddr txid tx
                      -> PeerActionChoice peeraddr
                      -> (PeerAction, PeerTxLocalState tx, SharedTxState peeraddr txid)
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
        (advertisersForKeysExcept (pacPeerAddr ctx) txTable txsToRequest)
        ((pacSharedState ctx) {
          sharedTxTable = txTable,
          sharedGeneration = sharedGeneration (pacSharedState ctx) + 1
        })

-- | Construct a 'PeerRequestTxIds' action and update local and shared txid state.
applyRequestTxIdsChoice
  :: (Ord peeraddr, Ord txid)
  => PeerActionContext peeraddr txid tx
  -> [TxKey]
  -> NumTxIdsToAck
  -> NumTxIdsToReq
  -> StrictSeq.StrictSeq TxKey
  -> (PeerAction, PeerTxLocalState tx, SharedTxState peeraddr txid)
applyRequestTxIdsChoice ctx acknowledgedTxIds txIdsToAcknowledge txIdsToRequest unacknowledgedTxIds' =
  ( PeerRequestTxIds txIdsToAcknowledge txIdsToRequest
  , peerState''
  , sharedState''
  )
  where
    peerState'' =
      (pacPeerState ctx) {
        peerAvailableTxIds =
          foldl' (flip IntMap.delete) (peerAvailableTxIds (pacPeerState ctx)) (unTxKey <$> acknowledgedTxIds),
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
                     if txSelectable ctx txEntry
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
        numOfAcked0 = StrictSeq.length ackablePrefix
        numOfAcked
          | numOfRequested > 0 = min numOfAcked0 (max 0 (numOfUnacked - 1))
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
nextWakeDelay :: Ord peeraddr
              => PeerActionContext peeraddr txid tx
              -> Maybe DiffTime
nextWakeDelay PeerActionContext { pacNow, pacPeerAddr, pacSharedState } =
    (`diffTime` pacNow) <$> minMaybe nextLeaseWake nextRetainWake
  where
    nextLeaseWake =
      IntMap.foldl' stepLease Nothing (sharedTxTable pacSharedState)

    stepLease acc txEntry@TxEntry { txLease } =
      if Map.member pacPeerAddr (txAdvertisers txEntry)
         then minMaybe acc (futureLeaseWake txLease)
         else acc

    nextRetainWake = retainedNextWake pacNow (sharedRetainedTxs pacSharedState)

    futureLeaseWake TxClaimable = Nothing
    futureLeaseWake (TxLeased _ leaseUntil)
      | leaseUntil > pacNow = Just leaseUntil
      | otherwise = Nothing

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
claimTx peeraddr leaseUntil txEntry@TxEntry { txAdvertisers, txAttempts } =
  txEntry {
    txLease = TxLeased peeraddr leaseUntil,
    txAdvertisers = Map.adjust setAckWhenBuffered peeraddr txAdvertisers,
    txAttempts = Map.insert peeraddr TxDownloading txAttempts
  }
  where
    setAckWhenBuffered advertiser = advertiser { txAckState = AckWhenBuffered }

-- | Determine if a tx is eligible for this peer to request.
--
-- A tx is selectable if it can be claimed or is already owned by this peer
-- and still being advertised.
txSelectable :: Ord peeraddr
             => PeerActionContext peeraddr txid tx
             -> TxEntry peeraddr
             -> Bool
txSelectable PeerActionContext { pacNow, pacPeerAddr, pacPolicy, pacIdlePeerScores }
             txEntry@TxEntry { txAdvertisers, txTieBreakSalt }
  | txSubmittingAnywhere txEntry = False
  | txPeerHasAttempt = False
  | txActiveAttemptCount txEntry >= txInflightMultiplicity pacPolicy = False
  | txOwnedByPeer txEntry && Map.member pacPeerAddr txAdvertisers = True
  | otherwise =
      let peerMayClaim = Map.member pacPeerAddr txAdvertisers &&
                           (case pickClaimOwner of
                                   Just owner -> owner == pacPeerAddr
                                   Nothing    -> False) in
      case txLease txEntry of
           TxClaimable            -> peerMayClaim
           TxLeased _ leaseExpiry -> (leaseExpiry <= pacNow) && peerMayClaim
  where

    -- Select which idle advertiser should claim a tx lease based on
    -- peer score.
    pickClaimOwner =
      case eligiblePeers of
        [] -> Nothing
        _  -> Just (pickBestPeer eligiblePeers)
      where
        eligiblePeers =
          [ (candidate, score)
          | candidate <- Map.keys txAdvertisers
          , Just score <- [Map.lookup candidate pacIdlePeerScores]
          ]

        pickBestPeer peers =
          case sort [ candidate | (candidate, score) <- peers, score == bestScore ] of
            []   -> assert False pacPeerAddr
            tied -> tied !! (txTieBreakSalt `mod` length tied)
          where
            bestScore = minimum [ score | (_, score) <- peers ]

    -- txOwnedByPeer :: TxEntry peeraddr -> Bool
    txOwnedByPeer TxEntry { txLease = TxLeased owner _ } = owner == pacPeerAddr
    txOwnedByPeer TxEntry { txLease = TxClaimable }      = False

    txPeerHasAttempt =
      case txAttemptOfPeer pacPeerAddr txEntry of
        Just TxNoAttempt -> False
        Just _           -> True
        Nothing          -> False

    txActiveAttemptCount :: TxEntry peeraddr -> Int
    txActiveAttemptCount TxEntry { txAttempts } =
      length
        [ ()
        | attempt <- Map.elems txAttempts
        , attempt == TxDownloading || attempt == TxBuffered
        ]


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
    | currentTime == peerScoreTs = peerScoreValue
    | otherwise = max 0 $ peerScoreValue - realToFrac (diffTime currentTime peerScoreTs) * scoreRate

-- | Acknowledge txids from a peer and update shared state.
acknowledgeTxIds :: (Ord peeraddr, Ord txid)
                 => peeraddr
                 -> [TxKey]
                 -> SharedTxState peeraddr txid
                 -> SharedTxState peeraddr txid
acknowledgeTxIds _ [] st = st
acknowledgeTxIds peeraddr acknowledgedTxIds st =
  foldl' acknowledgeOne st' acknowledgedTxIds
  where
    st' = st { sharedGeneration = sharedGeneration st + 1 }

    removeAdvertiser txEntry@TxEntry { txAdvertisers } =
      txEntry { txAdvertisers = Map.delete peeraddr txAdvertisers }

    acknowledgeOne acc (TxKey k) =
      case IntMap.lookup k (sharedTxTable acc) of
           Just txEntry ->
             let txEntry' = removeAdvertiser txEntry in
             if activeTxLive txEntry'
                then acc { sharedTxTable = IntMap.insert k txEntry' (sharedTxTable acc) }
                else dropTxKey k acc
           Nothing -> acc

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
txIdAckable PeerActionContext { pacPeerAddr, pacPeerState, pacSharedState } (TxKey k)
  | retainedMember k (sharedRetainedTxs pacSharedState) = True
  | otherwise =
      case IntMap.lookup k (sharedTxTable pacSharedState) of
           Just txEntry ->
             case Map.lookup pacPeerAddr (txAdvertisers txEntry) of
                  Just TxAdvertiser { txAckState = AckWhenBuffered } ->
                    -- Ack the txid if we downloaded it and no other
                    -- peer is in the process of submitting it to the
                    -- mempool.
                    IntMap.member k (peerDownloadedTxs pacPeerState)
                      && not (txBufferedByPeer pacPeerAddr txEntry
                               && txSubmittingByOther pacPeerAddr txEntry)
                  Just TxAdvertiser { txAckState = AckWhenResolved } ->
                    False -- This becomes ackable once the tx is retained or later pruned.
                  Nothing -> True -- Safe late ack after this peer was pruned from the shared entry.
           Nothing -> True -- Safe late ack after the resolved tx was pruned from shared state.

-- | Remove one transaction entry from all shared state maps by key.
dropTxKey :: Ord txid
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
           Just txid -> Map.delete txid txIdToKey
           Nothing   -> txIdToKey

-- | Remove transaction entries from all shared state maps by key.
dropTxKeys :: Ord txid
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
           Just txid -> Map.delete txid txIdToKey
           Nothing   -> txIdToKey

-- | Remove transaction keys that are no longer active from the shared state.
dropDeadActiveKeys :: Ord txid
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
activeTxLive TxEntry { txLease, txAdvertisers, txAttempts } =
     leaseLive txLease
  || not (Map.null txAdvertisers)
  || not (Map.null txAttempts)
  where
    leaseLive TxClaimable = False
    leaseLive TxLeased {} = True


-- | Collect the advertisers of the given active tx keys, excluding one peer.
advertisersForKeysExcept
  :: Ord peeraddr
  => peeraddr
  -> IntMap.IntMap (TxEntry peeraddr)
  -> [TxKey]
  -> Set.Set peeraddr
advertisersForKeysExcept currentPeer txTable =
    foldl' collectAdvertisers Set.empty
  where
    collectAdvertisers peers (TxKey k) =
      case IntMap.lookup k txTable of
           Just txEntry ->
             Set.union peers (advertisersForEntryExcept currentPeer txEntry)
           Nothing ->
             peers

-- | Get all advertisers for a transaction entry, excluding a specific peer.
advertisersForEntryExcept :: Ord peeraddr
                          => peeraddr
                          -> TxEntry peeraddr
                          -> Set.Set peeraddr
advertisersForEntryExcept currentPeer TxEntry { txAdvertisers } =
    Set.delete currentPeer (Map.keysSet txAdvertisers)


-- | Handle a batch of tx bodies received from one peer.
--
-- Received bodies are buffered locally in the peer state. Bodies that are
-- already retained or already in the mempool are counted as late and dropped.
-- Any requested tx omitted from the reply releases this peer's ownership.
-- Late TXs contributes to the returned penalty count.
handleReceivedTxs :: (Ord peeraddr, Ord txid)
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
    -- updated tables, and peers to wake up.
    ( lateCount
      , pendingRequestedKeys
      , txTableHandled
      , retainedHandled
      , receivedWakePeers
      , peerDownloadedTxs'
      ) =
      foldl'
        handleOne
        ( 0
        , requestedKeys
        , sharedTxTable sharedState
        , sharedRetainedTxs sharedState
        , Set.empty
        , peerDownloadedTxs peerState
        )
        txs

    -- Process omitted (not received) txs: count a penalty for every omitted
    -- request, release ownership for keys that are still live, and collect
    -- peers to wake up.
    (omittedCount, txTableReleased, omittedWakePeers) =
      IntSet.foldl' handleOmitted (0, txTableHandled, Set.empty) pendingRequestedKeys

    -- Build the final shared state with updated tables and cleaned-up dead entries.
    sharedState'' =
      dropDeadActiveKeys pendingRequestedKeys sharedState {
        sharedTxTable = txTableReleased,
        sharedRetainedTxs = retainedHandled,
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


    -- Fold function over received txs: classify as late, already in mempool, or buffer for
    -- download.
    handleOne
      ( lateCountAcc
      , pendingKeysAcc
      , txTableAcc
      , retainedAcc
      , wakePeersAcc
      , downloadedAcc
      )
      (txid, tx) =
        case Map.lookup txid txidToKey of
             Nothing ->
               ( lateCountAcc + 1
               , pendingKeysAcc
               , txTableAcc
               , retainedAcc
               , wakePeersAcc
               , downloadedAcc
               )
             Just (TxKey k)
               | retainedMember k retainedAcc ->
                   ( lateCountAcc + 1
                   , IntSet.delete k pendingKeysAcc
                   , IntMap.delete k txTableAcc
                   , retainedAcc
                   , wakePeersAcc
                   , downloadedAcc
                   )
               | mempoolHasTx txid ->
                   let wakePeers =
                         case IntMap.lookup k txTableAcc of
                              Just txEntry ->
                                Set.union (advertisersForEntryExcept peeraddr txEntry)
                                          wakePeersAcc
                              Nothing ->
                                wakePeersAcc in
                   ( lateCountAcc + 1
                   , IntSet.delete k pendingKeysAcc
                   , IntMap.delete k txTableAcc
                   , retainedInsertMax k retainUntil retainedAcc
                   , wakePeers
                   , downloadedAcc
                   )
               | otherwise ->
                   case IntMap.lookup k txTableAcc of
                        Just txEntry
                          | peerHasAttempt txEntry ->
                              ( lateCountAcc
                              , IntSet.delete k pendingKeysAcc
                              , IntMap.insert k (markBuffered txEntry) txTableAcc
                              , retainedAcc
                              , wakePeersAcc
                              , IntMap.insert k tx downloadedAcc
                              )
                        _ ->
                              ( lateCountAcc + 1
                              , IntSet.delete k pendingKeysAcc
                              , txTableAcc
                              , retainedAcc
                              , wakePeersAcc
                              , downloadedAcc
                              )

    -- Handle omitted (not received) txs: release ownership, count penalties,
    -- and wake up other advertisers if the tx is still active.
    handleOmitted (omittedCountAcc, txTableAcc, wakePeersAcc) k
      | keyWasLive k =
          let txTableAcc' = releaseOne txTableAcc k
              wakePeersAcc' =
                case IntMap.lookup k txTableAcc' of
                     Just txEntry
                       | activeTxLive txEntry ->
                           Set.union (advertisersForEntryExcept peeraddr txEntry) wakePeersAcc
                     _ -> wakePeersAcc in
          (omittedCountAcc + 1, txTableAcc', wakePeersAcc')
      | otherwise =
          (omittedCountAcc + 1, txTableAcc, wakePeersAcc)

    peerHasAttempt TxEntry { txAttempts } =
      Map.member peeraddr txAttempts

    markBuffered txEntry@TxEntry { txAttempts } =
      txEntry { txAttempts = Map.insert peeraddr TxBuffered txAttempts }

    releaseOne txTable k =
      IntMap.adjust releaseLease k txTable

    releaseLease txEntry@TxEntry { txLease, txAdvertisers, txAttempts } =
      txEntry {
        txLease = case txLease of
                       TxLeased owner _ | owner == peeraddr -> TxClaimable
                       _                                    -> txLease,
        txAdvertisers = Map.delete peeraddr txAdvertisers,
        txAttempts = Map.delete peeraddr txAttempts
      }


-- | Handle the result of submitting buffered txs to the mempool.
--
-- Accepted txs leave the active table and move into the retained set so later
-- txid advertisements can be acknowledged without re-requesting the body.
-- Txs rejected by the mempool release this peer's attempt state and advertiser
-- slot so another advertiser may try later.
handleSubmittedTxs :: (Ord peeraddr, Ord txid)
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

    (acceptedAdvertisers, activeTableAfterAccepted, retainedTxs') = acceptSubmittedTxs

    rejectedActive = IntSet.foldl' updateRejected activeTableAfterAccepted rejectedKeys

    sharedState' =
      bumpIdlePeerGenerations
        (Set.union acceptedAdvertisers (advertisersForKeysExcept peeraddr txTable' (fmap TxKey (IntSet.toList rejectedKeys))))
        sharedState''

    sharedState'' =
      dropDeadActiveKeys rejectedKeys sharedState {
        sharedTxTable = rejectedActive,
        sharedRetainedTxs = retainedTxs',
        sharedGeneration = sharedGeneration sharedState + 1
      }

    txTable' = sharedTxTable sharedState''

    retainedUntil = addTime (bufferedTxsMinLifetime policy) now

    acceptSubmittedTxs =
      ( advertisersForKeysExcept peeraddr (sharedTxTable sharedState) acceptedTxs
      , IntMap.withoutKeys (sharedTxTable sharedState) acceptedKeys
      , IntSet.foldl'
          (\retained k -> retainedInsertMax k retainedUntil retained)
          (sharedRetainedTxs sharedState)
          acceptedKeys
      )

    updateRejected txTable k =
      IntMap.adjust markRejected k txTable

    markRejected txEntry@TxEntry { txLease, txAdvertisers, txAttempts } =
      txEntry {
        txLease = case txLease of
          TxLeased owner _ | owner == peeraddr -> TxClaimable
          _                                    -> txLease,
        txAdvertisers = Map.delete peeraddr txAdvertisers,
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
-- and entered into the shared tx table. The first advertiser gets the initial
-- lease and may acknowledge once the body is buffered locally. Later
-- advertisers are tracked as backups and may only acknowledge once the tx is
-- resolved.
handleReceivedTxIds :: forall peeraddr txid tx. (Ord peeraddr, Ord txid)
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

    -- Fold over received txids: build unacknowledged list, update tables,
    -- and track peers to wake based on tx state (retained/mempool/new).
    ( peerUnacknowledgedTxIds'
      , peerAvailableTxIds'
      , sharedTxIdToKey'
      , sharedKeyToTxId'
      , sharedNextTxKey'
      , sharedTxTable'
      , sharedRetainedTxs'
      , peersToWake
      ) =
      foldl'
        step
        ( peerUnacknowledgedTxIds peerState
        , peerAvailableTxIds peerState
        , sharedTxIdToKey sharedState
        , sharedKeyToTxId sharedState
        , sharedNextTxKey sharedState
        , sharedTxTable sharedState
        , sharedRetainedTxs sharedState
        , Set.empty
        )
        txidsAndSizes

    peerState'' = peerState {
        peerUnacknowledgedTxIds = peerUnacknowledgedTxIds',
        peerRequestedTxIds = fromIntegral $
            max 0 ( fromIntegral (peerRequestedTxIds peerState) -
                    fromIntegral requestedTxIds :: Int ),
        peerAvailableTxIds = peerAvailableTxIds'
      }

    sharedState' = sharedState {
        sharedTxIdToKey = sharedTxIdToKey',
        sharedKeyToTxId = sharedKeyToTxId',
        sharedNextTxKey = sharedNextTxKey',
        sharedTxTable = sharedTxTable',
        sharedRetainedTxs = sharedRetainedTxs'
      }

    sharedState'' =
      bumpIdlePeerGenerations peersToWake sharedState' {
        sharedTxTable = sharedTxTable',
        sharedRetainedTxs = sharedRetainedTxs',
        sharedGeneration = sharedGeneration sharedState' + 1
      }

    retainUntil = addTime (bufferedTxsMinLifetime policy) now

    -- Process each received txid: classify as retained, in mempool, or new entry.
    step
      :: ( StrictSeq.StrictSeq TxKey
         , IntMap.IntMap SizeInBytes
         , Map.Map txid TxKey
         , IntMap.IntMap txid
         , Int
         , IntMap.IntMap (TxEntry peeraddr)
         , RetainedTxs
         , Set.Set peeraddr
         )
      -> (txid, SizeInBytes)
      -> ( StrictSeq.StrictSeq TxKey
         , IntMap.IntMap SizeInBytes
         , Map.Map txid TxKey
         , IntMap.IntMap txid
         , Int
         , IntMap.IntMap (TxEntry peeraddr)
         , RetainedTxs
         , Set.Set peeraddr
         )
    step
      ( unacknowledgedAcc
      , availableAcc
      , txIdToKeyAcc
      , keyToTxIdAcc
      , nextTxKeyAcc
      , txTableAcc
      , retainedAcc
      , peersAcc
      )
      (txid, txSize)
      | retainedMember k retainedAcc =
          ( unacknowledgedAcc StrictSeq.|> txKey
          , IntMap.delete k availableAcc
          , txIdToKeyAcc'
          , keyToTxIdAcc'
          , nextTxKeyAcc'
          , txTableAcc
          , retainedAcc
          , peersAcc
          )
      | mempoolHasTx txid =
          let wakePeers = case IntMap.lookup k txTableAcc of
                Just txEntry -> Set.union (advertisersForEntryExcept peeraddr txEntry) peersAcc
                Nothing -> peersAcc
          in ( unacknowledgedAcc StrictSeq.|> txKey
             , IntMap.delete k availableAcc
             , txIdToKeyAcc'
             , keyToTxIdAcc'
             , nextTxKeyAcc'
             , IntMap.delete k txTableAcc
             , retainedInsertMax k retainUntil retainedAcc
             , wakePeers
             )
      | otherwise =
          case IntMap.lookup k txTableAcc of
            Nothing ->
              let txEntry = TxEntry {
                              txLease = TxLeased peeraddr (addTime (interTxSpace policy) now),
                              txAdvertisers = Map.singleton peeraddr (TxAdvertiser AckWhenBuffered txSize),
                              txTieBreakSalt = k,
                              txAttempts = Map.empty
                            }
              in ( unacknowledgedAcc StrictSeq.|> txKey
                 , IntMap.insert k txSize availableAcc
                 , txIdToKeyAcc'
                 , keyToTxIdAcc'
                 , nextTxKeyAcc'
                 , IntMap.insert k txEntry txTableAcc
                 , retainedAcc
                 , peersAcc
                 )
            Just txEntry ->
              let txEntry' = addAdvertiser txSize txEntry
              in ( unacknowledgedAcc StrictSeq.|> txKey
                 , IntMap.insert k txSize availableAcc
                 , txIdToKeyAcc'
                 , keyToTxIdAcc'
                 , nextTxKeyAcc'
                 , IntMap.insert k txEntry' txTableAcc
                 , retainedAcc
                 , peersAcc
                 )
      where
        (txKey@(TxKey k), txIdToKeyAcc', keyToTxIdAcc', nextTxKeyAcc') =
          case Map.lookup txid txIdToKeyAcc of
            Just existingKey ->
              (existingKey, txIdToKeyAcc, keyToTxIdAcc, nextTxKeyAcc)
            Nothing ->
              let newKey = TxKey nextTxKeyAcc
              in ( newKey
                 , Map.insert txid newKey txIdToKeyAcc
                 , IntMap.insert nextTxKeyAcc txid keyToTxIdAcc
                 , nextTxKeyAcc + 1
                 )

    addAdvertiser txSize' txEntry@TxEntry { txAdvertisers }
      | Map.member peeraddr txAdvertisers =
          txEntry
      | otherwise =
          txEntry {
            txAdvertisers = Map.insert peeraddr (TxAdvertiser AckWhenResolved txSize') txAdvertisers
          }
