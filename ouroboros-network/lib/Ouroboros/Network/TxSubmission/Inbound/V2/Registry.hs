{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.TxSubmission.Inbound.V2.Registry
  ( SharedTxStateVar
  , PeerTxAPI (..)
  , TxSubmissionCountersVar
  , newSharedTxStateVar
  , newTxSubmissionCountersVar
  , txCountersThreadV2
  , withPeer
    -- Exported for testing
  , updatePeerPhase
  , updatePeerRequestedTxs
  ) where

import Control.Concurrent.Class.MonadSTM qualified as Lazy
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer, traceWith)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set qualified as Set
import Data.Void (Void)
import Data.Word (Word64)

import Ouroboros.Network.Protocol.TxSubmission2.Type
import Ouroboros.Network.TxSubmission.Inbound.V2.Policy (TxDecisionPolicy (..))
import Ouroboros.Network.TxSubmission.Inbound.V2.State qualified as State
import Ouroboros.Network.TxSubmission.Inbound.V2.Types
import Ouroboros.Network.TxSubmission.Mempool.Reader

-- | Shared STM handle for V2 coordination state.
type SharedTxStateVar m peeraddr txid = StrictTVar m (SharedTxState peeraddr txid)

-- | STM handle for V2 monotonic counters.
type TxSubmissionCountersVar m = StrictTVar m TxSubmissionCounters

newSharedTxStateVar
  :: MonadSTM m
  => SharedTxState peeraddr txid
  -> m (SharedTxStateVar m peeraddr txid)
newSharedTxStateVar = newTVarIO

newTxSubmissionCountersVar
  :: MonadSTM m
  => TxSubmissionCounters
  -> m (TxSubmissionCountersVar m)
newTxSubmissionCountersVar = newTVarIO

-- | Periodically emit the current V2 counters when they change.
txCountersThreadV2
  :: (MonadDelay m, MonadSTM m)
  => Tracer m TxSubmissionCounters
  -> TxSubmissionCountersVar m
  -> m Void
txCountersThreadV2 tracer countersVar = go mempty
  where
    countersInterval = 7

    go !previous = do
      threadDelay countersInterval
      current <- readTVarIO countersVar
      if current /= previous
         then traceWith tracer current >> go current
         else go previous

-- | Peer-facing coordination API.
--
-- The peer thread keeps its local protocol state in an local
-- variable. Registry helpers operate only on the shared STM state; any helper
-- that needs peer-local state should take it explicitly as an argument.
data PeerTxAPI m txid tx = PeerTxAPI {
    -- | Wait until either the peer's generation changes from the given
    -- value or the optional timeout expires.
    awaitSharedChange    :: Word64
                         -> Maybe DiffTime
                         -> m (),

    -- | Compute the next action for this peer in non-pipelined mode.
    runNextPeerAction    :: Time
                         -> PeerTxLocalState tx
                         -> m (PeerAction, PeerTxLocalState tx),

    -- | Compute the next action for this peer in pipelined mode.
    runNextPeerActionPipelined :: Time
                               -> PeerTxLocalState tx
                               -> m (PeerAction, PeerTxLocalState tx),

    -- | Process a batch of txids received from this peer.
    applyReceivedTxIds   :: Time
                         -> NumTxIdsToReq
                         -> [(txid, SizeInBytes)]
                         -> PeerTxLocalState tx
                         -> m (PeerTxLocalState tx),

    -- | Process a batch of tx bodies received from this peer.
    applyReceivedTxs     :: Time
                         -> [(txid, tx)]
                         -> PeerTxLocalState tx
                         -> m (Int, PeerTxLocalState tx),

    -- | Mark txs as submitted to the mempool and update shared state.
    applySubmittedTxs    :: Time
                         -> [TxKey]
                         -> [TxKey]
                         -> PeerTxLocalState tx
                         -> m (PeerTxLocalState tx),

    -- | Update the peer's rejection score based on the number of txs rejected
    -- by the mempool, or late/missing delivieries.
    countRejectedTxs     :: Time
                         -> Int
                         -> m Double,

    -- | Resolve txids and advertised sizes for a batch of tx keys to request.
    resolveTxRequest     :: PeerTxLocalState tx
                         -> [TxKey]
                         -> m (Map txid SizeInBytes),
    -- | Resolve buffered tx bodies into full submission records.
    resolveBufferedTxs   :: PeerTxLocalState tx
                         -> [TxKey]
                         -> m [(TxKey, txid, tx)],
    -- | Mark the given tx keys as entering mempool submission phase in shared
    -- state.
    startSubmittingTxs   :: [TxKey] -> m (),

    -- | Add a delta to the V2 monotonic counters.
    addCounters          :: TxSubmissionCounters -> m ()
  }

--
-- | A bracket function which registers / de-registers a new peer in
-- `SharedTxStateVar`,  which exposes `PeerTxStateAPI`.
-- `PeerTxStateAPI` is only safe inside the  `withPeer` scope.
--
withPeer
  :: forall peeraddr txid tx idx m a.
     ( MonadMask m
     , MonadTimer m
     , Ord peeraddr
     , Ord txid
     )
  => TxDecisionPolicy
  -> TxSubmissionMempoolReader txid tx idx m
  -> SharedTxStateVar m peeraddr txid
  -> TxSubmissionCountersVar m
  -> peeraddr
  -> (PeerTxAPI m txid tx -> m a)
  -> m a
withPeer policy TxSubmissionMempoolReader { mempoolGetSnapshot } sharedStateVar countersVar peeraddr io =
    bracket
      (do
          now <- getMonotonicTime
          atomically $ modifyTVar sharedStateVar (registerPeer now)
          pure PeerTxAPI {
              awaitSharedChange = awaitSharedChangeImp sharedStateVar peeraddr
            , runNextPeerAction = runNextPeerActionImp policy sharedStateVar peeraddr
            , runNextPeerActionPipelined = runNextPeerActionPipelinedImp policy sharedStateVar
                                             peeraddr
            , applyReceivedTxIds = applyReceivedTxIdsImp policy mempoolGetSnapshot sharedStateVar
                                     peeraddr
            , applyReceivedTxs = applyReceivedTxsImp policy mempoolGetSnapshot sharedStateVar
                                   countersVar peeraddr
            , applySubmittedTxs = applySubmittedTxsImp policy sharedStateVar peeraddr
            , countRejectedTxs = countRejectedTxsImp policy sharedStateVar peeraddr
            , resolveTxRequest = resolveTxRequestImp sharedStateVar
            , resolveBufferedTxs = resolveBufferedTxsImp sharedStateVar
            , startSubmittingTxs = atomically . modifyTVar sharedStateVar .
                                     State.markSubmittingTxs peeraddr
            , addCounters = \delta -> atomically $ modifyTVar countersVar (<> delta)
            }
      )
      (\_ -> atomically $ modifyTVar sharedStateVar unregisterPeer)
      io
  where
    registerPeer :: Time -> SharedTxState peeraddr txid -> SharedTxState peeraddr txid
    registerPeer now st@SharedTxState { sharedPeers, sharedGeneration } =
      st {
        sharedPeers = Map.insert peeraddr sharedPeerState sharedPeers,
        sharedGeneration = sharedGeneration + 1
      }
      where
        sharedPeerState = SharedPeerState {
            sharedPeerPhase = PeerIdle,
            sharedPeerScore = emptyPeerScore now,
            sharedPeerGeneration = 0,
            sharedPeerRequestedTxBatches = 0,
            sharedPeerRequestedTxsSize = 0
          }

    unregisterPeer :: SharedTxState peeraddr txid -> SharedTxState peeraddr txid
    unregisterPeer st@SharedTxState { sharedPeers, sharedTxTable, sharedRetainedTxs, sharedTxIdToKey, sharedKeyToTxId, sharedGeneration } =
      bumpIdlePeerGenerations peersToWake $ st {
        sharedPeers = sharedPeers',
        sharedTxTable = sharedTxTable',
        sharedRetainedTxs = sharedRetainedTxs,
        sharedTxIdToKey = Map.filter (\txKey -> IntSet.member (unTxKey txKey) liveKeys) sharedTxIdToKey,
        sharedKeyToTxId = IntMap.restrictKeys sharedKeyToTxId liveKeys,
        sharedGeneration = sharedGeneration + 1
      }
      where
        sharedPeers' = Map.delete peeraddr sharedPeers

        (sharedTxTable', peersToWake) =
          IntMap.foldlWithKey' scrubOne (IntMap.empty, Set.empty) sharedTxTable
        liveKeys = IntMap.keysSet sharedTxTable' `IntSet.union` retainedKeysSet sharedRetainedTxs

        scrubOne (txTableAcc, wakeAcc) k txEntry =
          let touched = txTouchesPeer txEntry
              txEntry' = scrubTxEntry txEntry
          in if txLive txEntry'
                then ( IntMap.insert k txEntry' txTableAcc
                     , if touched
                          then Set.union wakeAcc (Set.delete peeraddr (Map.keysSet (txAdvertisers txEntry')))
                          else wakeAcc
                     )
                else (txTableAcc, wakeAcc)

        scrubTxEntry txEntry@TxEntry { txLease, txAdvertisers, txAttempts } =
          txEntry {
            txLease = scrubLease txLease,
            txAdvertisers = Map.delete peeraddr txAdvertisers,
            txAttempts = Map.delete peeraddr txAttempts
          }

        scrubLease (TxLeased owner leaseUntil)
          | owner == peeraddr = TxClaimable
          | otherwise         = TxLeased owner leaseUntil
        scrubLease TxClaimable = TxClaimable

        txTouchesPeer TxEntry { txLease, txAdvertisers, txAttempts } =
             leaseOwnedByPeer txLease
          || Map.member peeraddr txAdvertisers
          || Map.member peeraddr txAttempts

        txLive TxEntry { txLease, txAdvertisers, txAttempts } =
             leaseLive txLease
          || not (Map.null txAdvertisers)
          || not (Map.null txAttempts)

        leaseOwnedByPeer (TxLeased owner _) = owner == peeraddr
        leaseOwnedByPeer TxClaimable        = False

        leaseLive TxClaimable    = False
        leaseLive (TxLeased _ _) = True

-- | Wait until either the peer's generation changes from the given
-- value or the optional timeout expires.
--
--  Used by idle peers to avoid busy-waiting while still being woken when relevant cross-peer
--  state (such as lease expiries or new tx advertisements) changes.
awaitSharedChangeImp :: ( MonadTimer m
                        , Ord peeraddr )
                     => SharedTxStateVar m peeraddr txid
                     -> peeraddr
                     -> Word64
                     -> Maybe DiffTime
                     -> m ()
awaitSharedChangeImp sharedStateVar peeraddr generation mDelay =
  case mDelay of
       Nothing ->
         atomically $ do
           sharedState <- readTVar sharedStateVar
           let generation' = peerGenerationOf peeraddr sharedState
           check (generation' /= generation)
       Just delay -> do
         delayVar <- registerDelay delay
         atomically $ do
           sharedState <- readTVar sharedStateVar
           let generation' = peerGenerationOf peeraddr sharedState
           expired <- Lazy.readTVar delayVar
           check (generation' /= generation || expired)

-- | Avoid rewriting the shared TVar when the pure state step made no shared
-- change. Callers use 'sharedGeneration' as the dirty bit for shared state.
writeSharedStateIfChanged :: MonadSTM m
                          => SharedTxStateVar m peeraddr txid
                          -> Word64
                          -> SharedTxState peeraddr txid
                          -> STM m ()
writeSharedStateIfChanged sharedStateVar sharedGeneration0 sharedState'
  | sharedGeneration sharedState' == sharedGeneration0 = pure ()
  | otherwise = writeTVar sharedStateVar sharedState'

-- | Compute the next action for this peer in non-pipelined mode.
--
-- Returns the selected 'PeerAction', an updated peer-local state, and applies
-- changes to shared state (such as lease/advertiser coordination).
-- Called from the main peer loop when not handling pipelined replies.
runNextPeerActionImp :: ( MonadSTM m
                        , Ord peeraddr
                        , Ord txid )
                     => TxDecisionPolicy
                     -> SharedTxStateVar m peeraddr txid
                     -> peeraddr
                     -> Time
                     -> PeerTxLocalState tx
                     -> m (PeerAction, PeerTxLocalState tx)
runNextPeerActionImp policy sharedStateVar peeraddr now peerState = atomically $ do
  sharedState <- readTVar sharedStateVar
  let sharedGeneration0 = sharedGeneration sharedState
      (peerAction, peerState', sharedState') = State.nextPeerAction now policy peeraddr
                                                 peerState sharedState
      sharedState''  = updatePeerPhase peeraddr (peerPhaseForActionIdle peerAction) sharedState'
      sharedState''' = updatePeerRequestedTxs policy peeraddr peerState' sharedState''
  writeSharedStateIfChanged sharedStateVar sharedGeneration0 sharedState'''
  return (peerAction, peerState')

-- | Compute the next action for this peer in pipelined mode.
--
-- Similar to 'runNextPeerAction' but allows pipelined txid request messages where
-- both acknowledgments and requests can be sent together. Used when waiting for
-- pipelined protocol replies.
runNextPeerActionPipelinedImp :: ( MonadSTM m
                                  , Ord peeraddr
                                  , Ord txid )
                              => TxDecisionPolicy
                              -> SharedTxStateVar m peeraddr txid
                              -> peeraddr
                              -> Time
                              -> PeerTxLocalState tx
                              -> m (PeerAction, PeerTxLocalState tx)
runNextPeerActionPipelinedImp policy sharedStateVar peeraddr now peerState = atomically $ do
  sharedState <- readTVar sharedStateVar
  let sharedGeneration0 = sharedGeneration sharedState
      (peerAction, peerState', sharedState') = State.nextPeerActionPipelined now policy
                                                 peeraddr peerState sharedState
      sharedState'' = updatePeerPhase peeraddr
                        (peerPhaseForActionPipelined peeraddr peerAction sharedState')
                        sharedState'
      sharedState''' = updatePeerRequestedTxs policy peeraddr peerState' sharedState''
  writeSharedStateIfChanged sharedStateVar sharedGeneration0 sharedState'''
  return (peerAction, peerState')

-- | Process a batch of txids received from this peer.
--
-- Interns new txids into the shared state, updates the peer's unacknowledged queue,
-- handles mempool fast-path for already-known txids, and sets up initial lease
-- ownership for first advertisers. Returns updated peer-local state.
applyReceivedTxIdsImp :: ( MonadSTM m
                         , Ord peeraddr
                         , Ord txid )
                      => TxDecisionPolicy
                      -> STM m (MempoolSnapshot txid tx idx)
                      -> SharedTxStateVar m peeraddr txid
                      -> peeraddr
                      -> Time
                      -> NumTxIdsToReq
                      -> [(txid, SizeInBytes)]
                      -> PeerTxLocalState tx
                      -> m (PeerTxLocalState tx)
applyReceivedTxIdsImp policy mempoolGetSnapshot sharedStateVar peeraddr now txIdsToReq
                      txidsAndSizes peerState = atomically $ do
  MempoolSnapshot { mempoolHasTx } <- mempoolGetSnapshot
  sharedState <- readTVar sharedStateVar
  let sharedGeneration0 = sharedGeneration sharedState
      (peerState', sharedState') = State.handleReceivedTxIds mempoolHasTx now policy peeraddr
                                     txIdsToReq txidsAndSizes peerState sharedState
      sharedState'' = updatePeerRequestedTxs policy peeraddr peerState' sharedState'
  writeSharedStateIfChanged sharedStateVar sharedGeneration0 sharedState''
  return peerState'

-- | Process a batch of tx bodies received from this peer.
--
-- Buffers the received bodies in peer-local state, updates shared advertiser tracking,
-- and handles omitted bodies by releasing ownership so other advertisers may claim them.
-- Returns the combined penalty count for bodies that were already resolved locally or
-- missing from the reply, together with the updated peer-local state.
applyReceivedTxsImp :: ( MonadSTM m
                       , Ord peeraddr
                       , Ord txid )
                    => TxDecisionPolicy
                    -> STM m (MempoolSnapshot txid tx idx)
                    -> SharedTxStateVar m peeraddr txid
                    -> TxSubmissionCountersVar m
                    -> peeraddr
                    -> Time
                    -> [(txid, tx)]
                    -> PeerTxLocalState tx
                    -> m (Int, PeerTxLocalState tx)
applyReceivedTxsImp policy mempoolGetSnapshot sharedStateVar countersVar peeraddr now txs
                    peerState = atomically $ do
  MempoolSnapshot { mempoolHasTx } <- mempoolGetSnapshot
  sharedState <- readTVar sharedStateVar
  let (omittedCount, lateCount, peerState', sharedState') =
          State.handleReceivedTxs mempoolHasTx now policy peeraddr txs peerState sharedState
      sharedState'' =
                        updatePeerRequestedTxs policy peeraddr peerState' sharedState'
  writeTVar sharedStateVar sharedState''
  modifyTVar countersVar (<> mempty {
                      txRepliesReceived = 1,
                      txsReceived       = fromIntegral (length txs),
                      txsOmitted        = fromIntegral omittedCount,
                      lateBodies        = fromIntegral lateCount
                    })
  return (omittedCount + lateCount, peerState')

-- | Mark txs as submitted to the mempool and update shared state.
--
-- Takes the keys that were resolved (either already-in-mempool or successfully
-- submitted) and the keys that were rejected, updating lease ownership, advertiser
-- ack states, and peer rejection scores.
-- Returns updated peer-local state.
applySubmittedTxsImp :: ( MonadSTM m
                       , Ord peeraddr
                       , Ord txid )
                     => TxDecisionPolicy
                     -> SharedTxStateVar m peeraddr txid
                     -> peeraddr
                     -> Time
                     -> [TxKey]
                     -> [TxKey]
                     -> PeerTxLocalState tx
                     -> m (PeerTxLocalState tx)
applySubmittedTxsImp policy sharedStateVar peeraddr now acceptedTxs rejectedTxs peerState =
  atomically $ do
    sharedState <- readTVar sharedStateVar
    let (peerState', sharedState') = State.handleSubmittedTxs now policy peeraddr acceptedTxs
                                       rejectedTxs peerState sharedState
        sharedState'' = updatePeerRequestedTxs policy peeraddr peerState' sharedState'
    writeTVar sharedStateVar sharedState''
    return peerState'

-- | Update the peer's rejection score based on the number of txs rejected
-- by the mempool.
-- Returns the new score value for tracing. The score
-- decays over time and affects fallback peer selection when leases expire.
countRejectedTxsImp :: ( MonadSTM m
                       , Ord peeraddr)
                    => TxDecisionPolicy
                    -> SharedTxStateVar m peeraddr txid
                    -> peeraddr
                    -> Time
                    -> Int
                    -> m Double
countRejectedTxsImp TxDecisionPolicy { scoreRate, scoreMax } sharedStateVar peeraddr now
                    rejectedCount = atomically $ stateTVar sharedStateVar $
  updatePeerRejects (fromIntegral rejectedCount)
  where
    updatePeerRejects n sharedState =
      case Map.lookup peeraddr (sharedPeers sharedState) of
           Nothing -> (0, sharedState) -- TODO this is an invariant violation
           Just sharedPeerState@SharedPeerState { sharedPeerScore } ->
             let sharedPeerScore' = updateRejects n sharedPeerScore
                 sharedPeerState' = sharedPeerState { sharedPeerScore = sharedPeerScore' }
                 sharedState' = sharedState {
                     sharedPeers = Map.insert peeraddr sharedPeerState' (sharedPeers sharedState),
                     sharedGeneration = sharedGeneration sharedState + 1
                   } in
             (peerScoreValue sharedPeerScore', sharedState')

    updateRejects 0 ps@PeerScore { peerScoreValue = 0 } = ps { peerScoreTs = now }
    updateRejects n ps@PeerScore { peerScoreValue, peerScoreTs } =
        let duration = diffTime now peerScoreTs
            !drain = realToFrac duration * scoreRate
            !drained = max 0 (peerScoreValue - drain) in
        ps { peerScoreValue = min scoreMax (drained + n)
           , peerScoreTs = now }

-- | Resolve txids and advertised sizes for a batch of tx keys to request.
--
-- Looks up the real txid and size from peer-local state for building the
-- protocol message. Used before sending 'MsgRequestTxs'.
resolveTxRequestImp :: ( MonadSTM m
                       , Ord txid )
                    => SharedTxStateVar m peeraddr txid
                    -> PeerTxLocalState tx
                    -> [TxKey]
                    -> m (Map txid SizeInBytes)
resolveTxRequestImp sharedStateVar peerState txKeys = atomically $ do
  sharedState <- readTVar sharedStateVar
  return $ Map.fromList (fmap (resolveOne sharedState) txKeys)
  where
    resolveOne sharedState key@(TxKey k) =
      ( resolveTxKey sharedState key
      , case IntMap.lookup k (peerAvailableTxIds peerState) of
             Just txSize -> txSize
             Nothing     -> error "TxSubmission.V2.resolveTxRequestImp: missing tx size"
      )

-- | Resolve buffered tx bodies into full submission records.
--
-- Takes tx keys that have been downloaded and buffered locally, looks up their txids and
-- body values from peer-local state, and returns triples ready for mempool submission.
-- Used when submitting txs after body collection.
resolveBufferedTxsImp :: ( MonadSTM m
                         )
                       => SharedTxStateVar m peeraddr txid
                       -> PeerTxLocalState tx
                       -> [TxKey]
                       -> m [(TxKey, txid, tx)]
resolveBufferedTxsImp sharedStateVar peerState txKeys = atomically $ do
  sharedState <- readTVar sharedStateVar
  return $ fmap (resolveOne sharedState) txKeys
  where
    resolveOne sharedState key@(TxKey k) =
      ( key
      , resolveTxKey sharedState key
      , case IntMap.lookup k (peerDownloadedTxs peerState) of
             Just tx -> tx
             Nothing -> error "TxSubmission.V2.resolveBufferedTxsImp: missing buffered tx"
      )

-- | Update a peer's phase.
--
-- A phase change always bumps the shared generation. In addition:
--
-- * When a peer becomes 'PeerIdle', bump that peer's own generation so a
--   'PeerDoNothing' action computed before the phase change does not put that
--   same peer thread to sleep on a stale generation. This makes its next
--   'awaitSharedChange' return immediately and re-run scheduling as an idle
--   claimant.
-- * When a peer leaves 'PeerIdle', bump the generations of other advertisers
--   for txs advertised by that peer. Claim-owner selection only considers idle
--   peers, so removing one idle advertiser can change which remaining idle
--   peer should wake and claim a tx.
updatePeerPhase
  :: Ord peeraddr
  => peeraddr
  -> PeerPhase
  -> SharedTxState peeraddr txid
  -> SharedTxState peeraddr txid
updatePeerPhase peeraddr peerPhaseNew st@SharedTxState { sharedPeers, sharedGeneration } =
  case Map.lookup peeraddr sharedPeers of
       Just sharedPeerState ->
         let peerPhaseOld = sharedPeerPhase sharedPeerState in
         if peerPhaseOld /= peerPhaseNew
            then
              let st' = st { sharedPeers = Map.insert peeraddr
                               (sharedPeerState { sharedPeerPhase = peerPhaseNew }) sharedPeers
                           , sharedGeneration = sharedGeneration + 1 } in
              bumpIdlePeerGenerations (phaseWakePeers peerPhaseOld) st'
            else st
       _ -> st -- TODO error?
  where
    phaseWakePeers peerPhaseOld
      | peerPhaseOld /= PeerIdle
      , peerPhaseNew == PeerIdle = Set.singleton peeraddr
      | peerPhaseOld == PeerIdle
      , peerPhaseNew /= PeerIdle = advertisersForPeerTxsExcept peeraddr st
      | otherwise = Set.empty


-- | Update the peer's shared TX state so that it is in sync with its local state.
updatePeerRequestedTxs
  :: Ord peeraddr
  => TxDecisionPolicy
  -> peeraddr
  -> PeerTxLocalState tx
  -> SharedTxState peeraddr txid
  -> SharedTxState peeraddr txid
updatePeerRequestedTxs _policy peeraddr peerState
                       st@SharedTxState { sharedPeers, sharedGeneration } =
  case Map.lookup peeraddr sharedPeers of
       Just sharedPeerState ->
         if sharedPeerRequestedTxBatches sharedPeerState /= requestedTxBatches
              || sharedPeerRequestedTxsSize sharedPeerState /= requestedTxsSize
            then
              let sharedPeerState' = sharedPeerState {
                                       sharedPeerRequestedTxBatches = requestedTxBatches
                                     , sharedPeerRequestedTxsSize = requestedTxsSize }
                  sharedPeers' = Map.insert peeraddr sharedPeerState' sharedPeers in
              st { sharedPeers = sharedPeers'
                 , sharedGeneration = sharedGeneration + 1 }
            else st
       _ -> st -- TODO: error?
  where
    requestedTxBatches = StrictSeq.length (peerRequestedTxBatches peerState)
    requestedTxsSize = peerRequestedTxsSize peerState

advertisersForPeerTxsExcept
  :: Ord peeraddr
  => peeraddr
  -> SharedTxState peeraddr txid
  -> Set.Set peeraddr
advertisersForPeerTxsExcept peeraddr SharedTxState { sharedTxTable } =
    IntMap.foldl' collect Set.empty sharedTxTable
  where
    collect peers TxEntry { txAdvertisers }
      | Map.member peeraddr txAdvertisers =
          Set.union peers (Set.delete peeraddr (Map.keysSet txAdvertisers))
      | otherwise =
          peers

peerPhaseForActionIdle :: PeerAction -> PeerPhase
peerPhaseForActionIdle peerAction =
    case peerAction of
         PeerDoNothing {}    -> PeerIdle
         PeerSubmitTxs {}    -> PeerSubmittingToMempool
         PeerRequestTxs {}   -> PeerWaitingTxs
         PeerRequestTxIds {} -> PeerWaitingTxIds

peerPhaseForActionPipelined
  :: Ord peeraddr
  => peeraddr
  -> PeerAction
  -> SharedTxState peeraddr txid
  -> PeerPhase
peerPhaseForActionPipelined peeraddr peerAction sharedState =
    case peerAction of
         PeerDoNothing {}    -> peerPhaseOf peeraddr sharedState
         PeerSubmitTxs {}    -> PeerSubmittingToMempool
         PeerRequestTxs {}   -> PeerWaitingTxs
         PeerRequestTxIds {} -> PeerWaitingTxIds
  where
    peerPhaseOf peer st =
      maybe PeerIdle sharedPeerPhase (Map.lookup peer (sharedPeers st))
