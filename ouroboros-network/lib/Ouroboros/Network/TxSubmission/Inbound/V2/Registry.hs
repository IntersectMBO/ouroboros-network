{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.TxSubmission.Inbound.V2.Registry
  ( SharedTxStateVar
  , PeerTxInFlightRegistry
  , PeerTxAPI (..)
  , TxSubmissionCountersVar
  , newSharedTxStateVar
  , newPeerTxInFlightRegistry
  , newTxSubmissionCountersVar
  , txCountersThreadV2
  , withPeer
  ) where

import Control.Concurrent.Class.MonadSTM qualified as Lazy
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad (when)
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer, traceWith)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Void (Void)
import Data.Word (Word64)

import Ouroboros.Network.Protocol.TxSubmission2.Type
import Ouroboros.Network.Tx (HasRawTxId)
import Ouroboros.Network.TxSubmission.Inbound.V2.Policy (TxDecisionPolicy (..))
import Ouroboros.Network.TxSubmission.Inbound.V2.State qualified as State
import Ouroboros.Network.TxSubmission.Inbound.V2.Types
import Ouroboros.Network.TxSubmission.Mempool.Reader

-- | Shared STM handle for V2 coordination state.
type SharedTxStateVar m peeraddr txid = StrictTVar m (SharedTxState peeraddr txid)

-- | STM handle for V2 monotonic counters.
type TxSubmissionCountersVar m = StrictTVar m TxSubmissionCounters

-- | Per-peer in-flight TVar.
type PeerTxInFlightVar m = StrictTVar m PeerTxInFlight

-- | Registry of every live peer's 'PeerTxInFlightVar'.
--
-- 'withPeer' adds the peer's TVar on bracket-enter and removes it on
-- bracket-exit (after scrubbing any contributions the peer still has
-- to shared state).  The sweep snapshots this map to compute the
-- @liveAdvertised@ union without touching peer-local protocol state.
type PeerTxInFlightRegistry m peeraddr =
       StrictTVar m (Map peeraddr (PeerTxInFlightVar m))

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

newPeerTxInFlightRegistry
  :: MonadSTM m
  => m (PeerTxInFlightRegistry m peeraddr)
newPeerTxInFlightRegistry = newTVarIO Map.empty

-- | Central bookkeeping thread for V2.
--
-- Wakes every @'bufferedTxsMinLifetime' policy \/ 4@ seconds (capped
-- between 100 ms and 1 s) to run 'State.sweepSharedState' on the
-- shared tx state.  The sweep needs the union of every live peer's
-- 'pifAdvertised' to decide which entries are still wanted; the
-- registry is read inside the same STM transaction as the sweep so
-- the snapshot is coherent.  On a slower cadence (every
-- 'countersInterval' seconds of elapsed time) it also emits the
-- current counters when they differ from the last emission.
txCountersThreadV2
  :: forall m peeraddr txid.
     (MonadDelay m, MonadSTM m, HasRawTxId txid)
  => TxDecisionPolicy
  -> Tracer m TxSubmissionCounters
  -> TxSubmissionCountersVar m
  -> SharedTxStateVar m peeraddr txid
  -> PeerTxInFlightRegistry m peeraddr
  -> m Void
txCountersThreadV2 policy tracer countersVar sharedStateVar registry = do
    now <- getMonotonicTime
    go mempty (addTime countersInterval now)
  where
    sweepInterval :: DiffTime
    sweepInterval = max 0.1 (min 1 (bufferedTxsMinLifetime policy / 4))

    countersInterval :: DiffTime
    countersInterval = 7

    go !previous !nextEmitAt = do
      threadDelay sweepInterval
      now <- getMonotonicTime
      atomically $ do
        liveReferences <- snapshotLiveReferences registry
        modifyTVar sharedStateVar (State.sweepSharedState now liveReferences)
      if now >= nextEmitAt
         then do
           current <- readTVarIO countersVar
           when (current /= previous) $ traceWith tracer current
           go current (addTime countersInterval now)
         else go previous nextEmitAt

-- | Read every live peer's 'pifAdvertised' and 'pifAcksPending' and
-- union them.  This is the set of keys still referenced by some peer
-- (advertised for fetch, or held in @peerUnacknowledgedTxIds@ awaiting
-- ack) and is used by the sweep to know which lookup table entries
-- can be safely reclaimed.
snapshotLiveReferences
  :: MonadSTM m
  => PeerTxInFlightRegistry m peeraddr
  -> STM m IntSet
snapshotLiveReferences registry = do
    peers <- readTVar registry
    foldr step (pure IntSet.empty) (Map.elems peers)
  where
    step var k = do
      pif <- readTVar var
      acc <- k
      pure $!  IntSet.union (pifAdvertised pif)
             $ IntSet.union (pifAcksPending pif) acc

-- | Peer-facing coordination API.
--
-- The peer thread keeps its local protocol state in a local
-- variable. Registry helpers operate only on the shared STM state
-- and the per-peer 'PeerTxInFlight' TVar (closure-captured); any
-- helper that needs peer-local state should take it explicitly as an
-- argument.
data PeerTxAPI m txid tx = PeerTxAPI {
    -- | Wait until either 'sharedGeneration' moves past the given
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

    -- | Resolve txids and advertised sizes for a batch of tx keys to request.
    resolveTxRequest     :: PeerTxLocalState tx
                         -> [TxKey]
                         -> m (Map txid SizeInBytes),
    -- | Resolve buffered tx bodies into full submission records.
    resolveBufferedTxs   :: PeerTxLocalState tx
                         -> [TxKey]
                         -> m [(TxKey, txid, tx)],

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
     , HasRawTxId txid
     )
  => TxDecisionPolicy
  -> TxSubmissionMempoolReader txid tx idx m
  -> SharedTxStateVar m peeraddr txid
  -> PeerTxInFlightRegistry m peeraddr
  -> TxSubmissionCountersVar m
  -> peeraddr
  -> (PeerTxAPI m txid tx -> m a)
  -> m a
withPeer policy TxSubmissionMempoolReader { mempoolGetSnapshot }
         sharedStateVar registry countersVar peeraddr io =
    bracket acquire release run
  where
    acquire = do
      peerInFlightVar <- newTVarIO emptyPeerTxInFlight
      atomically $ modifyTVar registry (Map.insert peeraddr peerInFlightVar)
      pure peerInFlightVar

    release peerInFlightVar = do
      now <- getMonotonicTime
      atomically $ do
        pif <- readTVar peerInFlightVar
        modifyTVar sharedStateVar (scrubFromPeerInFlight peeraddr now pif)
        modifyTVar registry (Map.delete peeraddr)

    run peerInFlightVar = io PeerTxAPI {
          awaitSharedChange = awaitSharedChangeImp sharedStateVar
        , runNextPeerAction = runNextPeerActionImp policy sharedStateVar
                                peerInFlightVar countersVar peeraddr
        , runNextPeerActionPipelined = runNextPeerActionPipelinedImp policy
                                         sharedStateVar peerInFlightVar
                                         countersVar peeraddr
        , applyReceivedTxIds = applyReceivedTxIdsImp policy mempoolGetSnapshot
                                 sharedStateVar peerInFlightVar countersVar
        , applyReceivedTxs = applyReceivedTxsImp policy mempoolGetSnapshot
                               sharedStateVar peerInFlightVar countersVar peeraddr
        , applySubmittedTxs = applySubmittedTxsImp policy sharedStateVar
                                peerInFlightVar countersVar peeraddr
        , resolveTxRequest = resolveTxRequestImp sharedStateVar
        , resolveBufferedTxs = resolveBufferedTxsImp sharedStateVar
        , addCounters = \delta -> atomically $ modifyTVar countersVar (<> delta)
        }

-- | Reverse this peer's still-outstanding contributions to the shared
-- 'TxEntry' counters.  Run by the bracket finalizer; uses the per-peer
-- TVar snapshot taken at exit time.
--
-- 'pifLeased' is best-effort (another peer can steal the lease in the
-- meantime), so the lease release verifies the entry still names this
-- peer as owner before claiming.
scrubFromPeerInFlight
  :: Eq peeraddr
  => peeraddr
  -> Time
  -> PeerTxInFlight
  -> SharedTxState peeraddr txid
  -> SharedTxState peeraddr txid
scrubFromPeerInFlight peeraddr now pif st
  | nothingToDo = st
  | otherwise   = st {
        sharedTxTable    = sharedTxTable',
        sharedGeneration = sharedGeneration st + 1
      }
  where
    nothingToDo =
         IntSet.null (pifLeased pif)
      && IntSet.null (pifAttempting pif)
      && IntSet.null (pifSubmitting pif)

    sharedTxTable' =
        IntSet.foldl' clearSubmission
          (IntSet.foldl' decAttempt
             (IntSet.foldl' releaseLease (sharedTxTable st)
                (pifLeased pif))
             (pifAttempting pif))
          (pifSubmitting pif)

    releaseLease tbl k =
      IntMap.adjust
        (\entry -> case txLease entry of
            TxLeased owner _ | owner == peeraddr ->
              entry { txLease = TxClaimable now }
            _ -> entry)
        k tbl

    decAttempt tbl k =
      IntMap.adjust
        (\entry -> entry { txAttempt = max 0 (txAttempt entry - 1) })
        k tbl

    clearSubmission tbl k =
      IntMap.adjust
        (\entry -> entry { txInSubmission = False })
        k tbl

-- | Wait until either 'sharedGeneration' moves past the given value or the
-- optional timeout expires.
--
-- Used by idle peers to avoid busy-waiting while still being woken when
-- shared state changes (lease expiries, new tx advertisements, mempool
-- resolutions).  A spurious wake on a change that doesn't grant this peer
-- new work is harmless: the peer immediately re-runs 'nextPeerAction',
-- selects 'PeerDoNothing' again, and goes back to sleep on the new
-- generation value.
awaitSharedChangeImp :: MonadTimer m
                     => SharedTxStateVar m peeraddr txid
                     -> Word64
                     -> Maybe DiffTime
                     -> m ()
awaitSharedChangeImp sharedStateVar generation mDelay =
  case mDelay of
       Nothing ->
         atomically $ do
           sharedState <- readTVar sharedStateVar
           check (sharedGeneration sharedState /= generation)
       Just delay -> do
         delayVar <- registerDelay delay
         atomically $ do
           sharedState <- readTVar sharedStateVar
           expired <- Lazy.readTVar delayVar
           check (sharedGeneration sharedState /= generation || expired)

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

-- | Avoid rewriting the per-peer TVar when nothing changed.
writePeerInFlightIfChanged :: MonadSTM m
                           => PeerTxInFlightVar m
                           -> PeerTxInFlight
                           -> PeerTxInFlight
                           -> STM m ()
writePeerInFlightIfChanged var before after
  | before == after = pure ()
  | otherwise       = writeTVar var after

-- | Update the counters for the action chosen by the peer scheduler.
--
updateCountersForAction :: MonadSTM m
                        => TxSubmissionCountersVar m
                        -> PeerAction
                        -> STM m ()
updateCountersForAction countersVar peerAction =
  case peerAction of
    PeerRequestTxIds flavour txIdsToAck txIdsToReq
      | txIdsToAck /= 0 || txIdsToReq /= 0 ->
          modifyTVar countersVar (<> mempty
            { txIdMessagesSent        = 1
            , txIdsRequested          = fromIntegral txIdsToReq
            , txIdBlockingReqsSent  = case flavour of
                                           TxIdsBlockingReq  -> 1
                                           TxIdsPipelinedReq -> 0
            , txIdPipelinedReqsSent = case flavour of
                                           TxIdsBlockingReq  -> 0
                                           TxIdsPipelinedReq -> 1
            })
    PeerRequestTxs txKeys ->
      modifyTVar countersVar (<> mempty { txMessagesSent = 1
                                        , txsRequested   = fromIntegral (length txKeys) })
    _ -> pure ()

-- | Compute the next action for this peer in non-pipelined mode.
runNextPeerActionImp :: ( MonadSTM m
                        , Ord peeraddr )
                     => TxDecisionPolicy
                     -> SharedTxStateVar m peeraddr txid
                     -> PeerTxInFlightVar m
                     -> TxSubmissionCountersVar m
                     -> peeraddr
                     -> Time
                     -> PeerTxLocalState tx
                     -> m (PeerAction, PeerTxLocalState tx)
runNextPeerActionImp policy sharedStateVar peerInFlightVar countersVar peeraddr
                     now peerState = atomically $ do
  sharedState <- readTVar sharedStateVar
  peerInFlight <- readTVar peerInFlightVar
  let sharedGeneration0 = sharedGeneration sharedState
      (peerAction, peerState', peerInFlight', sharedState') =
        State.nextPeerAction now policy peeraddr peerState peerInFlight sharedState
  writeSharedStateIfChanged sharedStateVar sharedGeneration0 sharedState'
  writePeerInFlightIfChanged peerInFlightVar peerInFlight peerInFlight'
  updateCountersForAction countersVar peerAction
  return (peerAction, peerState')

-- | Compute the next action for this peer in pipelined mode.
runNextPeerActionPipelinedImp :: ( MonadSTM m
                                  , Ord peeraddr )
                              => TxDecisionPolicy
                              -> SharedTxStateVar m peeraddr txid
                              -> PeerTxInFlightVar m
                              -> TxSubmissionCountersVar m
                              -> peeraddr
                              -> Time
                              -> PeerTxLocalState tx
                              -> m (PeerAction, PeerTxLocalState tx)
runNextPeerActionPipelinedImp policy sharedStateVar peerInFlightVar countersVar
                              peeraddr now peerState =
    atomically $ do
      sharedState <- readTVar sharedStateVar
      peerInFlight <- readTVar peerInFlightVar
      let sharedGeneration0 = sharedGeneration sharedState
          (peerAction, peerState', peerInFlight', sharedState') =
            State.nextPeerActionPipelined now policy peeraddr peerState
                                          peerInFlight sharedState
      writeSharedStateIfChanged sharedStateVar sharedGeneration0 sharedState'
      writePeerInFlightIfChanged peerInFlightVar peerInFlight peerInFlight'
      updateCountersForAction countersVar peerAction
      return (peerAction, peerState')

-- | Process a batch of txids received from this peer.
applyReceivedTxIdsImp :: ( MonadSTM m
                         , HasRawTxId txid )
                      => TxDecisionPolicy
                      -> STM m (MempoolSnapshot txid tx idx)
                      -> SharedTxStateVar m peeraddr txid
                      -> PeerTxInFlightVar m
                      -> TxSubmissionCountersVar m
                      -> Time
                      -> NumTxIdsToReq
                      -> [(txid, SizeInBytes)]
                      -> PeerTxLocalState tx
                      -> m (PeerTxLocalState tx)
applyReceivedTxIdsImp policy mempoolGetSnapshot sharedStateVar peerInFlightVar
                      countersVar now txIdsToReq txidsAndSizes peerState = do
  -- Snapshot the mempool outside the per-peer STM transaction so mempool
  -- writers don't kick the hot path into retries.  Stale answers are
  -- benign: a false positive delays re-fetch by 'bufferedTxsMinLifetime'
  -- via the retained set; a false negative wastes one body fetch that
  -- 'handleReceivedTxs' will reclassify as late.
  MempoolSnapshot { mempoolHasTx } <- atomically mempoolGetSnapshot
  atomically $ do
    sharedState <- readTVar sharedStateVar
    peerInFlight <- readTVar peerInFlightVar
    let sharedGeneration0 = sharedGeneration sharedState
        (peerState', peerInFlight', sharedState') =
          State.handleReceivedTxIds mempoolHasTx now policy txIdsToReq txidsAndSizes
                                    peerState peerInFlight sharedState
    writeSharedStateIfChanged sharedStateVar sharedGeneration0 sharedState'
    writePeerInFlightIfChanged peerInFlightVar peerInFlight peerInFlight'
    modifyTVar countersVar (<> mempty { txIdRepliesReceived = 1
                                      , txIdsReceived       = fromIntegral (length txidsAndSizes) })
    return peerState'

-- | Process a batch of tx bodies received from this peer.
applyReceivedTxsImp :: ( MonadSTM m
                       , Eq peeraddr
                       , HasRawTxId txid )
                    => TxDecisionPolicy
                    -> STM m (MempoolSnapshot txid tx idx)
                    -> SharedTxStateVar m peeraddr txid
                    -> PeerTxInFlightVar m
                    -> TxSubmissionCountersVar m
                    -> peeraddr
                    -> Time
                    -> [(txid, tx)]
                    -> PeerTxLocalState tx
                    -> m (Int, PeerTxLocalState tx)
applyReceivedTxsImp policy mempoolGetSnapshot sharedStateVar peerInFlightVar
                    countersVar peeraddr now txs peerState = do
  -- Mempool snapshot taken in its own STM tx; see 'applyReceivedTxIdsImp'.
  MempoolSnapshot { mempoolHasTx } <- atomically mempoolGetSnapshot
  atomically $ do
    sharedState <- readTVar sharedStateVar
    peerInFlight <- readTVar peerInFlightVar
    let sharedGeneration0 = sharedGeneration sharedState
        (omittedCount, lateCount, peerState', peerInFlight', sharedState') =
          State.handleReceivedTxs mempoolHasTx now policy peeraddr txs
                                  peerState peerInFlight sharedState
    writeSharedStateIfChanged sharedStateVar sharedGeneration0 sharedState'
    writePeerInFlightIfChanged peerInFlightVar peerInFlight peerInFlight'
    modifyTVar countersVar (<> mempty {
                        txRepliesReceived = 1,
                        txsReceived       = fromIntegral (length txs),
                        txsOmitted        = fromIntegral omittedCount,
                        lateBodies        = fromIntegral lateCount
                      })
    return (omittedCount + lateCount, peerState')

-- | Mark txs as submitted to the mempool and update shared state.
applySubmittedTxsImp :: ( MonadSTM m
                       , Eq peeraddr )
                     => TxDecisionPolicy
                     -> SharedTxStateVar m peeraddr txid
                     -> PeerTxInFlightVar m
                     -> TxSubmissionCountersVar m
                     -> peeraddr
                     -> Time
                     -> [TxKey]
                     -> [TxKey]
                     -> PeerTxLocalState tx
                     -> m (PeerTxLocalState tx)
applySubmittedTxsImp policy sharedStateVar peerInFlightVar countersVar peeraddr
                     now acceptedTxs rejectedTxs peerState =
  atomically $ do
    sharedState <- readTVar sharedStateVar
    peerInFlight <- readTVar peerInFlightVar
    let sharedGeneration0 = sharedGeneration sharedState
        (peerState', peerInFlight', sharedState') =
          State.handleSubmittedTxs now policy peeraddr acceptedTxs
                                   rejectedTxs peerState peerInFlight sharedState
    writeSharedStateIfChanged sharedStateVar sharedGeneration0 sharedState'
    writePeerInFlightIfChanged peerInFlightVar peerInFlight peerInFlight'
    modifyTVar countersVar (<> mempty { txsAccepted = fromIntegral (length acceptedTxs)
                                      , txsRejected = fromIntegral (length rejectedTxs) })
    return peerState'

-- | Resolve txids and advertised sizes for a batch of tx keys to request.
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
