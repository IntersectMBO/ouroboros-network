{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.TxSubmission.Inbound.V2.Registry
  ( SharedTxStateVar
  , PeerTxRegistry
  , PeerTxAPI (..)
  , TxSubmissionCountersVar
  , newSharedTxStateVar
  , newPeerTxRegistry
  , newTxSubmissionCountersVar
  , txCountersThreadV2
  , sweepInterval
  , sweepBatch
  , drainPause
  , withPeer
    -- * For tests
  , scrubFromPeerInFlight
  , writeSharedStateIfChanged
    -- re-exports
  , RegisteredDelay
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad (when)
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer, traceWith)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Sequence.Strict qualified as StrictSeq
import Data.Void (Void)
import Data.Word (Word64)
import GHC.Stack (HasCallStack)

import Ouroboros.Network.Protocol.TxSubmission2.Type
import Ouroboros.Network.RegisteredDelay (RegisteredDelay)
import Ouroboros.Network.RegisteredDelay qualified as RegisteredDelay
import Ouroboros.Network.Tx (HasRawTxId)
import Ouroboros.Network.TxSubmission.Inbound.V2.Policy (TxDecisionPolicy (..))
import Ouroboros.Network.TxSubmission.Inbound.V2.State qualified as State
import Ouroboros.Network.TxSubmission.Inbound.V2.Types
import Ouroboros.Network.TxSubmission.Mempool.Reader

-- | Shared STM handle for V2 coordination state.
type SharedTxStateVar m peeraddr txid = StrictTVar m (SharedTxState peeraddr txid)

-- | STM handle for V2 monotonic counters.  Used both for each peer's
-- private counters cell and for the shared retired-peers accumulator
-- (see 'withPeer' and 'txCountersThreadV2').
--
type TxSubmissionCountersVar m = StrictTVar m TxSubmissionCountersAcc

-- | Per-peer in-flight TVar.
type PeerTxInFlightVar m = StrictTVar m PeerTxInFlight

-- | Registry of every live peer's private counters cell.
--
-- 'withPeer' adds the cell on bracket-enter and removes it on
-- bracket-exit (after scrubbing any contributions the peer still has
-- to shared state and flushing its counters into the retired
-- accumulator); 'snapshotCounters' reads the cells to aggregate for
-- emission.
type PeerTxRegistry m peeraddr =
       StrictTVar m (Map peeraddr (TxSubmissionCountersVar m))

newSharedTxStateVar
  :: MonadSTM m
  => SharedTxState peeraddr txid
  -> m (SharedTxStateVar m peeraddr txid)
newSharedTxStateVar = newTVarIO

newTxSubmissionCountersVar
  :: MonadSTM m
  => TxSubmissionCountersAcc
  -> m (TxSubmissionCountersVar m)
newTxSubmissionCountersVar = newTVarIO

newPeerTxRegistry
  :: MonadSTM m
  => m (PeerTxRegistry m peeraddr)
newPeerTxRegistry = newTVarIO Map.empty

-- | Central bookkeeping thread for V2.
--
-- Wakes every 'sweepInterval' to run 'State.sweepSharedStateUpTo' on the
-- shared tx state, which reads nothing else.  Each sweep
-- expires at most 'sweepBatch' retained entries and examines as many
-- entries of the tx table from where the last one stopped, so its
-- transaction stays short however large the state and however far behind
-- it is; while expired entries remain it goes on with
-- 'State.expireSharedStateUpTo' in further such transactions, 'drainPause'
-- apart so that peers' transactions commit between them, for at most one
-- interval before the next sweep.  On a fixed cadence (every
-- 'countersInterval' seconds, kept whatever a drain delays) it also emits
-- the aggregated counters -- the retired-peers accumulator plus every live
-- peer's cell -- when they differ from the last emission, and a
-- 'TraceSharedTxState' snapshot when @sharedRevision@ or
-- @sharedGeneration@ has moved since the last emission.
txCountersThreadV2
  :: forall m peeraddr txid tx.
     (MonadDelay m, MonadSTM m, HasRawTxId txid)
  => TxDecisionPolicy
  -> Tracer m TxSubmissionCounters
  -> Tracer m (TraceTxLogic peeraddr txid tx)
  -> TxSubmissionCountersVar m
  -> SharedTxStateVar m peeraddr txid
  -> PeerTxRegistry m peeraddr
  -> m Void
txCountersThreadV2 policy countersTracer sharedStateTracer retiredCountersVar
                   sharedStateVar registry = do
    now <- getMonotonicTime
    initialSt <- readTVarIO sharedStateVar
    go interval
       sweepStart
       mempty
       (sharedRevision   initialSt)
       (sharedGeneration initialSt)
       (addTime countersInterval now)
  where
    interval :: DiffTime
    interval = sweepInterval policy

    countersInterval :: DiffTime
    countersInterval = 7

    go !delay !cursor !previous !lastRev !lastGen !nextEmitAt = do
      threadDelay delay
      tickAt <- getMonotonicTime
      (backlog, cursor') <- sweepWith $ \st ->
        case State.sweepSharedStateUpTo sweepBatch tickAt cursor st of
          (st', more, next) -> (st', (more, next))
      _ <- drain backlog tickAt
      drained <- getMonotonicTime
      (previous', lastRev', lastGen', nextEmitAt') <-
        if drained >= nextEmitAt
           then emit previous lastRev lastGen (nextEmission drained nextEmitAt)
           else pure (previous, lastRev, lastGen, nextEmitAt)
      -- The next sweep: an interval after this one, whatever the drain and
      -- an emission took, and a pause at least, so that the writes stay a
      -- pause apart when a transaction overran the interval.
      now <- getMonotonicTime
      let delay' = max drainPause (addTime interval tickAt `diffTime` now)
      go delay' cursor' previous' lastRev' lastGen' nextEmitAt'

    -- An emission: the counters when they changed since the last one, the
    -- shared state when a counter of it moved; what the next one carries.
    emit :: TxSubmissionCounters -> Word64 -> Word64 -> Time
         -> m (TxSubmissionCounters, Word64, Word64, Time)
    emit previous lastRev lastGen nextEmitAt = do
      current <- atomically (snapshotCounters retiredCountersVar registry)
      when (current /= previous) $ traceWith countersTracer current
      st <- readTVarIO sharedStateVar
      let curRev = sharedRevision   st
          curGen = sharedGeneration st
      if curRev /= lastRev || curGen /= lastGen
         then do
           traceWith sharedStateTracer (TraceSharedTxState st)
           pure (current, curRev, curGen, nextEmitAt)
         else pure (current, lastRev, lastGen, nextEmitAt)

    -- One transaction over the state alone: the step applied, the state
    -- written when it changed, the step's result returned.
    sweepWith :: (     SharedTxState peeraddr txid
                    -> (SharedTxState peeraddr txid, a))
              -> m a
    sweepWith step = atomically $ do
      st <- readTVar sharedStateVar
      let (st', result) = step st
      when (sharedRevision st' /= sharedRevision st) $
        writeTVar sharedStateVar st'
      return result

    -- Drain a backlog in expiry-only transactions 'drainPause' apart,
    -- each run only if its pause and the one before the next sweep, due an
    -- interval after this one, still fit; whether the last transaction
    -- reported expired entries remaining.
    drain :: Bool -> Time -> m Bool
    drain False _ = return False
    drain True tickAt = do
      now <- getMonotonicTime
      if addTime (2 * drainPause) now `diffTime` tickAt > interval
         then return True
         else do
           threadDelay drainPause
           now' <- getMonotonicTime
           more <- sweepWith (State.expireSharedStateUpTo sweepBatch now')
           drain more tickAt

    -- The emission after the one due at the given time: 'countersInterval'
    -- later, so a drain delaying this one does not delay the next; or, were
    -- that already past, that much after now.
    nextEmission :: Time -> Time -> Time
    nextEmission now dueAt
      | next > now = next
      | otherwise  = addTime countersInterval now
      where
        next = addTime countersInterval dueAt

-- | How often the counters thread sweeps: a quarter of the retention
-- lifetime, kept between 100 ms and 1 s.
sweepInterval :: TxDecisionPolicy -> DiffTime
sweepInterval policy = max 0.1 (min 1 (bufferedTxsMinLifetime policy / 4))

-- | Retained entries one sweep expires at most, and entries of the tx
-- table it examines: positive, so a sweep that finds work makes progress
-- on it.  Bounds the sweep's transaction, about 0.3 ms at this size,
-- against the peers' writes to the shared state, each of which restarts
-- it: at R writes a second it commits with probability about
-- exp(-R * 0.3 ms) per attempt, a few attempts at the thousands of writes
-- a second of a loaded relay.  Not a policy knob: the transaction length
-- is a property of this node's STM, not of the protocol, and the drain
-- rate it gives with 'drainPause', a hundred thousand entries a second,
-- exceeds any arrival rate, which is what the expiry rate equals whatever
-- the retention lifetime; only 'sweepInterval' follows the policy.  Steady
-- state expires the arrivals of one sweep interval, a few hundred at most.
-- An orphan waits for the walk to pass, a sweep per limit of table
-- entries: a larger walk would lengthen the transaction the limit is there
-- to bound, for the memory of an abandoned entry.
sweepBatch :: Int
sweepBatch = 1000

-- | The pause before each expiry-only transaction of a drain, so that
-- peers' transactions, which every write to the shared state restarts,
-- commit between them: at most a hundred such writes a second while a
-- backlog drains.  Well under the shortest 'sweepInterval'.
drainPause :: DiffTime
drainPause = 0.01


-- | Aggregate the V2 counters for emission: the retired-peers
-- accumulator plus every live peer's private counters cell.
--
-- Read in a single STM transaction so the snapshot is coherent with
-- 'withPeer' disconnect flushes (which move a peer's counters into the
-- retired accumulator and drop it from the registry atomically),
-- keeping the emitted aggregate monotonic.  The transaction is
-- read-only, so the hot-path per-peer writers never block on it; only
-- this reader (at the 'countersInterval' cadence) retries on conflict.
snapshotCounters
  :: MonadSTM m
  => TxSubmissionCountersVar m
  -> PeerTxRegistry m peeraddr
  -> STM m TxSubmissionCounters
snapshotCounters retiredCountersVar registry = do
    retired <- readTVar retiredCountersVar
    peers   <- readTVar registry
    live    <- traverse readTVar (Map.elems peers)
    pure $! emittedCounters (mconcat (retired : live))

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
                         -> Maybe (RegisteredDelay m)
                         -> STM m (),

    -- | Compute the next action for this peer in non-pipelined mode.
    runNextPeerAction    :: Time
                         -> Maybe DiffTime
                         -> PeerTxLocalState tx
                         -> m (PeerAction, PeerTxLocalState tx),

    -- | Compute the next action for this peer in pipelined mode.
    runNextPeerActionPipelined :: Time
                               -> PeerTxLocalState tx
                               -> m (PeerAction, PeerTxLocalState tx),

    -- | Process a batch of txids received from this peer.
    applyReceivedTxIds   :: Time
                         -> Maybe DiffTime
                         -> NumTxIdsToReq
                         -> [(txid, SizeInBytes)]
                         -> PeerTxLocalState tx
                         -> m (PeerTxLocalState tx),

    -- | Process a batch of tx bodies received from this peer.
    applyReceivedTxs     :: Time
                         -> [(txid, tx)]
                         -> PeerTxLocalState tx
                         -> m (Int, PeerTxLocalState tx),

    -- | Mark txs as submitted to the mempool and update shared state: the
    -- accepted ones with their txids, so that one whose key is gone
    -- meanwhile is retained afresh; the rejected ones by key.
    applySubmittedTxs    :: Time
                         -> DiffTime
                         -> [(TxKey, txid)]
                         -> [TxKey]
                         -> PeerTxLocalState tx
                         -> m (PeerTxLocalState tx),

    -- | Resolve txids and advertised sizes for a batch of tx keys to request.
    resolveTxRequest     :: PeerTxLocalState tx
                         -> [TxKey]
                         -> m (Map txid SizeInBytes),
    -- | Resolve buffered tx bodies into full submission records.
    --
    -- pre-condition: the list of keys is in `peerDownloadedTxs` field of
    -- `PeerTxLocalState`.  It is ensured by `pickSubmitAction`.
    resolveBufferedTxs   :: PeerTxLocalState tx
                         -> [TxKey]
                         -> m [(TxKey, txid, tx)]
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
     , Show peeraddr
     , Ord txid
     , HasRawTxId txid
     )
  => TxDecisionPolicy
  -> TxSubmissionMempoolReader txid tx idx m
  -> SharedTxStateVar m peeraddr txid
  -> PeerTxRegistry m peeraddr
  -> TxSubmissionCountersVar m
  -> peeraddr
  -> (PeerTxAPI m txid tx -> m a)
  -> m a
withPeer policy TxSubmissionMempoolReader { mempoolGetSnapshot }
         sharedStateVar registry retiredCountersVar peeraddr io =
    bracket acquire release run
  where
    acquire = do
      peerInFlightVar <- newTVarIO emptyPeerTxInFlight
      peerCountersVar <- newTVarIO mempty
      atomically $ modifyTVar registry (Map.insert peeraddr peerCountersVar)
      pure (peerInFlightVar, peerCountersVar)

    -- On exit: reverse this peer's in-flight contributions to shared
    -- state, flush its accumulated counters into the retired
    -- accumulator (so the emitted aggregate stays monotonic across
    -- disconnects), then drop it from the registry.  Flush and removal
    -- commit in one transaction so 'snapshotCounters' counts this peer
    -- exactly once: via its live cell before exit, via the retired
    -- accumulator after.
    release (peerInFlightVar, peerCountersVar) = do
      now <- getMonotonicTime
      atomically $ do
        pif <- readTVar peerInFlightVar
        sharedState <- readTVar sharedStateVar
        writeSharedStateIfChanged sharedStateVar (sharedRevision sharedState)
                                  (scrubFromPeerInFlight peeraddr now pif sharedState)
        peerCounters <- readTVar peerCountersVar
        modifyTVar retiredCountersVar (<> peerCounters)
        modifyTVar registry (Map.delete peeraddr)

    run (peerInFlightVar, peerCountersVar) = io PeerTxAPI {
          awaitSharedChange = awaitSharedChangeImp sharedStateVar
        , runNextPeerAction = runNextPeerActionImp policy sharedStateVar
                                peerInFlightVar peerCountersVar peeraddr
        , runNextPeerActionPipelined = runNextPeerActionPipelinedImp policy
                                         sharedStateVar peerInFlightVar
                                         peerCountersVar peeraddr
        , applyReceivedTxIds = applyReceivedTxIdsImp policy mempoolGetSnapshot
                                 sharedStateVar peerInFlightVar peerCountersVar
        , applyReceivedTxs = applyReceivedTxsImp policy mempoolGetSnapshot
                               sharedStateVar peerInFlightVar peerCountersVar peeraddr
        , applySubmittedTxs = applySubmittedTxsImp policy sharedStateVar
                                peerInFlightVar peerCountersVar peeraddr
        , resolveTxRequest = resolveTxRequestImp sharedStateVar
        , resolveBufferedTxs = resolveBufferedTxsImp sharedStateVar
        }

-- | Reverse this peer's still-outstanding contributions to the shared
-- 'TxEntry' counters.  Run by the bracket finalizer; uses the per-peer
-- TVar snapshot taken at exit time.  Touches only the keys still in the
-- table, and returns the state as it was, counters included, when none
-- of the peer's keys is: the rest expired or were dropped meanwhile,
-- nothing of this peer's left on them.
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
  | IntSet.null presentKeys = st
  | otherwise =
      st {
          sharedTxTable    = sharedTxTable',
          -- A released lease, attempt or submission may grant another
          -- peer an option: a wake.  An advertiser fewer grants none; the
          -- entry it leaves abandoned is the sweep's to drop.
          sharedGeneration = if tableWork then sharedGeneration st + 1
                                          else sharedGeneration st,
          sharedRevision   = sharedRevision st + 1
        }
  where
    -- Walk each unique key once and apply all the modifications in a
    -- single 'IntMap.adjust'.  Keys typically appear in several of the
    -- sets ('pifLeased' is a subset of 'pifAttempting ∪ pifSubmitting',
    -- and a leased key is advertised).
    workKeys = pifLeased pif
        `IntSet.union` pifAttempting pif
        `IntSet.union` pifSubmitting pif

    allKeys = workKeys `IntSet.union` pifAdvertised pif

    presentKeys = IntMap.keysSet (sharedTxTable st `IntMap.restrictKeys` allKeys)

    tableWork = not (IntSet.null (presentKeys `IntSet.intersection` workKeys))

    sharedTxTable' = IntSet.foldl' scrubOne (sharedTxTable st) presentKeys

    scrubOne tbl k = IntMap.adjust (modifyEntry k) k tbl

    modifyEntry k =
        applyIf (IntSet.member k (pifAdvertised pif)) State.dropAdvertiser
      . applyIf (IntSet.member k (pifSubmitting pif)) clearSubmission
      . applyIf (IntSet.member k (pifAttempting pif)) decAttempt
      . applyIf (IntSet.member k (pifLeased     pif)) releaseLease

    applyIf True  f = f
    applyIf False _ = id

    releaseLease entry = case txLease entry of
      TxLeased owner _ | owner == peeraddr -> entry { txLease = TxClaimable now }
      _                                    -> entry

    decAttempt entry = entry { txAttempt = max 0 (txAttempt entry - 1) }

    clearSubmission entry = entry { txInSubmission = False }

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
                     -> Maybe (RegisteredDelay m)
                     -> STM m ()
awaitSharedChangeImp sharedStateVar generation mRegisteredDelay =
  case mRegisteredDelay of
       Nothing -> do
         sharedState <- readTVar sharedStateVar
         check (sharedGeneration sharedState /= generation)
       Just registeredDelay -> do
         sharedState <- readTVar sharedStateVar
         expired <- RegisteredDelay.read registeredDelay
         check (sharedGeneration sharedState /= generation || expired)

-- | Write the shared TVar when the pure state step moved the revision,
-- with 'sharedRevision' one past the revision read whatever the step
-- bumped it by: the revision counts committed writes, and one step may
-- bump it twice (a stuck-entry bump and a submission in one transaction).
-- Every bump of the wake counter comes with one of the revision, so the
-- revision alone says whether the step changed the state.
writeSharedStateIfChanged :: MonadSTM m
                          => SharedTxStateVar m peeraddr txid
                          -> Word64                 -- ^ pre-step 'sharedRevision'
                          -> SharedTxState peeraddr txid
                          -> STM m ()
writeSharedStateIfChanged sharedStateVar sharedRevision0 sharedState'
  | sharedRevision sharedState' == sharedRevision0 = pure ()
  | otherwise =
      writeTVar sharedStateVar sharedState' { sharedRevision = sharedRevision0 + 1 }

-- | Avoid rewriting the per-peer TVar when nothing changed.
writePeerInFlightIfChanged :: MonadSTM m
                           => PeerTxInFlightVar m
                           -> PeerTxInFlight
                           -> PeerTxInFlight
                           -> STM m ()
writePeerInFlightIfChanged var before after
  | before == after = pure ()
  | otherwise       = writeTVar var after

-- | Which txid request message the server will send for a
-- 'PeerRequestTxIds' action.
data TxIdRequestKind = BlockingTxIdRequest | PipelinedTxIdRequest

-- | Classify the txid request the non-pipelined loop will send for this
-- post-action peer state. Must agree with 'serverReqTxIds'.
txIdRequestKind :: PeerTxLocalState tx -> TxIdRequestKind
txIdRequestKind peerState =
    if StrictSeq.null (peerUnacknowledgedTxIds peerState)
       then BlockingTxIdRequest
       else PipelinedTxIdRequest

-- | Update the counters for the action chosen by the peer scheduler and,
-- when a body-download episode has just drained, for the time it was
-- active. The counters cell is read and written at most once, and not at
-- all when there is nothing to record.
--
updateCountersForAction :: MonadSTM m
                        => TxSubmissionCountersVar m
                        -> TxIdRequestKind
                        -> Maybe DiffTime
                        -- ^ duration of the episode that has just drained
                        -> PeerAction
                        -> STM m ()
updateCountersForAction countersVar kind pipelineWait_m peerAction =
  case peerAction of
    PeerRequestTxIds txIdsToAck txIdsToReq
      | txIdsToAck /= 0 || txIdsToReq /= 0 ->
          modifyTVar countersVar $ \c -> c
            { txIdMessagesSent      = txIdMessagesSent c + 1
            , txIdsRequested        = txIdsRequested c + fromIntegral txIdsToReq
            , txIdBlockingReqsSent  = txIdBlockingReqsSent c + blockingSent
            , txIdPipelinedReqsSent = txIdPipelinedReqsSent c + pipelinedSent
            , txPipelineWait        = txPipelineWait c + pipelineWait
            }
    PeerRequestTxs txKeys ->
      modifyTVar countersVar $ \c -> c
        { txMessagesSent = txMessagesSent c + 1
        , txsRequested   = txsRequested c + fromIntegral (length txKeys)
        , txPipelineWait = txPipelineWait c + pipelineWait
        }
    _ | Just d <- pipelineWait_m ->
          modifyTVar countersVar $ \c -> c { txPipelineWait = txPipelineWait c + d }
      | otherwise -> pure ()
  where
    pipelineWait = fromMaybe 0 pipelineWait_m
    (blockingSent, pipelinedSent) =
      case kind of
        BlockingTxIdRequest  -> (1, 0)
        PipelinedTxIdRequest -> (0, 1)

-- | Compute the next action for this peer in non-pipelined mode.
runNextPeerActionImp :: ( MonadSTM m
                        , Ord peeraddr )
                     => TxDecisionPolicy
                     -> SharedTxStateVar m peeraddr txid
                     -> PeerTxInFlightVar m
                     -> TxSubmissionCountersVar m
                     -> peeraddr
                     -> Time
                     -> Maybe DiffTime
                     -> PeerTxLocalState tx
                     -> m (PeerAction, PeerTxLocalState tx)
runNextPeerActionImp policy sharedStateVar peerInFlightVar countersVar peeraddr
                     now downloadTime_m peerState = atomically $ do
  sharedState <- readTVar sharedStateVar
  peerInFlight <- readTVar peerInFlightVar
  let sharedRevision0 = sharedRevision sharedState
      (peerAction, peerState', peerInFlight', sharedState') =
        State.nextPeerAction now policy peeraddr peerState peerInFlight sharedState
  writeSharedStateIfChanged sharedStateVar sharedRevision0 sharedState'
  writePeerInFlightIfChanged peerInFlightVar peerInFlight peerInFlight'
  updateCountersForAction countersVar (txIdRequestKind peerState') downloadTime_m
                          peerAction
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
      let sharedRevision0 = sharedRevision sharedState
          (peerAction, peerState', peerInFlight', sharedState') =
            State.nextPeerActionPipelined now policy peeraddr peerState
                                          peerInFlight sharedState
      writeSharedStateIfChanged sharedStateVar sharedRevision0 sharedState'
      writePeerInFlightIfChanged peerInFlightVar peerInFlight peerInFlight'
      -- The pipeline cannot have drained at depth above zero.
      updateCountersForAction countersVar PipelinedTxIdRequest Nothing peerAction
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
                      -> Maybe DiffTime
                      -- ^ time spent waiting for a blocking reply
                      -> NumTxIdsToReq
                      -- ^ number of requested txid's
                      -> [(txid, SizeInBytes)]
                      -- ^ actually received txid's with sizes
                      -> PeerTxLocalState tx
                      -> m (PeerTxLocalState tx)
applyReceivedTxIdsImp policy mempoolGetSnapshot sharedStateVar peerInFlightVar
                      countersVar now blockingWait_m txIdsToReq txidsAndSizes
                      peerState = do
  -- Snapshot the mempool outside the per-peer STM transaction so mempool
  -- writers don't kick the hot path into retries.  Stale answers are
  -- benign: a false positive delays re-fetch by 'bufferedTxsMinLifetime'
  -- via the retained set; a false negative wastes one body fetch that
  -- 'handleReceivedTxs' will reclassify as late.
  MempoolSnapshot { mempoolHasTx } <- atomically mempoolGetSnapshot
  atomically $ do
    sharedState <- readTVar sharedStateVar
    peerInFlight <- readTVar peerInFlightVar
    let sharedRevision0 = sharedRevision sharedState
        (peerState', peerInFlight', sharedState') =
          State.handleReceivedTxIds mempoolHasTx now policy txIdsToReq txidsAndSizes
                                    peerState peerInFlight sharedState
    writeSharedStateIfChanged sharedStateVar sharedRevision0 sharedState'
    writePeerInFlightIfChanged peerInFlightVar peerInFlight peerInFlight'
    modifyTVar countersVar $ \c -> c
      { txIdRepliesReceived = txIdRepliesReceived c + 1
      , txIdsReceived       = txIdsReceived c + fromIntegral (length txidsAndSizes)
      , txIdBlockingWait    = txIdBlockingWait c + fromMaybe 0 blockingWait_m
      }
    return peerState'

-- | Process a batch of tx bodies received from this peer.
applyReceivedTxsImp :: ( MonadSTM m
                       , Eq peeraddr
                       , Show peeraddr
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
    let sharedRevision0 = sharedRevision sharedState
        (omittedCount, lateCount, peerState', peerInFlight', sharedState') =
          State.handleReceivedTxs mempoolHasTx now policy peeraddr txs
                                  peerState peerInFlight sharedState
    writeSharedStateIfChanged sharedStateVar sharedRevision0 sharedState'
    writePeerInFlightIfChanged peerInFlightVar peerInFlight peerInFlight'
    modifyTVar countersVar $ \c -> c
      { txRepliesReceived = txRepliesReceived c + 1
      , txsReceived       = txsReceived c + fromIntegral (length txs)
      , txsOmitted        = txsOmitted c + fromIntegral omittedCount
      , lateBodies        = lateBodies c + fromIntegral lateCount
      }
    return (omittedCount + lateCount, peerState')

-- | Mark txs as submitted to the mempool and update shared state.
applySubmittedTxsImp :: ( MonadSTM m
                       , Eq peeraddr
                       , HasRawTxId txid )
                     => TxDecisionPolicy
                     -> SharedTxStateVar m peeraddr txid
                     -> PeerTxInFlightVar m
                     -> TxSubmissionCountersVar m
                     -> peeraddr
                     -> Time
                     -> DiffTime
                     -- ^ time spent inside 'mempoolAddTxs'
                     -> [(TxKey, txid)]
                     -- ^ the accepted txs, by the key they were submitted under
                     -> [TxKey]
                     -> PeerTxLocalState tx
                     -> m (PeerTxLocalState tx)
applySubmittedTxsImp policy sharedStateVar peerInFlightVar countersVar peeraddr
                     now submissionWait acceptedTxs rejectedTxs peerState =
  atomically $ do
    sharedState <- readTVar sharedStateVar
    peerInFlight <- readTVar peerInFlightVar
    let sharedRevision0 = sharedRevision sharedState
        (peerState', peerInFlight', sharedState') =
          State.handleSubmittedTxs now policy peeraddr acceptedTxs
                                   rejectedTxs peerState peerInFlight sharedState
    writeSharedStateIfChanged sharedStateVar sharedRevision0 sharedState'
    writePeerInFlightIfChanged peerInFlightVar peerInFlight peerInFlight'
    modifyTVar countersVar $ \c -> c
      { txsAccepted      = txsAccepted c + fromIntegral (length acceptedTxs)
      , txsRejected      = txsRejected c + fromIntegral (length rejectedTxs)
      , txSubmissionWait = txSubmissionWait c + submissionWait
      }
    return peerState'

-- | Resolve txids and advertised sizes for a batch of tx keys to request.
resolveTxRequestImp :: ( HasCallStack
                       , MonadSTM m
                       , Ord txid )
                    => SharedTxStateVar m peeraddr txid
                    -> PeerTxLocalState tx
                    -> [TxKey]
                    -> m (Map txid SizeInBytes)
resolveTxRequestImp sharedStateVar peerState txKeys = do
  sharedState <- readTVarIO sharedStateVar
  return $ Map.fromList (fmap (resolveOne sharedState) txKeys)
  where
    resolveOne sharedState key@(TxKey k) =
      ( resolveTxKey sharedState key
      , case IntMap.lookup k (peerAvailableTxIds peerState) of
             Just txSize -> txSize
             Nothing     -> error $
               "TxSubmission.V2.resolveTxRequestImp: missing tx size for key "
               ++ show k
      )

-- | Resolve buffered tx bodies into full submission records.
resolveBufferedTxsImp :: ( HasCallStack
                         , MonadSTM m
                         )
                       => SharedTxStateVar m peeraddr txid
                       -> PeerTxLocalState tx
                       -> [TxKey]
                       -> m [(TxKey, txid, tx)]
resolveBufferedTxsImp sharedStateVar peerState txKeys = do
  sharedState <- readTVarIO sharedStateVar
  return $ fmap (resolveOne sharedState) txKeys
  where
    resolveOne sharedState key@(TxKey k) =
      ( key
      , resolveTxKey sharedState key
      , case IntMap.lookup k (peerDownloadedTxs peerState) of
             Just tx -> tx
             Nothing -> error $
               "TxSubmission.V2.resolveBufferedTxsImp: missing buffered tx for key "
               ++ show k
      )
