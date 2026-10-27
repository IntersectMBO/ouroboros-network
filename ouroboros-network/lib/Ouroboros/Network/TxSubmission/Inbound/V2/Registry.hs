{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.TxSubmission.Inbound.V2.Registry
  ( SharedTxStateVar
  , PeerTxRegistry (..)
  , PeerRegistration
  , PeerTxAPI (..)
  , TxSubmissionCountersVar
  , newSharedTxStateVar
  , newPeerTxRegistry
  , newTxSubmissionCountersVar
  , txCountersThreadV2
  , rotateTiers
  , placeTiers
  , registerPeerTier
  , joinTier
  , leaveTier
  , PeerTierEntry (..)
  , tierCensus
  , withTier
  , sweepInterval
  , sweepBatch
  , drainPause
  , withPeer
    -- * For tests
  , scrubFromPeerInFlight
  , writeSharedStateIfChanged
  , awaitClaimableChangeImp
  , snapshotCountersWith
  , retirePeerCounters
  , registerPeerCounters
    -- re-exports
  , RegisteredDelay
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad (forM_, unless, when)
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
import Data.Set qualified as Set
import Data.Void (Void)
import Data.Word (Word64)
import GHC.Stack (HasCallStack)
import System.Random (StdGen, uniform)

import Ouroboros.Network.Hashable (Hashable, Salt, mkSalt)
import Ouroboros.Network.Protocol.TxSubmission2.Type
import Ouroboros.Network.RegisteredDelay (RegisteredDelay)
import Ouroboros.Network.RegisteredDelay qualified as RegisteredDelay
import Ouroboros.Network.Tx (HasRawTxId)
import Ouroboros.Network.TxSubmission.Inbound.V2.Policy
           (InvalidTxDecisionPolicy (..), TxDecisionPolicy (..),
           saneTxDecisionPolicy)
import Ouroboros.Network.TxSubmission.Inbound.V2.State qualified as State
import Ouroboros.Network.TxSubmission.Inbound.V2.Tier (censusOf, effectiveTier,
           isSource, orderFast, tierStanding)
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

-- | A peer's key in the registry: its address and its registration number.
type PeerRegistration peeraddr = (peeraddr, Word64)

-- | A live peer's tier bookkeeping.
data PeerTierEntry m = PeerTierEntry {
    -- | The peer's place in the tiers: the coin's verdict, 'SlowTier'
    -- until it joined, written by a placement; whether it is a local root
    -- and a source, written by its decisions.  One variable, written on a
    -- change only, so a placement reads one per peer and the governor's
    -- state not at all.
    pteState     :: !(StrictTVar m PeerTierState),
    -- | Whether the peer is a local root now, from the governor.
    pteLocalRoot :: !(STM m Bool)
  }

-- | Registry of every live peer's private counters cell, keyed by the
-- peer's address and registration number, so that a release removes its
-- own entry whatever else registered under the address.
--
-- 'withPeer' adds the cell on bracket-enter and removes it on
-- bracket-exit (after scrubbing any contributions the peer still has
-- to shared state and flushing its counters into the retired
-- accumulator); 'snapshotCountersWith' reads the cells to aggregate for
-- emission.
data PeerTxRegistry m peeraddr = PeerTxRegistry {
    -- | The live peers' cells.
    registeredPeers  :: !(StrictTVar m (Map (PeerRegistration peeraddr)
                                            (TxSubmissionCountersVar m))),
    -- | The next registration number.
    nextRegistration :: !(StrictTVar m Word64),
    -- | The peers in the order and their fetch tiers, as the salted order
    -- placed them ('placeTiers'); each peer's decisions read its own
    -- tier, and local roots stay out of the order, fast by standing beside
    -- the 'txFastPeers' first in it.
    peerTiers        :: !(StrictTVar m (Map (PeerRegistration peeraddr)
                                            (PeerTierEntry m))),
    -- | The salt the order is by, drawn afresh by each rotation.
    registrySalt     :: !(StrictTVar m Salt),
    -- | The generator the coin draws salts from, and the peers' jitter
    -- seeds.
    registryRng      :: !(StrictTVar m StdGen),
    -- | The shared state's claimable counter, mirrored in a variable of
    -- its own by every write of the state through this module, so that the
    -- slow tier's park waits on it alone and not on every write of the
    -- state.
    registryClaimable :: !(StrictTVar m Word64)
  }

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
  => StdGen
  -- ^ the coin's generator
  -> m (PeerTxRegistry m peeraddr)
newPeerTxRegistry rng =
    PeerTxRegistry <$> newTVarIO Map.empty <*> newTVarIO 0
                   <*> newTVarIO Map.empty <*> newTVarIO salt <*> newTVarIO rng'
                   <*> newTVarIO 0
  where
    (salt, rng') = mkSalt rng

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
-- @sharedGeneration@ has moved since the last emission.  It also runs
-- the coin: every 'txTierRotation' of the policy it draws a new salt and
-- places the fetch tiers from the new order ('rotateTiers'), in a
-- transaction of its own; without it the fast set would stay the one the
-- boot salt gave.  It runs once per node, so it is where a policy that
-- fails 'saneTxDecisionPolicy' is refused: 'InvalidTxDecisionPolicy'
-- before anything else.
txCountersThreadV2
  :: forall m peeraddr txid tx.
     ( MonadDelay m, MonadSTM m, MonadThrow m
     , HasRawTxId txid, Ord peeraddr, Hashable peeraddr )
  => TxDecisionPolicy
  -> Tracer m TxSubmissionCounters
  -> Tracer m (TraceTxLogic peeraddr txid tx)
  -> TxSubmissionCountersVar m
  -> SharedTxStateVar m peeraddr txid
  -> PeerTxRegistry m peeraddr
  -> m Void
txCountersThreadV2 policy countersTracer sharedStateTracer retiredCountersVar
                   sharedStateVar registry = do
    unless (saneTxDecisionPolicy policy) $
      throwIO (InvalidTxDecisionPolicy policy)
    now <- getMonotonicTime
    initialSt <- readTVarIO sharedStateVar
    go interval
       sweepStart
       mempty
       (sharedRevision   initialSt)
       (sharedGeneration initialSt)
       Nothing
       (addTime countersInterval now)
       (addTime (txTierRotation policy) now)
  where
    interval :: DiffTime
    interval = sweepInterval policy

    countersInterval :: DiffTime
    countersInterval = 7

    go !delay !cursor !previous !lastRev !lastGen !lastCensus !nextEmitAt !nextRotateAt = do
      threadDelay delay
      tickAt <- getMonotonicTime
      -- the coin, on its own cadence, before the sweep
      nextRotateAt' <-
        if tickAt >= nextRotateAt
           then do atomically (rotateTiers policy registry)
                   pure (nextRotation tickAt nextRotateAt)
           else pure nextRotateAt
      (backlog, cursor') <- sweepWith $ \st ->
        case State.sweepSharedStateUpTo sweepBatch tickAt cursor st of
          (st', more, next) -> (st', (more, next))
      _ <- drain backlog tickAt
      drained <- getMonotonicTime
      (previous', lastRev', lastGen', lastCensus', nextEmitAt') <-
        if drained >= nextEmitAt
           then emit previous lastRev lastGen lastCensus (nextEmission drained nextEmitAt)
           else pure (previous, lastRev, lastGen, lastCensus, nextEmitAt)
      -- The next sweep: an interval after this one, whatever the drain and
      -- an emission took, and a pause at least, so that the writes stay a
      -- pause apart when a transaction overran the interval.
      now <- getMonotonicTime
      let delay' = max drainPause (addTime interval tickAt `diffTime` now)
      go delay' cursor' previous' lastRev' lastGen' lastCensus' nextEmitAt' nextRotateAt'

    -- An emission: the counters when they changed since the last one, the
    -- shared state when a counter of it moved, the tier census when it
    -- changed (always the first time); what the next one carries.
    emit :: TxSubmissionCounters -> Word64 -> Word64 -> Maybe TierCensus -> Time
         -> m (TxSubmissionCounters, Word64, Word64, Maybe TierCensus, Time)
    emit previous lastRev lastGen lastCensus nextEmitAt = do
      current <- snapshotCountersWith atomically retiredCountersVar registry
      when (current /= previous) $ traceWith countersTracer current
      census <- atomically (tierCensus registry)
      when (Just census /= lastCensus) $ traceWith sharedStateTracer (TraceTxTiers census)
      st <- readTVarIO sharedStateVar
      let curRev = sharedRevision   st
          curGen = sharedGeneration st
      if curRev /= lastRev || curGen /= lastGen
         then do
           traceWith sharedStateTracer (TraceSharedTxState st (sharedTxStateCounts st))
           pure (current, curRev, curGen, Just census, nextEmitAt)
         else pure (current, lastRev, lastGen, Just census, nextEmitAt)

    -- One transaction over the state alone: the step applied, the state
    -- written when it changed, as one write whatever the step bumped the
    -- revision by, the step's result returned.
    sweepWith :: (     SharedTxState peeraddr txid
                    -> (SharedTxState peeraddr txid, a))
              -> m a
    sweepWith step = atomically $ do
      st <- readTVar sharedStateVar
      let (st', result) = step st
      writeSharedStateIfChanged (registryClaimable registry) sharedStateVar
                                (sharedRevision st) st'
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

    -- The next rotation: a period after the one due, or after now when
    -- that is already past.
    nextRotation :: Time -> Time -> Time
    nextRotation now dueAt
      | next > now = next
      | otherwise  = addTime (txTierRotation policy) now
      where
        next = addTime (txTierRotation policy) dueAt

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
-- Small transactions, as for the mux counters: one reads the retired
-- accumulator together with the list of live cells, then each cell is
-- read on its own.  One transaction over every cell would be restarted by
-- every peer's write, and under load never commit.  A peer that retires
-- in between is still counted exactly once: 'retirePeerCounters' moves its
-- counts into the accumulator in the transaction that unregisters it, after
-- the accumulator was read, and its cell keeps its final counts.  Every
-- cell only grows, so the emitted aggregate stays monotonic.
snapshotCountersWith
  :: (MonadSTM m, Monad n)
  => (forall a. STM m a -> n a)
  -> TxSubmissionCountersVar m
  -> PeerTxRegistry m peeraddr
  -> n TxSubmissionCounters
snapshotCountersWith runTx retiredCountersVar registry = do
    (retired, cells) <- runTx $
      (,) <$> readTVar retiredCountersVar
          <*> (Map.elems <$> readTVar (registeredPeers registry))
    live <- mapM (runTx . readTVar) cells
    pure $! emittedCounters (mconcat (retired : live))

-- | Register a joining peer's counters cell under its address and a fresh
-- registration number: the key 'retirePeerCounters' takes.
registerPeerCounters
  :: (MonadSTM m, Ord peeraddr)
  => PeerTxRegistry m peeraddr
  -> peeraddr
  -> TxSubmissionCountersVar m
  -> STM m (PeerRegistration peeraddr)
registerPeerCounters registry peeraddr peerCountersVar = do
    n <- readTVar (nextRegistration registry)
    writeTVar (nextRegistration registry) (n + 1)
    modifyTVar (registeredPeers registry)
               (Map.insert (peeraddr, n) peerCountersVar)
    pure (peeraddr, n)

-- | Move a leaving peer's counts into the retired accumulator and drop its
-- registration, in one transaction, so 'snapshotCountersWith' counts it
-- exactly once.
retirePeerCounters
  :: (MonadSTM m, Ord peeraddr)
  => TxSubmissionCountersVar m
  -> PeerTxRegistry m peeraddr
  -> PeerRegistration peeraddr
  -> TxSubmissionCountersVar m
  -> STM m ()
retirePeerCounters retiredCountersVar registry registration peerCountersVar = do
    peerCounters <- readTVar peerCountersVar
    modifyTVar retiredCountersVar (<> peerCounters)
    modifyTVar (registeredPeers registry) (Map.delete registration)

-- | Make a joining peer's tier entry and draw its jitter seed from the
-- coin's generator.  The peer is not in the order yet: 'joinTier' puts it
-- there once its server's initial delay has passed.
registerPeerTier
  :: MonadSTM m
  => PeerTxRegistry m peeraddr
  -> STM m Bool
  -- ^ whether the peer is a local root now
  -> STM m (PeerTierEntry m, Word64)
registerPeerTier registry localRoot = do
    entry <- PeerTierEntry <$> newTVar (PeerTierState SlowTier False False)
                           <*> pure localRoot
    g <- readTVar (registryRng registry)
    let (seed, g') = uniform g
    writeTVar (registryRng registry) g'
    pure (entry, seed)

-- | Put a peer in the order and place the tiers, with whether it is a
-- local root read from the governor now, so that a root takes no coin
-- slot at its own placement; nothing when it is in the order already.
-- The server calls it once its initial delay has passed, before its
-- first decision, so a peer not polling yet holds no fast slot and the
-- first decision is served on the tier it was placed on.
joinTier
  :: (MonadSTM m, Ord peeraddr, Hashable peeraddr)
  => TxDecisionPolicy
  -> PeerTxRegistry m peeraddr
  -> PeerRegistration peeraddr
  -> PeerTierEntry m
  -> STM m ()
joinTier policy registry registration entry@PeerTierEntry { pteState, pteLocalRoot } = do
    tiers <- readTVar (peerTiers registry)
    unless (Map.member registration tiers) $ do
      root <- pteLocalRoot
      modifyTVar pteState (\s -> s { ptsRoot = root })
      writeTVar (peerTiers registry) (Map.insert registration entry tiers)
      placeTiers policy registry

-- | Take a leaving peer out of the order and place the tiers, so the next
-- peer in the order moves up at once; nothing when it never joined.
leaveTier
  :: (MonadSTM m, Ord peeraddr, Hashable peeraddr)
  => TxDecisionPolicy
  -> PeerTxRegistry m peeraddr
  -> PeerRegistration peeraddr
  -> STM m ()
leaveTier policy registry registration = do
    tiers <- readTVar (peerTiers registry)
    when (Map.member registration tiers) $ do
      writeTVar (peerTiers registry) (Map.delete registration tiers)
      placeTiers policy registry

-- | One rotation: a new salt from the coin's generator, and the tiers
-- placed from the order it gives.
rotateTiers :: (MonadSTM m, Ord peeraddr, Hashable peeraddr)
            => TxDecisionPolicy
            -> PeerTxRegistry m peeraddr
            -> STM m ()
rotateTiers policy registry = do
    g <- readTVar (registryRng registry)
    let (salt, g') = mkSalt g
    writeTVar (registryRng registry) g'
    writeTVar (registrySalt registry) salt
    placeTiers policy registry

-- | Place the tiers from the salted order: the first 'txFastPeers' of the
-- peers in the order that are not local roots, as their decisions last
-- read it, are fast, the local roots are fast besides, the rest slow.
-- Writes a peer's coin on a change only.  One read per peer and the
-- sort of the candidates; it reads nothing of the governor's, so only a
-- join, a leave or a peer's rare flag write restarts it.
placeTiers :: (MonadSTM m, Ord peeraddr, Hashable peeraddr)
           => TxDecisionPolicy
           -> PeerTxRegistry m peeraddr
           -> STM m ()
placeTiers policy registry = do
    tiers  <- readTVar (peerTiers registry)
    states <- traverse (readTVar . pteState) tiers
    salt   <- readTVar (registrySalt registry)
    let candidates = [ r | (r, s) <- Map.toList states, not (ptsRoot s) ]
        fastSet    = Set.fromList (orderFast (txFastPeers policy) salt candidates)
    forM_ (Map.toList (Map.intersectionWith (,) tiers states)) $
      \(registration, (PeerTierEntry { pteState }, s)) -> do
        -- the order's verdict alone: a root is fast by standing at each
        -- decision ('withTier'), so one that stops being a root is served
        -- by its coin at once, not fast until the next placement
        let coin = if registration `Set.member` fastSet then FastTier else SlowTier
        when (coin /= ptsCoin s) $ writeTVar pteState s { ptsCoin = coin }

-- | The peers in the order by the standing they are served on.
tierCensus :: MonadSTM m => PeerTxRegistry m peeraddr -> STM m TierCensus
tierCensus registry = do
    tiers <- readTVar (peerTiers registry)
    censusOf . map tierStanding <$> mapM (readTVar . pteState) (Map.elems tiers)

-- | Peer-facing coordination API.
--
-- The peer thread keeps its local protocol state in a local
-- variable. Registry helpers operate only on the shared STM state
-- and the per-peer 'PeerTxInFlight' TVar (closure-captured); any
-- helper that needs peer-local state should take it explicitly as an
-- argument.
data PeerTxAPI m txid tx = PeerTxAPI {
    -- | The seed of the peer's jitter generator, for its initial local state.
    apiJitterSeed        :: Word64,

    -- | Put the peer in the salted order and place the tiers: once, after
    -- the server's initial delay and before its first decision.
    joinTiers            :: m (),

    -- | Wait until either 'sharedGeneration' moves past the given
    -- value or the optional timeout expires.
    awaitSharedChange    :: Word64
                         -> Maybe (RegisteredDelay m)
                         -> STM m (),

    -- | Wait until 'sharedClaimable' moves past the given value or the
    -- optional timeout expires: the slow tier's park.
    awaitClaimableChange :: Word64
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
     , Hashable peeraddr
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
  -> STM m Bool
  -- ^ whether the peer is a local root now: fast by standing
  -> (PeerTxAPI m txid tx -> m a)
  -> m a
withPeer policy TxSubmissionMempoolReader { mempoolGetSnapshot }
         sharedStateVar registry retiredCountersVar peeraddr localRoot io =
    bracket acquire release run
  where
    acquire = do
      peerInFlightVar <- newTVarIO emptyPeerTxInFlight
      peerCountersVar <- newTVarIO mempty
      (registration, entry, seed) <- atomically $ do
        registration <- registerPeerCounters registry peeraddr peerCountersVar
        (entry, seed) <- registerPeerTier registry localRoot
        pure (registration, entry, seed)
      pure (registration, peerInFlightVar, peerCountersVar, entry, seed)

    -- On exit: reverse this peer's in-flight contributions to shared
    -- state, flush its accumulated counters into the retired
    -- accumulator (so the emitted aggregate stays monotonic across
    -- disconnects), then drop it from the registry and the order, so the
    -- next peer in the order takes its fast slot at once.  The flush and
    -- the retirement commit in one transaction so 'snapshotCountersWith'
    -- counts this peer exactly once: via its live cell before exit, via
    -- the retired accumulator after.  Leaving the order is a transaction
    -- of its own: placing reads every peer's tier state and sorts the
    -- candidates, which would make the scrub's transaction long and
    -- retried by every decision.
    release (registration, peerInFlightVar, peerCountersVar, _, _) = do
      now <- getMonotonicTime
      atomically $ do
        pif <- readTVar peerInFlightVar
        sharedState <- readTVar sharedStateVar
        writeSharedStateIfChanged claimableVar sharedStateVar (sharedRevision sharedState)
                                  (scrubFromPeerInFlight peeraddr now pif sharedState)
        retirePeerCounters retiredCountersVar registry registration
                           peerCountersVar
      atomically $ leaveTier policy registry registration

    run (registration, peerInFlightVar, peerCountersVar, entry, seed) = io PeerTxAPI {
          apiJitterSeed = seed
        , joinTiers = atomically (joinTier policy registry registration entry)
        , awaitSharedChange = awaitSharedChangeImp sharedStateVar
        , awaitClaimableChange = awaitClaimableChangeImp claimableVar
        , runNextPeerAction = runNextPeerActionImp policy registry sharedStateVar
                                peerInFlightVar peerCountersVar entry peeraddr
        , runNextPeerActionPipelined = runNextPeerActionPipelinedImp policy registry
                                         sharedStateVar peerInFlightVar
                                         peerCountersVar entry peeraddr
        , applyReceivedTxIds = applyReceivedTxIdsImp policy mempoolGetSnapshot claimableVar
                                 sharedStateVar peerInFlightVar peerCountersVar
        , applyReceivedTxs = applyReceivedTxsImp policy mempoolGetSnapshot claimableVar
                               sharedStateVar peerInFlightVar peerCountersVar peeraddr
        , applySubmittedTxs = applySubmittedTxsImp policy claimableVar sharedStateVar
                                peerInFlightVar peerCountersVar peeraddr
        , resolveTxRequest = resolveTxRequestImp sharedStateVar
        , resolveBufferedTxs = resolveBufferedTxsImp sharedStateVar
        }
    claimableVar = registryClaimable registry

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
          sharedRevision   = sharedRevision st + 1,
          sharedClaimable  = if tableWork then sharedClaimable st + 1
                                          else sharedClaimable st
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

-- | Wait until a counter of the shared state moves past the given value or
-- the optional timeout expires.
awaitCounterChange :: MonadTimer m
                   => (SharedTxState peeraddr txid -> Word64)
                   -> SharedTxStateVar m peeraddr txid
                   -> Word64
                   -> Maybe (RegisteredDelay m)
                   -> STM m ()
awaitCounterChange counter sharedStateVar seen mRegisteredDelay = do
  sharedState <- readTVar sharedStateVar
  expired <- maybe (pure False) RegisteredDelay.read mRegisteredDelay
  check (counter sharedState /= seen || expired)

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
awaitSharedChangeImp = awaitCounterChange sharedGeneration

-- | The slow tier's park: until the claimable counter's mirror
-- ('registryClaimable') moves past the value seen or the delay expires.
-- The mirror, not the state: a loaded relay writes the state many times a
-- second, and the parked peers would re-check at every write.
awaitClaimableChangeImp :: MonadTimer m
                        => StrictTVar m Word64
                        -> Word64
                        -> Maybe (RegisteredDelay m)
                        -> STM m ()
awaitClaimableChangeImp claimableVar seen mRegisteredDelay = do
  claimable <- readTVar claimableVar
  expired <- maybe (pure False) RegisteredDelay.read mRegisteredDelay
  check (claimable /= seen || expired)

-- | Write the shared TVar when the pure state step moved the revision,
-- with 'sharedRevision' one past the revision read whatever the step
-- bumped it by: the revision counts committed writes, and one step may
-- bump it twice (a stuck-entry bump and a submission in one transaction).
-- Every bump of the wake counter comes with one of the revision, so the
-- revision alone says whether the step changed the state.  The claimable
-- counter's mirror follows in the same transaction.
writeSharedStateIfChanged :: MonadSTM m
                          => StrictTVar m Word64    -- ^ the claimable counter's mirror
                          -> SharedTxStateVar m peeraddr txid
                          -> Word64                 -- ^ pre-step 'sharedRevision'
                          -> SharedTxState peeraddr txid
                          -> STM m ()
writeSharedStateIfChanged claimableVar sharedStateVar sharedRevision0 sharedState'
  | sharedRevision sharedState' == sharedRevision0 = pure ()
  | otherwise = do
      writeTVar sharedStateVar sharedState' { sharedRevision = sharedRevision0 + 1 }
      mirrorClaimable claimableVar sharedState'

-- | Bring the claimable counter's mirror up to the state's, written only
-- when it moved, so that the parked peers wake for that alone.
mirrorClaimable :: MonadSTM m
                => StrictTVar m Word64
                -> SharedTxState peeraddr txid
                -> STM m ()
mirrorClaimable claimableVar st = do
  seen <- readTVar claimableVar
  when (seen /= sharedClaimable st) $ writeTVar claimableVar (sharedClaimable st)

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
                        -> PeerTier
                        -- ^ the tier the peer was served on
                        -> Maybe DiffTime
                        -- ^ duration of the episode that has just drained
                        -> PeerAction
                        -> STM m ()
updateCountersForAction countersVar kind tier pipelineWait_m peerAction =
  case peerAction of
    PeerRequestTxIds txIdsToAck txIdsToReq
      | txIdsToAck /= 0 || txIdsToReq /= 0 ->
          modifyTVar countersVar $ \c -> c
            { txIdMessagesSent      = txIdMessagesSent c + 1
            , txIdsRequested        = txIdsRequested c + fromIntegral txIdsToReq
            , txIdBlockingReqsSent  = txIdBlockingReqsSent c + blockingSent
            , txIdPipelinedReqsSent = txIdPipelinedReqsSent c + pipelinedSent
            , txIdSlowReqsSent      = txIdSlowReqsSent c + slowSent
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
    slowSent = case tier of
      SlowTier -> 1
      FastTier -> 0

-- | Compute the next action for this peer in non-pipelined mode.
runNextPeerActionImp :: ( MonadSTM m
                        , Ord peeraddr
                        , Hashable peeraddr )
                     => TxDecisionPolicy
                     -> PeerTxRegistry m peeraddr
                     -> SharedTxStateVar m peeraddr txid
                     -> PeerTxInFlightVar m
                     -> TxSubmissionCountersVar m
                     -> PeerTierEntry m
                     -> peeraddr
                     -> Time
                     -> Maybe DiffTime
                     -> PeerTxLocalState tx
                     -> m (PeerAction, PeerTxLocalState tx)
runNextPeerActionImp policy registry sharedStateVar peerInFlightVar countersVar entry
                     peeraddr now downloadTime_m peerState = do
  -- the tier is read in a transaction of its own: the governor rewrites
  -- its public state every iteration, and reading it inside the decision
  -- transaction would retry every in-flight decision on each tick
  peerState0 <- atomically $ withTier policy now registry entry peerState
  atomically $ do
    sharedState <- readTVar sharedStateVar
    peerInFlight <- readTVar peerInFlightVar
    let sharedRevision0 = sharedRevision sharedState
        (peerAction, peerState', peerInFlight', sharedState') =
          State.nextPeerAction now policy peeraddr peerState0 peerInFlight sharedState
    writeSharedStateIfChanged (registryClaimable registry) sharedStateVar
                              sharedRevision0 sharedState'
    writePeerInFlightIfChanged peerInFlightVar peerInFlight peerInFlight'
    updateCountersForAction countersVar (txIdRequestKind peerState')
                            (effectiveTier policy now peerState0) downloadTime_m
                            peerAction
    return (peerAction, peerState')

-- | The peer's tier, read into its local state before a decision, in a
-- transaction of its own: a local root is fast whatever the coin said.
-- Whether the peer is a root and whether it is a source go to its tier
-- state when they changed, for the placements and the census.  A peer
-- that stopped being a root, or became one, is in or out of the order's
-- candidates from now: the tiers are placed at once, so an ex-root takes
-- a free fast slot now rather than at the next rotation.
withTier :: (MonadSTM m, Ord peeraddr, Hashable peeraddr)
         => TxDecisionPolicy
         -> Time
         -> PeerTxRegistry m peeraddr
         -> PeerTierEntry m
         -> PeerTxLocalState tx
         -> STM m (PeerTxLocalState tx)
withTier policy now registry PeerTierEntry { pteState, pteLocalRoot } peerState = do
  root <- pteLocalRoot
  s <- readTVar pteState
  let s' = s { ptsRoot   = root
             , ptsSource = isSource policy now (peerNovelAt peerState) }
  when (s' /= s) $ writeTVar pteState s'
  coin <- if ptsRoot s' /= ptsRoot s
            then placeTiers policy registry >> (ptsCoin <$> readTVar pteState)
            else pure (ptsCoin s')
  let tier = if root then FastTier else coin
  pure $ if peerTier peerState == tier then peerState else peerState { peerTier = tier }

-- | Compute the next action for this peer in pipelined mode.
runNextPeerActionPipelinedImp :: ( MonadSTM m
                                  , Ord peeraddr
                                  , Hashable peeraddr )
                              => TxDecisionPolicy
                              -> PeerTxRegistry m peeraddr
                              -> SharedTxStateVar m peeraddr txid
                              -> PeerTxInFlightVar m
                              -> TxSubmissionCountersVar m
                              -> PeerTierEntry m
                              -> peeraddr
                              -> Time
                              -> PeerTxLocalState tx
                              -> m (PeerAction, PeerTxLocalState tx)
runNextPeerActionPipelinedImp policy registry sharedStateVar peerInFlightVar countersVar
                              entry peeraddr now peerState = do
    peerState0 <- atomically $ withTier policy now registry entry peerState
    atomically $ do
      sharedState <- readTVar sharedStateVar
      peerInFlight <- readTVar peerInFlightVar
      let sharedRevision0 = sharedRevision sharedState
          (peerAction, peerState', peerInFlight', sharedState') =
            State.nextPeerActionPipelined now policy peeraddr peerState0
                                          peerInFlight sharedState
      writeSharedStateIfChanged (registryClaimable registry) sharedStateVar
                                sharedRevision0 sharedState'
      writePeerInFlightIfChanged peerInFlightVar peerInFlight peerInFlight'
      -- The pipeline cannot have drained at depth above zero.
      updateCountersForAction countersVar PipelinedTxIdRequest
                              (effectiveTier policy now peerState0) Nothing peerAction
      return (peerAction, peerState')

-- | Process a batch of txids received from this peer.
applyReceivedTxIdsImp :: ( MonadSTM m
                         , HasRawTxId txid )
                      => TxDecisionPolicy
                      -> STM m (MempoolSnapshot txid tx idx)
                      -> StrictTVar m Word64
                      -- ^ the claimable counter's mirror
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
applyReceivedTxIdsImp policy mempoolGetSnapshot claimableVar sharedStateVar peerInFlightVar
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
    writeSharedStateIfChanged claimableVar sharedStateVar sharedRevision0 sharedState'
    writePeerInFlightIfChanged peerInFlightVar peerInFlight peerInFlight'
    let received = fromIntegral (length txidsAndSizes)
        -- the reply answers the oldest outstanding request, on the tier
        -- recorded when it went out
        replyTier = fst (takeReplyTier peerState)
        (r0, r1, r2, r5, r10) = replyBucket (length txidsAndSizes)
    modifyTVar countersVar $ \c -> c
      { txIdRepliesReceived = txIdRepliesReceived c + 1
      , txIdsReceived       = txIdsReceived c + received
      , txIdsReceivedSlow   = txIdsReceivedSlow c
                            + (if replyTier == SlowTier then received else 0)
      , txIdReplies0        = txIdReplies0 c + r0
      , txIdReplies1        = txIdReplies1 c + r1
      , txIdReplies2to4     = txIdReplies2to4 c + r2
      , txIdReplies5to9     = txIdReplies5to9 c + r5
      , txIdReplies10       = txIdReplies10 c + r10
      , txIdBlockingWait    = txIdBlockingWait c + fromMaybe 0 blockingWait_m
      }
    return peerState'

-- | Which of the five reply-size counters a txid reply of the given size
-- bumps: none, one, two to four, five to nine, ten or more.
replyBucket :: Int -> (Word64, Word64, Word64, Word64, Word64)
replyBucket n
  | n <= 0    = (1, 0, 0, 0, 0)
  | n == 1    = (0, 1, 0, 0, 0)
  | n <= 4    = (0, 0, 1, 0, 0)
  | n <= 9    = (0, 0, 0, 1, 0)
  | otherwise = (0, 0, 0, 0, 1)

-- | Process a batch of tx bodies received from this peer.
applyReceivedTxsImp :: ( MonadSTM m
                       , Eq peeraddr
                       , Show peeraddr
                       , HasRawTxId txid )
                    => TxDecisionPolicy
                    -> STM m (MempoolSnapshot txid tx idx)
                    -> StrictTVar m Word64
                    -- ^ the claimable counter's mirror
                    -> SharedTxStateVar m peeraddr txid
                    -> PeerTxInFlightVar m
                    -> TxSubmissionCountersVar m
                    -> peeraddr
                    -> Time
                    -> [(txid, tx)]
                    -> PeerTxLocalState tx
                    -> m (Int, PeerTxLocalState tx)
applyReceivedTxsImp policy mempoolGetSnapshot claimableVar sharedStateVar peerInFlightVar
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
    writeSharedStateIfChanged claimableVar sharedStateVar sharedRevision0 sharedState'
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
                     -> StrictTVar m Word64
                     -- ^ the claimable counter's mirror
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
applySubmittedTxsImp policy claimableVar sharedStateVar peerInFlightVar countersVar peeraddr
                     now submissionWait acceptedTxs rejectedTxs peerState =
  atomically $ do
    sharedState <- readTVar sharedStateVar
    peerInFlight <- readTVar peerInFlightVar
    let sharedRevision0 = sharedRevision sharedState
        (peerState', peerInFlight', sharedState') =
          State.handleSubmittedTxs now policy peeraddr acceptedTxs
                                   rejectedTxs peerState peerInFlight sharedState
    writeSharedStateIfChanged claimableVar sharedStateVar sharedRevision0 sharedState'
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
