{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.TxSubmission.Inbound.V2.Registry
  ( SharedTxStateVar
  , PeerTxRegistry (..)
  , CounterMirrors (..)
  , PeerRegistration
  , PeerTxAPI (..)
  , TxSubmissionCountersVar
  , newSharedTxStateVar
  , newPeerTxRegistry
  , newTxSubmissionCountersVar
  , txCountersThreadV2
  , rotateTiers
  , registerPeerTier
  , RootAnswer (..)
  , askRoot
  , joinTier
  , leaveTier
  , refreshTierRoots
  , PeerTierEntry (..)
  , tierCensus
  , withTier
  , sweepInterval
  , sweepBatch
  , drainPause
  , doomGrace
  , PeerCells (..)
  , withPeer
    -- * For tests
  , TxIdRequestKind (..)
  , updateCountersForAction
  , resolvePeerAction
  , snapshotRoots
  , refreshRoots
  , scrubFromPeerInFlight
  , writeSharedStateIfChanged
  , awaitMirrorChange
  , snapshotCountersWith
  , retirePeerCounters
  , registerPeerCells
    -- re-exports
  , RegisteredDelay
  ) where

import Cardano.Prelude (forceElemsToWHNF)
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (assert)
import Control.Monad (forM, forM_, unless, when)
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer, traceWith)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
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
import Ouroboros.Network.TxSubmission.Inbound.V2.Policy (TxDecisionPolicy (..),
           TxSubmissionConfig (..), saneTxDecisionPolicy)
import Ouroboros.Network.TxSubmission.Inbound.V2.State qualified as State
import Ouroboros.Network.TxSubmission.Inbound.V2.Tier (censusOf, effectiveTier,
           orderFast, sourceUntil, tierStanding)
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
    -- until it joined, written by a placement; whether it is a local
    -- root, written by its join and the counters thread's refresh; until
    -- when it is a source, written by its decisions.  One variable,
    -- written on a change only, so a placement reads one per peer and the
    -- governor's state not at all.
    pteState :: !(StrictTVar m PeerTierState)
  }

-- | A live peer's cells: its counters, and its in-flight sets, which the
-- counters thread's review of doomed entries reads, one peer per
-- transaction ('heldDoomed').
data PeerCells m = PeerCells {
    peerCountersCell :: !(TxSubmissionCountersVar m),
    peerInFlightCell :: !(PeerTxInFlightVar m)
  }

-- | Registry of every live peer's cells, keyed by the peer's address and
-- registration number, so that a release removes its own entry whatever
-- else registered under the address.
--
-- 'withPeer' adds the cells on bracket-enter and removes them on
-- bracket-exit (after scrubbing any contributions the peer still has
-- to shared state and flushing its counters into the retired
-- accumulator); 'snapshotCountersWith' reads the counters cells to
-- aggregate for emission, 'heldDoomed' the in-flight cells for the
-- holders of doomed entries.
data PeerTxRegistry m peeraddr = PeerTxRegistry {
    -- | The live peers' cells.
    registeredPeers  :: !(StrictTVar m (Map (PeerRegistration peeraddr)
                                            (PeerCells m))),
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
    -- | The shared state's counters mirrored, for the parks.
    registryMirrors  :: !(CounterMirrors m),
    -- | Whether a peer is a local root now, from the governor: asked once
    -- for a joining peer and once a sweep for every peer in the order
    -- ('refreshTierRoots'), each time in a transaction of its own, so that
    -- no decision and no placement reads the governor's state, which it
    -- rewrites every iteration.
    registryLocalRoots :: !(STM m (peeraddr -> Bool)),
    -- | The root epoch: how many snapshots of the governor's answers the
    -- refresh has taken ('snapshotRoots').  A peer's flag carries the
    -- epoch at its ask ('ptsRootAsked'), so that a refresh never puts an
    -- older answer over a join's newer one.
    registryRootEpoch  :: !(StrictTVar m Word64)
  }

-- | The shared state's two counters, each mirrored in a variable of its
-- own by every write of the state through this module, when it moved: a
-- parked peer waits on the one it parks on, the fast tier on the wake
-- counter ('sharedGeneration'), the slow tier on the claimable counter
-- ('sharedClaimable'), and not on every write of the state, which a
-- loaded relay makes many times a second.
data CounterMirrors m = CounterMirrors {
    generationMirror :: !(StrictTVar m Word64),
    claimableMirror  :: !(StrictTVar m Word64)
  }

-- | The mirror of the counter a peer parks on; none for a park on no
-- counter, which waits for its delay alone.
counterMirror :: CounterMirrors m -> ParkCounter -> Maybe (StrictTVar m Word64)
counterMirror mirrors WakeCounter      = Just (generationMirror mirrors)
counterMirror mirrors ClaimableCounter = Just (claimableMirror mirrors)
counterMirror _       NoCounter        = Nothing

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
  -> STM m (peeraddr -> Bool)
  -- ^ whether a peer is a local root now: fast by standing
  -> m (PeerTxRegistry m peeraddr)
newPeerTxRegistry rng localRoots =
    PeerTxRegistry <$> newTVarIO Map.empty <*> newTVarIO 0
                   <*> newTVarIO Map.empty <*> newTVarIO salt <*> newTVarIO rng'
                   <*> (CounterMirrors <$> newTVarIO 0 <*> newTVarIO 0)
                   <*> pure localRoots <*> newTVarIO 0
  where
    (salt, rng') = mkSalt rng

-- | What the counters thread carries from one tick to the next.
data TickState = TickState {
    tickDelay    :: !DiffTime,
    -- ^ the sleep before the tick
    tickCursor   :: !SweepCursor,
    -- ^ where the sweep's walk goes on from
    tickDoomed   :: !IntSet,
    -- ^ the keys the last sweep doomed, for this tick's review
    tickCounters :: !TxSubmissionCounters,
    -- ^ the counters as last emitted
    tickRevision :: !Word64,
    -- ^ the shared state's revision as last emitted
    tickCensus   :: !(Maybe TierCensus),
    -- ^ the tier census as last emitted, none before the first emission
    tickEmitAt   :: !Time,
    -- ^ when the next emission is due
    tickRotateAt :: !Time
    -- ^ when the next rotation of the coin is due
  }

-- | Central bookkeeping thread for V2.
--
-- Wakes every 'sweepInterval' to run 'State.sweepSharedStateUpTo' on the
-- shared tx state, which reads nothing else.  Each sweep does at most
-- 'sweepBatch' units of work in its transaction: the review of what the
-- sweep before doomed is charged first, the expiry of retained entries
-- takes at most half of the rest, rounded up, and the walk of the tx
-- table, from where the last one stopped, the remainder; so its
-- transaction stays short however large the state and however far behind
-- it is.  While expired entries remain it goes on with
-- 'State.expireSharedStateUpTo' in further such transactions, 'drainPause'
-- apart so that peers' transactions commit between them, for at most one
-- interval before the next sweep.  On a fixed cadence (every
-- 'countersInterval' seconds, kept whatever a drain delays) it also emits
-- the aggregated counters -- the retired-peers accumulator plus every live
-- peer's cell -- when they differ from the last emission, and a
-- 'TraceSharedTxState' snapshot when @sharedRevision@, which every
-- write of the state advances, has moved since the last emission.  It also runs
-- the coin: every 'txTierRotation' of the policy it draws a new salt and
-- places the fetch tiers from the new order ('rotateTiers'), in a
-- transaction of its own; without it the fast set would stay the one the
-- boot salt gave.  It keeps the root flags too: each tick it asks the
-- governor whether each peer in the order is a local root, in a
-- transaction of its own, and places the tiers when a flag changed
-- ('refreshTierRoots'), so that no decision reads the governor's state,
-- which it rewrites every iteration, and a demoted root is served by its
-- coin within an interval.  A sweep dooms the active entries abandoned
-- for a grace ('doomGrace'), and the next tick reviews them ('heldDoomed',
-- then 'State.reviewDoomed' in the sweep's transaction): every
-- live peer's advertised set in a transaction of its own, like the
-- counters' cells, then one over the state that drops the doomed entries
-- no peer holds; entries doomed before the thread started, by an earlier
-- run over the same state, are reviewed at its first tick.  It runs once
-- per node, so it is where a policy and config that fail
-- 'saneTxDecisionPolicy' are refused: the 'TxConfigurationError' before
-- anything else.
txCountersThreadV2
  :: forall m peeraddr txid tx.
     ( MonadDelay m, MonadSTM m, MonadThrow m
     , HasRawTxId txid, Ord peeraddr, Hashable peeraddr )
  => TxDecisionPolicy
  -> TxSubmissionConfig
  -> Tracer m TxSubmissionCounters
  -> Tracer m (TraceTxLogic peeraddr txid tx)
  -> TxSubmissionCountersVar m
  -> SharedTxStateVar m peeraddr txid
  -> PeerTxRegistry m peeraddr
  -> m Void
txCountersThreadV2 policy config countersTracer sharedStateTracer retiredCountersVar
                   sharedStateVar registry = do
    either throwIO pure (saneTxDecisionPolicy policy config)
    now <- getMonotonicTime
    initialSt <- readTVarIO sharedStateVar
    go TickState {
         tickDelay    = interval,
         tickCursor   = sweepStart,
         -- the entries doomed before this thread started, by an earlier
         -- run over the same state: reviewed at its first tick
         tickDoomed   = State.doomedKeys initialSt,
         tickCounters = mempty,
         tickRevision = sharedRevision initialSt,
         tickCensus   = Nothing,
         tickEmitAt   = addTime countersInterval now,
         tickRotateAt = addTime (txTierRotation policy) now
       }
  where
    interval :: DiffTime
    interval = sweepInterval policy

    grace :: DiffTime
    grace = doomGrace policy

    countersInterval :: DiffTime
    countersInterval = 7

    go :: TickState -> m Void
    go tick@TickState { tickDelay, tickCursor, tickDoomed, tickEmitAt, tickRotateAt } = do
      threadDelay tickDelay
      tickAt <- getMonotonicTime
      -- the root flags, then the coin, on its own cadence, before the sweep
      refreshTierRoots policy registry
      rotateAt' <-
        if tickAt >= tickRotateAt
           then do atomically (rotateTiers policy registry)
                   pure (nextDue (txTierRotation policy) tickAt tickRotateAt)
           else pure tickRotateAt
      -- the holders of what the last sweep doomed, one peer's cell per
      -- transaction, for the review in this sweep's transaction: one
      -- write per tick
      -- at most a batch of the doomed reviewed this tick, the rest carried:
      -- a sweep dooms within the batch, but the thread's start may find any
      -- number doomed
      let (reviewNow, carried) = splitBatch tickDoomed
      held <- if IntSet.null reviewNow then pure IntSet.empty
                                       else heldDoomed registry reviewNow
      -- The review's entries count against the transaction's budget,
      -- 'sweepBatch': the review, the expiry and the walk together do at
      -- most that much work in it.
      (backlog, cursor', doomed') <- sweepWith $ \st ->
        let (reviewed, _) = State.reviewDoomed tickAt held reviewNow st in
        case State.sweepSharedStateUpTo (sweepBatch - IntSet.size reviewNow) grace tickAt
                                        tickCursor reviewed of
          (st', more, next, newlyDoomed) -> (st', (more, next, newlyDoomed `IntSet.union` carried))
      drain backlog tickAt
      drained <- getMonotonicTime
      tick' <- if drained >= tickEmitAt
                  then emit drained
                            tick { tickEmitAt = nextDue countersInterval drained tickEmitAt }
                  else pure tick
      -- The next sweep: an interval after this one, whatever the drain and
      -- an emission took, and a pause at least, so that the writes stay a
      -- pause apart when a transaction overran the interval.
      now <- getMonotonicTime
      go tick' { tickDelay    = max drainPause (addTime interval tickAt `diffTime` now),
                 tickCursor   = cursor',
                 tickDoomed   = doomed',
                 tickRotateAt = rotateAt' }

    -- the first 'sweepBatch' of a set of doomed keys, and the rest
    splitBatch :: IntSet -> (IntSet, IntSet)
    splitBatch doomed
      | IntSet.size doomed <= sweepBatch = (doomed, IntSet.empty)
      | otherwise =
          let (now', later) = splitAt sweepBatch (IntSet.toAscList doomed)
          in (IntSet.fromDistinctAscList now', IntSet.fromDistinctAscList later)

    -- An emission at a time: the counters when they changed since the
    -- last one, the tier census as of now when it changed (always the
    -- first time), the shared state when it was written since; what the
    -- next one carries.
    emit :: Time -> TickState -> m TickState
    emit now tick@TickState { tickCounters, tickRevision, tickCensus } = do
      current <- snapshotCountersWith atomically retiredCountersVar registry
      when (current /= tickCounters) $ traceWith countersTracer current
      census <- atomically (tierCensus now registry)
      when (Just census /= tickCensus) $ traceWith sharedStateTracer (TraceTxTiers census)
      st <- readTVarIO sharedStateVar
      let revision = sharedRevision st
      when (revision /= tickRevision) $
        traceWith sharedStateTracer (TraceSharedTxState st (sharedTxStateCounts st))
      pure tick { tickCounters = current, tickRevision = revision, tickCensus = Just census }

    -- One transaction over the state alone: the step applied, the state
    -- written when it changed, as one write whatever the step bumped the
    -- revision by, the step's result returned.
    sweepWith :: (     SharedTxState peeraddr txid
                    -> (SharedTxState peeraddr txid, a))
              -> m a
    sweepWith step = atomically $ do
      st <- readTVar sharedStateVar
      let (st', result) = step st
      writeSharedStateIfChanged (registryMirrors registry) sharedStateVar
                                (sharedRevision st) st'
      return result

    -- Drain a backlog in expiry-only transactions 'drainPause' apart,
    -- each run only if its pause and the one before the next sweep, due an
    -- interval after this one, still fit.
    drain :: Bool -> Time -> m ()
    drain False _ = return ()
    drain True tickAt = do
      now <- getMonotonicTime
      if addTime (2 * drainPause) now `diffTime` tickAt > interval
         then return ()
         else do
           threadDelay drainPause
           now' <- getMonotonicTime
           more <- sweepWith (State.expireSharedStateUpTo sweepBatch now')
           drain more tickAt

    -- The next time something on a period is due: a period after the time
    -- it was due, so that a drain delaying this one does not delay the
    -- next; or, were that already past, a period after now.
    nextDue :: DiffTime -> Time -> Time -> Time
    nextDue period now dueAt
      | next > now = next
      | otherwise  = addTime period now
      where
        next = addTime period dueAt

-- | How often the counters thread sweeps: a quarter of the retention
-- lifetime, kept between 100 ms and 1 s.
sweepInterval :: TxDecisionPolicy -> DiffTime
sweepInterval policy = max 0.1 (min 1 (bufferedTxsMinLifetime policy / 4))

-- | How long an active entry stays claimable with nobody attempting or
-- submitting it before a sweep dooms it, and how long a spared one stays
-- out of review: two sweep intervals.  A fresh entry is claimed within a
-- round trip, so a candidate is one whose advertisers all left, are full
-- or are stalled; the doom costs them one write to clear it, or the
-- review one read of each peer's cell, per grace.
doomGrace :: TxDecisionPolicy -> DiffTime
doomGrace policy = 2 * sweepInterval policy

-- | The most work one sweep's transaction does: the doomed entries it
-- reviews, the retained entries it expires and the table entries it
-- walks, together ('txCountersThreadV2'); positive, so a sweep that finds
-- work makes progress on it.  Every peer's write to the shared state
-- restarts the transaction, so it has to stay a fraction of a millisecond
-- under a loaded relay's hundreds of writes a second (the sweep benchmark
-- measures it).  Not a policy knob: the length is a property of this
-- node's STM, not of the protocol, and with 'drainPause' it drains a
-- hundred thousand entries a second, above any arrival rate.  An
-- abandoned entry waits for the walk to reach it, one sweep per batch of
-- table entries, at the cost of its memory meanwhile.
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
    live <- mapM (runTx . readTVar . peerCountersCell) cells
    pure $! emittedCounters (mconcat (retired : live))

-- | The keys among the given ones that some live peer advertises, read
-- from one peer's in-flight cell per transaction like the counters'
-- cells.  A peer registered after the cells were listed could not have
-- advertised a doomed key before the doom, and its reply after it clears
-- the doom; a peer that left meanwhile shows its last set, which spares
-- at worst, and is not listed next time.  Forced here, so that the
-- sweep's transaction, which takes the result, does none of the work.
heldDoomed
  :: MonadSTM m
  => PeerTxRegistry m peeraddr
  -> IntSet.IntSet
  -> m IntSet.IntSet
heldDoomed registry doomed = do
    cells <- Map.elems <$> readTVarIO (registeredPeers registry)
    held <- mapM (\cell -> do
                    pif <- readTVarIO (peerInFlightCell cell)
                    pure $! IntSet.intersection doomed (pifAdvertised pif))
                 cells
    pure $! IntSet.unions held

-- | Register a joining peer's cells under its address and a fresh
-- registration number: the key 'retirePeerCounters' takes.
registerPeerCells
  :: (MonadSTM m, Ord peeraddr)
  => PeerTxRegistry m peeraddr
  -> peeraddr
  -> TxSubmissionCountersVar m
  -> PeerTxInFlightVar m
  -> STM m (PeerRegistration peeraddr)
registerPeerCells registry peeraddr peerCountersVar peerInFlightVar = do
    n <- stateTVar (nextRegistration registry) (\i -> (i, i + 1))
    modifyTVar (registeredPeers registry)
               (Map.insert (peeraddr, n) (PeerCells peerCountersVar peerInFlightVar))
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
  -> STM m (PeerTierEntry m, Word64)
registerPeerTier registry = do
    entry <- PeerTierEntry <$> newTVar (PeerTierState SlowTier False 0 Nothing)
    seed <- stateTVar (registryRng registry) uniform
    pure (entry, seed)

-- | The governor's answer on whether a peer is a local root, with the
-- root epoch at the ask: a refresh ('refreshRoots') leaves a flag asked
-- at or after its own snapshot as it is, so the later ask wins whichever
-- transaction writes first.
data RootAnswer = RootAnswer {
    rootAnswer :: !Bool,
    rootAsked  :: !Word64
  }
  deriving (Eq, Show)

-- | Ask the governor whether a peer is a local root now, in a transaction
-- of its own ('joinTiers'), so that a placement reads nothing of the
-- governor's.
askRoot :: MonadSTM m
        => PeerTxRegistry m peeraddr
        -> peeraddr
        -> STM m RootAnswer
askRoot registry peeraddr =
    RootAnswer <$> (($ peeraddr) <$> registryLocalRoots registry)
               <*> readTVar (registryRootEpoch registry)

-- | Put a peer in the order and place the tiers, with the governor's
-- answer on whether it is a local root, asked just before in a
-- transaction of its own ('askRoot'), so that a root takes no coin slot
-- at its own placement; nothing when it is in the order already.  The
-- server calls it ('joinTiers') once its initial delay has passed, before
-- its first decision, so a peer not polling yet holds no fast slot and
-- the first decision is served on the tier it was placed on.
joinTier
  :: (MonadSTM m, Ord peeraddr, Hashable peeraddr)
  => TxDecisionPolicy
  -> PeerTxRegistry m peeraddr
  -> PeerRegistration peeraddr
  -> PeerTierEntry m
  -> RootAnswer
  -> STM m ()
joinTier policy registry registration entry@PeerTierEntry { pteState }
         RootAnswer { rootAnswer, rootAsked } = do
    tiers <- readTVar (peerTiers registry)
    unless (Map.member registration tiers) $ do
      modifyTVar pteState (\s -> s { ptsRoot = rootAnswer, ptsRootAsked = rootAsked })
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

-- | Bring every peer in the order's root flag up to date with the
-- governor's answer, from a snapshot taken just before in a transaction
-- of its own ('snapshotRoots'), and place the tiers when a flag changed,
-- so that a peer that stopped being a root, or became one, is in or out
-- of the order's candidates from now: an ex-root is served by its coin,
-- and takes a free fast slot, at once rather than at the next rotation.
-- A flag asked at or after the snapshot, by a join between the two
-- transactions, is left as it is: its answer is the newer; one asked
-- before the snapshot by a join after this transaction keeps its older
-- answer until the next refresh, a tick at most.  One read per
-- peer in the order, as a placement; the transaction's work grows with
-- the square of the peers, since the runtime finds a variable's entry by
-- a scan of the transaction's own, about a quarter of a millisecond at
-- five hundred, once a sweep; a decision that changes its source deadline
-- restarts it.
refreshRoots :: (MonadSTM m, Ord peeraddr, Hashable peeraddr)
             => TxDecisionPolicy
             -> PeerTxRegistry m peeraddr
             -> (peeraddr -> Bool)
             -> Word64
             -- ^ the snapshot's epoch
             -> STM m ()
refreshRoots policy registry isRoot epoch = do
    tiers <- readTVar (peerTiers registry)
    changed <- forM (Map.toList tiers) $ \((addr, _), PeerTierEntry { pteState }) -> do
      s <- readTVar pteState
      let root = isRoot addr
      if ptsRootAsked s >= epoch || root == ptsRoot s
         then pure False
         else True <$ writeTVar pteState s { ptsRoot = root, ptsRootAsked = epoch }
    when (or changed) $ placeTiers policy registry

-- | The refresh's snapshot, in a transaction of its own: the root epoch
-- advanced, and whether each peer is a local root now.
snapshotRoots :: MonadSTM m
              => PeerTxRegistry m peeraddr
              -> STM m (peeraddr -> Bool, Word64)
snapshotRoots registry = do
    epoch <- stateTVar (registryRootEpoch registry) (\e -> (e + 1, e + 1))
    isRoot <- registryLocalRoots registry
    pure (isRoot, epoch)

-- | The counters thread's refresh of the root flags: the governor's
-- answers in a transaction of its own ('snapshotRoots'), then
-- 'refreshRoots' in another, so that neither the governor's writes abort
-- a placement nor a placement's reads of every peer's tier state stand
-- in the governor's way.
refreshTierRoots :: (MonadSTM m, Ord peeraddr, Hashable peeraddr)
                 => TxDecisionPolicy
                 -> PeerTxRegistry m peeraddr
                 -> m ()
refreshTierRoots policy registry = do
    (isRoot, epoch) <- atomically (snapshotRoots registry)
    atomically (refreshRoots policy registry isRoot epoch)

-- | One rotation: a new salt from the coin's generator, and the tiers
-- placed from the order it gives.
rotateTiers :: (MonadSTM m, Ord peeraddr, Hashable peeraddr)
            => TxDecisionPolicy
            -> PeerTxRegistry m peeraddr
            -> STM m ()
rotateTiers policy registry = do
    salt <- stateTVar (registryRng registry) mkSalt
    writeTVar (registrySalt registry) salt
    placeTiers policy registry

-- | Place the tiers from the salted order: the first 'txFastPeers' of the
-- peers in the order that are not local roots, as their join or the last
-- refresh read it, are fast, the local roots are fast besides, the rest
-- slow.  Writes a peer's coin on a change only.  One read per peer and
-- the sort of the candidates; it reads nothing of the governor's, so
-- only a join, a leave or a peer's rare flag write restarts it.
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
        -- decision ('withTier') whatever its coin, and an ex-root is
        -- served by its coin from the refresh that demoted it, which
        -- places
        let coin = if registration `Set.member` fastSet then FastTier else SlowTier
        writeIfChanged pteState s s { ptsCoin = coin }

-- | The peers in the order by the standing they are served on at a time:
-- a source until its deadline.
tierCensus :: MonadSTM m => Time -> PeerTxRegistry m peeraddr -> STM m TierCensus
tierCensus now registry = do
    tiers <- readTVar (peerTiers registry)
    censusOf . map (tierStanding now) <$> mapM (readTVar . pteState) (Map.elems tiers)

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
    -- the server's initial delay and before its first decision; whether
    -- it is a local root is asked of the governor first, in a transaction
    -- of its own.
    joinTiers            :: m (),

    -- | Wait until the counter a decision parked on moves past the value
    -- it saw, or the optional timeout expires.
    awaitPark            :: ParkCounter
                         -> Word64
                         -> Maybe (RegisteredDelay m)
                         -> STM m (),

    -- | Compute the next action for this peer in non-pipelined mode,
    -- resolved against the state it was decided in.
    runNextPeerAction    :: Time
                         -> Maybe DiffTime
                         -> PeerTxLocalState tx
                         -> m (ResolvedPeerAction txid tx, PeerTxLocalState tx),

    -- | Compute the next action for this peer in pipelined mode, resolved
    -- against the state it was decided in.
    runNextPeerActionPipelined :: Time
                               -> PeerTxLocalState tx
                               -> m (ResolvedPeerAction txid tx, PeerTxLocalState tx),

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

    -- | Record the mempool's verdict on the txs this peer submitted, and
    -- the time the submission took.  An accepted tx moves from the table
    -- to the retained set, so its later advertisements are acknowledged
    -- without a body; the txid comes along so that, if the state has
    -- forgotten the key, the tx can be retained by its txid.  A rejected
    -- tx, given by key, releases this peer's lease so another advertiser
    -- may try it.
    applySubmittedTxs    :: Time
                         -> DiffTime
                         -> [(TxKey, txid)]
                         -> [TxKey]
                         -> PeerTxLocalState tx
                         -> m (PeerTxLocalState tx)
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
  -> TxSubmissionConfig
  -> TxSubmissionMempoolReader txid tx idx m
  -> SharedTxStateVar m peeraddr txid
  -> PeerTxRegistry m peeraddr
  -> TxSubmissionCountersVar m
  -> peeraddr
  -> (PeerTxAPI m txid tx -> m a)
  -> m a
withPeer policy config TxSubmissionMempoolReader { mempoolGetSnapshot }
         sharedStateVar registry retiredCountersVar peeraddr io =
    bracket acquire release run
  where
    acquire = do
      peerInFlightVar <- newTVarIO emptyPeerTxInFlight
      peerCountersVar <- newTVarIO mempty
      (registration, entry, seed) <- atomically $ do
        registration <- registerPeerCells registry peeraddr peerCountersVar peerInFlightVar
        (entry, seed) <- registerPeerTier registry
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
        writeSharedStateIfChanged mirrors sharedStateVar (sharedRevision sharedState)
                                  (scrubFromPeerInFlight peeraddr now pif sharedState)
        retirePeerCounters retiredCountersVar registry registration
                           peerCountersVar
      atomically $ leaveTier policy registry registration

    run (registration, peerInFlightVar, peerCountersVar, entry, seed) = io PeerTxAPI {
          apiJitterSeed = seed
        , joinTiers = do
            answer <- atomically (askRoot registry peeraddr)
            atomically (joinTier policy registry registration entry answer)
        , awaitPark = awaitMirrorChange . counterMirror mirrors
        , runNextPeerAction = \now downloadTime_m ->
            runDecisionImp policy config registry sharedStateVar peerInFlightVar peerCountersVar
                           entry peeraddr (NonPipelined downloadTime_m) now
        , runNextPeerActionPipelined =
            runDecisionImp policy config registry sharedStateVar peerInFlightVar peerCountersVar
                           entry peeraddr Pipelined
        , applyReceivedTxIds = applyReceivedTxIdsImp policy mempoolGetSnapshot mirrors
                                 sharedStateVar peerInFlightVar peerCountersVar
        , applyReceivedTxs = applyReceivedTxsImp policy mempoolGetSnapshot mirrors
                               sharedStateVar peerInFlightVar peerCountersVar peeraddr
        , applySubmittedTxs = applySubmittedTxsImp policy mirrors sharedStateVar
                                peerInFlightVar peerCountersVar peeraddr
        }
    mirrors = registryMirrors registry

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
          -- peer an option: a wake.  What the peer only advertised costs
          -- nothing here: the sweep's review finds it abandoned.
          sharedGeneration = sharedGeneration st + 1,
          sharedRevision   = sharedRevision st + 1,
          sharedClaimable  = sharedClaimable st + 1
        }
  where
    -- Walk each unique key once and apply all the modifications in a
    -- single 'IntMap.adjust'.  Keys typically appear in several of the
    -- sets ('pifLeased' is a subset of 'pifAttempting ∪ pifSubmitting',
    -- and a leased key is advertised).
    workKeys = pifLeased pif
        `IntSet.union` pifAttempting pif
        `IntSet.union` pifSubmitting pif

    presentKeys = IntMap.keysSet (sharedTxTable st `IntMap.restrictKeys` workKeys)

    sharedTxTable' = IntSet.foldl' scrubOne (sharedTxTable st) presentKeys

    scrubOne tbl k = IntMap.adjust (modifyEntry k) k tbl

    modifyEntry k =
        applyIf (IntSet.member k (pifSubmitting pif)) clearSubmission
      . applyIf (IntSet.member k (pifAttempting pif)) decAttempt
      . applyIf (IntSet.member k (pifLeased     pif)) releaseLease

    applyIf True  f = f
    applyIf False _ = id

    releaseLease entry = case txLease entry of
      TxLeased owner _ | owner == peeraddr -> entry { txLease = TxClaimable now }
      _                                    -> entry

    decAttempt entry = entry { txAttempt = max 0 (txAttempt entry - 1) }

    clearSubmission entry = entry { txInSubmission = False }

-- | A park: until the mirror of the counter the peer parks on
-- ('CounterMirrors') moves past the value seen or the delay expires: the
-- fast tier's on the wake counter, when the shared state changes (lease
-- expiries, new tx advertisements, mempool resolutions), the slow tier's
-- on the claimable counter.  The mirror, not the state: a loaded relay
-- writes the state many times a second, and the parked peers would
-- re-check at every write.  A spurious wake, on a move that grants this
-- peer nothing, is harmless: the peer re-runs 'nextPeerAction', selects
-- 'PeerDoNothing' again, and parks on the value it sees then.  A park on
-- no counter, no mirror: until the delay expires.
awaitMirrorChange :: MonadTimer m
                  => Maybe (StrictTVar m Word64)
                  -> Word64
                  -> Maybe (RegisteredDelay m)
                  -> STM m ()
awaitMirrorChange mirror_m seen mRegisteredDelay = do
  moved   <- maybe (pure False) (fmap (/= seen) . readTVar) mirror_m
  expired <- maybe (pure False) RegisteredDelay.read mRegisteredDelay
  check (moved || expired)

-- | Write the shared TVar when the pure state step moved the revision,
-- with 'sharedRevision' one past the revision read whatever the step
-- bumped it by: the revision counts committed writes, and one step may
-- bump it twice (a stuck-entry bump and a submission in one transaction).
-- Every bump of the wake counter comes with one of the revision, so the
-- revision alone says whether the step changed the state.  The counters'
-- mirrors follow in the same transaction.
writeSharedStateIfChanged :: MonadSTM m
                          => CounterMirrors m
                          -> SharedTxStateVar m peeraddr txid
                          -> Word64                 -- ^ pre-step 'sharedRevision'
                          -> SharedTxState peeraddr txid
                          -> STM m ()
writeSharedStateIfChanged mirrors sharedStateVar sharedRevision0 sharedState'
  | sharedRevision sharedState' == sharedRevision0 = pure ()
  | otherwise = do
      writeTVar sharedStateVar sharedState' { sharedRevision = sharedRevision0 + 1 }
      mirrorCounters mirrors sharedState'

-- | Bring the counters' mirrors up to the state's, each written only when
-- it moved, so that the parked peers wake for that alone.
mirrorCounters :: MonadSTM m
               => CounterMirrors m
               -> SharedTxState peeraddr txid
               -> STM m ()
mirrorCounters CounterMirrors { generationMirror, claimableMirror } st = do
    follow generationMirror (sharedGeneration st)
    follow claimableMirror  (sharedClaimable  st)
  where
    follow mirror current = do
      seen <- readTVar mirror
      writeIfChanged mirror seen current

-- | Write a variable when the value differs from the one read, so that a
-- transaction that changes nothing writes nothing and wakes nobody.
writeIfChanged :: (MonadSTM m, Eq a)
               => StrictTVar m a
               -> a  -- ^ as read
               -> a  -- ^ to write
               -> STM m ()
writeIfChanged var before after
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
    -- a request for acknowledgements alone is counted apart, and as no
    -- slow request: the slow count is of the requests for txids, which the
    -- gate spaces
    PeerRequestTxIds txIdsToAck txIdsToReq
      | txIdsToAck /= 0 || txIdsToReq /= 0 ->
          let forTxIds = txIdsToReq > 0 in
          modifyTVar countersVar $ \c -> c
            { txIdMessagesSent      = txIdMessagesSent c + 1
            , txIdsRequested        = txIdsRequested c + fromIntegral txIdsToReq
            , txIdBlockingReqsSent  = txIdBlockingReqsSent c + blockingSent
            , txIdPipelinedReqsSent = txIdPipelinedReqsSent c + pipelinedSent
            , txIdSlowReqsSent      = txIdSlowReqsSent c + (if forTxIds then slowSent else 0)
            , txIdAckOnlySent       = txIdAckOnlySent c + (if forTxIds then 0 else 1)
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

-- | How a decision is run: outside a pipeline, with the duration of the
-- body-download episode that has just drained, if one did; or inside one,
-- where the pipeline cannot have drained and a txid request is pipelined.
data DecisionMode
  = NonPipelined (Maybe DiffTime)
  | Pipelined

-- | Compute the next action for this peer, resolved against the state it
-- was decided in.
runDecisionImp :: ( MonadSTM m
                  , Ord peeraddr
                  , Ord txid )
               => TxDecisionPolicy
               -> TxSubmissionConfig
               -> PeerTxRegistry m peeraddr
               -> SharedTxStateVar m peeraddr txid
               -> PeerTxInFlightVar m
               -> TxSubmissionCountersVar m
               -> PeerTierEntry m
               -> peeraddr
               -> DecisionMode
               -> Time
               -> PeerTxLocalState tx
               -> m (ResolvedPeerAction txid tx, PeerTxLocalState tx)
runDecisionImp policy config registry sharedStateVar peerInFlightVar countersVar entry peeraddr
               mode now peerState =
  atomically $ do
    -- the tier from the peer's own tier state, which a placement, a root
    -- refresh or this peer's decisions write, rarely; nothing of the
    -- governor's, which rewrites its public state every iteration and
    -- would retry every in-flight decision on each of them
    peerState0 <- withTier policy now entry peerState
    sharedState <- readTVar sharedStateVar
    peerInFlight <- readTVar peerInFlightVar
    let sharedRevision0 = sharedRevision sharedState
        (peerAction, peerState', peerInFlight', sharedState') =
          step now policy config peeraddr peerState0 peerInFlight sharedState
    writeSharedStateIfChanged (registryMirrors registry) sharedStateVar
                              sharedRevision0 sharedState'
    writeIfChanged peerInFlightVar peerInFlight peerInFlight'
    updateCountersForAction countersVar (kind peerState')
                            (effectiveTier policy now peerState0) downloadTime_m
                            peerAction
    resolved <- resolvePeerAction (registryMirrors registry) sharedState' peerState' peerAction
    return (resolved, peerState')
  where
    (step, kind, downloadTime_m) = case mode of
      NonPipelined d -> (State.nextPeerAction, txIdRequestKind, d)
      Pipelined      -> (State.nextPeerActionPipelined, const PipelinedTxIdRequest, Nothing)

-- | The peer's tier, read into its local state in its decision's
-- transaction: a local root is fast whatever the coin said.  Until when
-- the peer is a source goes to its tier state when it changed, for the
-- census.
withTier :: MonadSTM m
         => TxDecisionPolicy
         -> Time
         -> PeerTierEntry m
         -> PeerTxLocalState tx
         -> STM m (PeerTxLocalState tx)
withTier policy now PeerTierEntry { pteState } peerState = do
  s <- readTVar pteState
  let sourceUntil' = sourceUntil policy now (peerNovelAt peerState)
  writeIfChanged pteState s s { ptsSourceUntil = sourceUntil' }
  let tier = if ptsRoot s then FastTier else ptsCoin s
  pure $ if peerTier peerState == tier then peerState else peerState { peerTier = tier }

-- | Process a batch of txids received from this peer.
applyReceivedTxIdsImp :: ( MonadSTM m
                         , HasRawTxId txid )
                      => TxDecisionPolicy
                      -> STM m (MempoolSnapshot txid tx idx)
                      -> CounterMirrors m
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
applyReceivedTxIdsImp policy mempoolGetSnapshot mirrors sharedStateVar peerInFlightVar
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
    writeSharedStateIfChanged mirrors sharedStateVar sharedRevision0 sharedState'
    writeIfChanged peerInFlightVar peerInFlight peerInFlight'
    let received = fromIntegral (length txidsAndSizes)
        -- the reply answers the oldest outstanding request, as recorded
        -- when it went out
        request = fst (takeTxIdReply peerState)
        slowReceived = case request of
          TxIdsOn SlowTier -> received
          _                -> 0
        -- a reply to a request for acknowledgements alone, empty by
        -- construction, bumps no size count
        (r0, r1, r2, r5, r10) = case request of
          TxIdsOn _ -> replyBucket (length txidsAndSizes)
          AcksOnly  -> (0, 0, 0, 0, 0)
    modifyTVar countersVar $ \c -> c
      { txIdRepliesReceived = txIdRepliesReceived c + 1
      , txIdsReceived       = txIdsReceived c + received
      , txIdsReceivedSlow   = txIdsReceivedSlow c + slowReceived
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
                    -> CounterMirrors m
                    -> SharedTxStateVar m peeraddr txid
                    -> PeerTxInFlightVar m
                    -> TxSubmissionCountersVar m
                    -> peeraddr
                    -> Time
                    -> [(txid, tx)]
                    -> PeerTxLocalState tx
                    -> m (Int, PeerTxLocalState tx)
applyReceivedTxsImp policy mempoolGetSnapshot mirrors sharedStateVar peerInFlightVar
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
    writeSharedStateIfChanged mirrors sharedStateVar sharedRevision0 sharedState'
    writeIfChanged peerInFlightVar peerInFlight peerInFlight'
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
                     -> CounterMirrors m
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
applySubmittedTxsImp policy mirrors sharedStateVar peerInFlightVar countersVar peeraddr
                     now submissionWait acceptedTxs rejectedTxs peerState =
  atomically $ do
    sharedState <- readTVar sharedStateVar
    peerInFlight <- readTVar peerInFlightVar
    let sharedRevision0 = sharedRevision sharedState
        (peerState', peerInFlight', sharedState') =
          State.handleSubmittedTxs now policy peeraddr acceptedTxs
                                   rejectedTxs peerState peerInFlight sharedState
    writeSharedStateIfChanged mirrors sharedStateVar sharedRevision0 sharedState'
    writeIfChanged peerInFlightVar peerInFlight peerInFlight'
    modifyTVar countersVar $ \c -> c
      { txsAccepted      = txsAccepted c + fromIntegral (length acceptedTxs)
      , txsRejected      = txsRejected c + fromIntegral (length rejectedTxs)
      , txSubmissionWait = txSubmissionWait c + submissionWait
      }
    return peerState'

-- | A decision as the peer thread carries it out, resolved against the
-- state it was made in: the txids and advertised sizes of the bodies to
-- request, the buffered bodies to submit, the value a park waits past.
-- Called in the deciding transaction, after its write, so a key that
-- leaves the state afterwards cannot fail it.
resolvePeerAction :: ( HasCallStack
                     , MonadSTM m
                     , Ord txid )
                  => CounterMirrors m
                  -> SharedTxState peeraddr txid
                  -> PeerTxLocalState tx
                  -> PeerAction
                  -> STM m (ResolvedPeerAction txid tx)
resolvePeerAction mirrors sharedState peerState action =
  case action of
    -- a park waits on a counter's mirror, so its value to wait past is
    -- the mirror's, read after this transaction's write: the state's own
    -- counter may be ahead of a mirror that has not followed it yet (the
    -- mirrors start at zero), and a park past a value the mirror never
    -- shows would wake at once
    PeerDoNothing delay_m counter ->
      -- a park on no counter has a delay, or nothing would wake it
      assert (counter /= NoCounter || isJust delay_m) $
      (\seen -> pure $! ResolvedDoNothing delay_m counter seen) =<< maybe (pure 0) readTVar (counterMirror mirrors counter)
    PeerRequestTxIds toAck toReq ->
      pure $! ResolvedRequestTxIds toAck toReq
    -- every field evaluated here, in the transaction: a thunk would hold
    -- the state the decision was made in until the request, or the
    -- submission, which may block on the mempool, forced it
    PeerRequestTxs txKeys ->
      pure $! ResolvedRequestTxs (Map.fromList (map resolveRequested txKeys))
    PeerSubmitTxs txKeys ->
      -- the list with its spine and every element evaluated: the strict
      -- map forces its own entries, a list forces nothing of its own
      pure $! ResolvedSubmitTxs (forceElemsToWHNF (map resolveBuffered txKeys))
  where
    resolveRequested key@(TxKey k) =
      let !txid   = resolveTxKey sharedState key
          !txSize = case IntMap.lookup k (peerAvailableTxIds peerState) of
             Just size -> size
             Nothing   -> error $
               "TxSubmission.V2.resolvePeerAction: missing tx size for key "
               ++ show k
      in (txid, txSize)
    -- pre-condition: the key is in 'peerDownloadedTxs', ensured by
    -- 'pickSubmitAction'
    resolveBuffered key@(TxKey k) =
      let !txid = resolveTxKey sharedState key
          !tx   = case IntMap.lookup k (peerDownloadedTxs peerState) of
             Just body -> body
             Nothing   -> error $
               "TxSubmission.V2.resolvePeerAction: missing buffered tx for key "
               ++ show k
      in (key, txid, tx)
