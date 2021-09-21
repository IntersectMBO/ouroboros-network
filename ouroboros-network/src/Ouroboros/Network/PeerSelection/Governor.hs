{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}


-- | This subsystem manages the discovery and selection of /upstream/ peers.
--
module Ouroboros.Network.PeerSelection.Governor (
    -- * Design overview
    -- $overview

    -- * Peer selection governor
    -- $peer-selection-governor

    PeerSelectionPolicy(..),
    PeerSelectionTargets(..),
    PeerSelectionActions(..),
    PeerStateActions(..),
    TracePeerSelection(..),
    DebugPeerSelection(..),
    peerSelectionGovernor,

    -- * Peer churn governor
    -- $peer-churn-governor
    peerChurnGovernor,

    -- * Internals exported for testing
    sanePeerSelectionTargets,
    establishedPeersStatus,
    PeerSelectionState(..),
    PeerSelectionCounters(..)
) where

import           Data.Void (Void)
import           Data.Semigroup (Min(..))

import           Control.Applicative (Alternative((<|>)))
import qualified Control.Concurrent.JobPool as JobPool
import           Control.Concurrent.JobPool (JobPool)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Tracer (Tracer(..), traceWith, contramap)
import           System.Random

import qualified Ouroboros.Network.PeerSelection.EstablishedPeers as EstablishedPeers
import qualified Ouroboros.Network.PeerSelection.KnownPeers as KnownPeers
import qualified Ouroboros.Network.PeerSelection.Governor.ActivePeers      as ActivePeers
import qualified Ouroboros.Network.PeerSelection.Governor.EstablishedPeers as EstablishedPeers
import qualified Ouroboros.Network.PeerSelection.Governor.KnownPeers       as KnownPeers
import qualified Ouroboros.Network.PeerSelection.Governor.Monitor          as Monitor
import qualified Ouroboros.Network.PeerSelection.Governor.RootPeers        as RootPeers
import           Ouroboros.Network.PeerSelection.Governor.Types
import           Ouroboros.Network.BlockFetch (FetchMode (..))


-- TODO: at a later patch it will be defined in
-- 'Ouroboros.Network.Diffusion.Policies'
--
closeConnectionTimeout :: DiffTime
closeConnectionTimeout = 120


{- $overview

We have a number of requirements for constructing our connectivity graphs:

 * We must do it in a decentralised way, using only local information;
 * It should avoid and recover from accidental or deliberate partitions or
   eclipse attacks;
 * The graph should give us good performance for block diffusion. This means
   we need the combination of low hop counts, and minimising the hop lengths.
   We want one slot leader to be able to send to the next within the deadline
   in at least 95% of cases.

[\"Small world" graph theory](https://press.princeton.edu/books/paperback/9780691117041/small-worlds)
tells us that we can use random graph construction to make graphs with a low
characteristic path length (i.e. hop count). We can build random graphs with
random gossip techniques. This deals with our requirement for decentralisation
and our goal of low hop counts.

The remaining significant issues are:

 * the goal of short hop lengths, and
 * avoiding and recovering from partitions and eclipse attacks.

Our design is to augment random gossip with two /governors/ (control loops) to
address these two issues. The design is relatively simple, and has the virtue
that the policy for the governors can be adjusted with relatively few
compatibility impacts. This should enable the policy to be optimised based on
real-world feedback, and feedback from simulations of scale or scenarios that
are hard (or undesirable) to test in a real deployment.

Each node maintains three sets of known peer nodes:

 [cold peers]: are peers that are known of but where there is no established
               network connection;

 [warm peers]: are peers where a bearer connection is established but it is used
               only for network measurements and is not used for any application
               level consensus protocols;

 [hot peers]: are peers where the bearer connection is actively used for the
              application level consensus protocols.

Limited information is maintained for these peers, based on previous direct
interactions. For cold nodes this will often be absent as there may have been
no previous direct interactions. This information is comparable with
\"reputation\" in other systems, but it should be emphasised that it is purely
local and not shared with any other node. It is not shared because it is not
necessary and because establishing trust in such information is difficult and
would add additional complexity. The information about peers is kept
persistently across node restarts, but it is always safe to re-bootstrap – as
new nodes must do.

For an individual node to join the network, the bootstrapping phase starts by
contacting root nodes and requesting sets of other peers. Newly discovered
peers are added to the cold peer set. It proceeds iteratively by randomly
selecting other peers to contact to request more known peers. This gossip
process is controlled by a governor that has a target to find and maintain a
certain number of cold peers. Bootstrapping is not a special mode, rather it is
just a phase for the governor following starting with a cold peers set
consisting only of the root nodes. This gossiping aspect is closely analogous
to the first stage of Kademlia, but with random selection rather than selection
directed towards finding peers in an artificial metric space.

The root nodes used in the bootstrapping phase are the stakepool relays
published in the blockchain as part of the stakepool registration process.
See the [Shelley delegation design specification, Sections 3.4.4 and 4.2](https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/delegationDesignSpec/latest/download-by-type/doc-pdf/delegation_design_spec).
As with Bitcoin, a recent snapshot of this root set must be distributed with
the software.

The peer selection governor engages in the following activities:

 * the random gossip used to discover more cold peers;
 * promotion of cold peers to be warm peers;
 * demotion of warm peers to cold peers;
 * promotion of warm peers to hot peers; and
 * demotion of hot peers to warm peers.

The peer selection governor has these goals to establish and maintain:

 * a target number of cold peers (e.g. 1000)
 * a target number of hot peers (e.g. order of 2–20)
 * a target number of warm peers (e.g. order of 10–50)
 * a set of warm peers that are sufficiently diverse in terms of hop distance
 * a target churn frequency for hot\/warm changes
 * a target churn frequency for warm\/cold changes
 * a target churn frequency for cold\/unknown changes

The target churn values are adjusted by the /peer churn governor/, which we
will discuss below.

Local static configuration can also be used to specify that certain known nodes
should be selected as hot or warm peers. This allows for fixed relationships
between nodes controlled by a single organisation, such as a stake pool with
several relays. It also enables private peering relationships between stake
pool operators and other likely deployment scenarios.

Using 5–20 hot peers is not as expensive as it might sound. Keep in mind that
only block headers are sent for each peer. The block body is typically only
requested once. It is also worth noting that the block body will tend to follow
the shortest paths through the connectivity graph formed by the hot peer links.
This is because nodes will typically request the block body from the first node
that sends the block header.

While the purpose of cold and hot peers is clear, the purpose of warm peers
requires further explanation. The primary purpose is to address the challenge
of avoiding too many long hops in the graph. The random gossip is oblivious to
hop distance. By actually connecting to a selection of peers and measuring the
round trip delays we can start to establish which peers are near or far. The
policy for selecting which warm peers to promote to hot peers will take into
account this network hop distance. The purpose of a degree of churn between
cold and warm peers is, in part, to discover the network distance for more
peers and enable further optimisation or adjust to changing conditions. The
purpose of a degree of churn between warm and hot peers is to allow potentially
better warm peers to take over from existing hot peers.

The purpose in maintaining a diversity in hop distances is to assist in
recovery from network events that may disrupt established short paths, such as
internet routing changes, partial loss of connectivity, or accidental formation
of cliques. For example, when a physical infrastructure failure causes the
short paths to a clique of nodes to be lost, if some or all of the nodes in
that clique maintain other longer distance warm links then they can quickly
promote them to hot links and recover. The time to promote from warm to hot
need be no more than one network round trip.

Overall, this approach follows a common pattern for probabilistic search or
optimisation that uses a balance of local optimisation with some elements of
higher order disruption to avoid becoming trapped in some poor local optimum.

The local peer reputation information is also updated when peer connections
fail. The implementation classifies the exceptions that cause connections to
fail into three classes:

 * internal node exceptions e.g. local disk corruption;
 * network failures e.g. dropped TCP connections; and
 * adversarial behaviour, e.g. a protocol violation detected by the
   typed-protocols layer or by the consensus layer.

In the case of adversarial behaviour the peer can be immediately demoted out of
the hot, warm and cold sets. We choose not to maintain negative peer
information for extended periods of time; to bound resources and due to the
simplicity of Sybil attacks.

The peer churn governor deals with the problem of partition and eclipse –
whether malicious or accidental. It adjusts the behaviour of the peer
selection governor over longer time scales. The outer peer churn governor's
actions are:

 * to adjust the target churn frequencies of the peer selection governor for
   promotion\/demotion between the cold\/warm\/hot states
 * partial or total re-bootstrapping under certain circumstances

The peer churn governor monitors the chain growth quality, comparing it with
the stake distribution. The probability of being in a disconnected clique or
being eclipsed is calculated. As this rises the governor increases the target
frequencies for the churn between the hot, warm, cold, and unknown states. In
the worst case it can re-bootstrap the peer discovery entirely by resetting
the set of known peers.
-}

{-

TODO: need to think about managing established connections with upstream/downstream peers in a more symmetric way.

Can we separate that connection management from policy of upstream/downstream selection?

Upstream peers are ones where we choose to talk to them, and we follow their
chain and submit transactions to them. There is a separate subsystem to manage
/downstream/ peers that initiate connections to us.

There is a distinction between which peer chooses to talk to which, and which
peer actually initiates the TCP connection. This is due to the fact that we
reuse TCP connections to run mini-protocols in both directions. So we can
choose to talk to another peer and find that they already initiated a TCP
connection to us, and so we reuse that. For example we can have cases like this:

 1. They initiate the connection to have our node as one of their upstream peers
 2. We decide to reuse the connection to have them as one of our upstream peers
 3. They decide to stop using us as an upstream peer

This is now more or less equivalent to our node having initiated the connection
in the first place because we chose to have them as an upstream peer.


-}


{- $peer-selection-governor

![A 19th century steam governor](https://upload.wikimedia.org/wikipedia/commons/c/c3/Centrifugal_governor_and_balanced_steam_valve_%28New_Catechism_of_the_Steam_Engine%2C_1904%29.jpg)

The 'peerSelectionGovernor' manages the discovery and selection of /upstream/
peers.

We classify (potential or actual) upstream peers in three nested categories:

@
                                                      ▲
                                               forget │
  ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┿━┿━━━━━━━━━━━━┓
  ┃                                                     │ discover   ┃
  ┃  Known peers: the set of all known peers.           ▼            ┃
  ┃  Consists of cold, warm and hot peers.                           ┃
  ┃  Expect ~1000                              demote ▲              ┃
  ┃                                            to cold│              ┃
  ┃ ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┿━┿━━━━━━━━━━┓ ┃
  ┃ ┃                                                   │ promote  ┃ ┃
  ┃ ┃  Established peers: with established bearer.      ▼ to warm  ┃ ┃
  ┃ ┃  Consists of warm and hot peers.                             ┃ ┃
  ┃ ┃  Expect ~10-50                           demote ▲            ┃ ┃
  ┃ ┃                                          to warm│            ┃ ┃
  ┃ ┃ ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┿━┿━━━━━━━━┓ ┃ ┃
  ┃ ┃ ┃                                                 │ promote┃ ┃ ┃
  ┃ ┃ ┃  Active peers: running consensus protocols.     ▼ to hot ┃ ┃ ┃
  ┃ ┃ ┃  Consists of hot peers.                                  ┃ ┃ ┃
  ┃ ┃ ┃  Expect ~2-20                                            ┃ ┃ ┃
  ┃ ┃ ┃                                                          ┃ ┃ ┃
  ┃ ┃ ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛ ┃ ┃
  ┃ ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛ ┃
  ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
@

We define the terms /known/, /established/ and /active/ to be nested sets.
We define the terms /cold/, /warm/ and /hot/ to be disjoint sets. Both
collections of terms are useful. For example there is information wish to
track for all known peers, irrespective of whether they are cold, warm or hot.

So we have six transitions to consider:

 * discover a cold peer
 * promote a cold peer to warm
 * promote a warm peer to hot
 * demote a hot peer to warm
 * demote a warm peer to cold
 * forget a cold peer

We want a design that separates the policy from the mechanism. We must
consider what kinds of policy we might like to express and make sure that
information that the policy needs can be made available.

We will consider each case.

== Discovering cold peers

There are two main mechanisms by which we discover cold peers:

 * Externally supplied peer root set
 * Peer gossip

=== Externally supplied peer root set

There are a few potential sources for root sets:

 * Simulation environment
 * IP address lists from static or dynamic configuration
 * DNS names from static or dynamic configuration
 * IP addresses or DNS names for stake pools registered in the blockchain

Note that none of these sources are fully static except for IP addresses from
static configuration. DNS name to IP address mappings are potentially dynamic.
DNS names can refer to both IPv4 and IPv6 addresses, and to pools of addresses.

In some cases we wish to advertise these root peers to others, and sometimes
we want to keep them private. In particular the deployment for stake pools may
involve keeping the stake pool node itself private, and only advertising
relays.

For an externally supplied peer root set, we divide the problem in two with an
interface where a root set provider is responsible for managing a time-varying
set of addresses, and the peer selection governor observes the time-varying
value. This allows multiple implementations of the root set provider, which
deal with the various sources.

=== Peer gossip

We can ask peers to give us a sample of their set of known peers.

For cold peers we can establish a one-shot connection to ask. For warm peers
we can also ask. We should not ask from the same peer too often. Peers are
expected to return the same set of answers over quite long periods of time.
(This helps peers to distinguish abusive behaviour and reduce the speed with
which the whole network can be mapped.)

So factors we might wish to base our decision on:

 * if we are below the target number of known peers
 * if there are any known peers we have not asked (or attempted to ask)
 * how long since we last asked (so we do not ask too frequently)
 * the known distance of the peer from the root set

This last factor is interesting. Consider what happens if we do a bootstrap
from one root peer. We'll ask it for some more peers and it will give us a
selection. Suppose we pick one of these to get more peers from and it gives us
a similar number of replies. If we now pick the next one randomly from our
combined set we'll have a roughly 50:50 chance of picking from either set.
This approach could quickly lead us into a mostly-depth first exploration of
the graph. But we probably want a more balanced approach between breadth first
and depth first. The traditional ways to do a breadth first or depth first is
to keep a queue or a stack of nodes that have not yet been asked.

Here's another danger: suppose we ask several nodes in parallel but suppose
one gets back to us quicker than all the others. If we are too quick to choose
again then we are giving an advantage to fast peers, and adversaries could
dedicate resources to exploit this to their advantage to get nodes to pick up
more peers from the set supplied by the adversary.

So this suggests that we should not give undue advantage to peers that respond
very quickly, and we should go mostly breadth first, but with a degree of
randomisation.


== Promoting a cold peer to warm

Promoting a cold peer to warm involves establishing a bearer connection. This
is initiated asynchronously and it is either successful or fails after a
timeout.

Once established, we track the connection for the established peer. The
established connection is used later to promote to hot, or to demote back to
cold. It is also used to be notified if the connection fails for any reason.

== Promoting a warm peer to hot

Promoting a warm peer to hot involves sending messages on the established
bearer to switch mode from the network protocol used with warm peers, to the
full set of consensus protocols used for hot peers.

== Demoting a hot peer to warm

If we have more hot peers than our target number (or target range) then we
want to pick one to demote. One policy is to pick randomly. It is likely to be
better to to pick the peer that is in some sense least useful.

One plausible measure of a peer being least useful is based on the following:
for blocks we adopt into our chain, look at which peer(s) received that header
first. A peer that is never first (or very rarely) is one that is likely to be
downstream from us and hence not useful as a choice of upstream peer. A peer
that is normally behind all others, but sometimes (even rarely) is first is
still useful, since it shows it's an upstream connection to some part of the
network where there are active block producers. Consider the case of a relay
in Europe with one connection to Australia: sometimes blocks will be produced
in Australia and so that connection may be first in those cases.

Tracking the necessary information for this policy would require a separate
component that observes the current chain and the peer candidate chains. Using
this information would need access to that shared state. So we should conclude
that the policy should not be pure as it may need access to such changing state.

== Demoting a warm peer to cold


== Forgetting cold peers

We will always forget known peers when the connection is terminated due to
detected adversarial behaviour. The remaining policy decision is which peers
to forget when we have more than our target number of known peers. We will
only select from the known peers that are cold. Warm or hot known peers have
to first be demoted to cold before we consider them to be forgotten.

We want to pick the least useful cold peers to forget. Factors we may wish to
base our decision on include:

 * Number of unsuccessful connection attempts since last successful connection
 * Pseudo-random selection: some degree of randomness can help mitigate
   accidental systematic correlations or some degree of adversarial behaviour.

-}


-- |
--
peerSelectionGovernor :: (MonadAsync m, MonadMask m, MonadTime m, MonadTimer m,
                          Ord peeraddr)
                      => Tracer m (TracePeerSelection peeraddr)
                      -> Tracer m (DebugPeerSelection peeraddr peerconn)
                      -> Tracer m PeerSelectionCounters
                      -> PeerSelectionActions peeraddr peerconn m
                      -> PeerSelectionPolicy  peeraddr m
                      -> m Void
peerSelectionGovernor tracer debugTracer countersTracer actions policy =
    JobPool.withJobPool $ \jobPool ->
      peerSelectionGovernorLoop
        tracer (debugTracer <> contramap transform countersTracer)
        actions policy
        jobPool
        emptyPeerSelectionState
  where
    transform :: Ord peeraddr => DebugPeerSelection peeraddr peerconn -> PeerSelectionCounters
    transform (TraceGovernorState _ _ st) = peerStateToCounters st


-- | Our pattern here is a loop with two sets of guarded actions:
--
-- * Actions guarded on predicates on the current immutable state, e.g.
--   * below known peer targets & below in-progress limit
--
-- * Actions guarded by blocking and waiting for state changes, e.g.
--   * root peer set changed
--   * churn timeout
--   * async action completed
--   * established connection failed
--
-- We check the internal actions first, and otherwise the blocking actions.
-- In each case we trace the action, update the state and execute the
-- action asynchronously.
--
peerSelectionGovernorLoop :: forall m peeraddr peerconn.
                             (MonadAsync m, MonadMask m,
                              MonadTime m, MonadTimer m,
                              Ord peeraddr)
                          => Tracer m (TracePeerSelection peeraddr)
                          -> Tracer m (DebugPeerSelection peeraddr peerconn)
                          -> PeerSelectionActions peeraddr peerconn m
                          -> PeerSelectionPolicy  peeraddr m
                          -> JobPool () m (Completion m peeraddr peerconn)
                          -> PeerSelectionState peeraddr peerconn
                          -> m Void
peerSelectionGovernorLoop tracer debugTracer actions policy jobPool =
    loop
  where
    loop :: PeerSelectionState peeraddr peerconn -> m Void
    loop !st = assertPeerSelectionState st $ do
      blockedAt <- getMonotonicTime
      let knownPeers'       = KnownPeers.setCurrentTime blockedAt (knownPeers st)
          establishedPeers' = EstablishedPeers.setCurrentTime blockedAt (establishedPeers st)
          st'               = st { knownPeers       = knownPeers',
                                   establishedPeers = establishedPeers' }

      timedDecision <- evalGuardedDecisions blockedAt st'

      -- get the current time after the governor returned from the blocking
      -- 'evalGuardedDecisions' call.
      now <- getMonotonicTime
      let Decision { decisionTrace, decisionJobs, decisionState } = timedDecision now
      traceWith tracer decisionTrace
      mapM_ (JobPool.forkJob jobPool) decisionJobs
      loop decisionState

    evalGuardedDecisions :: Time
                         -> PeerSelectionState peeraddr peerconn
                         -> m (TimedDecision m peeraddr peerconn)
    evalGuardedDecisions blockedAt st =
      case guardedDecisions blockedAt st of
        GuardedSkip _ ->
          -- impossible since guardedDecisions always has something to wait for
          error "peerSelectionGovernorLoop: impossible: nothing to do"

        Guarded Nothing decisionAction -> do
          traceWith debugTracer (TraceGovernorState blockedAt Nothing st)
          atomically decisionAction

        Guarded (Just (Min wakeupAt)) decisionAction -> do
          let wakeupIn = diffTime wakeupAt blockedAt
          traceWith debugTracer (TraceGovernorState blockedAt (Just wakeupIn) st)
          wakupTimeout <- newTimeout wakeupIn
          let wakeup    = awaitTimeout wakupTimeout >> pure (wakeupDecision st)
          timedDecision     <- atomically (decisionAction <|> wakeup)
          cancelTimeout wakupTimeout
          return timedDecision

    guardedDecisions :: Time
                     -> PeerSelectionState peeraddr peerconn
                     -> Guarded (STM m) (TimedDecision m peeraddr peerconn)
    guardedDecisions blockedAt st =
      -- All the alternative non-blocking internal decisions.
         RootPeers.belowTarget        actions blockedAt  st
      <> KnownPeers.belowTarget       actions     policy st
      <> KnownPeers.aboveTarget                   policy st
      <> EstablishedPeers.belowTarget actions     policy st
      <> EstablishedPeers.aboveTarget actions     policy st
      <> ActivePeers.belowTarget      actions     policy st
      <> ActivePeers.aboveTarget      actions     policy st

      -- All the alternative potentially-blocking decisions.
      <> Monitor.targetPeers          actions st
      <> Monitor.localRoots           actions st
      <> Monitor.jobs                 jobPool st
      <> Monitor.connections          actions st

      -- There is no rootPeersAboveTarget since the roots target is one sided.

      -- The changedTargets needs to come before the changedLocalRootPeers in
      -- the list of alternates above because our invariant requires that
      -- the number of root nodes be less than our target for known peers,
      -- but at startup our initial targets are 0, so we need to read and set
      -- the targets before we set the root peer set. Otherwise we violate our
      -- invariant (and if we ignored that, we'd try to immediately forget
      -- roots peers because we'd be above target for known peers).


wakeupDecision :: PeerSelectionState peeraddr peerconn
               -> TimedDecision m peeraddr peerconn
wakeupDecision st _now =
  Decision {
    decisionTrace = TraceGovernorWakeup,
    decisionState = st,
    decisionJobs  = []
  }


------------------------
-- Peer churn governor
--

{-
$peer-churn-governor
-}

-- |
--
peerChurnGovernor :: forall m peeraddr.
                     ( MonadSTM m
                     , MonadMonotonicTime m
                     , MonadDelay m
                     )
                  => Tracer m (TracePeerSelection peeraddr)
                  -> StdGen
                  -> STM m FetchMode
                  -> PeerSelectionTargets
                  -> StrictTVar m PeerSelectionTargets
                  -> m Void
peerChurnGovernor tracer inRng getFetchMode base peerSelectionVar = do
  -- Wait a while so that not only the closest peers have had the time
  -- to become warm.
  startTs0 <- getMonotonicTime
  -- TODO: revisit the policy once we have local root peers in the governor.
  -- The intention is to give local root peers give head start and avoid
  -- giving advantage to hostile and quick root peers.
  threadDelay 3
  atomically increaseActivePeers
  endTs0 <- getMonotonicTime
  fuzzyDelay inRng (endTs0 `diffTime` startTs0) >>= go

  where

    -- TODO: #3396 revisit the policy for genesis
    increaseActivePeers :: STM m ()
    increaseActivePeers =  do
        mode <- getFetchMode
        modifyTVar peerSelectionVar (\targets -> targets {
          targetNumberOfActivePeers =
              case mode of
                   FetchModeDeadline ->
                       targetNumberOfActivePeers base
                   FetchModeBulkSync ->
                       min 2 (targetNumberOfActivePeers base)
        })

    decreaseActivePeers :: STM m ()
    decreaseActivePeers =  do
        mode <- getFetchMode
        modifyTVar peerSelectionVar (\targets -> targets {
          targetNumberOfActivePeers =
              case mode of
                   FetchModeDeadline ->
                       decrease $ targetNumberOfActivePeers base
                   FetchModeBulkSync ->
                       min 1 (targetNumberOfActivePeers base - 1)
        })


    go :: StdGen -> m Void
    go !rng = do
      startTs <- getMonotonicTime

      -- Purge the worst active peer(s).
      atomically decreaseActivePeers

      -- Short delay, we may have no active peers right now
      threadDelay 1

      -- Pick new active peer(s) based on the best performing established
      -- peers.
      atomically increaseActivePeers

      -- Give the promotion process time to start
      threadDelay 1

      -- Forget the worst performing non-active peers.
      atomically $ modifyTVar peerSelectionVar (\targets -> targets {
          targetNumberOfRootPeers = decrease (targetNumberOfRootPeers base)
        , targetNumberOfKnownPeers = decrease (targetNumberOfKnownPeers base)
        , targetNumberOfEstablishedPeers =
              decrease (targetNumberOfEstablishedPeers base)
        })

      -- Give the governor time to properly demote them.
      threadDelay $ 1 + closeConnectionTimeout

      -- Pick new non-active peers
      atomically $ modifyTVar peerSelectionVar (\targets -> targets {
          targetNumberOfRootPeers = targetNumberOfRootPeers base
        , targetNumberOfKnownPeers = targetNumberOfKnownPeers base
        , targetNumberOfEstablishedPeers = targetNumberOfEstablishedPeers base
        })
      endTs <- getMonotonicTime

      fuzzyDelay rng (endTs `diffTime` startTs) >>= go

    -- Randomly delay between churnInterval and churnInterval + maxFuzz seconds.
    fuzzyDelay :: StdGen -> DiffTime -> m StdGen
    fuzzyDelay rng execTime = do
      mode <- atomically getFetchMode
      case mode of
           FetchModeDeadline -> longDelay rng execTime
           FetchModeBulkSync -> shortDelay rng execTime

    fuzzyDelay' :: DiffTime -> Double -> StdGen -> DiffTime -> m StdGen
    fuzzyDelay' baseDelay maxFuzz rng execTime = do
      let (fuzz, rng') = randomR (0, maxFuzz) rng
          delay = realToFrac fuzz + baseDelay - execTime
      traceWith tracer $ TraceChurnWait delay
      threadDelay delay
      return rng'


    longDelay :: StdGen -> DiffTime -> m StdGen
    longDelay = fuzzyDelay' churnInterval 600


    shortDelay :: StdGen -> DiffTime -> m StdGen
    shortDelay = fuzzyDelay' churnIntervalBulk 60

    -- The min time between running the churn governor.
    churnInterval :: DiffTime
    churnInterval = 3300

    churnIntervalBulk :: DiffTime
    churnIntervalBulk = 300

    -- Replace 20% or at least on peer every churnInterval.
    decrease :: Int -> Int
    decrease v = v  - max 1 (v `div` 5)


