{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}


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
    TracePeerSelection(..),
    peerSelectionGovernor,

    -- * Peer churn governor
    -- $peer-churn-governor
    peerChurnGovernor,

    sanePeerSelectionTargets, --TODO: perhaps better to move to Types module
    PeerSelectionState(..),
) where

import           Data.Void (Void)
import           Data.Maybe (fromMaybe)
import           Data.Semigroup (Min(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified Data.Set as Set
import           Data.Set (Set)

import           Control.Applicative (Alternative((<|>)))
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Tracer (Tracer(..), traceWith)
import           Control.Exception (Exception(..), assert, SomeException)


import           Ouroboros.Network.PeerSelection.Types
import qualified Ouroboros.Network.PeerSelection.KnownPeers as KnownPeers
import           Ouroboros.Network.PeerSelection.KnownPeers (KnownPeers, KnownPeerInfo(..))
import qualified Ouroboros.Network.PeerSelection.JobPool    as JobPool
import           Ouroboros.Network.PeerSelection.JobPool (JobPool, Job(..))



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


data PeerSelectionPolicy peeraddr m = PeerSelectionPolicy {

       --TODO: where do we decide how many to pick in one go?
       policyPickKnownPeersForGossip :: Map peeraddr KnownPeerInfo -> Int -> STM m (NonEmpty peeraddr),
--     policyPickColdPeersToPromote  :: Map peeraddr KnownPeerInfo -> Int -> STM m (NonEmpty peeraddr),
--     policyPickWarmPeersToPromote  :: Map peeraddr () -> Int -> STM m (NonEmpty peeraddr),
--     policyPickHotPeersToDemote    :: Map peeraddr () -> Int -> STM m (NonEmpty peeraddr),
--     policyPickWarmPeersToDemote   :: Map peeraddr () -> Int -> STM m (NonEmpty peeraddr),
       policyPickColdPeersToForget   :: Map peeraddr KnownPeerInfo -> Int -> STM m (NonEmpty peeraddr),

       policyFindPublicRootTimeout   :: !DiffTime,
       policyMaxInProgressGossipReqs :: !Int,
       policyGossipRetryTime         :: !DiffTime,
       policyGossipBatchWaitTime     :: !DiffTime,
       policyGossipOverallTimeout    :: !DiffTime
     }

-- | Adjustable targets for the peer selection mechanism.
--
-- These are used by the peer selection governor as targets. They are used by
-- the peer churn governor loop as knobs to adjust, to influence the peer
-- selection governor.
--
-- The /known/, /established/ and /active/ peer targets are targets both from
-- below and from above: the governor will attempt to grow or shrink the sets
-- to hit these targets.
--
-- Unlike the other targets, the /root/ peer target is \"one sided\", it is
-- only a target from below. The governor does not try to shrink the root set
-- to hit it, it simply stops looking for more.
--
data PeerSelectionTargets = PeerSelectionTargets {

       targetNumberOfRootPeers        :: !Int,
       targetNumberOfKnownPeers       :: !Int,
       targetNumberOfEstablishedPeers :: !Int,
       targetNumberOfActivePeers      :: !Int

       -- Expressed as intervals rather than frequencies
--     targetChurnIntervalKnownPeers       :: !DiffTime,
--     targetChurnIntervalEstablishedPeers :: !DiffTime,
--     targetChurnIntervalActivePeers      :: !DiffTime
     }
  deriving (Eq, Show)

nullPeerSelectionTargets :: PeerSelectionTargets
nullPeerSelectionTargets =
    PeerSelectionTargets {
       targetNumberOfRootPeers        = 0,
       targetNumberOfKnownPeers       = 0,
       targetNumberOfEstablishedPeers = 0,
       targetNumberOfActivePeers      = 0
--     targetChurnIntervalKnownPeers       = 0,
--     targetChurnIntervalEstablishedPeers = 0,
--     targetChurnIntervalActivePeers      = 0
    }

sanePeerSelectionTargets :: PeerSelectionTargets -> Bool
sanePeerSelectionTargets PeerSelectionTargets{..} =
                                 0 <= targetNumberOfActivePeers
 && targetNumberOfActivePeers      <= targetNumberOfEstablishedPeers
 && targetNumberOfEstablishedPeers <= targetNumberOfKnownPeers
 &&      targetNumberOfRootPeers   <= targetNumberOfKnownPeers
 && 0 <= targetNumberOfRootPeers

 && targetNumberOfActivePeers      <= 100
 && targetNumberOfEstablishedPeers <= 1000
 && targetNumberOfKnownPeers       <= 10000


-- | Actions performed by the peer selection governor.
--
-- These being pluggable allows:
--
-- * choice of known peer root sets
-- * running both in simulation and for real
--
data PeerSelectionActions peeraddr m = PeerSelectionActions {

       readPeerSelectionTargets :: STM m PeerSelectionTargets,

       -- | Read the current set of locally or privately known root peers.
       --
       -- In general this is expected to be updated asynchronously by some
       -- other thread. It is intended to cover the use case of peers from
       -- local configuration. It could be dynamic due to DNS resolution, or
       -- due to dynamic configuration updates.
       --
       readLocalRootPeers :: STM m (Map peeraddr PeerAdvertise),

       -- | Request a sample of public root peers.
       --
       -- It is intended to cover use cases including:
       --
       -- * federated relays from a DNS pool
       -- * stake pool relays published in the blockchain
       -- * a pre-distributed snapshot of stake pool relays from the blockchain
       --
       requestPublicRootPeers :: Int -> m (Set peeraddr, DiffTime),

       -- | The action to contact a known peer and request a sample of its
       -- known peers.
       --
       -- This is synchronous, but it should expect to be interrupted by a
       -- timeout asynchronous exception. Failures are throw as exceptions.
       --
       requestPeerGossip :: peeraddr -> m [peeraddr]

     }


-- | The internal state used by the 'peerSelectionGovernor'.
--
-- The local and public root sets are disjoint, and their union is the
-- overall root set.
--
data PeerSelectionState peeraddr = PeerSelectionState {

       targets              :: !PeerSelectionTargets,

       -- | The current set of local root peers.
       --
       localRootPeers       :: !(Map peeraddr PeerAdvertise),

       publicRootPeers      :: !(Set peeraddr),

       -- |
       --
       knownPeers           :: !(KnownPeers peeraddr),

       -- |
       --
       establishedPeers     :: !(Map peeraddr ()),

       -- |
       --
       activePeers          :: !(Map peeraddr ()),

       -- | A counter to manage the exponential backoff strategy for when to
       -- retry querying for more public root peers. It is negative for retry
       -- counts after failure, and positive for retry counts that are
       -- successful but make no progress.
       --
       publicRootBackoffs   :: !Int,

       -- | The earliest time we would be prepared to request more public root
       -- peers. This is used with the 'publicRootBackoffs' to manage the
       -- exponential backoff.
       --
       publicRootRetryTime  :: !Time,

       inProgressPublicRootsReq :: !Bool,

       inProgressGossipReqs     :: !Int

--     TODO: need something like this to distinguish between lots of bad peers
--     and us getting disconnected from the network locally. We don't want a
--     network disconnect to cause us to flush our full known peer set by
--     considering them all to have bad connectivity.
--     Should also take account of DNS failures for root peer set.
--     lastSucessfulNetworkEvent :: Time
     }
  deriving Show

emptyPeerSelectionState :: PeerSelectionState peeraddr
emptyPeerSelectionState =
    PeerSelectionState {
      targets              = nullPeerSelectionTargets,
      localRootPeers       = Map.empty,
      publicRootPeers      = Set.empty,
      knownPeers           = KnownPeers.empty,
      establishedPeers     = Map.empty,
      activePeers          = Map.empty,
      publicRootBackoffs   = 0,
      publicRootRetryTime  = Time 0,
      inProgressPublicRootsReq = False,
      inProgressGossipReqs = 0
    }

invariantPeerSelectionState :: Ord peeraddr
                            => PeerSelectionState peeraddr -> Bool
invariantPeerSelectionState PeerSelectionState{..} =
    KnownPeers.invariant knownPeers

    -- The activePeers is a subset of the establishedPeers
    -- which is a subset of the known peers
 && Map.isSubmapOfBy (\_ _ -> True) activePeers establishedPeers
 && Map.isSubmapOfBy (\_ _ -> True) establishedPeers (KnownPeers.toMap knownPeers)

    -- The localRootPeers and publicRootPeers must not overlap.
 && Set.null (Set.intersection
                (Map.keysSet localRootPeers)
                publicRootPeers)

    -- The localRootPeers are a subset of the knownPeers,
    -- and with correct source and other info in the knownPeers.
 && Map.isSubmapOfBy (\rootPeerAdvertise
                       KnownPeerInfo {knownPeerAdvertise, knownPeerSource} ->
                           knownPeerSource == PeerSourceLocalRoot
                        && knownPeerAdvertise == rootPeerAdvertise)
                     localRootPeers
                     (KnownPeers.toMap knownPeers)

    -- The publicRootPeers are a subset of the knownPeers,
    -- and with correct source info in the knownPeers.
 && Map.isSubmapOfBy (\_ KnownPeerInfo {knownPeerSource} ->
                         knownPeerSource == PeerSourcePublicRoot)
                     (Map.fromSet (const ()) publicRootPeers)
                     (KnownPeers.toMap knownPeers)

    -- We don't want to pick local root peers to forget, so it had better be
    -- the case that there's fewer of them than our target number.
 && Map.size localRootPeers <= targetNumberOfKnownPeers targets

    -- No constraint for publicRootBackoffs, publicRootRetryTime
    -- or inProgressPublicRootsReq

 && inProgressGossipReqs >= 0


-- |
--
peerSelectionGovernor :: (MonadAsync m, MonadMask m, MonadTime m, MonadTimer m,
                          Alternative (STM m), Ord peeraddr)
                      => Tracer m (TracePeerSelection peeraddr)
                      -> PeerSelectionActions peeraddr m
                      -> PeerSelectionPolicy  peeraddr m
                      -> m Void
peerSelectionGovernor tracer actions policy =
    JobPool.withJobPool $ \jobPool ->
      peerSelectionGovernorLoop
        tracer actions policy
        jobPool
        emptyPeerSelectionState


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
peerSelectionGovernorLoop :: forall m peeraddr.
                             (MonadAsync m, MonadMask m,
                              MonadTime m, MonadTimer m,
                              Alternative (STM m), Ord peeraddr)
                          => Tracer m (TracePeerSelection peeraddr)
                          -> PeerSelectionActions peeraddr m
                          -> PeerSelectionPolicy  peeraddr m
                          -> JobPool m (Completion m peeraddr)
                          -> PeerSelectionState peeraddr
                          -> m Void
peerSelectionGovernorLoop tracer
                          actions@PeerSelectionActions{..}
                          policy@PeerSelectionPolicy{..}
                          jobPool =
    loop
  where
    loop :: PeerSelectionState peeraddr -> m Void
    loop !st = assert (invariantPeerSelectionState st) $ do
      now <- getMonotonicTime
      let knownPeers' = KnownPeers.setCurrentTime now (knownPeers st)
          st'         = st { knownPeers = knownPeers' }

      decision <- evalGuardedDecisions now st'

      let Decision { decisionTrace, decisionEnact, decisionState } = decision
      traceWith tracer decisionTrace
      case decisionEnact of
        Nothing  -> return ()
        Just job -> JobPool.forkJob jobPool job
      loop decisionState

    evalGuardedDecisions :: Time
                         -> PeerSelectionState peeraddr
                         -> m (Decision m peeraddr)
    evalGuardedDecisions now st =
      case guardedDecisions now st of
        GuardedSkip _ ->
          -- impossible since guardedDecisions always has something to wait for
          fail "peerSelectionGovernorLoop: impossible: nothing to do"

        Guarded Nothing decisionAction -> do
          traceWith tracer (TraceGovernorLoopDebug st Nothing)
          atomically decisionAction

        Guarded (Just (Min wakeupAt)) decisionAction -> do
          let wakeupIn = diffTime wakeupAt now
          traceWith tracer (TraceGovernorLoopDebug st (Just wakeupIn))
          wakupTimeout <- newTimeout wakeupIn
          let wakeup    = awaitTimeout wakupTimeout >> pure (wakeupDecision st)
          decision     <- atomically (decisionAction <|> wakeup)
          cancelTimeout wakupTimeout
          return decision

    guardedDecisions :: Time
                     -> PeerSelectionState peeraddr
                     -> Guarded (STM m) (Decision m peeraddr)
    guardedDecisions now st =
      -- All the alternative non-blocking internal decisions.
         rootPeersBelowTarget  actions        st now
      <> knownPeersBelowTarget actions policy st now
      <> knownPeersAboveTarget         policy st
      <> establishedPeersBelowTarget   policy st
      <> establishedPeersAboveTarget   policy st
      <> activePeersBelowTarget        policy st
      <> activePeersAboveTarget        policy st

      -- All the alternative potentially-blocking decisions.
      <> changedTargets                actions st
      <> changedLocalRootPeers         actions st
      <> jobCompleted                  jobPool st now
      <> establishedConnectionFailed           st

      -- There is no rootPeersAboveTarget since the roots target is one sided.

      -- The changedTargets needs to come before the changedLocalRootPeers in
      -- the list of alternates above because our invariant requires that
      -- the number of root nodes be less than our target for known peers,
      -- but at startup our initial targets are 0, so we need to read and set
      -- the targets before we set the root peer set. Otherwise we violate our
      -- invariant (and if we ignored that, we'd try to immediately forget
      -- roots peers because we'd be above target for known peers).


data Guarded m a = GuardedSkip !(Maybe (Min Time))
                 | Guarded     !(Maybe (Min Time)) (m a)

instance Alternative m => Semigroup (Guarded m a) where
  Guarded     ta a <> Guarded     tb b = Guarded     (ta <> tb) (a <|> b)
  Guarded     ta a <> GuardedSkip tb   = Guarded     (ta <> tb)  a
  GuardedSkip ta   <> Guarded     tb b = Guarded     (ta <> tb)  b
  GuardedSkip ta   <> GuardedSkip tb   = GuardedSkip (ta <> tb)


data Decision m peeraddr = Decision {
         -- | A trace event to classify the decision and action
       decisionTrace :: TracePeerSelection peeraddr,

         -- | An updated state to use immediately
       decisionState :: PeerSelectionState peeraddr,

       -- | An optional 'Job' to execute asynchronously. This job leads to
       -- a further 'Decision'. This gives a state update to apply upon
       -- completion, but also allows chaining further job actions.
       --
       decisionEnact :: Maybe (Job m (Completion m peeraddr))
     }

wakeupDecision :: PeerSelectionState peeraddr -> Decision m peeraddr
wakeupDecision st =
  Decision {
    decisionTrace = TraceGovernorWakeup,
    decisionState = st,
    decisionEnact = Nothing
  }

newtype Completion m peeraddr =
        Completion (PeerSelectionState peeraddr -> Time -> Decision m peeraddr)

data TracePeerSelection peeraddr =
       TraceLocalRootPeersChanged (Map peeraddr PeerAdvertise)
                                  (Map peeraddr PeerAdvertise)
     | TraceTargetsChanged     PeerSelectionTargets PeerSelectionTargets
     | TracePublicRootsRequest Int Int
     | TracePublicRootsResults (Set peeraddr) Int DiffTime
     | TracePublicRootsFailure SomeException Int DiffTime
     | TraceGossipRequests     Int Int (Map peeraddr KnownPeerInfo) [peeraddr] -- target, actual, selected
     | TraceGossipResults      [(peeraddr, Either SomeException [peeraddr])] --TODO: classify failures
     | TraceForgetKnownPeers   Int Int [peeraddr] -- target, actual, selected
     | TraceGovernorLoopDebug  (PeerSelectionState peeraddr) (Maybe DiffTime)
     | TraceGovernorWakeup
  deriving Show


rootPeersBelowTarget :: (MonadSTM m, Ord peeraddr)
                     => PeerSelectionActions peeraddr m
                     -> PeerSelectionState peeraddr
                     -> Time
                     -> Guarded (STM m) (Decision m peeraddr)
rootPeersBelowTarget actions
                     st@PeerSelectionState {
                       localRootPeers,
                       publicRootPeers,
                       publicRootRetryTime,
                       inProgressPublicRootsReq,
                       targets = PeerSelectionTargets {
                                   targetNumberOfRootPeers
                                 }
                     }
                     now
    -- Are we under target for number of root peers?
  | maxExtraRootPeers > 0

    -- Are we already requesting more root peers?
  , not inProgressPublicRootsReq

    -- We limit how frequently we make requests, are we allowed to do it yet?
  , now >= publicRootRetryTime
  = Guarded Nothing $
      return Decision {
        decisionTrace = TracePublicRootsRequest
                          targetNumberOfRootPeers
                          numRootPeers,
        decisionState = st { inProgressPublicRootsReq = True },
        decisionEnact = Just (publicRootPeersJob actions maxExtraRootPeers)
      }

    -- If we would be able to do the request except for the time, return the
    -- next retry time.
  | maxExtraRootPeers > 0
  , not inProgressPublicRootsReq
  = GuardedSkip (Just (Min publicRootRetryTime))

  | otherwise
  = GuardedSkip Nothing
  where
    numRootPeers      = Map.size localRootPeers + Set.size publicRootPeers
    maxExtraRootPeers = targetNumberOfRootPeers - numRootPeers

publicRootPeersJob :: forall m peeraddr.
                      (Monad m, Ord peeraddr)
                   => PeerSelectionActions peeraddr m
                   -> Int
                   -> Job m (Completion m peeraddr)
publicRootPeersJob PeerSelectionActions{requestPublicRootPeers}
                   numExtraAllowed =
    Job job handler
  where
    handler :: SomeException -> Completion m peeraddr
    handler e =
      Completion $ \st now ->
      -- This is a failure, so move the backoff counter one in the failure
      -- direction (negative) and schedule the next retry time accordingly.
      -- We use an exponential backoff strategy. The max retry time of 2^12
      -- seconds is just over an hour.
      let publicRootBackoffs'      :: Int
          publicRootBackoffs'      = (publicRootBackoffs st `min` 0) - 1

          publicRootRetryDiffTime' :: DiffTime
          publicRootRetryDiffTime' = 2 ^ (abs publicRootBackoffs' `min` 12)

          publicRootRetryTime'     :: Time
          publicRootRetryTime'     = addTime publicRootRetryDiffTime' now
       in Decision {
            decisionTrace = TracePublicRootsFailure
                              e
                              publicRootBackoffs'
                              publicRootRetryDiffTime',
            decisionState = st {
                              inProgressPublicRootsReq = False,
                              publicRootBackoffs  = publicRootBackoffs',
                              publicRootRetryTime = publicRootRetryTime'
                            },
            decisionEnact = Nothing
          }

    job :: m (Completion m peeraddr)
    job = do
      (results, ttl) <- requestPublicRootPeers numExtraAllowed
      return $ Completion $ \st now ->
        let newPeers         = results Set.\\ Map.keysSet (localRootPeers st)
                                       Set.\\ publicRootPeers st
            publicRootPeers' = publicRootPeers st <> newPeers
            knownPeers'      = KnownPeers.insert
                                 PeerSourcePublicRoot
                                 DoAdvertisePeer
                                 (Set.toList newPeers) --TODO: perhaps as Set
                                 (knownPeers st)

            -- We got a successful response to our request, but if we're still
            -- below target we're going to want to try again at some point.
            -- If we made progress towards our target then we will retry at the
            -- suggested ttl. But if we did not make progress then we want to
            -- follow an exponential backoff strategy. The max retry time of 2^12
            -- seconds is just over an hour.
            publicRootBackoffs' :: Int
            publicRootBackoffs'
              | Set.null newPeers = (publicRootBackoffs st `max` 0) + 1
              | otherwise         = 0

            publicRootRetryDiffTime :: DiffTime
            publicRootRetryDiffTime
              | publicRootBackoffs' == 0
                          = ttl
              | otherwise = 2^(publicRootBackoffs' `min` 12)

            publicRootRetryTime :: Time
            publicRootRetryTime = addTime publicRootRetryDiffTime now
         in Decision {
              decisionTrace = TracePublicRootsResults
                                newPeers
                                publicRootBackoffs'
                                publicRootRetryDiffTime,
              decisionState = st {
                                publicRootPeers     = publicRootPeers',
                                knownPeers          = knownPeers',
                                publicRootBackoffs  = publicRootBackoffs',
                                publicRootRetryTime = publicRootRetryTime,
                                inProgressPublicRootsReq = False
                              },
              decisionEnact = Nothing
            }


knownPeersBelowTarget :: (MonadAsync m, MonadTimer m, Ord peeraddr)
                      => PeerSelectionActions peeraddr m
                      -> PeerSelectionPolicy peeraddr m
                      -> PeerSelectionState peeraddr
                      -> Time
                      -> Guarded (STM m) (Decision m peeraddr)
knownPeersBelowTarget actions
                      policy@PeerSelectionPolicy {
                        policyMaxInProgressGossipReqs,
                        policyPickKnownPeersForGossip,
                        policyGossipRetryTime
                      }
                      st@PeerSelectionState {
                        knownPeers,
                        inProgressGossipReqs,
                        targets = PeerSelectionTargets {
                                    targetNumberOfKnownPeers
                                  }
                      }
                      now
    -- Are we under target for number of known peers?
  | numKnownPeers < targetNumberOfKnownPeers

    -- Are we at our limit for number of gossip requests?
  , numGossipReqsPossible > 0

    -- Are there any known peers that we can send a gossip request to?
    -- We can only ask ones where we have not asked them within a certain time.
  , not (Map.null availableForGossip)
  = Guarded Nothing $ do
      selectedForGossip <- NonEmpty.toList
                       <$> policyPickKnownPeersForGossip
                             availableForGossip
                             numGossipReqsPossible
      let numGossipReqs = length selectedForGossip
      return Decision {
        decisionTrace = TraceGossipRequests
                          targetNumberOfKnownPeers
                          numKnownPeers
                          availableForGossip
                          selectedForGossip,
        decisionState = st {
                          inProgressGossipReqs = inProgressGossipReqs
                                               + numGossipReqs,
                          knownPeers = KnownPeers.setGossipTime
                                         selectedForGossip
                                         (addTime policyGossipRetryTime now)
                                         knownPeers
                        },
        decisionEnact = Just (gossipsJob actions policy selectedForGossip)
      }

    -- If we could gossip except that there are none currently available
    -- then we return the next wakeup time (if any)
  | numKnownPeers < targetNumberOfKnownPeers
  , numGossipReqsPossible > 0
  , Map.null availableForGossip
  = GuardedSkip (Min <$> KnownPeers.minGossipTime knownPeers)

  | otherwise
  = GuardedSkip Nothing
  where
    numKnownPeers         = KnownPeers.size knownPeers
    numGossipReqsPossible = policyMaxInProgressGossipReqs
                          - inProgressGossipReqs
    availableForGossip    = KnownPeers.availableForGossip knownPeers


gossipsJob :: forall m peeraddr.
              (MonadAsync m, MonadTimer m, Ord peeraddr)
           => PeerSelectionActions peeraddr m
           -> PeerSelectionPolicy peeraddr m
           -> [peeraddr]
           -> Job m (Completion m peeraddr)
gossipsJob PeerSelectionActions{requestPeerGossip}
           PeerSelectionPolicy{..} =
    \peers -> Job (jobPhase1 peers) (handler peers)
  where
    handler :: [peeraddr] -> SomeException -> Completion m peeraddr
    handler peers e =
      Completion $ \st _ ->
      Decision {
        decisionTrace = TraceGossipResults [ (p, Left e) | p <- peers ],
        decisionState = st {
                          inProgressGossipReqs = inProgressGossipReqs st
                                               - length peers
                        },
        decisionEnact = Nothing
      }

    jobPhase1 :: [peeraddr] -> m (Completion m peeraddr)
    jobPhase1 peers = do
      -- In the typical case, where most requests return within a short
      -- timeout we want to collect all the responses into a batch and
      -- add them to the known peers set in one go.
      --
      -- So fire them all off in one go:
      gossips <- sequence [ async (requestPeerGossip peer) | peer <- peers ]

      -- First to finish synchronisation between /all/ the gossips completing
      -- or the timeout (with whatever partial results we have at the time)
      results <- waitAllCatchOrTimeout gossips policyGossipBatchWaitTime
      case results of
        Right totalResults -> do
          let peerResults = zip peers totalResults
              newPeers    = [ p | Right ps <- totalResults, p <- ps ]
          return $ Completion $ \st _ -> Decision {
            decisionTrace = TraceGossipResults peerResults,
            decisionState = st {
                              --TODO: also update with the failures
                              knownPeers = KnownPeers.insert
                                             PeerSourceGossip
                                             DoAdvertisePeer
                                             newPeers (knownPeers st),
                              inProgressGossipReqs = inProgressGossipReqs st
                                                   - length peers
                            },
            decisionEnact = Nothing
          }

        -- But if any don't make the first timeout then they'll be added later
        -- when they do reply or never if we hit the hard timeout.
        Left partialResults -> do

          -- We have to keep track of the relationship between the peer
          -- addresses and the gossip requests, completed and still in progress:
          let peerResults      = [ (p, r)
                                 | (p, Just r)  <- zip peers   partialResults ]
              newPeers         = [  p
                                 | Just (Right ps) <-          partialResults
                                 ,  p <- ps ]
              peersRemaining   = [  p
                                 | (p, Nothing) <- zip peers   partialResults ]
              gossipsRemaining = [  a
                                 | (a, Nothing) <- zip gossips partialResults ]

          return $ Completion $ \st _ -> Decision {
            decisionTrace = TraceGossipResults peerResults,
            decisionState = st {
                              --TODO: also update with the failures
                              knownPeers = KnownPeers.insert
                                             PeerSourceGossip
                                             DoAdvertisePeer
                                             newPeers (knownPeers st),
                              inProgressGossipReqs = inProgressGossipReqs st
                                                   - length peerResults
                            },
            decisionEnact = Just $ Job (jobPhase2 peersRemaining gossipsRemaining)
                                       (handler peersRemaining)
          }

    jobPhase2 :: [peeraddr] -> [Async m [peeraddr]] -> m (Completion m peeraddr)
    jobPhase2 peers gossips = do

      -- Wait again, for all remaining to finish or a timeout.
      results <- waitAllCatchOrTimeout
                      gossips
                      (policyGossipOverallTimeout
                       - policyGossipBatchWaitTime)
      let peerResults =
            case results of
              Right totalResults  -> zip peers totalResults
              Left partialResults -> [ (p, fromMaybe err r)
                                     | (p, r) <- zip peers partialResults ]
                where err = Left (toException AsyncCancelled)

          newPeers =
            case results of
              Right totalResults  -> [ p | Right ps <- totalResults,  p <- ps ]
              Left partialResults -> [ p | Just (Right ps) <- partialResults,  p <- ps ]

          gossipsIncomplete =
            case results of
              Right _totalResults -> []
              Left partialResults ->
                [ a | (a, Nothing) <- zip gossips partialResults ]

      mapM_ cancel gossipsIncomplete

      return $ Completion $ \st _ -> Decision {
        decisionTrace = TraceGossipResults peerResults,
        decisionState = st {
                          --TODO: also update with the failures
                          knownPeers = KnownPeers.insert
                                         PeerSourceGossip
                                         DoAdvertisePeer
                                         newPeers (knownPeers st),
                          inProgressGossipReqs = inProgressGossipReqs st
                                               - length peers
                        },
        decisionEnact = Nothing
      }


knownPeersAboveTarget :: (MonadSTM m, Ord peeraddr)
                      => PeerSelectionPolicy peeraddr m
                      -> PeerSelectionState peeraddr
                      -> Guarded (STM m) (Decision m peeraddr)
knownPeersAboveTarget PeerSelectionPolicy {
                        policyPickColdPeersToForget
                      }
                      st@PeerSelectionState {
                        localRootPeers,
                        publicRootPeers,
                        knownPeers,
                        targets = PeerSelectionTargets {
                                    targetNumberOfKnownPeers,
                                    targetNumberOfRootPeers
                                  }
                      }
    -- Are we above the target for number of known peers?
  | let numKnownPeers, numPeersToForget :: Int
        numKnownPeers    = KnownPeers.size knownPeers
        numPeersToForget = numKnownPeers - targetNumberOfKnownPeers
  , numPeersToForget > 0
  = Guarded Nothing $ do
      -- We must never pick local root peers to forget as this would violate
      -- our invariant that the localRootPeers is a subset of the knownPeers.
      --
      -- We also need to avoid picking public root peers if that would put us
      -- below the target for root peers.
      --
      -- Since the targetNumberOfKnownPeers is at least the size of the root
      -- peers set (another invariant) then we know that if numPeersToForget > 0
      -- then availableToForget here is non-empty.
      --
      let numRootPeersCanForget = Map.size localRootPeers
                                + Set.size publicRootPeers
                                - targetNumberOfRootPeers
          protectedRootPeers    = Map.keysSet localRootPeers
                               <> Set.drop numRootPeersCanForget publicRootPeers
          availableToForget     = KnownPeers.toMap knownPeers
                                    `Map.withoutKeys` protectedRootPeers

      assert (not (Map.null availableToForget)) $ return ()

      selectedToForget <- NonEmpty.toList
                      <$> policyPickColdPeersToForget
                            availableToForget
                            numPeersToForget
      return Decision {
        decisionTrace = TraceForgetKnownPeers
                          targetNumberOfKnownPeers
                          numKnownPeers
                          selectedToForget,
        decisionState = st {
                          knownPeers      = KnownPeers.delete
                                              selectedToForget
                                              knownPeers,
                          publicRootPeers = publicRootPeers
                                              Set.\\ Set.fromList selectedToForget
                        },
        decisionEnact = Nothing
      }

  | otherwise
  = GuardedSkip Nothing


establishedPeersBelowTarget :: PeerSelectionPolicy peeraddr m
                            -> PeerSelectionState peeraddr
                            -> Guarded (STM m) (Decision m peeraddr)
establishedPeersBelowTarget _ _ = GuardedSkip Nothing


establishedPeersAboveTarget :: PeerSelectionPolicy peeraddr m
                            -> PeerSelectionState peeraddr
                            -> Guarded (STM m) (Decision m peeraddr)
establishedPeersAboveTarget _ _ = GuardedSkip Nothing


activePeersBelowTarget :: PeerSelectionPolicy peeraddr m
                       -> PeerSelectionState peeraddr
                       -> Guarded (STM m) (Decision m peeraddr)
activePeersBelowTarget _ _ = GuardedSkip Nothing


activePeersAboveTarget :: PeerSelectionPolicy peeraddr m
                      -> PeerSelectionState peeraddr
                       -> Guarded (STM m) (Decision m peeraddr)
activePeersAboveTarget _ _ = GuardedSkip Nothing



changedLocalRootPeers :: (MonadSTM m, Ord peeraddr)
                      => PeerSelectionActions peeraddr m
                      -> PeerSelectionState peeraddr
                      -> Guarded (STM m) (Decision m peeraddr)
changedLocalRootPeers PeerSelectionActions{readLocalRootPeers}
                      st@PeerSelectionState{
                        localRootPeers,
                        publicRootPeers,
                        knownPeers,
                        targets = PeerSelectionTargets{targetNumberOfKnownPeers}
                      } =
    Guarded Nothing $ do
      -- We have to enforce the invariant that the number of root peers is
      -- not more than the target number of known peers. It's unlikely in
      -- practice so it's ok to resolve it arbitrarily using Map.take.
      localRootPeers' <- Map.take targetNumberOfKnownPeers <$> readLocalRootPeers
      check (localRootPeers' /= localRootPeers)

      let (knownPeers', _removed) =
            KnownPeers.adjustRootSet localRootPeers localRootPeers' knownPeers
          -- Have to adjust the publicRootPeers to maintain the invariant that
          -- the local and public sets are non-overlapping.
          publicRootPeers' = publicRootPeers Set.\\ Map.keysSet localRootPeers'
      --TODO: when we have established/active peers and they're
      -- removed then we should disconnect from them.
      return Decision {
        decisionTrace = TraceLocalRootPeersChanged localRootPeers localRootPeers',
        decisionState = st {
                          localRootPeers  = localRootPeers',
                          publicRootPeers = publicRootPeers',
                          knownPeers      = knownPeers'
                        },
        decisionEnact = Nothing
      }


changedTargets :: MonadSTM m
               => PeerSelectionActions peeraddr m
               -> PeerSelectionState peeraddr
               -> Guarded (STM m) (Decision m peeraddr)
changedTargets PeerSelectionActions{readPeerSelectionTargets}
               st@PeerSelectionState{
                 localRootPeers,
                 targets = targets@PeerSelectionTargets{targetNumberOfKnownPeers}
               } =
    Guarded Nothing $ do
      targets' <- readPeerSelectionTargets
      check (targets' /= targets)

      -- We have to enforce the invariant that the number of root peers is
      -- not more than the target number of known peers. It's unlikely in
      -- practice so it's ok to resolve it arbitrarily using Map.take.
      let localRootPeers' = Map.take targetNumberOfKnownPeers localRootPeers

      return Decision {
        decisionTrace = TraceTargetsChanged targets targets',
        decisionEnact = Nothing,
        decisionState = assert (sanePeerSelectionTargets targets')
                        st {
                          targets        = targets',
                          localRootPeers = localRootPeers'
                        }
      }


jobCompleted :: MonadSTM m
             => JobPool m (Completion m peeraddr)
             -> PeerSelectionState peeraddr
             -> Time
             -> Guarded (STM m) (Decision m peeraddr)
jobCompleted jobPool st now =
    -- This case is simple because the job pool returns a 'Completion' which is
    -- just a function from the current state to a new 'Decision'.
    Guarded Nothing $ do
      Completion completion <- JobPool.collect jobPool
      return $! completion st now


establishedConnectionFailed :: MonadSTM m
                            => PeerSelectionState peeraddr
                            -> Guarded (STM m) (Decision m peeraddr)
establishedConnectionFailed _ = GuardedSkip Nothing


------------------------
-- Peer churn governor
--

{-
$peer-churn-governor
-}

-- |
--
peerChurnGovernor :: MonadSTM m
                  => PeerSelectionTargets
                  -> m () --Void
peerChurnGovernor _ =
    return ()


-------------------------------
-- Utils
--

-- | Perform a first-to-finish synchronisation between:
--
-- * /all/ the async actions completing; or
-- * the timeout with whatever partial results we have at the time
--
-- The result list is the same length and order as the asyncs, so the results
-- can be paired up.
--
waitAllCatchOrTimeout :: (MonadAsync m, MonadTimer m)
                      => [Async m a]
                      -> DiffTime
                      -> m (Either [Maybe (Either SomeException a)]
                                   [Either SomeException a])
waitAllCatchOrTimeout as t = do
    timeout <- newTimeout t
    results <- atomically $
                         (Right <$> mapM waitCatchSTM as)
                `orElse` (Left  <$> (awaitTimeout timeout >> mapM pollSTM as))
    case results of
      Right{} -> cancelTimeout timeout
      _       -> return ()
    return results
