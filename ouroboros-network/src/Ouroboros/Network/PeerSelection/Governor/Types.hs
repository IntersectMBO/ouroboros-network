{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}

module Ouroboros.Network.PeerSelection.Governor.Types
  ( -- * P2P governor policies
    PeerSelectionPolicy (..)
  , PeerSelectionTargets (..)
  , nullPeerSelectionTargets
  , sanePeerSelectionTargets
  , PickPolicy
  , pickPeers
    -- * P2P governor low level API
    -- These records are needed to run the peer selection.
  , PeerStateActions (..)
  , PeerSelectionActions (..)
  , ChurnMode (..)
    -- * P2P governor internals
  , PeerSelectionState (..)
  , emptyPeerSelectionState
  , assertPeerSelectionState
  , establishedPeersStatus
  , PublicPeerSelectionState (..)
  , emptyPublicPeerSelectionState
  , toPublicState
  , Guarded (GuardedSkip, Guarded)
  , Decision (..)
  , TimedDecision
  , MkGuardedDecision
  , Completion (..)
  , PeerSelectionCounters (..)
  , peerStateToCounters
    -- * Peer Sharing Auxiliary data type
  , PeerSharingResult (..)
    -- * Traces
  , TracePeerSelection (..)
  , DebugPeerSelection (..)
  ) where

import           Data.Cache (Cache (..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid.Synchronisation (FirstToFinish (..))
import           Data.Semigroup (Min (..))
import           Data.Set (Set)
import qualified Data.Set as Set

import           Control.Applicative (Alternative)
import           Control.Concurrent.JobPool (Job)
import           Control.Exception (SomeException, assert)
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTime.SI
import           System.Random (StdGen)

import           Ouroboros.Network.ExitPolicy
import           Ouroboros.Network.PeerSelection.LedgerPeers (IsBigLedgerPeer,
                     IsLedgerPeer)
import           Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise)
import           Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing)
import           Ouroboros.Network.PeerSelection.State.EstablishedPeers
                     (EstablishedPeers)
import qualified Ouroboros.Network.PeerSelection.State.EstablishedPeers as EstablishedPeers
import           Ouroboros.Network.PeerSelection.State.KnownPeers (KnownPeers)
import qualified Ouroboros.Network.PeerSelection.State.KnownPeers as KnownPeers
import           Ouroboros.Network.PeerSelection.State.LocalRootPeers
                     (HotValency, LocalRootPeers, WarmValency)
import qualified Ouroboros.Network.PeerSelection.State.LocalRootPeers as LocalRootPeers
import           Ouroboros.Network.PeerSelection.Types (PeerSource (..),
                     PeerStatus (PeerHot, PeerWarm))
import           Ouroboros.Network.Protocol.PeerSharing.Type (PeerSharingAmount,
                     PeerSharingResult (..))


-- | A peer pick policy is an action that picks a subset of elements from a
-- map of peers.
--
-- The pre-condition is that the map of available choices will be non-empty,
-- and the requested number to pick will be strictly positive.
--
-- The post-condition is that the picked set is non-empty but must not be
-- bigger than the requested number.
--
-- Peer selection API is using `STM m` monad, internally it is using `m`.
--
type PickPolicy peeraddr m =
         -- Extra peer attributes available to use in the picking policy.
         -- As more attributes are needed, extend this with more such functions.
         (peeraddr -> PeerSource) -- Where the peer is known from
      -> (peeraddr -> Int)        -- Connection failure count
      -> (peeraddr -> Bool)       -- Found to be tepid flag
      -> Set peeraddr             -- The set to pick from
      -> Int                      -- Max number to choose, fewer is ok.
      -> m (Set peeraddr)         -- The set picked.


data PeerSelectionPolicy peeraddr m = PeerSelectionPolicy {

       policyPickKnownPeersForPeerShare :: PickPolicy peeraddr (STM m),
       policyPickColdPeersToPromote     :: PickPolicy peeraddr (STM m),
       policyPickWarmPeersToPromote     :: PickPolicy peeraddr (STM m),
       policyPickHotPeersToDemote       :: PickPolicy peeraddr (STM m),
       policyPickWarmPeersToDemote      :: PickPolicy peeraddr (STM m),
       policyPickColdPeersToForget      :: PickPolicy peeraddr (STM m),

       policyFindPublicRootTimeout      :: !DiffTime,
       policyMaxInProgressPeerShareReqs :: !Int,
       -- ^ Maximum number of peer sharing requests that can be in progress
       policyPeerShareRetryTime         :: !DiffTime,
       -- ^ Amount of time a node has to wait before issuing a new peer sharing
       -- request
       policyPeerShareBatchWaitTime     :: !DiffTime,
       -- ^ Amount of time a batch of peer sharing requests is allowed to take
       policyPeerShareOverallTimeout    :: !DiffTime,
       -- ^ Amount of time the overall batches of peer sharing requests are
       -- allowed to take
       policyPeerShareActivationDelay    :: !DiffTime,
       -- ^ Delay until we consider a peer suitable for peer sharing.

       -- | Reconnection delay, passed from `ExitPolicy`.
       --
       policyErrorDelay                 :: !DiffTime
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
-- There is also an implicit target that enough local root peers are selected
-- as active. This comes from the configuration for local roots, and is not an
-- independently adjustable target.
--
data PeerSelectionTargets = PeerSelectionTargets {

       targetNumberOfRootPeers                 :: !Int,

       -- | The target number of all known peers.  This includes ledger,
       -- big ledger peers.
       targetNumberOfKnownPeers                :: !Int,
       -- | The target number of established peers (does not include big ledger
       -- peers).
       --
       -- The target includes root peers, local root peers, ledger peers and big
       -- ledger peers.
       --
       targetNumberOfEstablishedPeers          :: !Int,
       -- | The target number of active peers (does not include big ledger
       -- peers).
       --
       -- The
       targetNumberOfActivePeers               :: !Int,

       -- | Target number of known big ledger peers.
       --
       -- This target is independent of `targetNumberOfKnownPeers`.  The total
       -- number of known peers will be sum of the two targets.
       --
       targetNumberOfKnownBigLedgerPeers       :: !Int,
       -- | Target number of established big ledger peers.
       --
       -- This target is independent of `targetNumberOfEstablishedPeers`.  The
       -- total number of established peers will be sum of the two targets and
       -- local root peers.
       --
       targetNumberOfEstablishedBigLedgerPeers :: !Int,
       -- | Target number of active big ledger peers.
       --
       -- This target is independent of `targetNumberOfActivePeers`.  The total
       -- number of active peers will be sum of the two targets and active local
       -- root peers.
       --
       targetNumberOfActiveBigLedgerPeers      :: !Int

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
       targetNumberOfActivePeers      = 0,
       targetNumberOfKnownBigLedgerPeers       = 0,
       targetNumberOfEstablishedBigLedgerPeers = 0,
       targetNumberOfActiveBigLedgerPeers      = 0
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
 &&                              0 <= targetNumberOfRootPeers

 &&                                       0 <= targetNumberOfActiveBigLedgerPeers
 && targetNumberOfActiveBigLedgerPeers      <= targetNumberOfEstablishedBigLedgerPeers
 && targetNumberOfEstablishedBigLedgerPeers <= targetNumberOfKnownBigLedgerPeers

 && targetNumberOfActivePeers      <= 100
 && targetNumberOfEstablishedPeers <= 1000
 && targetNumberOfKnownPeers       <= 10000

 && targetNumberOfActiveBigLedgerPeers      <= 100
 && targetNumberOfEstablishedBigLedgerPeers <= 1000
 && targetNumberOfKnownBigLedgerPeers       <= 10000


-- | Actions performed by the peer selection governor.
--
-- These being pluggable allows:
--
-- * choice of known peer root sets
-- * running both in simulation and for real
--
data PeerSelectionActions peeraddr peerconn m = PeerSelectionActions {

       readPeerSelectionTargets :: STM m PeerSelectionTargets,

       -- | Read the current set of locally or privately known root peers.
       --
       -- In general this is expected to be updated asynchronously by some
       -- other thread. It is intended to cover the use case of peers from
       -- local configuration. It could be dynamic due to DNS resolution, or
       -- due to dynamic configuration updates.
       --
       -- It is structured as a collection of (non-overlapping) groups of peers
       -- where we are supposed to select n from each group.
       --
       readLocalRootPeers       :: STM m [( HotValency
                                          , WarmValency
                                          , Map peeraddr PeerAdvertise)],

       readNewInboundConnection :: STM m (peeraddr, PeerSharing),

       -- | Read the current Peer Sharing willingness value
       --
       -- This value comes from the Node's configuration file.
       --
       peerSharing :: PeerSharing,

       -- | Get the remote's side PeerSharing value from 'peerconn'
       --
       -- 'peerconn' ideally comes from a call to 'establishPeerConnection'.
       -- This will establish a connection and perform handshake. The returned
       -- 'peerconn' has all the versionData negotiated in the handshake,
       -- including the remote peer's 'PeerSharing' willingness information.
       --
       peerConnToPeerSharing :: peerconn -> PeerSharing,

       -- | Request a sample of public root peers.
       --
       -- It is intended to cover use cases including:
       --
       -- * federated relays from a DNS pool
       -- * stake pool relays published in the blockchain
       -- * a pre-distributed snapshot of stake pool relays from the blockchain
       --
       requestPublicRootPeers :: Int -> m (Map peeraddr (PeerAdvertise, IsLedgerPeer), DiffTime),


       -- | Request a sample of big ledger peers.
       --
       requestBigLedgerPeers  :: Int -> m (Set peeraddr, DiffTime),

       -- | The action to contact a known peer and request a sample of its
       -- known peers.
       --
       requestPeerShare       :: PeerSharingAmount -> peeraddr -> m (PeerSharingResult peeraddr),

       -- | Core actions run by the governor to change 'PeerStatus'.
       --
       peerStateActions       :: PeerStateActions peeraddr peerconn m
     }

-- | Callbacks which are performed to change peer state.
--
data PeerStateActions peeraddr peerconn m = PeerStateActions {
    -- | Monitor peer state.  Must be non-blocking.
    --
    monitorPeerConnection    :: peerconn -> STM m (PeerStatus, Maybe ReconnectDelay),

    -- | Establish new connection: cold to warm.
    --
    -- 'IsBigLedgerPeer' is passed from the outbound governor to the
    -- mini-protocol callbacks.
    --
    establishPeerConnection  :: IsBigLedgerPeer
                             -> peeraddr -> m peerconn,

    -- | Activate a connection: warm to hot promotion.
    --
    -- 'IsBigLedgerPeer' is passed from the outbound governor to the
    -- mini-protocol callbacks.
    --
    activatePeerConnection   :: IsBigLedgerPeer
                             -> peerconn -> m (),

    -- | Deactive a peer: hot to warm demotion.
    --
    deactivatePeerConnection :: peerconn -> m (),

    -- | Close a connection: warm to cold transition.
    --
    closePeerConnection      :: peerconn -> m ()
  }

-----------------------
-- Peer Selection State
--

-- | The internal state used by the 'peerSelectionGovernor'.
--
-- The local and public root sets are disjoint, and their union is the
-- overall root set.
--
-- Documentation of individual fields describes some of the invariants these
-- structures should maintain. For the entire picture, see
-- 'assertPeerSelectionState'.
--
data PeerSelectionState peeraddr peerconn = PeerSelectionState {

       targets                     :: !PeerSelectionTargets,

       -- | The current set of local root peers. This is structured as a
       -- bunch of groups, with a target for each group. This gives us a set of
       -- n-of-m choices, e.g. \"pick 2 from this group and 1 from this group\".
       --
       -- The targets must of course be achievable, and to keep things simple,
       -- the groups must be disjoint.
       --
       localRootPeers              :: !(LocalRootPeers peeraddr),

       publicRootPeers             :: !(Set peeraddr),

       -- | Set of big ledger peers.
       --
       bigLedgerPeers              :: !(Set peeraddr),

       -- | Known peers.
       --
       knownPeers                  :: !(KnownPeers peeraddr),

       -- | Established peers.
       --
       establishedPeers            :: !(EstablishedPeers peeraddr peerconn),

       -- | Active peers.
       --
       activePeers                 :: !(Set peeraddr),

       -- | A counter to manage the exponential backoff strategy for when to
       -- retry querying for more public root peers. It is negative for retry
       -- counts after failure, and positive for retry counts that are
       -- successful but make no progress.
       --
       publicRootBackoffs          :: !Int,

       -- | The earliest time we would be prepared to request more public root
       -- peers. This is used with the 'publicRootBackoffs' to manage the
       -- exponential backoff.
       --
       publicRootRetryTime         :: !Time,

       -- | Whether a request for more public root peers is in progress.
       --
       inProgressPublicRootsReq    :: !Bool,

       -- | A counter to manage the exponential backoff strategy for when to
       -- retry querying for more public root peers. It is negative for retry
       -- counts after failure, and positive for retry counts that are
       -- successful but make no progress.
       --
       bigLedgerPeerBackoffs       :: !Int,

       -- | The earliest time we would be prepared to request more big ledger
       -- peers. This is used with the 'bigLedgerPeerBackoffs' to manage the
       -- exponential backoff.
       --
       bigLedgerPeerRetryTime      :: !Time,

       -- | Whether a request for more big ledger peers is in progress.
       --
       inProgressBigLedgerPeersReq :: !Bool,

       inProgressPeerShareReqs     :: !Int,
       inProgressPromoteCold       :: !(Set peeraddr),
       inProgressPromoteWarm       :: !(Set peeraddr),
       inProgressDemoteWarm        :: !(Set peeraddr),
       inProgressDemoteHot         :: !(Set peeraddr),

       -- | Peers that had an async demotion and their connections are still
       -- being closed
       inProgressDemoteToCold      :: !(Set peeraddr),

       -- | Rng for fuzzy delay
       fuzzRng                     :: !StdGen,

       -- | 'PeerSelectionCounters' counters cache. Allows to only trace
       -- values when necessary.
       --
       countersCache               :: !(Cache PeerSelectionCounters)

--     TODO: need something like this to distinguish between lots of bad peers
--     and us getting disconnected from the network locally. We don't want a
--     network disconnect to cause us to flush our full known peer set by
--     considering them all to have bad connectivity.
--     Should also take account of DNS failures for root peer set.
--     lastSuccessfulNetworkEvent :: Time
     }
  deriving Show

-- | Public 'PeerSelectionState' that can be accessed by Peer Sharing
-- mechanisms without any problem.
--
-- This data type should not expose too much information and keep only
-- essential data needed for computing the peer sharing request result
--
newtype PublicPeerSelectionState peeraddr =
  PublicPeerSelectionState {
    availableToShare :: Set peeraddr
  }

emptyPublicPeerSelectionState :: Ord peeraddr
                              => PublicPeerSelectionState peeraddr
emptyPublicPeerSelectionState =
  PublicPeerSelectionState {
    availableToShare = mempty
  }

-- | Convert a 'PeerSelectionState' into a public record accessible by the
-- Peer Sharing mechanisms so we can know about which peers are available and
-- possibly other needed context.
--
toPublicState :: PeerSelectionState peeraddr peerconn
              -> PublicPeerSelectionState peeraddr
toPublicState PeerSelectionState { knownPeers
                                 } =
   PublicPeerSelectionState {
     availableToShare = KnownPeers.getPeerSharingResponsePeers knownPeers
   }

-- Peer selection counters.
--
data PeerSelectionCounters = PeerSelectionCounters {
      -- | All cold peers including ledger peers, root peers, big ledger peers
      -- and peers discovered through peer sharing.
      coldPeers          :: !Int,
      -- | All warm peers including ledger peers, root peers, big ledger peers,
      -- local root peers and peers discovered through peer sharing.
      warmPeers          :: !Int,
      -- | All hot peers including ledger peers, root peers, big ledger peers,
      -- local root peers and peers discovered through peer sharing.
      hotPeers           :: !Int,
      -- | Cold big ledger peers.
      coldBigLedgerPeers :: !Int,
      -- | Warm big ledger peers.
      warmBigLedgerPeers :: !Int,
      -- | Hot big ledger peers.
      hotBigLedgerPeers  :: !Int,
      -- | Local root peers with one entry per group. First entry is the number
      -- of warm peers in that group the second is the number of hot peers in
      -- that group.
      localRoots         :: ![(Int, Int)]
    } deriving (Eq, Show)

peerStateToCounters :: Ord peeraddr => PeerSelectionState peeraddr peerconn -> PeerSelectionCounters
peerStateToCounters st@PeerSelectionState { activePeers, bigLedgerPeers, localRootPeers } =
    PeerSelectionCounters {
      coldPeers          = Set.size $ coldPeersSet Set.\\ bigLedgerPeers,
      warmPeers          = Set.size $ warmPeersSet Set.\\ bigLedgerPeers,
      hotPeers           = Set.size $ hotPeersSet  Set.\\ bigLedgerPeers,
      coldBigLedgerPeers = Set.size $ coldPeersSet `Set.intersection` bigLedgerPeers,
      warmBigLedgerPeers = Set.size $ warmPeersSet `Set.intersection` bigLedgerPeers,
      hotBigLedgerPeers  = Set.size $ hotPeersSet  `Set.intersection` bigLedgerPeers,
      localRoots
    }
  where
    knownPeersSet = KnownPeers.toSet (knownPeers st)
    establishedPeersSet = EstablishedPeers.toSet (establishedPeers st)
    coldPeersSet  = knownPeersSet Set.\\ establishedPeersSet
    warmPeersSet  = establishedPeersSet Set.\\ activePeers
    hotPeersSet   = activePeers
    localRoots    =
      [ (warm, hot)
      | (_,_, members) <- LocalRootPeers.toGroupSets localRootPeers
      , let warm   = Set.size $ members `Set.intersection` warmPeersSet
            hot    = Set.size $ members `Set.intersection` hotPeersSet
      ]

emptyPeerSelectionState :: StdGen
                        -> [(Int, Int)]
                        -> PeerSelectionState peeraddr peerconn
emptyPeerSelectionState rng localRoots =
    PeerSelectionState {
      targets                       = nullPeerSelectionTargets,
      localRootPeers                = LocalRootPeers.empty,
      publicRootPeers               = Set.empty,
      bigLedgerPeers                = Set.empty,
      knownPeers                    = KnownPeers.empty,
      establishedPeers              = EstablishedPeers.empty,
      activePeers                   = Set.empty,
      publicRootBackoffs            = 0,
      publicRootRetryTime           = Time 0,
      inProgressPublicRootsReq      = False,
      bigLedgerPeerBackoffs         = 0,
      bigLedgerPeerRetryTime        = Time 0,
      inProgressBigLedgerPeersReq   = False,
      inProgressPeerShareReqs       = 0,
      inProgressPromoteCold         = Set.empty,
      inProgressPromoteWarm         = Set.empty,
      inProgressDemoteWarm          = Set.empty,
      inProgressDemoteHot           = Set.empty,
      inProgressDemoteToCold        = Set.empty,
      fuzzRng                       = rng,
      countersCache                 = Cache (PeerSelectionCounters 0 0 0 0 0 0 localRoots)
    }


assertPeerSelectionState :: Ord peeraddr
                         => PeerSelectionState peeraddr peerconn
                         -> a -> a
assertPeerSelectionState PeerSelectionState{..} =
    assert (KnownPeers.invariant knownPeers)
  . assert (EstablishedPeers.invariant establishedPeers)
  . assert (LocalRootPeers.invariant localRootPeers)

    -- The activePeers is a subset of the establishedPeers
    -- which is a subset of the known peers
  . assert (Set.isSubsetOf activePeersSet establishedReadySet)
  . assert (Set.isSubsetOf establishedPeersSet knownPeersSet)

   -- The localRootPeers and publicRootPeers must not overlap.
  . assert (Set.null (Set.intersection localRootPeersSet publicRootPeers))

    -- The localRootPeers are a subset of the knownPeers,
    -- and with correct source info in the knownPeers (either
    -- 'PeerSourcePublicRoot' or 'PeerSourceLocalRoot', as local and public
    -- root peers might overlap).
  . assert (Set.isSubsetOf localRootPeersSet knownPeersSet)

    -- The publicRootPeers are a subset of the knownPeers,
    -- and with correct source info in the knownPeers.
  . assert (Set.isSubsetOf publicRootPeers knownPeersSet)

    -- The targets should respect the containment relationships of the root,
    -- known, established and active peers
  . assert (sanePeerSelectionTargets targets)

    -- All the local root peers are always a subset of the known peers. The
    -- target for known peers is a target from both below and above. Thus the
    -- number of local root peers must be less than or equal to the known peers
    -- target, otherwise we could get stuck.
  . assert (LocalRootPeers.size localRootPeers <= targetNumberOfKnownPeers targets)

    -- Interestingly, although the local root peers are also a subset of the
    -- root peers, the equivalent constraint does not apply to the target
    -- number of root peers. The reason is that the root peers target is only
    -- a target from below, not from above. It is ok to have more than the
    -- target number of root peers.
    --
    --That is, we do /not/ need or want this invariant:
    --    LocalRootPeers.size   localRootPeers <= targetNumberOfRootPeers
    --
    -- It is also not necessary for all the targets to be achievable. It is
    -- just necessary that we do not get stuck. So although we have an implicit
    -- target that all local root peers become established, and a certain
    -- number of them become active, these targets do not need to be achievable.
    --
    --That is, we do /not/ need or want this invariant:
    --    LocalRootPeers.size   localRootPeers <= targetNumberOfEstablishedPeers
    --    LocalRootPeers.target localRootPeers <= targetNumberOfActivePeers
    --

    -- All currently established peers are in the availableToConnect set since
    -- the alternative is a record of failure, but these are not (yet) failed.
  . assert (Set.isSubsetOf establishedPeersSet (KnownPeers.availableToConnect knownPeers))

    -- No constraint for publicRootBackoffs, publicRootRetryTime
    -- or inProgressPublicRootsReq

  . assert (inProgressPeerShareReqs >= 0)
  . assert (Set.isSubsetOf inProgressPromoteCold coldPeersSet)
  . assert (Set.isSubsetOf inProgressPromoteWarm warmPeersSet)
  . assert (Set.isSubsetOf inProgressDemoteWarm  warmPeersSet)
  . assert (Set.isSubsetOf inProgressDemoteHot   hotPeersSet)
  . assert (Set.null (Set.intersection inProgressPromoteWarm inProgressDemoteWarm))

  . assert (Set.isSubsetOf inProgressDemoteToCold establishedPeersSet)
  . assert (Set.null (Set.intersection inProgressDemoteToCold inProgressPromoteWarm))
  . assert (Set.null (Set.intersection inProgressDemoteToCold inProgressPromoteCold))
  . assert (Set.null (Set.intersection inProgressDemoteToCold inProgressDemoteHot))
  . assert (Set.null (Set.intersection inProgressDemoteToCold inProgressDemoteWarm))

    -- `bigLedgerPeers` is a subset of known peers and disjoint from public and
    -- local root peers.
  . assert (Set.isSubsetOf bigLedgerPeers knownPeersSet)
  . assert (Set.null (Set.intersection bigLedgerPeers publicRootPeers))
  . assert (Set.null (Set.intersection bigLedgerPeers localRootPeersSet))
  where
    knownPeersSet       = KnownPeers.toSet knownPeers
    localRootPeersSet   = LocalRootPeers.keysSet localRootPeers
    establishedPeersSet = EstablishedPeers.toSet      establishedPeers
    establishedReadySet = EstablishedPeers.readyPeers establishedPeers
    activePeersSet      = activePeers
    coldPeersSet        = knownPeersSet Set.\\ establishedPeersSet
    warmPeersSet        = establishedPeersSet Set.\\ activePeersSet
    hotPeersSet         = activePeersSet


-- | A view of the status of each established peer, for testing and debugging.
--
establishedPeersStatus :: Ord peeraddr
                       => PeerSelectionState peeraddr peerconn
                       -> Map peeraddr PeerStatus
establishedPeersStatus PeerSelectionState{establishedPeers, activePeers} =
    -- map union-override, left to right
    Map.fromSet (\_ -> PeerHot)  activePeers
 <> Map.fromSet (\_ -> PeerWarm) (EstablishedPeers.toSet establishedPeers)


--------------------------------
-- PickPolicy wrapper function
--

-- | Check pre-conditions and post-conditions on the pick policies,
-- and supply additional peer attributes from the current state.
--
pickPeers :: (Ord peeraddr, Functor m)
          => PeerSelectionState peeraddr peerconn
          -> PickPolicy peeraddr m
          -> Set peeraddr -> Int -> m (Set peeraddr)
pickPeers PeerSelectionState{localRootPeers, publicRootPeers, knownPeers}
          pick available num =
    assert precondition $
    fmap (\picked -> assert (postcondition picked) picked)
         (pick peerSource peerConnectFailCount peerTepidFlag
               available numClamped)
  where
    precondition         = not (Set.null available) && num > 0
                        && available `Set.isSubsetOf` KnownPeers.toSet knownPeers
    postcondition picked = not (Set.null picked)
                        && Set.size picked <= numClamped
                        && picked `Set.isSubsetOf` available
    numClamped           = min num (Set.size available)

    peerSource p
      | LocalRootPeers.member p localRootPeers = PeerSourceLocalRoot
      | Set.member p publicRootPeers           = PeerSourcePublicRoot
      | KnownPeers.member p knownPeers         = PeerSourcePeerShare
      | otherwise                              = errorUnavailable

    peerConnectFailCount p =
        fromMaybe errorUnavailable $
          KnownPeers.lookupFailCount p knownPeers

    peerTepidFlag p  =
        fromMaybe errorUnavailable $
          KnownPeers.lookupTepidFlag p knownPeers

    -- This error can trigger if `available` is not a subset of `knownPeers`. In
    -- practice, values supplied by callers as `available` tend to be
    -- constructed from the `PeerSelectionState` passed here, and `knownPeers`
    -- represents a superset of the peers in the state. We also check this
    -- relationship in this function's precondition.
    errorUnavailable =
        error $ "A pick policy requested an attribute for peer address "
             ++ " which is outside of the set given to pick from"


---------------------------
-- Peer Selection Decisions
--


-- | The governor is using @Guarded m (Decision m peeraddr peerconn)@ where 'm'
-- is an 'STM' monad, to drive its progress.
--
data Guarded m a =
    -- | 'GuardedSkip' is used to instruct that there is no action to be made
    -- by the governor.
    --
    -- Let us note that the combined value which is computed by
    -- @guardedDecisions@ term in
    -- 'Ouroboros.Network.PeerSelection.Governor.peerSelectionGovernorLoop' will
    -- never return it: this is because there are monitoring decisions which
    -- never return this constructor, e.g.  'Monitor.targetPeers',
    -- 'Monitor.jobs', 'Monitor.connections', and thus the governor has always
    -- something to do.
    --
    GuardedSkip !(Maybe (Min Time))

    -- | 'Guarded' is used to provide an action through 'FirstToFinish'
    -- synchronisation, possibly with a timeout, to
    -- the governor main loop.
    --
  | Guarded'   !(Maybe (Min Time)) !(FirstToFinish m a)


-- | 'Guarded' constructor which provides an action, possibly with a timeout,
-- to the governor main loop.  It hides the use of 'FirstToFinish'
-- synchronisation.
--
pattern Guarded :: Maybe (Min Time) -> m a -> Guarded m a
pattern Guarded a b <- Guarded' !a (FirstToFinish !b)
  where
    Guarded !a !b = Guarded' a (FirstToFinish b)

{-# COMPLETE GuardedSkip, Guarded #-}

-- | 'Guarded' constructor is absorbing in the sense that
--
-- > Guarded x y <> a = Guarded x' y'
-- > a <> Guarded x y = Guarded x' y'
--
-- In the algebraic sense, @'Guarded' (Just minBound) (return x)@ is a left
-- absorbing element when "m ~ STM m'@ for some monad @m'@.  There is no right
-- absorbing element since there is no right absorbing element in @STM m'@.
--
-- Ref. [absorbing element](https://en.wikipedia.org/wiki/Absorbing_element)
--
instance Alternative m => Semigroup (Guarded m a) where
  Guarded'    ta a <> Guarded'    tb b = Guarded'    (ta <> tb) (a <> b)
  Guarded'    ta a <> GuardedSkip tb   = Guarded'    (ta <> tb)  a
  GuardedSkip ta   <> Guarded'    tb b = Guarded'    (ta <> tb)  b
  GuardedSkip ta   <> GuardedSkip tb   = GuardedSkip (ta <> tb)


data Decision m peeraddr peerconn = Decision {
         -- | A trace event to classify the decision and action
       decisionTrace :: [TracePeerSelection peeraddr],

         -- | An updated state to use immediately
       decisionState :: PeerSelectionState peeraddr peerconn,

       -- | An optional 'Job' to execute asynchronously. This job leads to
       -- a further 'Decision'. This gives a state update to apply upon
       -- completion, but also allows chaining further job actions.
       --
       decisionJobs  :: [Job () m (Completion m peeraddr peerconn)]
     }

-- | Decision which has access to the current time, rather than the time when
-- the governor's loop blocked to make a decision.
--
type TimedDecision m peeraddr peerconn = Time -> Decision m peeraddr peerconn

-- | Type alias for function types which are used to create governor decisions.
-- Almost all decisions are following this pattern.
--
type MkGuardedDecision peeraddr peerconn m
     = PeerSelectionPolicy peeraddr m
    -> PeerSelectionState peeraddr peerconn
    -> Guarded (STM m) (TimedDecision m peeraddr peerconn)


newtype Completion m peeraddr peerconn =
        Completion (PeerSelectionState peeraddr peerconn
                 -> Time -> Decision m peeraddr peerconn)

data TracePeerSelection peeraddr =
       TraceLocalRootPeersChanged (LocalRootPeers peeraddr)
                                  (LocalRootPeers peeraddr)
     -- | Peer selection targets changed: old targets, new targets.
     | TraceTargetsChanged     PeerSelectionTargets PeerSelectionTargets

     | TracePublicRootsRequest Int Int
     | TracePublicRootsResults (Map peeraddr PeerAdvertise) Int DiffTime
     | TracePublicRootsFailure SomeException Int DiffTime
     -- | target known peers, actual known peers, selected peers
     | TraceForgetColdPeers    Int Int (Set peeraddr)

     | TraceBigLedgerPeersRequest Int Int
     | TraceBigLedgerPeersResults (Set peeraddr) Int DiffTime
     | TraceBigLedgerPeersFailure SomeException Int DiffTime
     -- | target known big ledger peers, actual known big ledger peers, selected
     -- peers
     | TraceForgetBigLedgerPeers Int Int (Set peeraddr)

     -- | target known peers, actual known peers, peers available for
     -- peer sharing, peers selected for peer sharing
     | TracePeerShareRequests     Int Int (Set peeraddr) (Set peeraddr)
     | TracePeerShareResults         [(peeraddr, Either SomeException (PeerSharingResult peeraddr))] --TODO: classify failures
     | TracePeerShareResultsFiltered [peeraddr]
     | TraceKnownInboundConnection peeraddr PeerSharing

     -- | target established, actual established, selected peers
     | TracePromoteColdPeers   Int Int (Set peeraddr)
     -- | target local established, actual local established, selected peers
     | TracePromoteColdLocalPeers [(WarmValency, Int)] (Set peeraddr)
     -- promotion, reason
     | TracePromoteColdFailed  Int Int peeraddr DiffTime SomeException
     -- | target established, actual established, peer
     | TracePromoteColdDone    Int Int peeraddr

     -- | target established big ledger peers, actual established big ledger
     -- peers, selected peers
     | TracePromoteColdBigLedgerPeers   Int Int (Set peeraddr)
     -- | target established big ledger peers, actual established big ledger
     -- peers, peer, delay until next promotion, reason
     | TracePromoteColdBigLedgerPeerFailed  Int Int peeraddr DiffTime SomeException
     -- | target established big ledger peers, actual established big ledger
     -- peers, peer
     | TracePromoteColdBigLedgerPeerDone    Int Int peeraddr

     -- | target active, actual active, selected peers
     | TracePromoteWarmPeers   Int Int (Set peeraddr)
     -- | Promote local peers to warm
     | TracePromoteWarmLocalPeers
         [(HotValency, Int)]
         -- ^ local per-group `(target active, actual active)`,
         -- only limited to groups which are below their target.
         (Set peeraddr) -- ^ selected peers
     -- | target active, actual active, peer, reason
     | TracePromoteWarmFailed  Int Int peeraddr SomeException
     -- | target active, actual active, peer
     | TracePromoteWarmDone    Int Int peeraddr
     -- | aborted promotion of a warm peer; likely it was asynchronously
     -- demoted in the meantime.
     --
     -- target active, actual active, peer
     | TracePromoteWarmAborted Int Int peeraddr

     -- | target active big ledger peers, actual active big ledger peers,
     -- selected peers
     | TracePromoteWarmBigLedgerPeers   Int Int (Set peeraddr)
     -- | target active big ledger peers, actual active big ledger peers, peer,
     -- reason
     | TracePromoteWarmBigLedgerPeerFailed  Int Int peeraddr SomeException
     -- | target active big ledger peers, actual active big ledger peers, peer
     | TracePromoteWarmBigLedgerPeerDone    Int Int peeraddr
     -- | aborted promotion of a warm big ledger peer; likely it was
     -- asynchronously demoted in the meantime.
     --
     -- target active, actual active, peer
     | TracePromoteWarmBigLedgerPeerAborted Int Int peeraddr

     -- | target established, actual established, selected peers
     | TraceDemoteWarmPeers    Int Int (Set peeraddr)
     -- | target established, actual established, peer, reason
     | TraceDemoteWarmFailed   Int Int peeraddr SomeException
     -- | target established, actual established, peer
     | TraceDemoteWarmDone     Int Int peeraddr

     -- | target established big ledger peers, actual established big ledger
     -- peers, selected peers
     | TraceDemoteWarmBigLedgerPeers    Int Int (Set peeraddr)
     -- | target established big ledger peers, actual established big ledger
     -- peers, peer, reason
     | TraceDemoteWarmBigLedgerPeerFailed   Int Int peeraddr SomeException
     -- | target established big ledger peers, actual established big ledger
     -- peers, peer
     | TraceDemoteWarmBigLedgerPeerDone     Int Int peeraddr

     -- | target active, actual active, selected peers
     | TraceDemoteHotPeers     Int Int (Set peeraddr)
     -- | local per-group (target active, actual active), selected peers
     | TraceDemoteLocalHotPeers [(HotValency, Int)] (Set peeraddr)
     -- | target active, actual active, peer, reason
     | TraceDemoteHotFailed    Int Int peeraddr SomeException
     -- | target active, actual active, peer
     | TraceDemoteHotDone      Int Int peeraddr

     -- | target active big ledger peers, actual active big ledger peers,
     -- selected peers
     | TraceDemoteHotBigLedgerPeers      Int Int (Set peeraddr)
     -- | target active big ledger peers, actual active big ledger peers, peer,
     -- reason
     | TraceDemoteHotBigLedgerPeerFailed Int Int peeraddr SomeException
     -- | target active big ledger peers, actual active big ledger peers, peer
     | TraceDemoteHotBigLedgerPeerDone   Int Int peeraddr

     | TraceDemoteAsynchronous      (Map peeraddr (PeerStatus, Maybe ReconnectDelay))
     | TraceDemoteLocalAsynchronous (Map peeraddr (PeerStatus, Maybe ReconnectDelay))
     | TraceDemoteBigLedgerPeersAsynchronous
                                    (Map peeraddr (PeerStatus, Maybe ReconnectDelay))

     | TraceGovernorWakeup
     | TraceChurnWait          DiffTime
     | TraceChurnMode          ChurnMode
  deriving Show

data DebugPeerSelection peeraddr where
  TraceGovernorState :: forall peeraddr peerconn.
                        Show peerconn
                     => Time            -- blocked time
                     -> Maybe DiffTime  -- wait time
                     -> PeerSelectionState peeraddr peerconn
                     -> DebugPeerSelection peeraddr

deriving instance (Ord peeraddr, Show peeraddr)
               => Show (DebugPeerSelection peeraddr)

data ChurnMode = ChurnModeBulkSync
               | ChurnModeNormal deriving Show

