{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE ViewPatterns              #-}

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
  , DebugPeerSelectionState (..)
  , makeDebugPeerSelectionState
  , assertPeerSelectionState
  , establishedPeersStatus
  , PublicPeerSelectionState (..)
  , makePublicPeerSelectionStateVar
  , toPublicState
  , Guarded (GuardedSkip, Guarded)
  , Decision (..)
  , TimedDecision
  , MkGuardedDecision
  , Completion (..)
  , PeerSelectionCounters (.., PeerSelectionCountersHWC, numberOfColdPeers, numberOfWarmPeers, numberOfHotPeers, numberOfColdBigLedgerPeers, numberOfWarmBigLedgerPeers, numberOfHotBigLedgerPeers, numberOfColdLocalRootPeers, numberOfWarmLocalRootPeers, numberOfHotLocalRootPeers, localRootsHWC)
  , emptyPeerSelectionCounters
  , peerStateToCounters
    -- * Peer Sharing Auxiliary data type
  , PeerSharingResult (..)
    -- * Traces
  , TracePeerSelection (..)
  , ChurnAction (..)
  , DebugPeerSelection (..)
    -- * Error types
  , BootstrapPeersCriticalTimeoutError (..)
  ) where

import Data.Cache (Cache (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Monoid.Synchronisation (FirstToFinish (..))
import Data.OrdPSQ qualified as PSQ
import Data.Semigroup (Min (..))
import Data.Set (Set)
import Data.Set qualified as Set

import Control.Applicative (Alternative)
import Control.Concurrent.JobPool (Job)
import Control.Exception (Exception (..), SomeException, assert)
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadTime.SI
import System.Random (StdGen)

import Control.Concurrent.Class.MonadSTM.Strict
import Ouroboros.Network.ExitPolicy
import Ouroboros.Network.PeerSelection.Bootstrap (UseBootstrapPeers (..))
import Ouroboros.Network.PeerSelection.LedgerPeers (IsBigLedgerPeer,
           LedgerPeersKind)
import Ouroboros.Network.PeerSelection.LedgerPeers.Type
           (LedgerStateJudgement (..))
import Ouroboros.Network.PeerSelection.LocalRootPeers (OutboundConnectionsState)
import Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise)
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing)
import Ouroboros.Network.PeerSelection.PeerTrustable (PeerTrustable)
import Ouroboros.Network.PeerSelection.PublicRootPeers (PublicRootPeers)
import Ouroboros.Network.PeerSelection.PublicRootPeers qualified as PublicRootPeers
import Ouroboros.Network.PeerSelection.State.EstablishedPeers (EstablishedPeers)
import Ouroboros.Network.PeerSelection.State.EstablishedPeers qualified as EstablishedPeers
import Ouroboros.Network.PeerSelection.State.KnownPeers (KnownPeers)
import Ouroboros.Network.PeerSelection.State.KnownPeers qualified as KnownPeers
import Ouroboros.Network.PeerSelection.State.LocalRootPeers (HotValency (..),
           LocalRootPeers, WarmValency (..))
import Ouroboros.Network.PeerSelection.State.LocalRootPeers qualified as LocalRootPeers
import Ouroboros.Network.PeerSelection.Types (PeerSource (..),
           PeerStatus (PeerHot, PeerWarm))
import Ouroboros.Network.Protocol.PeerSharing.Type (PeerSharingAmount,
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
       policyPeerShareActivationDelay   :: !DiffTime,
       -- ^ Delay until we consider a peer suitable for peer sharing

       -- | Re-promote delay, passed from `ExitPolicy`.
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
                                          , Map peeraddr ( PeerAdvertise
                                                         , PeerTrustable))],

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
       -- * Official bootstrap peers from a trusted source
       -- * stake pool relays published in the blockchain
       -- * a pre-distributed snapshot of stake pool relays from the blockchain
       --
       -- It also makes a distinction between normal and big ledger peers to be
       -- fetched.
       --
       requestPublicRootPeers   :: LedgerPeersKind -> Int -> m (PublicRootPeers peeraddr, DiffTime),

       -- | The action to contact a known peer and request a sample of its
       -- known peers.
       --
       requestPeerShare       :: PeerSharingAmount -> peeraddr -> m (PeerSharingResult peeraddr),

       -- | Core actions run by the governor to change 'PeerStatus'.
       --
       peerStateActions       :: PeerStateActions peeraddr peerconn m,

       -- | Read the current bootstrap peers flag
       readUseBootstrapPeers :: STM m UseBootstrapPeers,

       -- | Read the current ledger state judgement
       --
       readLedgerStateJudgement :: STM m LedgerStateJudgement,

       -- | Callback provided by consensus to inform it if the node is
       -- connected to only local roots or also some external peers.
       --
       -- This is useful in order for the Bootstrap State Machine to
       -- simply refuse to transition from TooOld to YoungEnough while
       -- it only has local peers.
       --
       updateOutboundConnectionsState :: OutboundConnectionsState -> STM m ()

     }

-- | Callbacks which are performed to change peer state.
--
data PeerStateActions peeraddr peerconn m = PeerStateActions {
    -- | Monitor peer state.  Must be non-blocking.
    --
    monitorPeerConnection    :: peerconn -> STM m (PeerStatus, Maybe RepromoteDelay),

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

       -- | This set holds the public root peers (i.e. Ledger (small and big),
       -- Bootstrap peers and locally configured public root peers).
       --
       publicRootPeers             :: !(PublicRootPeers peeraddr),

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
       countersCache               :: !(Cache PeerSelectionCounters),

       -- | Current ledger state judgement
       --
       ledgerStateJudgement        :: !LedgerStateJudgement,

       -- | Current value of 'UseBootstrapPeers'.
       --
       bootstrapPeersFlag          :: !UseBootstrapPeers,

       -- | Has the governor fully reset its state
       --
       hasOnlyBootstrapPeers       :: !Bool,

       -- | Has the governor fully reset its state
       --
       bootstrapPeersTimeout       :: !(Maybe Time)

--     TODO: need something like this to distinguish between lots of bad peers
--     and us getting disconnected from the network locally. We don't want a
--     network disconnect to cause us to flush our full known peer set by
--     considering them all to have bad connectivity.
--     Should also take account of DNS failures for root peer set.
--     lastSuccessfulNetworkEvent :: Time
     }
  deriving Show

-----------------------
-- Debug copy of Peer Selection State
--
-- Used for dumping the peer selection state upon getting a USR1 signal.
--
data DebugPeerSelectionState peeraddr = DebugPeerSelectionState {
       dpssTargets                     :: !PeerSelectionTargets,
       dpssLocalRootPeers              :: !(LocalRootPeers peeraddr),
       dpssPublicRootPeers             :: !(PublicRootPeers peeraddr),
       dpssKnownPeers                  :: !(KnownPeers peeraddr),
       dpssEstablishedPeers            :: !(Set peeraddr),
       dpssActivePeers                 :: !(Set peeraddr),
       dpssPublicRootBackoffs          :: !Int,
       dpssPublicRootRetryTime         :: !Time,
       dpssInProgressPublicRootsReq    :: !Bool,
       dpssBigLedgerPeerBackoffs       :: !Int,
       dpssBigLedgerPeerRetryTime      :: !Time,
       dpssInProgressBigLedgerPeersReq :: !Bool,
       dpssInProgressPeerShareReqs     :: !Int,
       dpssInProgressPromoteCold       :: !(Set peeraddr),
       dpssInProgressPromoteWarm       :: !(Set peeraddr),
       dpssInProgressDemoteWarm        :: !(Set peeraddr),
       dpssInProgressDemoteHot         :: !(Set peeraddr),
       dpssInProgressDemoteToCold      :: !(Set peeraddr),
       dpssUpstreamyness               :: !(Map peeraddr Int),
       dpssFetchynessBlocks            :: !(Map peeraddr Int)
} deriving Show

makeDebugPeerSelectionState :: PeerSelectionState peeraddr peerconn
                            -> Map peeraddr Int
                            -> Map peeraddr Int
                            -> DebugPeerSelectionState peeraddr
makeDebugPeerSelectionState PeerSelectionState {..} up bp =
  DebugPeerSelectionState {
      dpssTargets                     = targets
    , dpssLocalRootPeers              = localRootPeers
    , dpssPublicRootPeers             = publicRootPeers
    , dpssKnownPeers                  = knownPeers
    , dpssEstablishedPeers            = EstablishedPeers.toSet establishedPeers
    , dpssActivePeers                 = activePeers
    , dpssPublicRootBackoffs          = publicRootBackoffs
    , dpssPublicRootRetryTime         = publicRootRetryTime
    , dpssInProgressPublicRootsReq    = inProgressPublicRootsReq
    , dpssBigLedgerPeerBackoffs       = bigLedgerPeerBackoffs
    , dpssBigLedgerPeerRetryTime      = bigLedgerPeerRetryTime
    , dpssInProgressBigLedgerPeersReq = inProgressBigLedgerPeersReq
    , dpssInProgressPeerShareReqs     = inProgressPeerShareReqs
    , dpssInProgressPromoteCold       = inProgressPromoteCold
    , dpssInProgressPromoteWarm       = inProgressPromoteWarm
    , dpssInProgressDemoteWarm        = inProgressDemoteWarm
    , dpssInProgressDemoteHot         = inProgressDemoteHot
    , dpssInProgressDemoteToCold      = inProgressDemoteToCold
    , dpssUpstreamyness               = up
    , dpssFetchynessBlocks            = bp
    }

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

makePublicPeerSelectionStateVar
 :: (MonadSTM m, Ord peeraddr)
 => m (StrictTVar m (PublicPeerSelectionState peeraddr))
makePublicPeerSelectionStateVar = newTVarIO emptyPublicPeerSelectionState


-- | Convert a 'PeerSelectionState' into a public record accessible by the
-- Peer Sharing mechanisms so we can know about which peers are available and
-- possibly other needed context.
--
toPublicState :: PeerSelectionState peeraddr peerconn
              -> PublicPeerSelectionState peeraddr
toPublicState PeerSelectionState { knownPeers } =
   PublicPeerSelectionState {
     availableToShare =
       KnownPeers.getPeerSharingResponsePeers knownPeers
   }

-- Peer selection counters.
--
data PeerSelectionCounters = PeerSelectionCounters {
      numberOfRootPeers                     :: Int,

      --
      -- Non Big Ledger Peers
      --

      numberOfKnownPeers                    :: Int,
      -- ^ number of known peers excluding big ledger peers
      numberOfColdPeersPromotions           :: Int,
      -- ^ number of known peers (excluding big ledger peers) being promoted to
      -- warm
      numberOfEstablishedPeers              :: Int,
      -- ^ number of established peers excluding big ledger peers
      numberOfWarmPeersDemotions            :: Int,
      -- ^ number of warm peers (excluding big ledger peers) being demoted to
      -- cold
      numberOfWarmPeersPromotions           :: Int,
      -- ^ number of warm peers (excluding big ledger peers) being promote to
      -- hot
      numberOfActivePeers                   :: Int,
      -- ^ number of active peers excluding big ledger peers
      numberOfActivePeersDemotions          :: Int,
      -- ^ number of active peers (excluding big ledger peers) being demoted to
      -- warm

      --
      -- Big Ledger Peers
      --

      numberOfKnownBigLedgerPeers           :: Int,
      -- ^ number of known big ledger peers
      numberOfColdBigLedgerPeersPromotions  :: Int,
      -- ^ number of cold big ledger peers being promoted to warm
      numberOfEstablishedBigLedgerPeers     :: Int,
      -- ^ number of established big ledger peers
      numberOfWarmBigLedgerPeersDemotions   :: Int,
      -- ^ number of warm big ledger peers being demoted to cold
      numberOfWarmBigLedgerPeersPromotions  :: Int,
      -- ^ number of warm big ledger peers being promote to hot
      numberOfActiveBigLedgerPeers          :: Int,
      -- ^ number of active big ledger peers
      numberOfActiveBigLedgerPeersDemotions :: Int,
      -- ^ number of active big ledger peers being demoted to warm

      --
      -- Local Roots
      --

      numberOfKnownLocalRootPeers           :: Int,
      -- ^ number of known local root peers should always be equal to the sum
      -- of established & active local roots.
      numberOfEstablishedLocalRootPeers     :: Int,
      numberOfWarmLocalRootPeersPromotions  :: Int,
      numberOfActiveLocalRootPeers          :: Int,
      numberOfActiveLocalRootPeersDemotions :: Int,

      -- | Local root peers with one entry per group. First entry is the number
      -- of warm peers in that group the second is the number of hot peers in
      -- that group.
      localRoots                            :: ![(HotValency, WarmValency)],

      --
      -- Share Peers
      -- (peers received through peer sharing)
      --

      numberOfKnownSharedPeers              :: Int,
      numberOfColdSharedPeersPromotions     :: Int,
      numberOfEstablishedSharedPeers        :: Int,
      numberOfWarmSharedPeersDemotions      :: Int,
      numberOfWarmSharedPeersPromotions     :: Int,
      numberOfActiveSharedPeers             :: Int,
      numberOfActiveSharedPeersDemotions    :: Int,

      --
      -- Bootstrap Peers
      --

      numberOfKnownBootstrapPeers           :: Int,
      numberOfColdBootstrapPeersPromotions  :: Int,
      numberOfEstablishedBootstrapPeers     :: Int,
      numberOfWarmBootstrapPeersDemotions   :: Int,
      numberOfWarmBootstrapPeersPromotions  :: Int,
      numberOfActiveBootstrapPeers          :: Int,
      numberOfActiveBootstrapPeersDemotions :: Int
    } deriving (Eq, Show)


-- | A Pattern synonym which computes `hot`, `warm`, `cold` counters from
-- `PeerSelectionCounters`.
--
pattern PeerSelectionCountersHWC :: Int -> Int -> Int -- peers
                                 -> Int -> Int -> Int -- big ledger peers
                                 -> Int -> Int -> Int -- local roots
                                 -> [(HotValency, WarmValency)]
                                 -> PeerSelectionCounters
pattern PeerSelectionCountersHWC { numberOfColdPeers,
                                   numberOfWarmPeers,
                                   numberOfHotPeers,

                                   numberOfColdBigLedgerPeers,
                                   numberOfWarmBigLedgerPeers,
                                   numberOfHotBigLedgerPeers,

                                   numberOfColdLocalRootPeers,
                                   numberOfWarmLocalRootPeers,
                                   numberOfHotLocalRootPeers,
                                   localRootsHWC }

        <- (peerSelectionCountersHWC ->
             PeerSelectionCounters { numberOfKnownPeers                = numberOfColdPeers,
                                     numberOfEstablishedPeers          = numberOfWarmPeers,
                                     numberOfActivePeers               = numberOfHotPeers,

                                     numberOfKnownBigLedgerPeers       = numberOfColdBigLedgerPeers,
                                     numberOfEstablishedBigLedgerPeers = numberOfWarmBigLedgerPeers,
                                     numberOfActiveBigLedgerPeers      = numberOfHotBigLedgerPeers,

                                     numberOfKnownLocalRootPeers       = numberOfColdLocalRootPeers,
                                     numberOfEstablishedLocalRootPeers = numberOfWarmLocalRootPeers,
                                     numberOfActiveLocalRootPeers      = numberOfHotLocalRootPeers,
                                     localRoots                        = localRootsHWC })

{-# COMPLETE PeerSelectionCountersHWC #-}

-- | Internal function, used to pattern match with `PeerSelectionCountersHWC`
peerSelectionCountersHWC :: PeerSelectionCounters -> PeerSelectionCounters
peerSelectionCountersHWC PeerSelectionCounters {..} =
    PeerSelectionCounters {
      numberOfRootPeers,

      numberOfKnownPeers                         = numberOfKnownPeers
                                                 - numberOfEstablishedPeers,
      numberOfColdPeersPromotions,
      numberOfEstablishedPeers                   = numberOfEstablishedPeers
                                                 - numberOfActivePeers,
      numberOfWarmPeersDemotions,
      numberOfWarmPeersPromotions,
      numberOfActivePeers,
      numberOfActivePeersDemotions,

      numberOfKnownBigLedgerPeers                = numberOfKnownBigLedgerPeers
                                                 - numberOfEstablishedBigLedgerPeers,
      numberOfColdBigLedgerPeersPromotions,
      numberOfEstablishedBigLedgerPeers          = numberOfEstablishedBigLedgerPeers
                                                 - numberOfActiveBigLedgerPeers,
      numberOfWarmBigLedgerPeersDemotions,
      numberOfWarmBigLedgerPeersPromotions,
      numberOfActiveBigLedgerPeers,
      numberOfActiveBigLedgerPeersDemotions,

      numberOfKnownLocalRootPeers                = numberOfKnownLocalRootPeers
                                                 - numberOfEstablishedLocalRootPeers,
      numberOfEstablishedLocalRootPeers          = numberOfEstablishedLocalRootPeers
                                                 - numberOfActiveLocalRootPeers,
      numberOfWarmLocalRootPeersPromotions,
      numberOfActiveLocalRootPeers,
      numberOfActiveLocalRootPeersDemotions,

      localRoots,

      numberOfKnownSharedPeers                   = numberOfKnownSharedPeers
                                                 - numberOfEstablishedSharedPeers,
      numberOfColdSharedPeersPromotions,
      numberOfEstablishedSharedPeers             = numberOfEstablishedSharedPeers
                                                 - numberOfActiveSharedPeers,
      numberOfWarmSharedPeersDemotions,
      numberOfWarmSharedPeersPromotions,
      numberOfActiveSharedPeers,
      numberOfActiveSharedPeersDemotions,

      numberOfKnownBootstrapPeers                = numberOfKnownBootstrapPeers
                                                 - numberOfEstablishedBootstrapPeers,
      numberOfColdBootstrapPeersPromotions,
      numberOfEstablishedBootstrapPeers          = numberOfEstablishedBootstrapPeers
                                                 - numberOfActiveBootstrapPeers,
      numberOfWarmBootstrapPeersDemotions,
      numberOfWarmBootstrapPeersPromotions,
      numberOfActiveBootstrapPeers,
      numberOfActiveBootstrapPeersDemotions
    }


peerStateToCounters :: Ord peeraddr => PeerSelectionState peeraddr peerconn -> PeerSelectionCounters
peerStateToCounters PeerSelectionState { knownPeers,
                                         establishedPeers,
                                         activePeers,
                                         publicRootPeers,
                                         localRootPeers,
                                         inProgressPromoteCold,
                                         inProgressPromoteWarm,
                                         inProgressDemoteWarm,
                                         inProgressDemoteHot } =
    PeerSelectionCounters {
      numberOfRootPeers                          = LocalRootPeers.size localRootPeers
                                                 + PublicRootPeers.size publicRootPeers,

      numberOfKnownPeers                         = Set.size   knownPeersSet,
      numberOfColdPeersPromotions                = Set.size $ knownPeersSet `Set.intersection` inProgressPromoteCold,
      numberOfEstablishedPeers                   = Set.size   establishedPeersSet,
      numberOfWarmPeersDemotions                 = Set.size $ establishedPeersSet `Set.intersection` inProgressDemoteWarm,
      numberOfWarmPeersPromotions                = Set.size $ establishedPeersSet `Set.intersection` inProgressPromoteWarm,
      numberOfActivePeers                        = Set.size   activePeersSet,
      numberOfActivePeersDemotions               = Set.size $ activePeersSet `Set.intersection` inProgressDemoteHot,

      numberOfKnownBigLedgerPeers                = Set.size   knownBigLedgerPeersSet,
      numberOfColdBigLedgerPeersPromotions       = Set.size $ knownBigLedgerPeersSet `Set.intersection` inProgressPromoteCold,
      numberOfEstablishedBigLedgerPeers          = Set.size   establishedBigLedgerPeersSet,
      numberOfWarmBigLedgerPeersDemotions        = Set.size $ establishedBigLedgerPeersSet `Set.intersection` inProgressDemoteWarm,
      numberOfWarmBigLedgerPeersPromotions       = Set.size $ establishedBigLedgerPeersSet `Set.intersection` inProgressPromoteWarm,
      numberOfActiveBigLedgerPeers               = Set.size   activeBigLedgerPeersSet,
      numberOfActiveBigLedgerPeersDemotions      = Set.size $ activeBigLedgerPeersSet `Set.intersection` inProgressDemoteHot,


      numberOfKnownBootstrapPeers                = Set.size   knownBootstrapPeersSet,
      numberOfColdBootstrapPeersPromotions       = Set.size $ knownBootstrapPeersSet `Set.intersection` inProgressPromoteCold,
      numberOfEstablishedBootstrapPeers          = Set.size   establishedBootstrapPeersSet,
      numberOfWarmBootstrapPeersDemotions        = Set.size $ establishedBootstrapPeersSet `Set.intersection` inProgressDemoteWarm,
      numberOfWarmBootstrapPeersPromotions       = Set.size $ establishedBootstrapPeersSet `Set.intersection` inProgressPromoteWarm,
      numberOfActiveBootstrapPeers               = Set.size   activeBootstrapPeersSet,
      numberOfActiveBootstrapPeersDemotions      = Set.size $ activeBootstrapPeersSet `Set.intersection` inProgressDemoteHot,

      numberOfKnownLocalRootPeers                = Set.size   knownLocalRootPeersSet,
      numberOfEstablishedLocalRootPeers          = Set.size $ establishedLocalRootsPeersSet,
      numberOfWarmLocalRootPeersPromotions       = Set.size $ establishedLocalRootsPeersSet `Set.intersection` inProgressPromoteWarm,
      numberOfActiveLocalRootPeers               = Set.size   activeLocalRootsPeersSet,
      numberOfActiveLocalRootPeersDemotions      = Set.size $ activeLocalRootsPeersSet `Set.intersection` inProgressDemoteHot,

      localRoots,

      numberOfKnownSharedPeers                   = Set.size   knownSharedPeersSet,
      numberOfColdSharedPeersPromotions          = Set.size $ knownSharedPeersSet `Set.intersection` inProgressPromoteCold,
      numberOfEstablishedSharedPeers             = Set.size   establishedSharedPeersSet,
      numberOfWarmSharedPeersDemotions           = Set.size $ establishedSharedPeersSet `Set.intersection` inProgressDemoteWarm,
      numberOfWarmSharedPeersPromotions          = Set.size $ establishedSharedPeersSet `Set.intersection` inProgressPromoteWarm,
      numberOfActiveSharedPeers                  = Set.size   activeSharedPeersSet,
      numberOfActiveSharedPeersDemotions         = Set.size $ activeSharedPeersSet `Set.intersection` inProgressDemoteHot
    }
  where
    -- convention: only `{known,established,active}.*PeersSet` and
    -- `inProgress{Promote,Demote}{Cold,Warm,Hot}` identifiers are used in the
    -- calculations above ↑

    -- common sets
    knownSet       = KnownPeers.toSet knownPeers
    establishedSet = EstablishedPeers.toSet establishedPeers
    bigLedgerSet   = PublicRootPeers.getBigLedgerPeers publicRootPeers

    -- non big ledger peers
    knownPeersSet       = knownSet Set.\\ bigLedgerSet
    establishedPeersSet = establishedSet Set.\\ bigLedgerSet
    activePeersSet      = activePeers Set.\\ bigLedgerSet

    -- big ledger peers
    knownBigLedgerPeersSet       = knownSet `Set.intersection` bigLedgerSet
    establishedBigLedgerPeersSet = establishedSet `Set.intersection` bigLedgerSet
    activeBigLedgerPeersSet      = activePeers `Set.intersection` bigLedgerSet

    -- local roots
    localRoots =
      [ (hot, warm)
      | (hot, warm, _) <- LocalRootPeers.toGroupSets localRootPeers
      ]
    localRootSet                  = LocalRootPeers.keysSet localRootPeers
    -- local roots and big ledger peers are disjoin, hence we can use
    -- `knownPeersSet`, `establishedPeersSet` and `activePeersSet` below.
    knownLocalRootPeersSet        = knownPeersSet `Set.intersection` localRootSet
    establishedLocalRootsPeersSet = establishedPeersSet `Set.intersection` localRootSet
    activeLocalRootsPeersSet      = activePeersSet `Set.intersection` localRootSet

    -- bootstrap peers
    bootstrapSet                 = PublicRootPeers.getBootstrapPeers publicRootPeers
    -- boostrap peers and big ledger peers are disjoin, hence we can use
    -- `knownPeersSet`, `establishedPeersSet` and `activePeersSet` below.
    knownBootstrapPeersSet       = knownPeersSet `Set.intersection` bootstrapSet
    establishedBootstrapPeersSet = establishedPeersSet `Set.intersection` bootstrapSet
    activeBootstrapPeersSet      = activePeersSet `Set.intersection` bootstrapSet

    -- shared peers
    rootSet                    = PublicRootPeers.toSet publicRootPeers
                              <> localRootSet
    -- shared peers are not big ledger peers, hence we can use `knownPeersSet`,
    -- `establishedPeersSet` and `activePeersSet` below.
    knownSharedPeersSet        = knownPeersSet Set.\\ rootSet
    establishedSharedPeersSet  = establishedPeersSet Set.\\ rootSet
    activeSharedPeersSet       = activePeersSet Set.\\ rootSet



emptyPeerSelectionCounters :: [(HotValency, WarmValency)]
                           -> PeerSelectionCounters
emptyPeerSelectionCounters localRoots =
  PeerSelectionCounters {
    numberOfRootPeers                     = 0,

    numberOfKnownPeers                    = 0,
    numberOfColdPeersPromotions           = 0,
    numberOfEstablishedPeers              = 0,
    numberOfWarmPeersDemotions            = 0,
    numberOfWarmPeersPromotions           = 0,
    numberOfActivePeers                   = 0,
    numberOfActivePeersDemotions          = 0,

    numberOfKnownBigLedgerPeers           = 0,
    numberOfColdBigLedgerPeersPromotions  = 0,
    numberOfEstablishedBigLedgerPeers     = 0,
    numberOfWarmBigLedgerPeersDemotions   = 0,
    numberOfWarmBigLedgerPeersPromotions  = 0,
    numberOfActiveBigLedgerPeers          = 0,
    numberOfActiveBigLedgerPeersDemotions = 0,

    numberOfKnownBootstrapPeers           = 0,
    numberOfColdBootstrapPeersPromotions  = 0,
    numberOfEstablishedBootstrapPeers     = 0,
    numberOfWarmBootstrapPeersDemotions   = 0,
    numberOfWarmBootstrapPeersPromotions  = 0,
    numberOfActiveBootstrapPeers          = 0,
    numberOfActiveBootstrapPeersDemotions = 0,

    numberOfKnownLocalRootPeers           = 0,
    numberOfEstablishedLocalRootPeers     = 0,
    numberOfWarmLocalRootPeersPromotions  = 0,
    numberOfActiveLocalRootPeers          = 0,
    numberOfActiveLocalRootPeersDemotions = 0,

    localRoots,

    numberOfKnownSharedPeers              = 0,
    numberOfColdSharedPeersPromotions     = 0,
    numberOfEstablishedSharedPeers        = 0,
    numberOfWarmSharedPeersDemotions      = 0,
    numberOfWarmSharedPeersPromotions     = 0,
    numberOfActiveSharedPeers             = 0,
    numberOfActiveSharedPeersDemotions    = 0
  }

emptyPeerSelectionState :: StdGen
                        -> [(HotValency, WarmValency)]
                        -> PeerSelectionState peeraddr peerconn
emptyPeerSelectionState rng localRoots =
    PeerSelectionState {
      targets                     = nullPeerSelectionTargets,
      localRootPeers              = LocalRootPeers.empty,
      publicRootPeers             = PublicRootPeers.empty,
      knownPeers                  = KnownPeers.empty,
      establishedPeers            = EstablishedPeers.empty,
      activePeers                 = Set.empty,
      publicRootBackoffs          = 0,
      publicRootRetryTime         = Time 0,
      inProgressPublicRootsReq    = False,
      bigLedgerPeerBackoffs       = 0,
      bigLedgerPeerRetryTime      = Time 0,
      inProgressBigLedgerPeersReq = False,
      inProgressPeerShareReqs     = 0,
      inProgressPromoteCold       = Set.empty,
      inProgressPromoteWarm       = Set.empty,
      inProgressDemoteWarm        = Set.empty,
      inProgressDemoteHot         = Set.empty,
      inProgressDemoteToCold      = Set.empty,
      fuzzRng                     = rng,
      countersCache               = Cache (emptyPeerSelectionCounters localRoots),
      ledgerStateJudgement        = TooOld,
      bootstrapPeersFlag          = DontUseBootstrapPeers,
      hasOnlyBootstrapPeers       = False,
      bootstrapPeersTimeout       = Nothing
    }


assertPeerSelectionState :: Ord peeraddr
                         => PeerSelectionState peeraddr peerconn
                         -> a -> a
assertPeerSelectionState PeerSelectionState{..} =
    assert (KnownPeers.invariant knownPeers)
  . assert (EstablishedPeers.invariant establishedPeers)
  . assert (LocalRootPeers.invariant localRootPeers)
  . assert (PublicRootPeers.invariant publicRootPeers)

    -- The activePeers is a subset of the establishedPeers
    -- which is a subset of the known peers
  . assert (Set.isSubsetOf activePeersSet establishedReadySet)
  . assert (Set.isSubsetOf establishedPeersSet knownPeersSet)

   -- The localRootPeers and publicRootPeers must not overlap.
  . assert (Set.null (Set.intersection localRootPeersSet publicRootPeersSet))

    -- The localRootPeers are a subset of the knownPeers,
    -- and with correct source info in the knownPeers (either
    -- 'PeerSourcePublicRoot' or 'PeerSourceLocalRoot', as local and public
    -- root peers might overlap).
  . assert (Set.isSubsetOf localRootPeersSet knownPeersSet)

    -- The publicRootPeers are a subset of the knownPeers,
  . assert (Set.isSubsetOf publicRootPeersSet knownPeersSet)

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

    -- The following aren't hard invariants but rather eventually consistent
    -- invariants that are checked via testing:

    -- 1. If node is not in sensitive state then it can't have only BootstrapPeers
    --
    -- 2. If hasOnlyBootstrapPeers is true and bootstrap peers are enabled then known
    -- peers set is a subset of the bootstrap peers + trusted local roots.
    -- Unless the TargetKnownRootPeers is 0, in that case there can be a delay
    -- where the node forgets the local roots.

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

    -- `bigLedgerPeers` is a subset of known peers (and also public root peers)
    -- and disjoint local root peers.
  . assert (Set.isSubsetOf bigLedgerPeers knownPeersSet)
  . assert (Set.null (Set.intersection bigLedgerPeers localRootPeersSet))

    -- Only peer which has support peersharing should be possible to issue
    -- peersharing requests to.
  . assert (Set.isSubsetOf establishedShareSet
      (KnownPeers.getPeerSharingRequestPeers knownPeersSet knownPeers))
  where
    knownPeersSet       = KnownPeers.toSet knownPeers
    localRootPeersSet   = LocalRootPeers.keysSet localRootPeers
    publicRootPeersSet  = PublicRootPeers.toSet publicRootPeers
    bigLedgerPeers      = PublicRootPeers.getBigLedgerPeers publicRootPeers
    establishedPeersSet = EstablishedPeers.toSet      establishedPeers
    establishedReadySet = EstablishedPeers.readyPeers establishedPeers
    establishedShareSet = EstablishedPeers.availableForPeerShare establishedPeers <>
                            Set.fromList (PSQ.keys (EstablishedPeers.nextPeerShareTimes
                                                    establishedPeers))
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
      | LocalRootPeers.member p localRootPeers   = PeerSourceLocalRoot
      | PublicRootPeers.member p publicRootPeers = PeerSourcePublicRoot
      | KnownPeers.member p knownPeers           = PeerSourcePeerShare
      | otherwise                                = errorUnavailable

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
    -- | `GuardedSkip'` is used to instruct that there is no action to be made
    -- by the governor. See 'GuardedSkip'.
    --
    GuardedSkip' !(Maybe (Min Time))

    -- | `Guarded'` is used to provide an action through 'FirstToFinish'
    -- synchronisation, possibly with a timeout, to the governor main loop. See
    -- 'Guarded'.
    --
  | Guarded'     !(Maybe (Min Time)) !(FirstToFinish m a)


-- | 'Guarded' is used to provide an action possibly with a timeout, to the
-- governor main loop.
--
-- 'Guarded' is a pattern which which  hides the use of 'FirstToFinish' and
-- 'Min' newtype wrappers.
--
pattern Guarded :: Maybe Time -> m a -> Guarded m a
pattern Guarded a b <- Guarded' !(fmap getMin -> a) (FirstToFinish !b)
  where
    Guarded !a !b = Guarded' (Min <$> a) (FirstToFinish b)

-- | 'GuardedSkip' is used to instruct that there is no action to be made
-- by the governor. See 'GuardedSkip'.
--
-- 'GuardedSkip' is a pattern which hides the usage of `Min` newtype wrapper in
-- `GuardedSkip'` constructor (private).
--
-- Let us note that the combined value which is computed by @guardedDecisions@
-- term in 'Ouroboros.Network.PeerSelection.Governor.peerSelectionGovernorLoop'
-- will never return it: this is because there are monitoring decisions which
-- never return this constructor, e.g.  'Monitor.targetPeers', 'Monitor.jobs',
-- 'Monitor.connections', and thus the governor has always something to do.
--
pattern GuardedSkip :: Maybe Time -> Guarded m a
pattern GuardedSkip a <- GuardedSkip' !(fmap getMin -> a)
  where
    GuardedSkip !a = GuardedSkip' (Min <$> a)

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
  Guarded'     ta a <> Guarded'     tb b = Guarded'     (ta <> tb) (a <> b)
  Guarded'     ta a <> GuardedSkip' tb   = Guarded'     (ta <> tb)  a
  GuardedSkip' ta   <> Guarded'     tb b = Guarded'     (ta <> tb)  b
  GuardedSkip' ta   <> GuardedSkip' tb   = GuardedSkip' (ta <> tb)


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
     --
     -- Churn
     --

     -- | Peer selection targets changed: old targets, new targets.
     | TraceTargetsChanged     PeerSelectionTargets PeerSelectionTargets

     --
     -- Ledger Peers
     --

     | TracePublicRootsRequest Int Int
     | TracePublicRootsResults (PublicRootPeers peeraddr) Int DiffTime
     | TracePublicRootsFailure SomeException Int DiffTime

     -- | target known peers, actual known peers, selected peers
     | TraceForgetColdPeers    Int Int (Set peeraddr)

     | TraceBigLedgerPeersRequest Int Int
     | TraceBigLedgerPeersResults (Set peeraddr) Int DiffTime
     | TraceBigLedgerPeersFailure SomeException Int DiffTime
     -- | target known big ledger peers, actual known big ledger peers, selected
     -- peers
     | TraceForgetBigLedgerPeers Int Int (Set peeraddr)

     --
     -- Peer Sharing
     --

     -- | target known peers, actual known peers, number of peers to request,
     -- peers available for peer sharing, peers selected for peer sharing
     | TracePeerShareRequests     Int Int PeerSharingAmount (Set peeraddr) (Set peeraddr)
     | TracePeerShareResults         [(peeraddr, Either SomeException (PeerSharingResult peeraddr))] --TODO: classify failures
     | TracePeerShareResultsFiltered [peeraddr]
     | TraceKnownInboundConnection peeraddr PeerSharing

     --
     -- Promote Cold Peers
     --

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

     --
     -- Promote Warm Peers
     --

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

     --
     -- Demote Warm Peers
     --

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

     --
     -- Demote Hot Peers
     --

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

     --
     -- Async Demotions
     --

     | TraceDemoteAsynchronous      (Map peeraddr (PeerStatus, Maybe RepromoteDelay))
     | TraceDemoteLocalAsynchronous (Map peeraddr (PeerStatus, Maybe RepromoteDelay))
     | TraceDemoteBigLedgerPeersAsynchronous
                                    (Map peeraddr (PeerStatus, Maybe RepromoteDelay))

     | TraceGovernorWakeup

     --
     -- Churn Trace
     --

     | TraceChurnWait          DiffTime
     | TraceChurnMode          ChurnMode
     | TraceChurnAction  ChurnAction
     | TraceChurnTimeout ChurnAction

     | TraceLedgerStateJudgementChanged LedgerStateJudgement
     | TraceOnlyBootstrapPeers
     | TraceBootstrapPeersFlagChangedWhilstInSensitiveState
     | TraceUseBootstrapPeersChanged UseBootstrapPeers

     --
     -- Critical Failures
     --
     | TraceOutboundGovernorCriticalFailure SomeException

     --
     -- Debug Tracer
     --

     | TraceDebugState Time (DebugPeerSelectionState peeraddr)
  deriving Show


data ChurnAction = DecreasedActivePeers
                 | IncreasedActivePeers
                 | DecreasedActiveBigLedgerPeers
                 | IncreasedActiveBigLedgerPeers
                 | DecreasedEstablishedPeers
                 | IncreasedEstablishedPeers
                 | IncreasedEstablishedBigLedgerPeers
                 | DecreasedEstablishedBigLedgerPeers
                 | DecreasedKnownPeers
                 | IncreasedKnownPeers
                 | DecreasedKnownBigLedgerPeers
                 | IncreasedKnownBigLedgerPeers
  deriving (Eq, Show)


data BootstrapPeersCriticalTimeoutError =
  BootstrapPeersCriticalTimeoutError
  deriving (Eq, Show)

instance Exception BootstrapPeersCriticalTimeoutError where
   displayException BootstrapPeersCriticalTimeoutError =
     "The peer selection did not converged to a clean state in 15 minutes. Something is wrong!"

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

