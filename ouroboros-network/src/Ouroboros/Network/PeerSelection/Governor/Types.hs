{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE ViewPatterns              #-}

#if __GLASGOW_HASKELL__ < 904
-- Pattern synonym record fields with GHC-8.10 is issuing the `-Wname-shadowing`
-- warning.
{-# OPTIONS_GHC -Wno-name-shadowing #-}
#endif

module Ouroboros.Network.PeerSelection.Governor.Types
  ( -- * P2P governor policies
    PeerSelectionPolicy (..)
  , PeerSelectionTargets (..)
  , nullPeerSelectionTargets
  , sanePeerSelectionTargets
  , PickPolicy

    -- ** Cardano Node specific functions
  , pickPeers
  , pickUnknownPeers

    -- * P2P governor low level API
    -- These records are needed to run the peer selection.
  , PeerStateActions (..)
  , PeerSelectionActions (..)
  , PeerSelectionInterfaces (..)
  , MonitoringAction
  , ExtraGuardedDecisions (..)
  , PeerSelectionGovernorArgs (..)
    -- * P2P governor internals
  , PeerSelectionState (..)
  , emptyPeerSelectionState
  , AssociationMode (..)
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
  , PeerSelectionView
      ( ..,
        PeerSelectionCounters,
        numberOfRootPeers,

        numberOfKnownPeers,
        numberOfAvailableToConnectPeers,
        numberOfColdPeersPromotions,
        numberOfEstablishedPeers,
        numberOfWarmPeersDemotions,
        numberOfWarmPeersPromotions,
        numberOfActivePeers,
        numberOfActivePeersDemotions,

        numberOfKnownBigLedgerPeers,
        numberOfAvailableToConnectBigLedgerPeers,
        numberOfColdBigLedgerPeersPromotions,
        numberOfEstablishedBigLedgerPeers,
        numberOfWarmBigLedgerPeersDemotions,
        numberOfWarmBigLedgerPeersPromotions,
        numberOfActiveBigLedgerPeers,
        numberOfActiveBigLedgerPeersDemotions,

        numberOfKnownLocalRootPeers,
        numberOfAvailableToConnectLocalRootPeers,
        numberOfColdLocalRootPeersPromotions,
        numberOfEstablishedLocalRootPeers,
        numberOfWarmLocalRootPeersPromotions,
        numberOfActiveLocalRootPeers,
        numberOfActiveLocalRootPeersDemotions,

        numberOfKnownNonRootPeers,
        numberOfColdNonRootPeersPromotions,
        numberOfEstablishedNonRootPeers,
        numberOfWarmNonRootPeersDemotions,
        numberOfWarmNonRootPeersPromotions,
        numberOfActiveNonRootPeers,
        numberOfActiveNonRootPeersDemotions,

        extraCounters,

        PeerSelectionCountersHWC,
        numberOfColdPeers,
        numberOfWarmPeers,
        numberOfHotPeers,

        numberOfColdBigLedgerPeers,
        numberOfWarmBigLedgerPeers,
        numberOfHotBigLedgerPeers,

        numberOfColdLocalRootPeers,
        numberOfWarmLocalRootPeers,
        numberOfHotLocalRootPeers
      )
  , PeerSelectionCounters
  , PeerSelectionSetsWithSizes
  , emptyPeerSelectionCounters

    -- ** Cardano Node specific functions
  , peerSelectionStateToCounters
  , peerSelectionStateToView

    -- * Peer Sharing Auxiliary data type
  , PeerSharingResult (..)
    -- * Traces
  , TracePeerSelection (..)
  , ChurnAction (..)
  , DebugPeerSelection (..)
    -- * Error types
  , BootstrapPeersCriticalTimeoutError (..)
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Monoid.Synchronisation (FirstToFinish (..))
import Data.OrdPSQ qualified as PSQ
import Data.Semigroup (Min (..))
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Stack (HasCallStack)

import Control.Applicative (Alternative)
import Control.Concurrent.JobPool (Job)
import Control.Exception (Exception (..), SomeException, assert)
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadTime.SI
import System.Random (StdGen)

import Control.Concurrent.Class.MonadSTM.Strict
import Ouroboros.Network.ExitPolicy
import Cardano.Network.PeerSelection.Bootstrap (UseBootstrapPeers (..))
import Ouroboros.Network.PeerSelection.LedgerPeers.Type
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing)
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
           PeerStatus (PeerHot, PeerWarm), PublicExtraPeersActions)
import Ouroboros.Network.Protocol.PeerSharing.Type (PeerSharingAmount,
           PeerSharingResult (..))
import Cardano.Network.Types (LedgerStateJudgement (..))
import Cardano.Network.PeerSelection.Types (ChurnMode)
import Ouroboros.Network.PeerSelection.RelayAccessPoint (RelayAccessPoint)

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
       policyPickInboundPeers           :: PickPolicy peeraddr (STM m),

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

       -- | The target number of all known peers.  Doesn't include big ledger peers
       -- |
       targetNumberOfKnownPeers                :: !Int,
       -- | The target number of established peers (does not include big ledger
       -- peers).
       --
       -- The target includes root peers, local root peers, and ledger peers
       --
       targetNumberOfEstablishedPeers          :: !Int,
       -- | The target number of active peers (does not include big ledger
       -- peers).
       --
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


-- These being pluggable allows:
--
-- * choice of known peer root sets
-- * running both in simulation and for real
--
data PeerSelectionActions extraState extraActions extraPeers extraFlags extraAPI extraCounters peeraddr peerconn m =
  PeerSelectionActions {
       -- | These are the original targets as seen in the static configuration
       --
       originalPeerSelectionTargets :: PeerSelectionTargets,

       -- | Read current Peer Selection Targets these can be changed by Churn
       -- Governor
       --
       readPeerSelectionTargets   :: STM m PeerSelectionTargets,

       -- | Read the original set of locally or privately known root peers.
       --
       -- This should come from 'ArgumentsExtra' when initializing Diffusion
       --
       readOriginalLocalRootPeers :: STM m (LocalRootPeers.Config extraFlags RelayAccessPoint),

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
       readLocalRootPeers     :: STM m (LocalRootPeers.Config extraFlags peeraddr),

       -- | Read inbound peers which negotiated duplex connection.
       --
       readInboundPeers       :: m (Map peeraddr PeerSharing),

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

       -- | Public Extra Peers Actions
       --
       extraPeersActions :: PublicExtraPeersActions extraPeers peeraddr,

       -- | Compute extraCounters from PeerSelectionState
       extraStateToExtraCounters
         :: PeerSelectionState extraState extraFlags extraPeers peeraddr peerconn
         -> extraCounters,

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
       requestPublicRootPeers   :: LedgerPeersKind -> Int -> m (PublicRootPeers extraPeers peeraddr, DiffTime),

       -- | The action to contact a known peer and request a sample of its
       -- known peers.
       --
       requestPeerShare       :: PeerSharingAmount -> peeraddr -> m (PeerSharingResult peeraddr),

       -- | Core actions run by the governor to change 'PeerStatus'.
       --
       peerStateActions       :: PeerStateActions peeraddr peerconn m,

       -- | Read the current ledger state
       --
       getLedgerStateCtx :: LedgerPeersConsensusInterface extraAPI m,

       -- | Read the current state of ledger peer snapshot
       --
       readLedgerPeerSnapshot :: STM m (Maybe LedgerPeerSnapshot),

       -- | Extension point so that third party users can add more actions
       --
       extraActions :: extraActions

     }

-- | Interfaces required by the peer selection governor, which do not need to
-- be shared with actions and thus are not part of `PeerSelectionActions`.
--
data PeerSelectionInterfaces extraState extraFlags extraPeers extraCounters peeraddr peerconn m =
  PeerSelectionInterfaces {
      -- | PeerSelectionCounters are shared with churn through a `StrictTVar`.
      --
      countersVar        :: StrictTVar m (PeerSelectionCounters extraCounters),

      -- | PublicPeerSelectionState var.
      --
      publicStateVar     :: StrictTVar m (PublicPeerSelectionState peeraddr),

      -- | PeerSelectionState shared for debugging purposes (to support SIGUSR1
      -- debug event tracing)
      --
      debugStateVar      :: StrictTVar m (PeerSelectionState extraState extraFlags extraPeers peeraddr peerconn),

      -- | `UseLedgerPeers` used by `peerSelectionGovernor` to support
      -- `HiddenRelayOrBP`
      --
      readUseLedgerPeers :: STM m UseLedgerPeers
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
-- Extra Guarded Decisions
--
type MonitoringAction extraState extraActions extraPeers extraAPI extraFlags extraCounters peeraddr peerconn m =
    PeerSelectionPolicy peeraddr m
  -> PeerSelectionActions extraState extraActions extraPeers extraFlags extraAPI extraCounters peeraddr peerconn m
  -> PeerSelectionState extraState extraFlags extraPeers peeraddr peerconn
  -> Guarded (STM m) (TimedDecision m extraState extraFlags extraPeers peeraddr peerconn)

type MonitoringActions extraState extraActions extraPeers extraAPI extraFlags extraCounters peeraddr peerconn m =
  [ MonitoringAction extraState extraActions extraPeers extraAPI extraFlags extraCounters peeraddr peerconn m ]

data ExtraGuardedDecisions extraState extraActions extraPeers extraAPI extraFlags extraCounters peeraddr peerconn m =
  ExtraGuardedDecisions {

    -- | This list of guarded decisions will come before all default possibly
    -- blocking -- decisions. The order matters, making the first decisions
    -- have priority over the later ones.
    --
    -- Note that these actions should be blocking.
    preBlocking      :: MonitoringActions extraState extraActions extraPeers extraAPI extraFlags extraCounters peeraddr peerconn m

    -- | This list of guarded decisions will come after all possibly preBlocking
    -- and default blocking decisions. The order matters, making the first
    -- decisions have priority over the later ones.
    --
    -- Note that these actions should be blocking.
  , postBlocking     :: MonitoringActions extraState extraActions extraPeers extraAPI extraFlags extraCounters peeraddr peerconn m

    -- | This list of guarded decisions will come before all default non-blocking
    -- decisions. The order matters, making the first decisions have priority over
    -- the later ones.
    --
    -- Note that these actions should not be blocking.
  , preNonBlocking   :: MonitoringActions extraState extraActions extraPeers extraAPI extraFlags extraCounters peeraddr peerconn m

    -- | This list of guarded decisions will come before all preNonBlocking and
    -- default non-blocking decisions. The order matters, making the first
    -- decisions have priority over the later ones.
    --
    -- Note that these actions should not be blocking.
  , postNonBlocking  :: MonitoringActions extraState extraActions extraPeers extraAPI extraFlags extraCounters peeraddr peerconn m

    -- | This action is necessary to the well functioning of the Outbound
    -- Governor. In particular this action should monitor 'PeerSelectionTargets',
    -- if they change, update 'PeerSelectionState' accordingly.
    --
    -- Customization of this monitoring action is allowed since a 3rd party
    -- user might require more granular control over the targets of its
    -- Outbound Governor.
    --
    -- If no custom action is required just use the default provided by
    -- 'Ouroboros.Network.PeerSelection.Governor.Monitor.targetPeers'
  , requiredTargetsAction :: MonitoringAction extraState extraActions extraPeers extraAPI extraFlags extraCounters peeraddr peerconn m

    -- | This action is necessary to the well functioning of the Outbound
    -- Governor. In particular this action should monitor Monitor local roots
    -- using 'readLocalRootPeers' 'STM' action.
    --
    -- Customization of this monitoring action is allowed since a 3rd party
    -- user might require more granular control over the local roots of its
    -- Outbound Governor, according to 'extraFlags' for example.
    --
    -- If no custom action is required just use the default provided by
    -- 'Ouroboros.Network.PeerSelection.Governor.Monitor.localRoots'
  , requiredLocalRootsAction :: MonitoringAction extraState extraActions extraPeers extraAPI extraFlags extraCounters peeraddr peerconn m

    -- | This enables third party users to add extra guards to the following monitoring
    -- actions that make progress towards targets:
    --
    -- * BigLedgerPeers.belowTarget
    -- * KnownPeers.belowTarget
    -- * EstablishedPeers.belowTargetBigLedgerPeers
    -- * ActivePeers.belowTargetBigLedgerPeers
    --
    -- This might be useful if the user requires its diffusion layer to stop
    -- making progress during a sensitive/vulnerable situation and quarantine
    -- it and make sure it is only connected to trusted peers.
    --
  , enableProgressMakingActions :: extraState -> Bool

    -- | This can safely be left as 'id'. This parameter is an artifact of the
    -- process of making the diffusion layer reusable. This function allows a
    -- constant 'extraState' change after a successfull ledger peer snapshot
    -- change.
    --
    -- TODO: Come up with a better solution
  , ledgerPeerSnapshotExtraStateChange :: extraState -> extraState
  }

-----------------------
-- Peer Selection Arguments
--

data PeerSelectionGovernorArgs extraState extraActions extraPeers extraAPI extraFlags extraCounters peeraddr peerconn exception m =
  PeerSelectionGovernorArgs {
    abortGovernor :: PeerSelectionState extraState extraFlags extraPeers peeraddr peerconn
                  -> Maybe exception
  , updateWithState :: PeerSelectionSetsWithSizes extraCounters peeraddr
                    -> PeerSelectionState extraState extraFlags extraPeers peeraddr peerconn
                    -> STM m ()
  , extraDecisions :: ExtraGuardedDecisions extraState extraActions extraPeers extraAPI extraFlags extraCounters peeraddr peerconn m
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
data PeerSelectionState extraState extraFlags extraPeers peeraddr peerconn = PeerSelectionState {

       targets                     :: !PeerSelectionTargets,

       -- | The current set of local root peers. This is structured as a
       -- bunch of groups, with a target for each group. This gives us a set of
       -- n-of-m choices, e.g. \"pick 2 from this group and 1 from this group\".
       --
       -- The targets must of course be achievable, and to keep things simple,
       -- the groups must be disjoint.
       --
       localRootPeers              :: !(LocalRootPeers extraFlags peeraddr),

       -- | This set holds the public root peers (i.e. Ledger (small and big),
       -- Bootstrap peers and locally configured public root peers).
       --
       publicRootPeers             :: !(PublicRootPeers extraPeers peeraddr),

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

       -- | Rng for fuzzy delays and random choices.
       --
       stdGen                      :: !StdGen,

       -- | Time to query of inbound peers time.
       --
       inboundPeersRetryTime       :: !Time,

       -- | Internal state of ledger peer snapshot
       --
       ledgerPeerSnapshot          :: Maybe LedgerPeerSnapshot,

       -- | Extension point so that 3rd party users can plug their own peer
       -- selection state if needed
       extraState                  :: extraState

--     TODO: need something like this to distinguish between lots of bad peers
--     and us getting disconnected from the network locally. We don't want a
--     network disconnect to cause us to flush our full known peer set by
--     considering them all to have bad connectivity.
--     Should also take account of DNS failures for root peer set.
--     lastSuccessfulNetworkEvent :: Time
     }
  deriving Show

-- | A node is classified as `LocalRootsOnly` if it is a hidden relay or
-- a BP, e.g. if it is configured such that it can only have a chance to be
-- connected to local roots. This is true if the node is configured in one of
-- two ways:
--
-- * `DontUseBootstrapPeers`, `DontUseLedgerPeers` and
--   `PeerSharingDisabled`; or
-- * `UseBootstrapPeers`, `DontUseLedgerPeers` and
--   `PeerSharingDisabled`, but it's not using any bootstrap peers (i.e. it is
--   synced).
--
-- Note that in the second case a node might transition between `LocalRootsOnly`
-- and `Unrestricted` modes, depending on `LedgerStateJudgement`.
--
-- See `Ouroboros.Network.PeerSelection.Governor.readAssociationMode`.
--
data AssociationMode =
     LocalRootsOnly
   | Unrestricted
  deriving Show

-----------------------
-- Debug copy of Peer Selection State
--
-- Used for dumping the peer selection state upon getting a USR1 signal.
--
data DebugPeerSelectionState extraState extraFlags extraPeers peeraddr = DebugPeerSelectionState {
       dpssTargets                     :: !PeerSelectionTargets,
       dpssLocalRootPeers              :: !(LocalRootPeers extraFlags peeraddr),
       dpssPublicRootPeers             :: !(PublicRootPeers extraPeers peeraddr),
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
       dpssFetchynessBlocks            :: !(Map peeraddr Int),
       dpssAssociationMode             :: !AssociationMode,
       dpssExtraState                  :: !extraState
} deriving Show

makeDebugPeerSelectionState :: PeerSelectionState extraState extraFlags extraPeers peeraddr peerconn
                            -> Map peeraddr Int
                            -> Map peeraddr Int
                            -> extraDebugState
                            -> AssociationMode
                            -> DebugPeerSelectionState extraDebugState extraFlags extraPeers peeraddr
makeDebugPeerSelectionState PeerSelectionState {..} up bp es am =
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
    , dpssAssociationMode             = am
    , dpssExtraState                  = es
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
toPublicState :: PeerSelectionState extraState extraFlags extraPeers peeraddr peerconn
              -> PublicPeerSelectionState peeraddr
toPublicState PeerSelectionState { knownPeers } =
   PublicPeerSelectionState {
     availableToShare =
       KnownPeers.getPeerSharingResponsePeers knownPeers
   }

-- | Peer selection view.
--
-- This is a functor which is used to hold computation of various peer sets and
-- their sizes.  See `peerSelectionStateToView`, `peerSelectionStateToCounters`.
--
data PeerSelectionView extraViews a = PeerSelectionView {
      viewRootPeers                        :: a,

      --
      -- Non Big Ledger Peers
      --

      viewKnownPeers                       :: a,
      -- ^ number of known peers excluding big ledger peers
      viewAvailableToConnectPeers          :: a,
      -- ^ number of known peers available to connect
      viewColdPeersPromotions              :: a,
      -- ^ number of known peers (excluding big ledger peers) being promoted to
      -- warm
      viewEstablishedPeers                 :: a,
      -- ^ number of established peers excluding big ledger peers
      viewWarmPeersDemotions               :: a,
      -- ^ number of warm peers (excluding big ledger peers) being demoted to
      -- cold
      viewWarmPeersPromotions              :: a,
      -- ^ number of warm peers (excluding big ledger peers) being promote to
      -- hot
      viewActivePeers                      :: a,
      -- ^ number of active peers excluding big ledger peers
      viewActivePeersDemotions             :: a,
      -- ^ number of active peers (excluding big ledger peers) being demoted to
      -- warm

      --
      -- Big Ledger Peers
      --

      viewKnownBigLedgerPeers              :: a,
      -- ^ number of known big ledger peers
      viewAvailableToConnectBigLedgerPeers :: a,
      -- ^ number of known big ledger peers available to connect
      viewColdBigLedgerPeersPromotions     :: a,
      -- ^ number of cold big ledger peers being promoted to warm
      viewEstablishedBigLedgerPeers        :: a,
      -- ^ number of established big ledger peers
      viewWarmBigLedgerPeersDemotions      :: a,
      -- ^ number of warm big ledger peers being demoted to cold
      viewWarmBigLedgerPeersPromotions     :: a,
      -- ^ number of warm big ledger peers being promote to hot
      viewActiveBigLedgerPeers             :: a,
      -- ^ number of active big ledger peers
      viewActiveBigLedgerPeersDemotions    :: a,
      -- ^ number of active big ledger peers being demoted to warm

      --
      -- Local Roots
      --

      viewKnownLocalRootPeers              :: a,
      -- ^ number of known local root peers should always be equal to the sum
      -- of established & active local roots.
      viewAvailableToConnectLocalRootPeers :: a,
      viewColdLocalRootPeersPromotions     :: a,
      viewEstablishedLocalRootPeers        :: a,
      viewWarmLocalRootPeersPromotions     :: a,
      viewActiveLocalRootPeers             :: a,
      viewActiveLocalRootPeersDemotions    :: a,

      --
      -- Non-Root Peers
      --

      viewKnownNonRootPeers                 :: a,
      -- ^ number of known non root peers.  These are mostly peers received
      -- through peer sharing (or light peer sharing); but also will contains
      -- peers which used to be local roots after a reconfiguration.
      viewColdNonRootPeersPromotions        :: a,
      viewEstablishedNonRootPeers           :: a,
      viewWarmNonRootPeersDemotions         :: a,
      viewWarmNonRootPeersPromotions        :: a,
      viewActiveNonRootPeers                :: a,
      viewActiveNonRootPeersDemotions       :: a,

      viewExtraViews :: extraViews
    } deriving (Eq, Functor, Show)


type PeerSelectionCounters extraCounters = PeerSelectionView extraCounters Int
pattern PeerSelectionCounters
          :: Int
          -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
          -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
          -> Int -> Int -> Int -> Int -> Int -> Int -> Int
          -> Int -> Int -> Int -> Int -> Int -> Int -> Int
          -> extraCounters
          -> PeerSelectionCounters extraCounters
pattern PeerSelectionCounters {
      numberOfRootPeers,

      numberOfKnownPeers,
      numberOfAvailableToConnectPeers,
      numberOfColdPeersPromotions,
      numberOfEstablishedPeers,
      numberOfWarmPeersDemotions,
      numberOfWarmPeersPromotions,
      numberOfActivePeers,
      numberOfActivePeersDemotions,

      numberOfKnownBigLedgerPeers,
      numberOfAvailableToConnectBigLedgerPeers,
      numberOfColdBigLedgerPeersPromotions,
      numberOfEstablishedBigLedgerPeers,
      numberOfWarmBigLedgerPeersDemotions,
      numberOfWarmBigLedgerPeersPromotions,
      numberOfActiveBigLedgerPeers,
      numberOfActiveBigLedgerPeersDemotions,

      numberOfKnownLocalRootPeers,
      numberOfAvailableToConnectLocalRootPeers,
      numberOfColdLocalRootPeersPromotions,
      numberOfEstablishedLocalRootPeers,
      numberOfWarmLocalRootPeersPromotions,
      numberOfActiveLocalRootPeers,
      numberOfActiveLocalRootPeersDemotions,

      numberOfKnownNonRootPeers,
      numberOfColdNonRootPeersPromotions,
      numberOfEstablishedNonRootPeers,
      numberOfWarmNonRootPeersDemotions,
      numberOfWarmNonRootPeersPromotions,
      numberOfActiveNonRootPeers,
      numberOfActiveNonRootPeersDemotions,

      extraCounters
    }
  =
  PeerSelectionView {
      viewRootPeers                        = numberOfRootPeers,

      viewKnownPeers                       = numberOfKnownPeers,
      viewAvailableToConnectPeers          = numberOfAvailableToConnectPeers,
      viewColdPeersPromotions              = numberOfColdPeersPromotions,
      viewEstablishedPeers                 = numberOfEstablishedPeers,
      viewWarmPeersDemotions               = numberOfWarmPeersDemotions,
      viewWarmPeersPromotions              = numberOfWarmPeersPromotions,
      viewActivePeers                      = numberOfActivePeers,
      viewActivePeersDemotions             = numberOfActivePeersDemotions,

      viewKnownBigLedgerPeers              = numberOfKnownBigLedgerPeers,
      viewAvailableToConnectBigLedgerPeers = numberOfAvailableToConnectBigLedgerPeers,
      viewColdBigLedgerPeersPromotions     = numberOfColdBigLedgerPeersPromotions,
      viewEstablishedBigLedgerPeers        = numberOfEstablishedBigLedgerPeers,
      viewWarmBigLedgerPeersDemotions      = numberOfWarmBigLedgerPeersDemotions,
      viewWarmBigLedgerPeersPromotions     = numberOfWarmBigLedgerPeersPromotions,
      viewActiveBigLedgerPeers             = numberOfActiveBigLedgerPeers,
      viewActiveBigLedgerPeersDemotions    = numberOfActiveBigLedgerPeersDemotions,

      viewKnownLocalRootPeers              = numberOfKnownLocalRootPeers,
      viewAvailableToConnectLocalRootPeers = numberOfAvailableToConnectLocalRootPeers,
      viewColdLocalRootPeersPromotions     = numberOfColdLocalRootPeersPromotions,
      viewEstablishedLocalRootPeers        = numberOfEstablishedLocalRootPeers,
      viewWarmLocalRootPeersPromotions     = numberOfWarmLocalRootPeersPromotions,
      viewActiveLocalRootPeers             = numberOfActiveLocalRootPeers,
      viewActiveLocalRootPeersDemotions    = numberOfActiveLocalRootPeersDemotions,

      viewKnownNonRootPeers                 = numberOfKnownNonRootPeers,
      viewColdNonRootPeersPromotions        = numberOfColdNonRootPeersPromotions,
      viewEstablishedNonRootPeers           = numberOfEstablishedNonRootPeers,
      viewWarmNonRootPeersDemotions         = numberOfWarmNonRootPeersDemotions,
      viewWarmNonRootPeersPromotions        = numberOfWarmNonRootPeersPromotions,
      viewActiveNonRootPeers                = numberOfActiveNonRootPeers,
      viewActiveNonRootPeersDemotions       = numberOfActiveNonRootPeersDemotions,

      viewExtraViews = extraCounters
    }

{-# COMPLETE PeerSelectionCounters #-}

type PeerSelectionSetsWithSizes extraViews peeraddr = PeerSelectionView extraViews (Set peeraddr, Int)

-- | A Pattern synonym which computes `hot`, `warm`, `cold` counters from
-- `PeerSelectionCounters`.
--
pattern PeerSelectionCountersHWC :: Int -> Int -> Int -- peers
                                 -> Int -> Int -> Int -- big ledger peers
                                 -> Int -> Int -> Int -- local roots
                                 -> PeerSelectionCounters extraCounters
pattern PeerSelectionCountersHWC { numberOfColdPeers,
                                   numberOfWarmPeers,
                                   numberOfHotPeers,

                                   numberOfColdBigLedgerPeers,
                                   numberOfWarmBigLedgerPeers,
                                   numberOfHotBigLedgerPeers,

                                   numberOfColdLocalRootPeers,
                                   numberOfWarmLocalRootPeers,
                                   numberOfHotLocalRootPeers }

        <- (peerSelectionCountersHWC ->
             PeerSelectionView { viewKnownPeers                = numberOfColdPeers,
                                 viewEstablishedPeers          = numberOfWarmPeers,
                                 viewActivePeers               = numberOfHotPeers,

                                 viewKnownBigLedgerPeers       = numberOfColdBigLedgerPeers,
                                 viewEstablishedBigLedgerPeers = numberOfWarmBigLedgerPeers,
                                 viewActiveBigLedgerPeers      = numberOfHotBigLedgerPeers,

                                 viewKnownLocalRootPeers       = numberOfColdLocalRootPeers,
                                 viewEstablishedLocalRootPeers = numberOfWarmLocalRootPeers,
                                 viewActiveLocalRootPeers      = numberOfHotLocalRootPeers
                               })

{-# COMPLETE PeerSelectionCountersHWC #-}


-- | Internal function; used to implement `PeerSelectionCountersHWC` pattern synonym.
--
peerSelectionCountersHWC :: PeerSelectionCounters extraCounters -> PeerSelectionCounters extraCounters
peerSelectionCountersHWC PeerSelectionCounters {..} =
    PeerSelectionCounters {
      numberOfRootPeers,

      numberOfKnownPeers                         = numberOfKnownPeers
                                                 - numberOfEstablishedPeers,
      numberOfAvailableToConnectPeers,
      numberOfColdPeersPromotions,
      numberOfEstablishedPeers                   = numberOfEstablishedPeers
                                                 - numberOfActivePeers,
      numberOfWarmPeersDemotions,
      numberOfWarmPeersPromotions,
      numberOfActivePeers,
      numberOfActivePeersDemotions,

      numberOfKnownBigLedgerPeers                = numberOfKnownBigLedgerPeers
                                                 - numberOfEstablishedBigLedgerPeers,
      numberOfAvailableToConnectBigLedgerPeers,
      numberOfColdBigLedgerPeersPromotions,
      numberOfEstablishedBigLedgerPeers          = numberOfEstablishedBigLedgerPeers
                                                 - numberOfActiveBigLedgerPeers,
      numberOfWarmBigLedgerPeersDemotions,
      numberOfWarmBigLedgerPeersPromotions,
      numberOfActiveBigLedgerPeers,
      numberOfActiveBigLedgerPeersDemotions,

      numberOfKnownLocalRootPeers                = numberOfKnownLocalRootPeers
                                                 - numberOfEstablishedLocalRootPeers,
      numberOfAvailableToConnectLocalRootPeers,
      numberOfColdLocalRootPeersPromotions,
      numberOfEstablishedLocalRootPeers          = numberOfEstablishedLocalRootPeers
                                                 - numberOfActiveLocalRootPeers,
      numberOfWarmLocalRootPeersPromotions,
      numberOfActiveLocalRootPeers,
      numberOfActiveLocalRootPeersDemotions,

      numberOfKnownNonRootPeers                   = numberOfKnownNonRootPeers
                                                 - numberOfEstablishedNonRootPeers,
      numberOfColdNonRootPeersPromotions,
      numberOfEstablishedNonRootPeers             = numberOfEstablishedNonRootPeers
                                                 - numberOfActiveNonRootPeers,
      numberOfWarmNonRootPeersDemotions,
      numberOfWarmNonRootPeersPromotions,
      numberOfActiveNonRootPeers,
      numberOfActiveNonRootPeersDemotions,

      extraCounters
    }

emptyPeerSelectionState :: StdGen
                        -> extraState
                        -> extraPeers
                        -> PeerSelectionState extraState extraFlags extraPeers peeraddr peerconn
emptyPeerSelectionState rng es ep =
    PeerSelectionState {
      targets                     = nullPeerSelectionTargets,
      localRootPeers              = LocalRootPeers.empty,
      publicRootPeers             = PublicRootPeers.empty ep,
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
      stdGen                      = rng,
      inboundPeersRetryTime       = Time 0,
      ledgerPeerSnapshot          = Nothing,
      extraState                  = es
    }

emptyPeerSelectionCounters :: extraCounters -> PeerSelectionCounters extraCounters
emptyPeerSelectionCounters emptyEC =
  PeerSelectionCounters {
    numberOfRootPeers                        = 0,

    numberOfKnownPeers                       = 0,
    numberOfAvailableToConnectPeers          = 0,
    numberOfColdPeersPromotions              = 0,
    numberOfEstablishedPeers                 = 0,
    numberOfWarmPeersDemotions               = 0,
    numberOfWarmPeersPromotions              = 0,
    numberOfActivePeers                      = 0,
    numberOfActivePeersDemotions             = 0,

    numberOfKnownBigLedgerPeers              = 0,
    numberOfAvailableToConnectBigLedgerPeers = 0,
    numberOfColdBigLedgerPeersPromotions     = 0,
    numberOfEstablishedBigLedgerPeers        = 0,
    numberOfWarmBigLedgerPeersDemotions      = 0,
    numberOfWarmBigLedgerPeersPromotions     = 0,
    numberOfActiveBigLedgerPeers             = 0,
    numberOfActiveBigLedgerPeersDemotions    = 0,

    numberOfKnownLocalRootPeers              = 0,
    numberOfAvailableToConnectLocalRootPeers = 0,
    numberOfColdLocalRootPeersPromotions     = 0,
    numberOfEstablishedLocalRootPeers        = 0,
    numberOfWarmLocalRootPeersPromotions     = 0,
    numberOfActiveLocalRootPeers             = 0,
    numberOfActiveLocalRootPeersDemotions    = 0,

    numberOfKnownNonRootPeers                 = 0,
    numberOfColdNonRootPeersPromotions        = 0,
    numberOfEstablishedNonRootPeers           = 0,
    numberOfWarmNonRootPeersDemotions         = 0,
    numberOfWarmNonRootPeersPromotions        = 0,
    numberOfActiveNonRootPeers                = 0,
    numberOfActiveNonRootPeersDemotions       = 0,

    extraCounters = emptyEC
  }

-- | A view of the status of each established peer, for testing and debugging.
--
establishedPeersStatus :: Ord peeraddr
                       => PeerSelectionState extraState extraFlags extraPeers peeraddr peerconn
                       -> Map peeraddr PeerStatus
establishedPeersStatus PeerSelectionState{establishedPeers, activePeers} =
    -- map union-override, left to right
    Map.fromSet (\_ -> PeerHot)  activePeers
 <> Map.fromSet (\_ -> PeerWarm) (EstablishedPeers.toSet establishedPeers)

-----------------------------------------------
-- Cardano Node specific PeerSelection functions
--
-- | Compute peer selection sets & their sizes.
--
-- This function is used internally by the outbound-governor and to compute
-- `PeerSelectionCounters` which are used by churn or are traced (e.g. as EKG
-- metrics).  For this reason one has to be very careful when changing the
-- function, as it will affect the outbound governor behaviour.
--
peerSelectionStateToView
  :: Ord peeraddr
  => (extraPeers -> Set peeraddr)
  -> (PeerSelectionState extraState extraFlags extraPeers peeraddr peerconn -> extraViews)
  -> PeerSelectionState extraState extraFlags extraPeers peeraddr peerconn
  -> PeerSelectionSetsWithSizes extraViews peeraddr
peerSelectionStateToView
    extraPeersToSet
    extraStateToExtraViews
    st@PeerSelectionState {
        knownPeers,
        establishedPeers,
        activePeers,
        publicRootPeers,
        localRootPeers,
        inProgressPromoteCold,
        inProgressPromoteWarm,
        inProgressDemoteWarm,
        inProgressDemoteHot
      }
    =
    PeerSelectionView {
      viewRootPeers                          = size rootPeersSet,

      viewKnownPeers                         = size   knownPeersSet,
      viewAvailableToConnectPeers            = size $ availableToConnectSet
                                                      Set.\\ bigLedgerSet,
      viewColdPeersPromotions                = size $ inProgressPromoteCold
                                                      Set.\\ bigLedgerSet,
      viewEstablishedPeers                   = size   establishedPeersSet,
      viewWarmPeersDemotions                 = size $ inProgressDemoteWarm
                                                      Set.\\ bigLedgerSet,
      viewWarmPeersPromotions                = size $ inProgressPromoteWarm
                                                      Set.\\ bigLedgerSet,
      viewActivePeers                        = size $ activePeersSet,
      viewActivePeersDemotions               = size $ activePeersSet
                                                      `Set.intersection` inProgressDemoteHot,

      viewKnownBigLedgerPeers                = size   bigLedgerSet,
      viewAvailableToConnectBigLedgerPeers   = size $ availableToConnectSet
                                                      `Set.intersection` bigLedgerSet,
      viewColdBigLedgerPeersPromotions       = size $ bigLedgerSet
                                                      `Set.intersection` inProgressPromoteCold,
      viewEstablishedBigLedgerPeers          = size   establishedBigLedgerPeersSet,
      viewWarmBigLedgerPeersDemotions        = size $ inProgressDemoteWarm
                                                      `Set.intersection` establishedBigLedgerPeersSet,
      viewWarmBigLedgerPeersPromotions       = size $ inProgressPromoteWarm
                                                      `Set.intersection` establishedBigLedgerPeersSet,
      viewActiveBigLedgerPeers               = size   activeBigLedgerPeersSet,
      viewActiveBigLedgerPeersDemotions      = size $ activeBigLedgerPeersSet
                                                      `Set.intersection` inProgressDemoteHot,

      viewKnownLocalRootPeers                = size   knownLocalRootPeersSet,
      viewAvailableToConnectLocalRootPeers   = size $ availableToConnectSet
                                                      `Set.intersection` knownLocalRootPeersSet,
      viewColdLocalRootPeersPromotions       = size $ inProgressPromoteCold
                                                      `Set.intersection` knownLocalRootPeersSet,

      viewEstablishedLocalRootPeers          = size $ establishedLocalRootsPeersSet,
      viewWarmLocalRootPeersPromotions       = size $ establishedLocalRootsPeersSet
                                                      `Set.intersection` inProgressPromoteWarm,
      viewActiveLocalRootPeers               = size   activeLocalRootsPeersSet,
      viewActiveLocalRootPeersDemotions      = size $ activeLocalRootsPeersSet
                                                      `Set.intersection` inProgressDemoteHot,

      viewKnownNonRootPeers                   = size   knownNonRootPeersSet,
      viewColdNonRootPeersPromotions          = size $ knownNonRootPeersSet
                                                      `Set.intersection` inProgressPromoteCold,
      viewEstablishedNonRootPeers             = size   establishedNonRootPeersSet,
      viewWarmNonRootPeersDemotions           = size $ establishedNonRootPeersSet
                                                      `Set.intersection` inProgressDemoteWarm,
      viewWarmNonRootPeersPromotions          = size $ establishedNonRootPeersSet
                                                      `Set.intersection` inProgressPromoteWarm,
      viewActiveNonRootPeers                  = size   activeNonRootPeersSet,
      viewActiveNonRootPeersDemotions         = size $ activeNonRootPeersSet
                                                      `Set.intersection` inProgressDemoteHot,
      viewExtraViews = extraStateToExtraViews st
    }
  where
    size s = (s, Set.size s)

    -- common sets
    knownSet       = KnownPeers.toSet knownPeers
    establishedSet = EstablishedPeers.toSet establishedPeers
    bigLedgerSet   = PublicRootPeers.getBigLedgerPeers publicRootPeers
    availableToConnectSet = KnownPeers.availableToConnect knownPeers

    -- root peers
    rootPeersSet   = PublicRootPeers.toSet extraPeersToSet publicRootPeers
                  <> localRootSet

    -- non big ledger peers
    knownPeersSet       = knownSet Set.\\ bigLedgerSet
    establishedPeersSet = establishedSet Set.\\ establishedBigLedgerPeersSet
    activePeersSet      = activePeers Set.\\ activeBigLedgerPeersSet

    -- big ledger peers
    establishedBigLedgerPeersSet = establishedSet `Set.intersection` bigLedgerSet
    activeBigLedgerPeersSet      = establishedBigLedgerPeersSet `Set.intersection` activePeers

    -- local roots
    localRootSet                  = LocalRootPeers.keysSet localRootPeers
    -- local roots and big ledger peers are disjoint, hence we can use
    -- `knownPeersSet`, `establishedPeersSet` and `activePeersSet` below.
    knownLocalRootPeersSet        = localRootSet
    establishedLocalRootsPeersSet = establishedPeersSet `Set.intersection` localRootSet
    activeLocalRootsPeersSet      = activePeersSet `Set.intersection` localRootSet

    -- shared peers
    -- shared peers are not big ledger peers, hence we can use `knownPeersSet`,
    -- `establishedPeersSet` and `activePeersSet` below.
    knownNonRootPeersSet        = knownPeersSet Set.\\ rootPeersSet
    establishedNonRootPeersSet  = establishedPeersSet Set.\\ rootPeersSet
    activeNonRootPeersSet       = activePeersSet Set.\\ rootPeersSet


peerSelectionStateToCounters
  :: Ord peeraddr
  => (extraPeers -> Set peeraddr)
  -> (PeerSelectionState extraState extraFlags extraPeers peeraddr peerconn -> extraCounters)
  -> PeerSelectionState extraState extraFlags extraPeers peeraddr peerconn
  -> PeerSelectionCounters extraCounters
peerSelectionStateToCounters extraPeersToSet extraStateToExtraCounters =
    fmap snd
  . peerSelectionStateToView extraPeersToSet
                             extraStateToExtraCounters

assertPeerSelectionState :: Ord peeraddr
                         => (extraPeers -> Set peeraddr)
                         -> (extraPeers -> Bool)
                         -> PeerSelectionState extraState extraFlags extraPeers peeraddr peerconn
                         -> a -> a
assertPeerSelectionState extraPeersToSet invariantExtraPeers PeerSelectionState{..} =
    assert (KnownPeers.invariant knownPeers)
  . assert (EstablishedPeers.invariant establishedPeers)
  . assert (LocalRootPeers.invariant localRootPeers)
  . assert (PublicRootPeers.invariant invariantExtraPeers
                                      extraPeersToSet
                                      publicRootPeers)

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
    publicRootPeersSet  = PublicRootPeers.toSet extraPeersToSet publicRootPeers
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


--------------------------------
-- PickPolicy wrapper function
--

-- | Check pre-conditions and post-conditions on the pick policies,
-- and supply additional peer attributes from the current state.
--
pickPeers' :: (Ord peeraddr, Functor m, HasCallStack)
           => (Int -> Set peeraddr -> PeerSelectionState extraState extraFlags extraPeers peeraddr peerconn -> Bool)
           -- ^ precondition
           -> (peeraddr -> extraPeers -> Bool)
           -> PeerSelectionState extraState extraFlags extraPeers peeraddr peerconn
           -> PickPolicy peeraddr m
           -> Set peeraddr -> Int -> m (Set peeraddr)
pickPeers' precondition memberExtraPeers st@PeerSelectionState{localRootPeers, publicRootPeers, knownPeers}
          pick available num =
    assert (precondition num available st) $
    fmap (\picked -> assert (postcondition picked) picked)
         (pick peerSource peerConnectFailCount peerTepidFlag
               available numClamped)
  where
    postcondition picked = not (Set.null picked)
                        && Set.size picked <= numClamped
                        && picked `Set.isSubsetOf` available
    numClamped           = min num (Set.size available)

    peerSource p
      | PublicRootPeers.member
          p memberExtraPeers publicRootPeers   = PeerSourcePublicRoot
      | LocalRootPeers.member p localRootPeers = PeerSourceLocalRoot
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


-- | Pick some known peers.
--
pickPeers :: (Ord peeraddr, Functor m, HasCallStack)
          => (peeraddr -> extraPeers -> Bool)
          -> PeerSelectionState extraState extraFlags extraPeers peeraddr peerconn
          -> PickPolicy peeraddr m
          -> Set peeraddr -> Int -> m (Set peeraddr)
{-# INLINE pickPeers #-}
pickPeers memberExtraPeers =
  pickPeers' (\num available PeerSelectionState { knownPeers } ->
       not (Set.null available) && num > 0
    && available `Set.isSubsetOf` KnownPeers.toSet knownPeers)
             memberExtraPeers

-- | Pick some unknown peers.
--
pickUnknownPeers :: (Ord peeraddr, Functor m, HasCallStack)
                 => (peeraddr -> extraPeers -> Bool)
                 -> PeerSelectionState extraState extraFlags extraPeers peeraddr peerconn
                 -> PickPolicy peeraddr m
                 -> Set peeraddr -> Int -> m (Set peeraddr)
{-# INLINE pickUnknownPeers #-}
pickUnknownPeers memberExtraPeers =
  pickPeers' (\num available PeerSelectionState { knownPeers } ->
       not (Set.null available) && num > 0
    && not (available `Set.isSubsetOf` KnownPeers.toSet knownPeers))
             memberExtraPeers

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

instance Alternative m => Monoid (Guarded m a) where
  mempty = GuardedSkip' mempty
  mappend = (<>)


data Decision m extraState extraFlags extraPeers peeraddr peerconn = Decision {
         -- | A trace event to classify the decision and action
       decisionTrace :: [TracePeerSelection extraState extraFlags extraPeers peeraddr],

         -- | An updated state to use immediately
       decisionState :: PeerSelectionState extraState extraFlags extraPeers peeraddr peerconn,

       -- | An optional 'Job' to execute asynchronously. This job leads to
       -- a further 'Decision'. This gives a state update to apply upon
       -- completion, but also allows chaining further job actions.
       --
       decisionJobs  :: [Job () m (Completion m extraState extraFlags extraPeers peeraddr peerconn)]
     }

-- | Decision which has access to the current time, rather than the time when
-- the governor's loop blocked to make a decision.
--
type TimedDecision m extraState extraFlags extraPeers peeraddr peerconn =
  Time -> Decision m extraState extraFlags extraPeers peeraddr peerconn

-- | Type alias for function types which are used to create governor decisions.
-- Almost all decisions are following this pattern.
--
type MkGuardedDecision extraState extraFlags extraPeers peeraddr peerconn m
     = PeerSelectionPolicy peeraddr m
    -> PeerSelectionState extraState extraFlags extraPeers peeraddr peerconn
    -> Guarded (STM m) (TimedDecision m extraState extraFlags extraPeers peeraddr peerconn)


newtype Completion m extraState extraFlags extraPeers peeraddr peerconn =
        Completion (PeerSelectionState extraState extraFlags extraPeers peeraddr peerconn
                 -> Time -> Decision m extraState extraFlags extraPeers peeraddr peerconn)

data TracePeerSelection extraDebugState extraFlags extraPeers peeraddr =
       TraceLocalRootPeersChanged (LocalRootPeers extraFlags peeraddr)
                                  (LocalRootPeers extraFlags peeraddr)
     --
     -- Churn
     --

     -- | Peer selection targets changed: old targets, new targets.
     | TraceTargetsChanged     PeerSelectionTargets PeerSelectionTargets

     --
     -- Ledger Peers
     --

     | TracePublicRootsRequest Int Int
     | TracePublicRootsResults (PublicRootPeers extraPeers peeraddr) Int DiffTime
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
     -- | target known peers, actual known peers, selected inbound peers, available peers
     | TracePickInboundPeers Int Int (Map peeraddr PeerSharing) (Set peeraddr)

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
     | TraceChurnAction
         DiffTime    -- ^ duration of the churn action
         ChurnAction -- ^ churn action type
         Int         -- ^ how many peers were removed or added within the
                     --   duration of the action.
     | TraceChurnTimeout
         DiffTime    -- ^ duration of the churn action
         ChurnAction -- ^ churn action type
         Int         -- ^ how many peers were removed or added within the
                     --   duration of the action; note that if the action
                     --   timeouts the governor will still look to remove or
                     --   add peers as required.

     | TraceLedgerStateJudgementChanged LedgerStateJudgement
     | TraceOnlyBootstrapPeers
     | TraceBootstrapPeersFlagChangedWhilstInSensitiveState
     | TraceUseBootstrapPeersChanged UseBootstrapPeers
     | TraceVerifyPeerSnapshot Bool

     --
     -- Critical Failures
     --
     | TraceOutboundGovernorCriticalFailure SomeException

     --
     -- Debug Tracer
     --

     | TraceDebugState Time (DebugPeerSelectionState extraDebugState extraFlags extraPeers peeraddr)
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

data DebugPeerSelection extraState extraFlags extraPeers peeraddr where
  TraceGovernorState :: forall extraState extraFlags extraPeers peeraddr peerconn.
                        Show peerconn
                     => Time            -- blocked time
                     -> Maybe DiffTime  -- wait time
                     -> PeerSelectionState extraState extraFlags extraPeers peeraddr peerconn
                     -> DebugPeerSelection extraState extraFlags extraPeers peeraddr

deriving instance ( Show extraState
                  , Show extraFlags
                  , Show extraPeers
                  , Ord peeraddr
                  , Show peeraddr
                  ) => Show (DebugPeerSelection extraState extraFlags extraPeers peeraddr)
