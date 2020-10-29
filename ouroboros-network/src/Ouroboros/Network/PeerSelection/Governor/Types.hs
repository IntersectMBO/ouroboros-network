{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.PeerSelection.Governor.Types where

import           Data.Semigroup (Min(..))
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified Data.Set as Set
import           Data.Set (Set)

import           Control.Applicative (Alternative((<|>)))
import           Control.Concurrent.JobPool (Job)
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTime
import           Control.Exception (assert, SomeException)

import qualified Ouroboros.Network.PeerSelection.EstablishedPeers as EstablishedPeers
import           Ouroboros.Network.PeerSelection.EstablishedPeers (EstablishedPeers)
import qualified Ouroboros.Network.PeerSelection.KnownPeers as KnownPeers
import           Ouroboros.Network.PeerSelection.KnownPeers (KnownPeers, KnownPeerInfo(..))
import           Ouroboros.Network.PeerSelection.Types


-- | A peer pick policy is an action that picks a subset of elements from a
-- map of peers.
--
-- The pre-condition is that the map of available choices will be non-empty,
-- and the requested number to pick will be strictly positive.
--
-- The post-condition is that the picked set is non-empty but must not be
-- bigger than the requested number.
--
type PickPolicy peeraddr m = Map peeraddr KnownPeerInfo
                          -> Int
                          -> STM m (Set peeraddr)


-- | Check pre-conditions and post-conditions on the pick policies
pickPeers :: (Ord peeraddr, Functor m)
          => (Map peeraddr a -> Int -> m (Set peeraddr))
          ->  Map peeraddr a -> Int -> m (Set peeraddr)
pickPeers pick available num =
    assert precondition $
    fmap (\picked -> assert (postcondition picked) picked)
         (pick available numClamped)
  where
    precondition         = not (Map.null available) && num > 0
    postcondition picked = not (Set.null picked)
                        && Set.size picked <= numClamped
                        && picked `Set.isSubsetOf` Map.keysSet available
    numClamped           = min num (Map.size available)


data PeerSelectionPolicy peeraddr m = PeerSelectionPolicy {

       policyPickKnownPeersForGossip :: PickPolicy peeraddr m,
       policyPickColdPeersToPromote  :: PickPolicy peeraddr m,
       policyPickWarmPeersToPromote  :: PickPolicy peeraddr m,
       policyPickHotPeersToDemote    :: PickPolicy peeraddr m,
       policyPickWarmPeersToDemote   :: PickPolicy peeraddr m,
       policyPickColdPeersToForget   :: PickPolicy peeraddr m,

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
data PeerSelectionActions peeraddr peerconn m = PeerSelectionActions {

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
       requestPeerGossip :: peeraddr -> m [peeraddr],

       -- | Core actions run by the governor to change 'PeerStatus'.
       --
       peerStateActions :: PeerStateActions peeraddr peerconn m
     }


-- | Callbacks which are performed to change peer state.
--
data PeerStateActions peeraddr peerconn m = PeerStateActions {
    -- | Monitor peer state.
    --
    monitorPeerConnection    :: peerconn -> STM m PeerStatus,

    -- | Establish new connection.
    --
    establishPeerConnection  :: peeraddr -> m peerconn,

    -- | Activate a connection: warm to hot promotion.
    --
    activatePeerConnection   :: peerconn -> m (),

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
data PeerSelectionState peeraddr peerconn = PeerSelectionState {

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
       establishedPeers     :: !(EstablishedPeers peeraddr peerconn),

       -- |
       --
       activePeers          :: !(Set peeraddr),

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
       inProgressGossipReqs     :: !Int,
       inProgressPromoteCold    :: !(Set peeraddr),
       inProgressPromoteWarm    :: !(Set peeraddr),
       inProgressDemoteWarm     :: !(Set peeraddr),
       inProgressDemoteHot      :: !(Set peeraddr)

--     TODO: need something like this to distinguish between lots of bad peers
--     and us getting disconnected from the network locally. We don't want a
--     network disconnect to cause us to flush our full known peer set by
--     considering them all to have bad connectivity.
--     Should also take account of DNS failures for root peer set.
--     lastSucessfulNetworkEvent :: Time
     }
  deriving (Show, Functor)


emptyPeerSelectionState :: PeerSelectionState peeraddr peerconn
emptyPeerSelectionState =
    PeerSelectionState {
      targets              = nullPeerSelectionTargets,
      localRootPeers       = Map.empty,
      publicRootPeers      = Set.empty,
      knownPeers           = KnownPeers.empty,
      establishedPeers     = EstablishedPeers.empty,
      activePeers          = Set.empty,
      publicRootBackoffs   = 0,
      publicRootRetryTime  = Time 0,
      inProgressPublicRootsReq = False,
      inProgressGossipReqs     = 0,
      inProgressPromoteCold    = Set.empty,
      inProgressPromoteWarm    = Set.empty,
      inProgressDemoteWarm     = Set.empty,
      inProgressDemoteHot      = Set.empty
    }


assertPeerSelectionState :: Ord peeraddr
                         => PeerSelectionState peeraddr peerconn
                         -> a -> a
assertPeerSelectionState PeerSelectionState{..} =
   assert (KnownPeers.invariant knownPeers)

    -- The activePeers is a subset of the establishedPeers
    -- which is a subset of the known peers
 . assert (Set.isSubsetOf activePeersSet establishedReadySet)
 . assert (Set.isSubsetOf establishedPeersSet knownPeersSet)
 . assert (EstablishedPeers.invariant establishedPeers)
   -- The localRootPeers and publicRootPeers must not overlap.
 . assert (Set.null (Set.intersection localRootPeersSet publicRootPeers))
    -- The localRootPeers are a subset of the knownPeers,
    -- and with correct source info in the knownPeers (either
    -- 'PeerSroucePublicRoot' or 'PeerSourceLocalRoot', as local and public
    -- root peers might overlap).
 . assert (Map.isSubmapOfBy (\rootPeerAdvertise
               KnownPeerInfo {knownPeerAdvertise, knownPeerSource} ->
                   knownPeerSource == PeerSourceLocalRoot
                && knownPeerAdvertise == rootPeerAdvertise)
             localRootPeers
             (KnownPeers.toMap knownPeers))

    -- The publicRootPeers are a subset of the knownPeers,
    -- and with correct source info in the knownPeers.
 . assert (Map.isSubmapOfBy (\_ KnownPeerInfo {knownPeerSource} ->
                 knownPeerSource == PeerSourcePublicRoot
              || knownPeerSource == PeerSourceLocalRoot)
             (Map.fromSet (const ()) publicRootPeers)
             (KnownPeers.toMap knownPeers))

    --TODO: all other peers have PeerSourceGossip, so no stale source info.

    -- We don't want to pick local root peers to forget, so it had better be
    -- the case that there's fewer of them than our target number.
 . assert (Map.size localRootPeers <= targetNumberOfKnownPeers targets)
    -- All currently established peers are in the availableToConnect set since
    -- the alternative is a record of failure, but these are not (yet) failed.
 . assert (Set.isSubsetOf establishedPeersSet (KnownPeers.availableToConnect knownPeers))

    -- No constraint for publicRootBackoffs, publicRootRetryTime
    -- or inProgressPublicRootsReq

 . assert (inProgressGossipReqs >= 0)
 . assert (Set.isSubsetOf inProgressPromoteCold coldPeersSet)
 . assert (Set.isSubsetOf inProgressPromoteWarm warmPeersSet)
 . assert (Set.isSubsetOf inProgressDemoteWarm  warmPeersSet)
 . assert (Set.isSubsetOf inProgressDemoteHot   hotPeersSet)
 . assert (Set.null (Set.intersection inProgressPromoteWarm inProgressDemoteWarm))
  where
    localRootPeersSet   = Map.keysSet localRootPeers
    knownPeersSet       = Map.keysSet (KnownPeers.toMap knownPeers)
    establishedReadySet = Map.keysSet (EstablishedPeers.establishedReady establishedPeers)
    establishedPeersSet = Map.keysSet (EstablishedPeers.toMap establishedPeers)
    activePeersSet      = activePeers
    coldPeersSet        = knownPeersSet Set.\\ establishedPeersSet
    warmPeersSet        = establishedPeersSet Set.\\ activePeersSet
    hotPeersSet         = activePeersSet


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
    -- 'Ouroboros.Newtork.PeerSelection.Governor.peerSelectionGovernorLoop' will
    -- never return it: this is bacause there are monitoring decisions which
    -- never return this constructor, e.g.  'Monitor.targetPeers',
    -- 'Monitor.jobs', 'Monitor.connections', and thus the governor has always
    -- something to do.
    --
    GuardedSkip !(Maybe (Min Time))

    -- | 'Guarded' is used to provide an action, possibly with a timeout, to
    -- the governor main loop.
    --
  | Guarded     !(Maybe (Min Time)) (m a)

-- | 'Guarded' constructor is absorbing in the sense that
--
-- > Guarded x y <> a = Guarded x' y'
-- > a <> Guarded x y = Guarded x' y'
--
-- In the algebraic sense, @'Guarded' (Just minBound) (return x)@ is a left
-- absorbing element when "m ~ STM m'@ for some monad @m'@.  There is no right
-- absorbing element since there is no right absorbing elemnt in @STM m'@.
--
-- Ref. [absorbing element](https://en.wikipedia.org/wiki/Absorbing_element)
--
instance Alternative m => Semigroup (Guarded m a) where
  Guarded     ta a <> Guarded     tb b = Guarded     (ta <> tb) (a <|> b)
  Guarded     ta a <> GuardedSkip tb   = Guarded     (ta <> tb)  a
  GuardedSkip ta   <> Guarded     tb b = Guarded     (ta <> tb)  b
  GuardedSkip ta   <> GuardedSkip tb   = GuardedSkip (ta <> tb)


data Decision m peeraddr peerconn = Decision {
         -- | A trace event to classify the decision and action
       decisionTrace :: TracePeerSelection peeraddr,

         -- | An updated state to use immediately
       decisionState :: PeerSelectionState peeraddr peerconn,

       -- | An optional 'Job' to execute asynchronously. This job leads to
       -- a further 'Decision'. This gives a state update to apply upon
       -- completion, but also allows chaining further job actions.
       --
       decisionJobs  :: [Job m (Completion m peeraddr peerconn)]
     }

-- | Decision which has access to the current time, rather than the time when
-- the governor's loop blocked to make a decision.
--
type TimedDecision m peeraddr peerconn = Time -> Decision m peeraddr peerconn

-- | Type alias for function types which are used to create governor decisions.
-- Allmost all decisions are following this pattern.
--
type MkGuardedDecision peeraddr peerconn m
     = PeerSelectionPolicy peeraddr m
    -> PeerSelectionState peeraddr peerconn
    -> Guarded (STM m) (TimedDecision m peeraddr peerconn)


newtype Completion m peeraddr peerconn =
        Completion (PeerSelectionState peeraddr peerconn
                 -> Time -> Decision m peeraddr peerconn)

data TracePeerSelection peeraddr =
       TraceLocalRootPeersChanged (Map peeraddr PeerAdvertise)
                                  (Map peeraddr PeerAdvertise)
     | TraceTargetsChanged     PeerSelectionTargets PeerSelectionTargets
     | TracePublicRootsRequest Int Int
     | TracePublicRootsResults (Set peeraddr) Int DiffTime
     | TracePublicRootsFailure SomeException Int DiffTime
     -- | target known peers, actual known peers, peers available for gossip,
     -- peers selected for gossip
     | TraceGossipRequests     Int Int (Set peeraddr) (Set peeraddr)
     | TraceGossipResults      [(peeraddr, Either SomeException [peeraddr])] --TODO: classify failures
     -- | target known peers, actual known peers, selected peers
     | TraceForgetColdPeers    Int Int (Set peeraddr)
     -- | target established, actual established, selected peers
     | TracePromoteColdPeers   Int Int (Set peeraddr)
     -- | target established, actual established, peer, delay until next
     -- promotion, reason
     | TracePromoteColdFailed  Int Int peeraddr DiffTime SomeException
     -- | target established, actual established, peer
     | TracePromoteColdDone    Int Int peeraddr
     -- | target active, actual active, selected peers
     | TracePromoteWarmPeers   Int Int (Set peeraddr)
     -- | target active, actual active, peer, reason
     | TracePromoteWarmFailed  Int Int peeraddr SomeException
     -- | target active, actual active, peer
     | TracePromoteWarmDone    Int Int peeraddr
     -- | target established, actual established, selected peers
     | TraceDemoteWarmPeers    Int Int (Set peeraddr)
     -- | target established, actual established, peer, reason
     | TraceDemoteWarmFailed   Int Int  peeraddr SomeException
     -- | target established, actual established, peer
     | TraceDemoteWarmDone     Int Int peeraddr
     -- | target active, actual active, selected peers
     | TraceDemoteHotPeers     Int Int (Set peeraddr)
     -- | target active, actual active, peer, reason
     | TraceDemoteHotFailed    Int Int peeraddr SomeException
     -- | target active, actual active, peer
     | TraceDemoteHotDone      Int Int peeraddr
     | TraceDemoteAsynchronous (Map peeraddr PeerStatus)
     | TraceGovernorWakeup
  deriving Show

data DebugPeerSelection peeraddr peerconn =
       TraceGovernorState Time
                          (Maybe DiffTime)
                          (PeerSelectionState peeraddr peerconn)
  deriving (Show, Functor)
