{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module contains governor decisions for monitoring tasks:
--
-- * monitoring local root peer config changes
-- * monitoring changes to the peer target numbers
-- * monitoring the completion of asynchronous governor job
-- * monitoring connections
--
module Ouroboros.Network.PeerSelection.Governor.Monitor
  ( targetPeers
  , jobs
  , jobVerifyPeerSnapshot
  , connections
  , localRoots
  , ledgerPeerSnapshotChange
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Set (Set)
import Data.Set qualified as Set

import Control.Concurrent.JobPool (Job (..), JobPool)
import Control.Concurrent.JobPool qualified as JobPool
import Control.Exception (assert)
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadTime.SI
import System.Random (randomR)

import Ouroboros.Network.ExitPolicy (RepromoteDelay)
import Ouroboros.Network.ExitPolicy qualified as ExitPolicy
import Ouroboros.Network.PeerSelection.Governor.ActivePeers
           (jobDemoteActivePeer)
import Ouroboros.Network.PeerSelection.Governor.Types hiding
           (PeerSelectionCounters)
import Ouroboros.Network.PeerSelection.LedgerPeers.Type
           (LedgerPeerSnapshot (..), LedgerPeersConsensusInterface (..),
           compareLedgerPeerSnapshotApproximate)
import Ouroboros.Network.PeerSelection.LedgerPeers.Utils
import Ouroboros.Network.PeerSelection.PublicRootPeers qualified as PublicRootPeers
import Ouroboros.Network.PeerSelection.State.EstablishedPeers qualified as EstablishedPeers
import Ouroboros.Network.PeerSelection.State.KnownPeers qualified as KnownPeers
import Ouroboros.Network.PeerSelection.State.LocalRootPeers qualified as LocalRootPeers
import Ouroboros.Network.PeerSelection.Types

-- | Monitor 'PeerSelectionTargets', if they change, we just need to update
-- 'PeerSelectionState', since we return it in a 'Decision' action it will be
-- picked by the governor's 'peerSelectionGovernorLoop'.
--
-- It should be noted if the node is in bootstrap mode (i.e. in a sensitive
-- state) then, until the node reaches a clean state, this monitoring action
-- will be disabled and thus churning will be disabled as well.
--
-- On the other hand, if Genesis mode is on for the node, this action responds
-- to changes in ledger state judgement monitoring actions to change the static
-- set of target peers.
targetPeers :: (MonadSTM m, Ord peeraddr)
            => PeerSelectionActions (CardanoPeerSelectionActions m) extraPeers extraFlags extraAPI peeraddr peerconn m
            -> PeerSelectionState CardanoPeerSelectionState PeerTrustable (CardanoPublicRootPeers peeraddr) peeraddr peerconn
            -> Guarded (STM m) (TimedDecision m CardanoPeerSelectionState PeerTrustable (CardanoPublicRootPeers peeraddr) peeraddr peerconn)
targetPeers PeerSelectionActions{ originalPeerSelectionTargets,
                                  readPeerSelectionTargets,
                                  extraActions = CardanoPeerSelectionActions {
                                    cpsaSyncPeerTargets
                                  },
      targets' <- readPeerSelectionTargets
      check (targets' /= targets && sanePeerSelectionTargets targets')
                  originalPeerSelectionTargets
              (TooOld, GenesisMode)
                | churnTargets == originalPeerSelectionTargets ->
                  cpsaSyncPeerTargets
              _otherwise -> churnTargets

      -- nb. first check is redundant in Genesis mode
      check (   isNodeAbleToMakeProgress cpstBootstrapPeersFlag
                                         cpstLedgerStateJudgement
                                         cpstHasOnlyBootstrapPeers
             && targets /= targets'
             && sanePeerSelectionTargets targets')
      -- We simply ignore target updates that are not "sane".

      let usingBootstrapPeers = requiresBootstrapPeers cpstBootstrapPeersFlag
                                                       cpstLedgerStateJudgement
          -- We have to enforce the invariant that the number of root peers is
          -- not more than the target number of known peers. It's unlikely in
          -- practice so it's ok to resolve it arbitrarily using clampToLimit.
          --
          -- Here we need to make sure that we prioritise the trustable peers
          -- by clamping to trustable first.
          --
          -- TODO: we ought to add a warning if 'clampToLimit' modified local
          -- root peers, even though this is unexpected in the most common
          -- scenarios.
          localRootPeers' =
              LocalRootPeers.clampToLimit
                              (targetNumberOfKnownPeers targets')
            $ (if usingBootstrapPeers
                  then LocalRootPeers.clampToTrustable
                  else id)
            $ localRootPeers

          -- We have to enforce that local and big ledger peers are disjoint.
          publicRootPeers' =
            PublicRootPeers.difference (differenceExtraPeers extraPeersActions)
              publicRootPeers (LocalRootPeers.keysSet localRootPeers')

      return $ \_now -> Decision {
        decisionTrace = [TraceTargetsChanged targets targets'],
        decisionJobs  = [],
        decisionState = st {
                          targets        = targets',
                          localRootPeers = localRootPeers',
                          publicRootPeers = publicRootPeers'
                        } }

-- | Await for the first result from 'JobPool' and return its 'Decision'.
--
jobs :: MonadSTM m
     => JobPool () m (Completion m extraState extraFlags extraPeers peeraddr peerconn)
     -> PeerSelectionState extraState extraFlags extraPeers peeraddr peerconn
     -> Guarded (STM m) (TimedDecision m extraState extraFlags extraPeers peeraddr peerconn)
jobs jobPool st =
    -- This case is simple because the job pool returns a 'Completion' which is
    -- just a function from the current state to a new 'Decision'.
    Guarded Nothing $ do
      Completion completion <- JobPool.waitForJob jobPool
      return (completion st)


-- | Monitor connections.
--
connections :: forall m extraActions extraState extraFlags extraPeers extraAPI extraCounters peeraddr peerconn.
               (MonadSTM m, Ord peeraddr)
            => PeerSelectionActions extraState extraActions extraPeers extraFlags extraAPI extraCounters peeraddr peerconn m
            -> PeerSelectionState extraState extraFlags extraPeers peeraddr peerconn
            -> Guarded (STM m) (TimedDecision m extraState extraFlags extraPeers peeraddr peerconn)
connections PeerSelectionActions{
              peerStateActions = PeerStateActions {monitorPeerConnection}
            }
            st@PeerSelectionState {
              publicRootPeers,
              localRootPeers,
              activePeers,
              establishedPeers,
              inProgressDemoteHot,
              inProgressDemoteWarm,
              inProgressPromoteWarm,
              inProgressDemoteToCold,
              stdGen
            } =
    Guarded Nothing $ do
      -- Get previously cooling peers
      monitorStatus <- traverse monitorPeerConnection
                                (EstablishedPeers.toMap establishedPeers)
      let demotions = asynchronousDemotions monitorStatus
      check (not (Map.null demotions))
      let (demotedToWarm, demotedToCoolingOrCold) = Map.partition ((==PeerWarm) . fst) demotions
          (demotedToCold, demotedToCooling) = Map.partition ((==PeerCold) . fst) demotedToCoolingOrCold
          -- fuzz reconnect delays
          (aFuzz, stdGen')  = randomR (0.1, 10 :: Double) stdGen
          (rFuzz, stdGen'') = randomR (0.1, 4  :: Double) stdGen'
          demotions' = (\a@(peerState, repromoteDelay) -> case peerState of
                         PeerHot  -> a
                         PeerWarm ->
                           ( peerState
                           , (\x -> (x + realToFrac aFuzz) `max` 0) <$> repromoteDelay
                           )
                         PeerCooling -> a
                         PeerCold ->
                           ( peerState
                           , (\x -> (x + realToFrac rFuzz) `max` 0) <$> repromoteDelay
                           )
                       ) <$> demotions
      return $ \now ->
        let -- Remove all asynchronous demotions from 'activePeers'
            activePeers'       = activePeers Set.\\ Map.keysSet demotions'

            -- Note that we do not use establishedStatus' which
            -- has the synchronous ones that are supposed to be
            -- handled elsewhere. We just update the async ones:
            establishedPeers'  = EstablishedPeers.setActivateTimes
                                   ( (\(_, a) -> ExitPolicy.repromoteDelay (fromMaybe 0 a) `addTime` now)
                                      -- 'monitorPeerConnection' returns
                                      -- 'Nothing' iff all mini-protocols are
                                      -- either still running or 'NotStarted'
                                      -- (e.g.  this possible for warm or hot
                                      -- peers).  In such case we don't want to
                                      -- `setActivateTimes`
                                      <$> Map.filter (isJust . snd) demotedToWarm
                                   )
                               . EstablishedPeers.deletePeers
                                  (Map.keysSet demotedToCold)
                               $ establishedPeers

            -- Asynchronous transition to cold peer can only be
            -- a result of a failure.
            knownPeers'        = KnownPeers.setConnectTimes
                                    ( (\(_, a) -> ExitPolicy.repromoteDelay (fromMaybe 0 a) `addTime` now)
                                      <$> demotedToCold
                                    )
                               . Set.foldr'
                                   ((snd .) . KnownPeers.incrementFailCount)
                                   (knownPeers st)
                               $ Map.keysSet demotedToCold
            (localDemotions, nonLocalDemotions) =
              Map.partitionWithKey
                (\peer _ -> peer `LocalRootPeers.member` localRootPeers)
                demotions'

            publicRootDemotions     = nonLocalDemotions
                   `Map.withoutKeys`  bigLedgerPeersSet
            bigLedgerPeersDemotions = nonLocalDemotions
                   `Map.restrictKeys` bigLedgerPeersSet

            -- Peers in this state won't be able to be promoted nor demoted.
            inProgressDemoteToCold' =
              (inProgressDemoteToCold Set.\\ Map.keysSet demotedToCold )
               <> Map.keysSet demotedToCooling
        in assert (activePeers' `Set.isSubsetOf`
                     Map.keysSet (EstablishedPeers.toMap establishedPeers'))
            Decision {
              decisionTrace = [ TraceDemoteLocalAsynchronous localDemotions
                              | not $ null localDemotions ]
                           <> [ TraceDemoteAsynchronous publicRootDemotions
                              | not $ null publicRootDemotions ]
                           <> [ TraceDemoteBigLedgerPeersAsynchronous
                                  bigLedgerPeersDemotions
                              | not $ null bigLedgerPeersDemotions ],
              decisionJobs  = [],
              decisionState = st {
                                activePeers       = activePeers',
                                establishedPeers  = establishedPeers',
                                knownPeers        = knownPeers',

                                -- When promoting a warm peer, it might happen
                                -- that the connection will break (or one of the
                                -- established protocols will error).  For that
                                -- reason we need to adjust 'inProgressPromoteWarm'.
                                inProgressPromoteWarm
                                                  = inProgressPromoteWarm
                                                      Set.\\ Map.keysSet demotedToCoolingOrCold,

                                inProgressDemoteToCold =
                                  inProgressDemoteToCold',

                                -- Note that we do not need to adjust
                                -- inProgressDemoteWarm or inProgressDemoteHot
                                -- here since we define the async demotions
                                -- to not include peers in those sets. Instead,
                                -- those ones will complete synchronously.

                                stdGen = stdGen''
                              }
          }
  where
    bigLedgerPeersSet = PublicRootPeers.getBigLedgerPeers publicRootPeers
    -- Those demotions that occurred not as a result of action by the governor.
    -- They're further classified into demotions to warm, and demotions to cold.
    asynchronousDemotions :: Map peeraddr (PeerStatus, Maybe RepromoteDelay)
                          -> Map peeraddr (PeerStatus, Maybe RepromoteDelay)
    asynchronousDemotions = Map.mapMaybeWithKey asyncDemotion

    -- The asynchronous ones, those not directed by the governor, are:
    -- hot -> warm, warm -> cold and hot -> cold, other than the ones in the in
    -- relevant progress set.
    asyncDemotion :: peeraddr
                  -> (PeerStatus, Maybe RepromoteDelay)
                  -> Maybe (PeerStatus, Maybe RepromoteDelay)

    -- a hot -> warm transition has occurred if it is now warm, and it was
    -- hot, but not in the set we were deliberately demoting synchronously
    asyncDemotion peeraddr (PeerWarm, returnCommand)
      | peeraddr `Set.member`    activePeers
      , peeraddr `Set.notMember` inProgressDemoteHot  = Just (PeerWarm, returnCommand)

    -- a `{PeerHot,PeerWarm} -> PeerCooling` transition has occurred if it is
    -- now cooling, and it was warm, but not in the set of peers being demoted
    -- synchronously, e.g. `inProgressDemote{Hot,Warm}`.
    --
    -- If the peer is a member of the `inProgressDemoteToCold` set it means we
    -- already accounted it, since we are adding peers to
    -- `inProgressDemoteToCold` only if this function returns
    -- `Just (PeerCooling, ...)`.
    --
    -- A peer in the `PeerCooling` state is going to be a member of the established set
    -- until its connection is effectively terminated on the outbound side when
    -- it will become `PeerCold`. We check if the peer does not exist in the
    -- `inProgressDemoteToCold` to see if it is a new asynchronous demotion.
localRoots :: forall extraState extraActions extraPeers extraFlags extraAPI extraCounters peeraddr peerconn m.
              (MonadSTM m, Ord peeraddr, Eq extraFlags)
           => PeerSelectionActions extraState extraActions extraPeers extraFlags extraAPI extraCounters peeraddr peerconn m
           -> PeerSelectionState extraState extraFlags extraPeers peeraddr peerconn
           -> Guarded (STM m) (TimedDecision m extraState extraFlags extraPeers peeraddr peerconn)
    -- a cooling -> cold transition has occurred if it is now cold, and it was cooling
    asyncDemotion peeraddr (PeerCold, returnCommand)
      | peeraddr `EstablishedPeers.member` establishedPeers || peeraddr `Set.member` activePeers
      , peeraddr `Set.notMember` inProgressDemoteWarm
      , peeraddr `Set.notMember` inProgressDemoteHot  = Just (PeerCold, returnCommand)
      -- Note:
             targets = PeerSelectionTargets{targetNumberOfKnownPeers}
           } =


-----------------------------------------------
-- Monitoring changes to the local root peers
--


-- | Monitor local roots using 'readLocalRootPeers' 'STM' action.
      let localRootPeers' = LocalRootPeers.clampToLimit
-- root peers, this means that if we remove any local root peer we
-- might no longer abide by the invariant that we are only connected to
-- trusted peers. E.g. Local peers = A, B*, C* (* means trusted peer), if
-- the node is in bootstrap mode and decided to reconfigure the local root
-- peers to the following set: A*, B, C*, D*, E. Notice that B is no longer
-- trusted, however we will keep a connection to it until the outbound
-- governor notices it and disconnects from it.
localRoots :: forall extraActions extraAPI extraCounters peeraddr peerconn m.
                                       , extraPeersActions = PublicExtraPeersActions {
                                           differenceExtraPeers
                                         , extraPeersToSet
                                         }
            => PeerSelectionActions CardanoPeerSelectionState extraActions (CardanoPublicRootPeers peeraddr) PeerTrustable extraAPI extraCounters peeraddr peerconn m
            -> PeerSelectionState CardanoPeerSelectionState PeerTrustable (CardanoPublicRootPeers peeraddr) peeraddr peerconn
           -> Guarded (STM m) (TimedDecision m CardanoPeerSelectionState PeerTrustable (CardanoPublicRootPeers peeraddr) peeraddr peerconn)
localRoots actions@PeerSelectionActions{ readLocalRootPeers
                                       , extraPeersActions
                                       }
           st@PeerSelectionState{
             localRootPeers,
             publicRootPeers,
             knownPeers,
             establishedPeers,
             activePeers,
             inProgressDemoteHot,
             inProgressDemoteToCold,
             targets = PeerSelectionTargets{targetNumberOfKnownPeers},
             extraState = cpst@CardanoPeerSelectionState {
               cpstBootstrapPeersFlag,
               cpstHasOnlyBootstrapPeers,
               cpstLedgerStateJudgement
             }
           }
  | isNodeAbleToMakeProgress cpstBootstrapPeersFlag
          addedInfoMap = Map.map (\(pa, _) -> (Nothing, Just pa)) added
          removedSet   = Map.keysSet removed
          knownPeers'  = KnownPeers.insert addedInfoMap knownPeers
                        -- We do not immediately remove old ones from the
                        -- known peers set because we may have established
                        -- connections

          localRootPeersSet = LocalRootPeers.keysSet localRootPeers'

          -- We have to adjust the publicRootPeers to maintain the invariant
          -- that the local and public sets are non-overlapping.
          --
          publicRootPeers' =
            PublicRootPeers.difference (differenceExtraPeers extraPeersActions)
              publicRootPeers
              localRootPeersSet

          -- Non trustable peers that the outbound governor might keep. These
          -- should be demoted forgot as soon as possible. In order to do that
          -- we set 'hasOnlyBootstrapPeers' to False and
          -- 'ledgerStateJudgement' to TooOld in order to force clean state
          -- reconnections.
          hasOnlyBootstrapPeers' =
            Set.null $ KnownPeers.toSet knownPeers'
                       (  LocalRootPeers.keysSet localRootPeers'
            PublicRootPeers.difference differenceExtraPeers
            decisionTrace = [TraceLocalRootPeersChanged localRootPeers
                                                        localRootPeers'],
          -- 'ledgerStateJudgement' value to 'YoungEnough' which
          -- 'monitorLedgerStateJudgement' will then read the original
          -- 'TooOld' value and trigger the clean state protocol.
          --
          -- Note that if the state actually changes to 'YoungEnough' it
                                                 <> selectedToDemote
            then YoungEnough
            else cpstLedgerStateJudgement

          -- If we are removing local roots and we have active connections to
          -- them then things are a little more complicated. We would typically
          -- change local roots so that we can establish new connections to
          -- the new local roots. But since we will typically already be at our
          -- target for active peers then that will not be possible without us
          -- taking additional action. What we choose to do here is to demote
          -- the peer from active to warm, which will then allow new ones to
          -- be promoted to active.
          selectedToDemote  :: Set peeraddr
          selectedToDemote' :: Map peeraddr peerconn

                    (PublicRootPeers.toSet extraPeersToSet
                                Set.\\ inProgressDemoteToCold
          selectedToDemote' = EstablishedPeers.toMap establishedPeers
                               `Map.restrictKeys` selectedToDemote

      return $ \_now ->

          assert (Set.isSubsetOf
                    (PublicRootPeers.toSet (extraPeersToSet extraPeersActions)
                                           publicRootPeers')
                   (KnownPeers.toSet knownPeers'))
        . assert (Set.isSubsetOf
                   (LocalRootPeers.keysSet localRootPeers')
                   (KnownPeers.toSet knownPeers'))

        $ Decision {
            decisionTrace = TraceLocalRootPeersChanged localRootPeers localRootPeers'
                          : [ TraceLedgerStateJudgementChanged YoungEnough
                            | cpstLedgerStateJudgement /= ledgerStateJudgement'
                            ],
            decisionState = st {
                              localRootPeers      = localRootPeers',
                              publicRootPeers     = publicRootPeers',
                              knownPeers          = knownPeers',
                              inProgressDemoteHot = inProgressDemoteHot
                                                 <> selectedToDemote,
                              extraState = cpst {
                                cpstHasOnlyBootstrapPeers = hasOnlyBootstrapPeers',
                                cpstLedgerStateJudgement  = ledgerStateJudgement'
                              }
                            },
            decisionJobs  = [ jobDemoteActivePeer actions peeraddr peerconn
                          | (peeraddr, peerconn) <- Map.assocs selectedToDemote' ]
          }
  | otherwise = GuardedSkip Nothing

-- |This job, which is initiated by monitorLedgerStateJudgement job,
-- verifies whether the provided big ledger pools match up with the
-- ledger state once the node catches up to the slot at which the
-- snapshot was ostensibly taken
--
jobVerifyPeerSnapshot :: ( MonadSTM m )
                      => LedgerPeerSnapshot
                      -> LedgerPeersConsensusInterface extraAPI m
                      -> Job () m (Completion m extraState extraFlags extraPeers peeraddr peerconn)
jobVerifyPeerSnapshot baseline@(LedgerPeerSnapshot (slot, _))
                      LedgerPeersConsensusInterface {
                        lpGetLatestSlot,
                        lpGetLedgerPeers }
                                extraState = extraStateChange cpst,
      Decision {
        decisionTrace = [TraceVerifyPeerSnapshot result],
        decisionState = st,
        decisionJobs  = [] }

    job = do
                         => (extraState -> extraState)
                         -> PeerSelectionActions extraState extraActions extraPeers extraFlags extraAPI extraCounters peeraddr peerconn m
                         -> PeerSelectionState extraState extraFlags extraPeers peeraddr peerconn
                         -> Guarded (STM m) (TimedDecision m extraState extraFlags extraPeers peeraddr peerconn)
ledgerPeerSnapshotChange extraStateChange
                         PeerSelectionActions {
      let candidate = LedgerPeerSnapshot (slot, ledgerPeers) -- ^ slot here is intentional
      completion $ compareLedgerPeerSnapshotApproximate baseline candidate

-- |This job monitors for any changes in the big ledger peer snapshot
-- and flips ledger state judgement private state so that monitoring action
-- can launch `jobVerifyPeerSnapshot`
--
ledgerPeerSnapshotChange :: (MonadSTM m)
                         => PeerSelectionActions CardanoPeerSelectionState extraActions extraPeers extraFlags extraAPI extraCounters peeraddr peerconn m
                         -> PeerSelectionState CardanoPeerSelectionState extraFlags extraPeers peeraddr peerconn
                         -> Guarded (STM m) (TimedDecision m CardanoPeerSelectionState extraFlags extraPeers peeraddr peerconn)
ledgerPeerSnapshotChange PeerSelectionActions {
                           readLedgerPeerSnapshot
                         }
                         st@PeerSelectionState {
                           ledgerPeerSnapshot,
                           extraState = cpst
                         } =
  Guarded Nothing $ do
    ledgerPeerSnapshot' <- readLedgerPeerSnapshot
    case (ledgerPeerSnapshot', ledgerPeerSnapshot) of
      (Nothing, _) -> retry
      (Just (LedgerPeerSnapshot (slot, _)), Just (LedgerPeerSnapshot (slot', _)))
        | slot == slot' -> retry
      _otherwise ->
        return $ \_now ->
                   Decision { decisionTrace = [],
                              decisionJobs  = [],
                              decisionState = st {
                                extraState = cpst {
                                  cpstLedgerStateJudgement = YoungEnough
                                },
                                ledgerPeerSnapshot = ledgerPeerSnapshot'
                              }
                            }
