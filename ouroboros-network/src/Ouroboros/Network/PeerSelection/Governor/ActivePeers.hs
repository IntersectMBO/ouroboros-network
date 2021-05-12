{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.PeerSelection.Governor.ActivePeers
  ( belowTarget
  , aboveTarget

  , jobDemoteActivePeer
  ) where

import           Data.Semigroup (Min(..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set

import           Control.Monad.Class.MonadSTM
import           Control.Concurrent.JobPool (Job(..))
import           Control.Exception (SomeException, assert)

import qualified Ouroboros.Network.PeerSelection.EstablishedPeers as EstablishedPeers
import qualified Ouroboros.Network.PeerSelection.LocalRootPeers as LocalRootPeers
import           Ouroboros.Network.PeerSelection.Governor.Types


----------------------------
-- Active peers below target
--


-- | If we are below the target of /hot peers/ we promote some of the /warm
-- peers/ according to 'policyPickWarmPeersToPromote' policy.
--
belowTarget :: forall peeraddr peerconn m.
               (MonadSTM m, Ord peeraddr)
            => PeerSelectionActions peeraddr peerconn m
            -> MkGuardedDecision peeraddr peerconn m
belowTarget = belowTargetLocal <> belowTargetOther


belowTargetLocal :: forall peeraddr peerconn m.
                    (MonadSTM m, Ord peeraddr)
                 => PeerSelectionActions peeraddr peerconn m
                 -> MkGuardedDecision peeraddr peerconn m
belowTargetLocal actions
                 PeerSelectionPolicy {
                   policyPickWarmPeersToPromote
                 }
                 st@PeerSelectionState {
                   localRootPeers,
                   establishedPeers,
                   activePeers,
                   inProgressPromoteWarm,
                   inProgressDemoteWarm
                 }
    -- Are there any groups of local peers that are below target?
  | not (null groupsBelowTarget)
    -- We need this detailed check because it is not enough to check we are
    -- below an aggregate target. We can be above target for some groups
    -- and below for others.

    -- Are there any groups where we can pick members to promote?
  , let groupsAvailableToPromote =
          [ (numMembersToPromote, membersAvailableToPromote)
          | let availableToPromote =
                  (LocalRootPeers.keysSet localRootPeers
                    `Set.intersection`
                   EstablishedPeers.readyPeers establishedPeers)
                     Set.\\ activePeers
                     Set.\\ inProgressPromoteWarm
                     Set.\\ inProgressDemoteWarm
                numPromoteInProgress = Set.size inProgressPromoteWarm
          , not (Set.null availableToPromote)
          , (target, members, membersActive) <- groupsBelowTarget
          , let membersAvailableToPromote = Set.intersection
                                              members availableToPromote
                numMembersToPromote       = target
                                          - Set.size membersActive
                                          - numPromoteInProgress
          , not (Set.null membersAvailableToPromote)
          , numMembersToPromote > 0
          ]
  , not (null groupsAvailableToPromote)
  = Guarded Nothing $ do
      selectedToPromote <-
        Set.unions <$> sequence
          [ pickPeers
              policyPickWarmPeersToPromote
              membersAvailableToPromote
              numMembersToPromote
          | (numMembersToPromote,
             membersAvailableToPromote) <- groupsAvailableToPromote ]

      let selectedToPromote' :: Map peeraddr peerconn
          selectedToPromote' = EstablishedPeers.toMap establishedPeers
                                 `Map.restrictKeys` selectedToPromote
      return $ \_now -> Decision {
        decisionTrace = TracePromoteWarmLocalPeers
                          [ (target, Set.size membersActive)
                          | (target, _, membersActive) <- groupsBelowTarget ]
                          selectedToPromote,
        decisionState = st {
                          inProgressPromoteWarm = inProgressPromoteWarm
                                               <> selectedToPromote
                        },
        decisionJobs  = [ jobPromoteWarmPeer actions peeraddr peerconn
                        | (peeraddr, peerconn) <- Map.assocs selectedToPromote' ]
      }


    -- If we could promote except that there are no peers currently available
    -- then we return the next wakeup time (if any)
  | not (null groupsBelowTarget)
  , let potentialToPromote =
          -- These are local peers that are warm but not ready.
          (LocalRootPeers.keysSet localRootPeers
            `Set.intersection`
           EstablishedPeers.toSet establishedPeers)
             Set.\\ activePeers
             Set.\\ EstablishedPeers.readyPeers establishedPeers
  , not (Set.null potentialToPromote)
  = GuardedSkip (Min <$> EstablishedPeers.minActivateTime establishedPeers)

  | otherwise
  = GuardedSkip Nothing
  where
    groupsBelowTarget =
      [ (target, members, membersActive)
      | (target, members) <- LocalRootPeers.toGroupSets localRootPeers
      , let membersActive = members `Set.intersection` activePeers
      , Set.size membersActive < target
      ]

belowTargetOther :: forall peeraddr peerconn m.
                    (MonadSTM m, Ord peeraddr)
                 => PeerSelectionActions peeraddr peerconn m
                 -> MkGuardedDecision peeraddr peerconn m
belowTargetOther actions
                 PeerSelectionPolicy {
                   policyPickWarmPeersToPromote
                 }
                 st@PeerSelectionState {
                   localRootPeers,
                   establishedPeers,
                   activePeers,
                   inProgressPromoteWarm,
                   inProgressDemoteWarm,
                   targets = PeerSelectionTargets {
                               targetNumberOfActivePeers
                             }
                 }
    -- Are we below the target for number of active peers?
  | numActivePeers + numPromoteInProgress < targetNumberOfActivePeers

    -- Are there any warm peers we could pick to promote?
  , let availableToPromote :: Set peeraddr
        availableToPromote = EstablishedPeers.readyPeers establishedPeers
                               Set.\\ activePeers
                               Set.\\ inProgressPromoteWarm
                               Set.\\ inProgressDemoteWarm
                               Set.\\ LocalRootPeers.keysSet localRootPeers
        numPeersToPromote = targetNumberOfActivePeers
                          - numActivePeers
                          - numPromoteInProgress
  , not (Set.null availableToPromote)
  , numPeersToPromote > 0
  = Guarded Nothing $ do
      selectedToPromote <- pickPeers
                             policyPickWarmPeersToPromote
                             availableToPromote
                             numPeersToPromote
      let selectedToPromote' :: Map peeraddr peerconn
          selectedToPromote' = EstablishedPeers.toMap establishedPeers
                                 `Map.restrictKeys` selectedToPromote
      return $ \_now -> Decision {
        decisionTrace = TracePromoteWarmPeers
                          targetNumberOfActivePeers
                          numActivePeers
                          selectedToPromote,
        decisionState = st {
                          inProgressPromoteWarm = inProgressPromoteWarm
                                               <> selectedToPromote
                        },
        decisionJobs  = [ jobPromoteWarmPeer actions peeraddr peerconn
                        | (peeraddr, peerconn) <- Map.assocs selectedToPromote' ]
      }

    -- If we could promote except that there are no peers currently available
    -- then we return the next wakeup time (if any)
  | numActivePeers + numPromoteInProgress < targetNumberOfActivePeers
  = GuardedSkip (Min <$> EstablishedPeers.minActivateTime establishedPeers)

  | otherwise
  = GuardedSkip Nothing
  where
    numActivePeers       = Set.size activePeers
    numPromoteInProgress = Set.size inProgressPromoteWarm


jobPromoteWarmPeer :: forall peeraddr peerconn m.
                      (Monad m, Ord peeraddr)
                   => PeerSelectionActions peeraddr peerconn m
                   -> peeraddr
                   -> peerconn
                   -> Job m (Completion m peeraddr peerconn)
jobPromoteWarmPeer PeerSelectionActions{peerStateActions = PeerStateActions {activatePeerConnection}}
                   peeraddr peerconn =
    Job job handler "promoteWarmPeer"
  where
    handler :: SomeException -> Completion m peeraddr peerconn
    handler e =
      --TODO: decide what happens if promotion fails, do we stay warm or go to
      -- cold? Will this be reported asynchronously via the state monitoring?
      Completion $ \st@PeerSelectionState {
                               activePeers,
                               targets = PeerSelectionTargets {
                                           targetNumberOfActivePeers
                                         }
                             }
                    _now -> Decision {
        decisionTrace = TracePromoteWarmFailed targetNumberOfActivePeers
                                               (Set.size activePeers) 
                                               peeraddr e,
        decisionState = st {
                          inProgressPromoteWarm = Set.delete peeraddr
                                                    (inProgressPromoteWarm st)
                        },
        decisionJobs  = []
      }

    job :: m (Completion m peeraddr peerconn)
    job = do
      --TODO: decide if we should do timeouts here or if we should make that
      -- the responsibility of activatePeerConnection
      activatePeerConnection peerconn
      return $ Completion $ \st@PeerSelectionState {
                               activePeers,
                               targets = PeerSelectionTargets {
                                           targetNumberOfActivePeers
                                         }
                             }
                           _now ->
        assert (peeraddr `EstablishedPeers.member` establishedPeers st) $
        let activePeers' = Set.insert peeraddr activePeers in
        Decision {
          decisionTrace = TracePromoteWarmDone targetNumberOfActivePeers
                                               (Set.size activePeers')
                                               peeraddr,
          decisionState = st {
                            activePeers           = activePeers',
                            inProgressPromoteWarm = Set.delete peeraddr
                                                      (inProgressPromoteWarm st)
                          },
          decisionJobs  = []
        }


----------------------------
-- Active peers above target
--

-- | If we are above the target of /hot peers/ we demote some hot peers to be
-- /warm peers/, according to 'policyPickHotPeersToDemote'.
--
aboveTarget :: forall peeraddr peerconn m.
               (MonadSTM m, Ord peeraddr)
            => PeerSelectionActions peeraddr peerconn m
            -> MkGuardedDecision peeraddr peerconn m
aboveTarget = aboveTargetLocal <> aboveTargetOther
  -- Start with the local root targets, then the general target. This makes
  -- sense since we need to hit both and making progress downwards with the
  -- local root targets makes progress for the general target too.


aboveTargetLocal :: forall peeraddr peerconn m.
                    (MonadSTM m, Ord peeraddr)
                 => PeerSelectionActions peeraddr peerconn m
                 -> MkGuardedDecision peeraddr peerconn m
aboveTargetLocal actions
                 PeerSelectionPolicy {
                   policyPickHotPeersToDemote
                 }
                 st@PeerSelectionState {
                   localRootPeers,
                   establishedPeers,
                   activePeers,
                   inProgressDemoteHot
                 }
    -- Are there any groups of local peers that are below target?
  | let groupsAboveTarget =
          [ (target, members, membersActive)
          | (target, members) <- LocalRootPeers.toGroupSets localRootPeers
          , let membersActive = members `Set.intersection` activePeers
          , Set.size membersActive > target
          ]
  , not (null groupsAboveTarget)
    -- We need this detailed check because it is not enough to check we are
    -- above an aggregate target. We can be above target for some groups
    -- and below for others.

    -- Are there any groups where we can pick members to demote?
  , let groupsAvailableToDemote =
          [ (numMembersToDemote, membersAvailableToDemote)
          | let availableToDemote = (LocalRootPeers.keysSet localRootPeers
                                       `Set.intersection`
                                     activePeers)
                                       Set.\\ inProgressDemoteHot
                numDemoteInProgress = Set.size inProgressDemoteHot
          , not (Set.null availableToDemote)
          , (target, members, membersActive) <- groupsAboveTarget
          , let membersAvailableToDemote = Set.intersection
                                             members availableToDemote
                numMembersToDemote       = Set.size membersActive
                                         - target
                                         - numDemoteInProgress
          , not (Set.null membersAvailableToDemote)
          , numMembersToDemote > 0
          ]
  , not (null groupsAvailableToDemote)
  = Guarded Nothing $ do
      selectedToDemote <-
        Set.unions <$> sequence
          [ pickPeers
              policyPickHotPeersToDemote
              membersAvailableToDemote
              numMembersToDemote
          | (numMembersToDemote,
             membersAvailableToDemote) <- groupsAvailableToDemote ]
      let selectedToDemote' :: Map peeraddr peerconn
          selectedToDemote' = EstablishedPeers.toMap establishedPeers
                                `Map.restrictKeys` selectedToDemote

      return $ \_now -> Decision {
        decisionTrace = TraceDemoteLocalHotPeers
                          [ (target, Set.size membersActive)
                          | (target, _, membersActive) <- groupsAboveTarget ]
                          selectedToDemote,
        decisionState = st {
                          inProgressDemoteHot = inProgressDemoteHot
                                             <> selectedToDemote
                        },
        decisionJobs  = [ jobDemoteActivePeer actions peeraddr peerconn
                        | (peeraddr, peerconn) <- Map.assocs selectedToDemote' ]
      }

  | otherwise
  = GuardedSkip Nothing


aboveTargetOther :: forall peeraddr peerconn m.
                    (MonadSTM m, Ord peeraddr)
                 => PeerSelectionActions peeraddr peerconn m
                 -> MkGuardedDecision peeraddr peerconn m
aboveTargetOther actions
                 PeerSelectionPolicy {
                   policyPickHotPeersToDemote
                 }
                 st@PeerSelectionState {
                   localRootPeers,
                   establishedPeers,
                   activePeers,
                   inProgressDemoteHot,
                   targets = PeerSelectionTargets {
                               targetNumberOfActivePeers
                             }
                 }
    -- Are we above the general target for number of active peers?
  | numActivePeers > targetNumberOfActivePeers

    -- Would we demote any if we could?
  , let numPeersToDemote = numActivePeers
                         - targetNumberOfActivePeers
                         - numDemoteInProgress
  , numPeersToDemote > 0

    -- Are there any hot peers we actually can pick to demote?
    -- For the moment we say we cannot demote local root peers.
    -- TODO: review this decision. If we want to be able to demote local root
    -- peers, e.g. for churn and improved selection, then we'll need an extra
    -- mechanism to avoid promotion/demotion loops for local peers.
  , let availableToDemote = activePeers
                              Set.\\ inProgressDemoteHot
                              Set.\\ LocalRootPeers.keysSet localRootPeers
  , not (Set.null availableToDemote)
  = Guarded Nothing $ do
      selectedToDemote <- pickPeers
                            policyPickHotPeersToDemote
                            availableToDemote
                            numPeersToDemote
      let selectedToDemote' :: Map peeraddr peerconn
          selectedToDemote' = EstablishedPeers.toMap establishedPeers
                                `Map.restrictKeys` selectedToDemote

      return $ \_now -> Decision {
        decisionTrace = TraceDemoteHotPeers
                          targetNumberOfActivePeers
                          numActivePeers
                          selectedToDemote,
        decisionState = st {
                          inProgressDemoteHot = inProgressDemoteHot
                                             <> selectedToDemote
                        },
        decisionJobs  = [ jobDemoteActivePeer actions peeraddr peerconn
                        | (peeraddr, peerconn) <- Map.assocs selectedToDemote' ]
      }

  | otherwise
  = GuardedSkip Nothing
  where
    numActivePeers      = Set.size activePeers
    numDemoteInProgress = Set.size inProgressDemoteHot


jobDemoteActivePeer :: forall peeraddr peerconn m.
                       (Monad m, Ord peeraddr)
                    => PeerSelectionActions peeraddr peerconn m
                    -> peeraddr
                    -> peerconn
                    -> Job m (Completion m peeraddr peerconn)
jobDemoteActivePeer PeerSelectionActions{peerStateActions = PeerStateActions {deactivatePeerConnection}}
                    peeraddr peerconn =
    Job job handler "demoteActivePeer"
  where
    handler :: SomeException -> Completion m peeraddr peerconn
    handler e =
      -- It's quite bad if closing fails, but the best we can do is revert to
      -- the state where we believed these peers are still warm, since then we
      -- can have another go at the ones we didn't yet try to close, or perhaps
      -- it'll be closed for other reasons and our monitoring will notice it.
      Completion $ \st@PeerSelectionState {
                      activePeers,
                      targets = PeerSelectionTargets {
                                  targetNumberOfActivePeers
                                }
                    }
                    _now -> Decision {
        decisionTrace = TraceDemoteHotFailed targetNumberOfActivePeers
                                             (Set.size activePeers) peeraddr e,
        decisionState = st {
                          inProgressDemoteHot = Set.delete peeraddr
                                                  (inProgressDemoteHot st)
                        },
        decisionJobs  = []
      }

    job :: m (Completion m peeraddr peerconn)
    job = do
      deactivatePeerConnection peerconn
      return $ Completion $ \st@PeerSelectionState {
                                activePeers,
                                targets = PeerSelectionTargets {
                                            targetNumberOfActivePeers
                                          }
                             }
                             _now ->
        assert (peeraddr `EstablishedPeers.member` establishedPeers st) $
        let activePeers' = Set.delete peeraddr activePeers in
        Decision {
          decisionTrace = TraceDemoteHotDone targetNumberOfActivePeers
                                             (Set.size activePeers')
                                             peeraddr,
          decisionState = st {
                            activePeers         = activePeers',
                            inProgressDemoteHot = Set.delete peeraddr
                                                    (inProgressDemoteHot st)
                          },
          decisionJobs  = []
        }
