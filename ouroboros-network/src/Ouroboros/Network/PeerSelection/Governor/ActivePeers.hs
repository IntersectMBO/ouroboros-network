{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.PeerSelection.Governor.ActivePeers
  ( belowTarget
  , aboveTarget
  , jobDemoteActivePeer
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Semigroup (Min (..))
import           Data.Set (Set)
import qualified Data.Set as Set

import           Control.Concurrent.JobPool (Job (..))
import           Control.Exception (SomeException, assert)
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           System.Random (randomR)

import qualified Ouroboros.Network.PeerSelection.EstablishedPeers as EstablishedPeers
import           Ouroboros.Network.PeerSelection.Governor.Types
import           Ouroboros.Network.PeerSelection.KnownPeers (setTepidFlag)
import qualified Ouroboros.Network.PeerSelection.KnownPeers as KnownPeers
import qualified Ouroboros.Network.PeerSelection.LocalRootPeers as LocalRootPeers


----------------------------
-- Active peers below target
--


-- | If we are below the target of /hot peers/ we promote some of the /warm
-- peers/ according to 'policyPickWarmPeersToPromote' policy.
--
belowTarget :: forall peeraddr peerconn m.
               (MonadDelay m, MonadSTM m, Ord peeraddr)
            => PeerSelectionActions peeraddr peerconn m
            -> MkGuardedDecision peeraddr peerconn m
belowTarget = belowTargetLocal <> belowTargetOther


belowTargetLocal :: forall peeraddr peerconn m.
                    (MonadDelay m, MonadSTM m, Ord peeraddr)
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
          [ pickPeers st
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
                    (MonadDelay m, MonadSTM m, Ord peeraddr)
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
      selectedToPromote <- pickPeers st
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
                      (MonadDelay m, Ord peeraddr)
                   => PeerSelectionActions peeraddr peerconn m
                   -> peeraddr
                   -> peerconn
                   -> Job () m (Completion m peeraddr peerconn)
jobPromoteWarmPeer PeerSelectionActions{peerStateActions = PeerStateActions {activatePeerConnection}}
                   peeraddr peerconn =
    Job job handler () "promoteWarmPeer"
  where
    baseReconnectDelay :: Double
    baseReconnectDelay = 10

    handler :: SomeException -> m (Completion m peeraddr peerconn)
    handler e = do
      -- We wait here, not to avoid race conditions or broken locking, this is
      -- just a very simple back-off strategy. For cold -> warm failures we use
      -- an exponential back-off retry strategy. For warm -> hot we do not need
      -- such a strategy. Simply a short pause is enough to ensure we never
      -- busy-loop.  Failures of warm -> hot will be accompanied by an
      -- asynchronous demotion to cold relatively promptly. If we did ever need
      -- to carry on after warm -> hot failures then we would need to implement
      -- a more sophisticated back-off strategy, like an exponential back-off
      -- (and perhaps use that failure count in the policy to drop useless peers
      -- in the warm -> cold transition).
      threadDelay 1
      return $
        -- When promotion fails we set the peer as cold.
        Completion $ \st@PeerSelectionState {
                                 activePeers,
                                 establishedPeers,
                                 knownPeers,
                                 fuzzRng,
                                 targets = PeerSelectionTargets {
                                             targetNumberOfActivePeers
                                           }
                               }
                      now ->
          -- TODO: this is a temporary fix, which will by addressed by
          -- #3460
          let establishedPeers' = EstablishedPeers.delete peeraddr
                                    establishedPeers
              (fuzz, fuzzRng')  = randomR (-2, 2 :: Double) fuzzRng
              delay             = realToFrac $ fuzz + baseReconnectDelay
              knownPeers'       = if peeraddr `KnownPeers.member` knownPeers
                                     then KnownPeers.setConnectTime
                                            (Set.singleton peeraddr)
                                            (delay `addTime` now)
                                          $ snd $ KnownPeers.incrementFailCount
                                            peeraddr
                                            knownPeers
                                     else
                                       -- Apparently the governor can remove
                                       -- the peer we failed to promote from the
                                       -- set of known peers before we can process
                                       -- the failure.
                                       knownPeers in
          Decision {
          decisionTrace = TracePromoteWarmFailed targetNumberOfActivePeers
                                                 (Set.size activePeers)
                                                 peeraddr e,
          decisionState = st {
                            inProgressPromoteWarm = Set.delete peeraddr
                                                      (inProgressPromoteWarm st),
                            knownPeers            = knownPeers',
                            establishedPeers      = establishedPeers',
                            fuzzRng               = fuzzRng'
                          },
          decisionJobs  = []
        }

    job :: m (Completion m peeraddr peerconn)
    job = do
      activatePeerConnection peerconn
      return $ Completion $ \st@PeerSelectionState {
                               activePeers,
                               targets = PeerSelectionTargets {
                                           targetNumberOfActivePeers
                                         }
                             }
                           _now ->
        assert (peeraddr `EstablishedPeers.member` establishedPeers st) $
        assert (peeraddr `KnownPeers.member` knownPeers st) $
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
          [ pickPeers st
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
      selectedToDemote <- pickPeers st
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

-- | Reconnect delay for peers which asynchronously transitioned to cold state.
--
reconnectDelay :: DiffTime
reconnectDelay = 10
--TODO: make this a policy param

jobDemoteActivePeer :: forall peeraddr peerconn m.
                       (Monad m, Ord peeraddr)
                    => PeerSelectionActions peeraddr peerconn m
                    -> peeraddr
                    -> peerconn
                    -> Job () m (Completion m peeraddr peerconn)
jobDemoteActivePeer PeerSelectionActions{peerStateActions = PeerStateActions {deactivatePeerConnection}}
                    peeraddr peerconn =
    Job job handler () "demoteActivePeer"
  where
    handler :: SomeException -> m (Completion m peeraddr peerconn)
    handler e = return $
      -- It's quite bad if demoting fails. The peer is cold so
      -- remove if from the set of established and hot peers.
      Completion $ \st@PeerSelectionState {
                      activePeers,
                      establishedPeers,
                      inProgressDemoteHot,
                      knownPeers,
                      targets = PeerSelectionTargets {
                                  targetNumberOfActivePeers
                                },
                      fuzzRng
                    }
                    now ->
        let (rFuzz, fuzzRng')     = randomR (-2, 2 :: Double) fuzzRng
            peerSet               = Set.singleton peeraddr
            activePeers'          = Set.delete peeraddr activePeers
            inProgressDemoteHot'  = Set.delete peeraddr inProgressDemoteHot
            knownPeers'           = KnownPeers.setConnectTime
                                     peerSet
                                     ((realToFrac rFuzz + reconnectDelay)
                                      `addTime` now)
                                   . Set.foldr'
                                     ((snd .) . KnownPeers.incrementFailCount)
                                     knownPeers
                                   $ peerSet
            establishedPeers'     = EstablishedPeers.deletePeers
                                     peerSet
                                     establishedPeers in
        Decision {
        decisionTrace = TraceDemoteHotFailed targetNumberOfActivePeers
                                             (Set.size activePeers) peeraddr e,
        decisionState = st {
                          inProgressDemoteHot = inProgressDemoteHot',
                          fuzzRng = fuzzRng',
                          activePeers = activePeers',
                          knownPeers = knownPeers',
                          establishedPeers = establishedPeers'
                        },
        decisionJobs  = []
      }

    job :: m (Completion m peeraddr peerconn)
    job = do
      deactivatePeerConnection peerconn
      return $ Completion $ \st@PeerSelectionState {
                                activePeers,
                                knownPeers,
                                targets = PeerSelectionTargets {
                                            targetNumberOfActivePeers
                                          }
                             }
                             _now ->
        assert (peeraddr `EstablishedPeers.member` establishedPeers st) $
        let activePeers' = Set.delete peeraddr activePeers
            knownPeers'  = setTepidFlag peeraddr knownPeers in
        Decision {
          decisionTrace = TraceDemoteHotDone targetNumberOfActivePeers
                                             (Set.size activePeers')
                                             peeraddr,
          decisionState = st {
                            activePeers         = activePeers',
                            knownPeers          = knownPeers',
                            inProgressDemoteHot = Set.delete peeraddr
                                                    (inProgressDemoteHot st)
                          },
          decisionJobs  = []
        }
