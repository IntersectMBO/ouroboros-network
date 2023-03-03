{-# LANGUAGE FlexibleContexts    #-}
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

import           Control.Applicative (Alternative)
import           Control.Concurrent.JobPool (Job (..))
import           Control.Exception (SomeException, assert)
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTime.SI
import           Control.Monad.Class.MonadTimer.SI
import           System.Random (randomR)

import           Ouroboros.Network.PeerSelection.Governor.Types
import qualified Ouroboros.Network.PeerSelection.State.EstablishedPeers as EstablishedPeers
import           Ouroboros.Network.PeerSelection.State.KnownPeers (setTepidFlag)
import qualified Ouroboros.Network.PeerSelection.State.KnownPeers as KnownPeers
import           Ouroboros.Network.PeerSelection.State.LocalRootPeers
                     (HotValency (..))
import qualified Ouroboros.Network.PeerSelection.State.LocalRootPeers as LocalRootPeers


----------------------------
-- Active peers below target
--


-- | If we are below the target of /hot peers/ we promote some of the /warm
-- peers/ according to 'policyPickWarmPeersToPromote' policy.
--
belowTarget :: forall peeraddr peerconn m.
               ( Alternative (STM m)
               , MonadDelay m
               , MonadSTM m
               , Ord peeraddr
               )
            => PeerSelectionActions peeraddr peerconn m
            -> MkGuardedDecision peeraddr peerconn m
belowTarget = belowTargetBigLedgerPeers
           <> belowTargetLocal
           <> belowTargetOther


belowTargetBigLedgerPeers :: forall peeraddr peerconn m.
                             (MonadDelay m, MonadSTM m, Ord peeraddr)
                          => PeerSelectionActions peeraddr peerconn m
                          -> MkGuardedDecision peeraddr peerconn m
belowTargetBigLedgerPeers actions
                          policy@PeerSelectionPolicy {
                            policyPickWarmPeersToPromote
                          }
                          st@PeerSelectionState {
                            bigLedgerPeers,
                            establishedPeers,
                            activePeers,
                            inProgressPromoteWarm,
                            inProgressDemoteWarm,
                            targets = PeerSelectionTargets {
                                        targetNumberOfActiveBigLedgerPeers
                                      }
                          }
    -- Are we below the target for number of active peers?
  | numActiveBigLedgerPeers + numPromoteInProgressBigLedgerPeers
    < targetNumberOfActiveBigLedgerPeers

    -- Are there any warm peers we could pick to promote?
  , let availableToPromote :: Set peeraddr
        availableToPromote = EstablishedPeers.readyPeers establishedPeers
                               `Set.intersection` bigLedgerPeers
                               Set.\\ activePeers
                               Set.\\ inProgressPromoteWarm
                               Set.\\ inProgressDemoteWarm
        numPeersToPromote = targetNumberOfActiveBigLedgerPeers
                          - numActiveBigLedgerPeers
                          - numPromoteInProgressBigLedgerPeers
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
        decisionTrace = [TracePromoteWarmBigLedgerPeers
                           targetNumberOfActiveBigLedgerPeers
                           numActiveBigLedgerPeers
                           selectedToPromote],
        decisionState = st {
                          inProgressPromoteWarm = inProgressPromoteWarm
                                               <> selectedToPromote
                        },
        decisionJobs  = [ jobPromoteWarmPeer actions policy peeraddr peerconn
                        | (peeraddr, peerconn) <- Map.assocs selectedToPromote' ]
      }

  | otherwise
  = GuardedSkip Nothing
  where
    numActiveBigLedgerPeers
      = Set.size $ activePeers
                   `Set.intersection` bigLedgerPeers
    numPromoteInProgressBigLedgerPeers
      = Set.size $ inProgressPromoteWarm
                   `Set.intersection` bigLedgerPeers


belowTargetLocal :: forall peeraddr peerconn m.
                    (MonadDelay m, MonadSTM m, Ord peeraddr)
                 => PeerSelectionActions peeraddr peerconn m
                 -> MkGuardedDecision peeraddr peerconn m
belowTargetLocal actions
                 policy@PeerSelectionPolicy {
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
          , (HotValency hotTarget, members, membersActive) <- groupsBelowTarget
          , let membersAvailableToPromote = Set.intersection
                                              members availableToPromote
                numMembersToPromote       = hotTarget
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
        decisionTrace = [TracePromoteWarmLocalPeers
                           [ (target, Set.size membersActive)
                           | (target, _, membersActive) <- groupsBelowTarget ]
                           selectedToPromote],
        decisionState = st {
                          inProgressPromoteWarm = inProgressPromoteWarm
                                               <> selectedToPromote
                        },
        decisionJobs  = [ jobPromoteWarmPeer actions policy peeraddr peerconn
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
      [ (hotValency, members, membersActive)
      | (hotValency, _, members) <- LocalRootPeers.toGroupSets localRootPeers
      , let membersActive = members `Set.intersection` activePeers
      , Set.size membersActive < getHotValency hotValency
      ]

belowTargetOther :: forall peeraddr peerconn m.
                    (MonadDelay m, MonadSTM m, Ord peeraddr)
                 => PeerSelectionActions peeraddr peerconn m
                 -> MkGuardedDecision peeraddr peerconn m
belowTargetOther actions
                 policy@PeerSelectionPolicy {
                   policyPickWarmPeersToPromote
                 }
                 st@PeerSelectionState {
                   bigLedgerPeers,
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
                               Set.\\ bigLedgerPeers
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
        decisionTrace = [TracePromoteWarmPeers
                           targetNumberOfActivePeers
                           numActivePeers
                           selectedToPromote],
        decisionState = st {
                          inProgressPromoteWarm = inProgressPromoteWarm
                                               <> selectedToPromote
                        },
        decisionJobs  = [ jobPromoteWarmPeer actions policy peeraddr peerconn
                        | (peeraddr, peerconn) <- Map.assocs selectedToPromote' ]
      }

    -- If we could promote except that there are no peers currently available
    -- then we return the next wakeup time (if any)
  | numActivePeers + numPromoteInProgress < targetNumberOfActivePeers
  = GuardedSkip (Min <$> EstablishedPeers.minActivateTime establishedPeers)

  | otherwise
  = GuardedSkip Nothing
  where
    numActivePeers       = Set.size $ activePeers
                               Set.\\ bigLedgerPeers
    numPromoteInProgress = Set.size $ inProgressPromoteWarm
                               Set.\\ bigLedgerPeers


jobPromoteWarmPeer :: forall peeraddr peerconn m.
                      (MonadDelay m, Ord peeraddr)
                   => PeerSelectionActions peeraddr peerconn m
                   -> PeerSelectionPolicy peeraddr m
                   -> peeraddr
                   -> peerconn
                   -> Job () m (Completion m peeraddr peerconn)
jobPromoteWarmPeer PeerSelectionActions{peerStateActions = PeerStateActions {activatePeerConnection}}
                   PeerSelectionPolicy { policyErrorDelay }
                   peeraddr peerconn =
    Job job handler () "promoteWarmPeer"
  where
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
                                 bigLedgerPeers,
                                 activePeers,
                                 establishedPeers,
                                 knownPeers,
                                 fuzzRng,
                                 targets = PeerSelectionTargets {
                                             targetNumberOfActivePeers,
                                             targetNumberOfActiveBigLedgerPeers
                                           }
                               }
                      now ->
          -- TODO: this is a temporary fix, which will by addressed by
          -- #3460
          if peeraddr `Set.member` inProgressPromoteWarm st
            then let establishedPeers' = EstablishedPeers.delete peeraddr
                                           establishedPeers
                     (fuzz, fuzzRng')  = randomR (-2, 2 :: Double) fuzzRng
                     delay             = realToFrac fuzz + policyErrorDelay
                     knownPeers'       = if peeraddr `KnownPeers.member` knownPeers
                                            then KnownPeers.setConnectTimes
                                                   (Map.singleton
                                                     peeraddr
                                                     (delay `addTime` now))
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
                   decisionTrace = if peeraddr `Set.member` bigLedgerPeers
                                   then [TracePromoteWarmBigLedgerPeerFailed
                                          targetNumberOfActiveBigLedgerPeers
                                          (Set.size $ activePeers
                                                      `Set.intersection`
                                                      bigLedgerPeers)
                                          peeraddr e]
                                   else [TracePromoteWarmFailed
                                          targetNumberOfActivePeers
                                          (Set.size $ activePeers
                                               Set.\\ bigLedgerPeers)
                                          peeraddr e],
                   decisionState = st {
                                     inProgressPromoteWarm = Set.delete peeraddr
                                                               (inProgressPromoteWarm st),
                                     knownPeers            = knownPeers',
                                     establishedPeers      = establishedPeers',
                                     fuzzRng               = fuzzRng'
                                   },
                   decisionJobs  = []
                 }
            else Decision {
                   decisionTrace = if peeraddr `Set.member` bigLedgerPeers
                                   then [TracePromoteWarmBigLedgerPeerAborted
                                          targetNumberOfActiveBigLedgerPeers
                                          (Set.size $ activePeers
                                                      `Set.intersection`
                                                      bigLedgerPeers)
                                          peeraddr]
                                   else [TracePromoteWarmAborted
                                          targetNumberOfActivePeers
                                          (Set.size $ activePeers
                                               Set.\\ bigLedgerPeers)
                                          peeraddr],
                   decisionState = st,
                   decisionJobs  = []
                 }


    job :: m (Completion m peeraddr peerconn)
    job = do
      activatePeerConnection peerconn
      return $ Completion $ \st@PeerSelectionState {
                               bigLedgerPeers,
                               activePeers,
                               targets = PeerSelectionTargets {
                                           targetNumberOfActivePeers
                                         }
                             }
                           _now ->

        if peeraddr `EstablishedPeers.member` establishedPeers st
          then
            let activePeers' = Set.insert peeraddr activePeers in
            Decision {
              decisionTrace = if peeraddr `Set.member` bigLedgerPeers
                              then [TracePromoteWarmBigLedgerPeerDone
                                     targetNumberOfActivePeers
                                     (Set.size $ activePeers'
                                                 `Set.intersection`
                                                 bigLedgerPeers)
                                     peeraddr]
                              else [TracePromoteWarmDone
                                     targetNumberOfActivePeers
                                     (Set.size $ activePeers'
                                          Set.\\ bigLedgerPeers)
                                     peeraddr],
              decisionState = st {
                                activePeers           = activePeers',
                                inProgressPromoteWarm = Set.delete peeraddr
                                                          (inProgressPromoteWarm st)
                              },
              decisionJobs  = []
            }
          else
            Decision {
              decisionTrace = if peeraddr `Set.member` bigLedgerPeers
                              then [TracePromoteWarmBigLedgerPeerAborted
                                     targetNumberOfActivePeers
                                     (Set.size $ activePeers
                                                 `Set.intersection`
                                                 bigLedgerPeers)
                                     peeraddr]
                              else [TracePromoteWarmAborted
                                     targetNumberOfActivePeers
                                     (Set.size $ activePeers
                                          Set.\\ bigLedgerPeers)
                                     peeraddr],
              decisionState = st,
              decisionJobs  = []
            }

----------------------------
-- Active peers above target
--

-- | If we are above the target of /hot peers/ we demote some hot peers to be
-- /warm peers/, according to 'policyPickHotPeersToDemote'.
--
aboveTarget :: forall peeraddr peerconn m.
               ( Alternative (STM m)
               , MonadSTM m
               , Ord peeraddr
               )
            => PeerSelectionActions peeraddr peerconn m
            -> MkGuardedDecision peeraddr peerconn m
aboveTarget = aboveTargetBigLedgerPeers
           <> aboveTargetLocal
           <> aboveTargetOther
  -- Start with big ledger peers then local root targets, then the general
  -- target. Note that making progress downwards with the local root targets
  -- makes progress for the general target too.


aboveTargetBigLedgerPeers :: forall peeraddr peerconn m.
                             (MonadSTM m, Ord peeraddr)
                          => PeerSelectionActions peeraddr peerconn m
                          -> MkGuardedDecision peeraddr peerconn m
aboveTargetBigLedgerPeers actions
                          policy@PeerSelectionPolicy {
                            policyPickHotPeersToDemote
                          }
                          st@PeerSelectionState {
                            bigLedgerPeers,
                            localRootPeers,
                            establishedPeers,
                            activePeers,
                            inProgressDemoteHot,
                            targets = PeerSelectionTargets {
                                        targetNumberOfActiveBigLedgerPeers
                                      }
                          }
    -- Are we above the general target for number of active peers?
  | numActiveBigLedgerPeers > targetNumberOfActiveBigLedgerPeers

    -- Would we demote any if we could?
  , let numPeersToDemote = numActiveBigLedgerPeers
                         - targetNumberOfActiveBigLedgerPeers
                         - numDemoteInProgressBigLedgerPeers
  , numPeersToDemote > 0

    -- Are there any hot peers we actually can pick to demote?
    -- For the moment we say we cannot demote local root peers.
    -- TODO: review this decision. If we want to be able to demote local root
    -- peers, e.g. for churn and improved selection, then we'll need an extra
    -- mechanism to avoid promotion/demotion loops for local peers.
  , let availableToDemote = activePeers
                              `Set.intersection` bigLedgerPeers
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
        decisionTrace = [TraceDemoteHotBigLedgerPeers
                           targetNumberOfActiveBigLedgerPeers
                           numActiveBigLedgerPeers
                           selectedToDemote],
        decisionState = st {
                          inProgressDemoteHot = inProgressDemoteHot
                                             <> selectedToDemote
                        },
        decisionJobs  = [ jobDemoteActivePeer actions policy peeraddr peerconn
                        | (peeraddr, peerconn) <- Map.assocs selectedToDemote' ]
      }

  | otherwise
  = GuardedSkip Nothing
  where
    numActiveBigLedgerPeers
      = Set.size $ activePeers
                   `Set.intersection`
                   bigLedgerPeers
    numDemoteInProgressBigLedgerPeers
      = Set.size $ inProgressDemoteHot
                   `Set.intersection`
                   bigLedgerPeers


aboveTargetLocal :: forall peeraddr peerconn m.
                    (MonadSTM m, Ord peeraddr)
                 => PeerSelectionActions peeraddr peerconn m
                 -> MkGuardedDecision peeraddr peerconn m
aboveTargetLocal actions
                 policy@PeerSelectionPolicy {
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
          [ (hotValency, members, membersActive)
          | (hotValency, _, members) <- LocalRootPeers.toGroupSets localRootPeers
          , let membersActive = members `Set.intersection` activePeers
          , Set.size membersActive > getHotValency hotValency
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
          , (HotValency hotTarget, members, membersActive) <- groupsAboveTarget
          , let membersAvailableToDemote = Set.intersection
                                             members availableToDemote
                numMembersToDemote       = Set.size membersActive
                                         - hotTarget
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
        decisionTrace = [TraceDemoteLocalHotPeers
                           [ (target, Set.size membersActive)
                           | (target, _, membersActive) <- groupsAboveTarget ]
                           selectedToDemote],
        decisionState = st {
                          inProgressDemoteHot = inProgressDemoteHot
                                             <> selectedToDemote
                        },
        decisionJobs  = [ jobDemoteActivePeer actions policy peeraddr peerconn
                        | (peeraddr, peerconn) <- Map.assocs selectedToDemote' ]
      }

  | otherwise
  = GuardedSkip Nothing


aboveTargetOther :: forall peeraddr peerconn m.
                    (MonadSTM m, Ord peeraddr)
                 => PeerSelectionActions peeraddr peerconn m
                 -> MkGuardedDecision peeraddr peerconn m
aboveTargetOther actions
                 policy@PeerSelectionPolicy {
                   policyPickHotPeersToDemote
                 }
                 st@PeerSelectionState {
                   bigLedgerPeers,
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
                              Set.\\ bigLedgerPeers
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
        decisionTrace = [TraceDemoteHotPeers
                           targetNumberOfActivePeers
                           numActivePeers
                           selectedToDemote],
        decisionState = st {
                          inProgressDemoteHot = inProgressDemoteHot
                                             <> selectedToDemote
                        },
        decisionJobs  = [ jobDemoteActivePeer actions policy peeraddr peerconn
                        | (peeraddr, peerconn) <- Map.assocs selectedToDemote' ]
      }

  | otherwise
  = GuardedSkip Nothing
  where
    numActivePeers      = Set.size $ activePeers
                              Set.\\ bigLedgerPeers
    numDemoteInProgress = Set.size $ inProgressDemoteHot
                              Set.\\ bigLedgerPeers


jobDemoteActivePeer :: forall peeraddr peerconn m.
                       (Monad m, Ord peeraddr)
                    => PeerSelectionActions peeraddr peerconn m
                    -> PeerSelectionPolicy peeraddr m
                    -> peeraddr
                    -> peerconn
                    -> Job () m (Completion m peeraddr peerconn)
jobDemoteActivePeer PeerSelectionActions{peerStateActions = PeerStateActions {deactivatePeerConnection}}
                    PeerSelectionPolicy { policyErrorDelay }
                    peeraddr peerconn =
    Job job handler () "demoteActivePeer"
  where
    handler :: SomeException -> m (Completion m peeraddr peerconn)
    handler e = return $
      -- It's quite bad if demoting fails. The peer is cold so
      -- remove if from the set of established and hot peers.
      Completion $ \st@PeerSelectionState {
                      bigLedgerPeers,
                      activePeers,
                      establishedPeers,
                      inProgressDemoteHot,
                      knownPeers,
                      targets = PeerSelectionTargets {
                                  targetNumberOfActivePeers,
                                  targetNumberOfActiveBigLedgerPeers
                                },
                      fuzzRng
                    }
                    now ->
        let (rFuzz, fuzzRng')     = randomR (-2, 2 :: Double) fuzzRng
            peerSet               = Set.singleton peeraddr
            activePeers'          = Set.delete peeraddr activePeers
            inProgressDemoteHot'  = Set.delete peeraddr inProgressDemoteHot
            knownPeers'           = KnownPeers.setConnectTimes
                                      (Map.singleton
                                        peeraddr
                                        ((realToFrac rFuzz + policyErrorDelay) `addTime` now))
                                  . Set.foldr'
                                      ((snd .) . KnownPeers.incrementFailCount)
                                      knownPeers
                                  $ peerSet
            establishedPeers'     = EstablishedPeers.deletePeers
                                     peerSet
                                     establishedPeers in
        Decision {
        decisionTrace = if peeraddr `Set.member` bigLedgerPeers
                        then [TraceDemoteHotBigLedgerPeerFailed
                               targetNumberOfActiveBigLedgerPeers
                               (Set.size $ activePeers
                                           `Set.intersection`
                                           bigLedgerPeers)
                               peeraddr e]
                        else [TraceDemoteHotFailed
                               targetNumberOfActivePeers
                               (Set.size $ activePeers
                                    Set.\\ bigLedgerPeers)
                               peeraddr e],
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
                                bigLedgerPeers,
                                activePeers,
                                knownPeers,
                                targets = PeerSelectionTargets {
                                            targetNumberOfActivePeers,
                                            targetNumberOfActiveBigLedgerPeers
                                          }
                             }
                             _now ->
        assert (peeraddr `EstablishedPeers.member` establishedPeers st) $
        let activePeers' = Set.delete peeraddr activePeers
            knownPeers'  = setTepidFlag peeraddr knownPeers in
        Decision {
          decisionTrace = if peeraddr `Set.member` bigLedgerPeers
                          then [TraceDemoteHotBigLedgerPeerDone
                                 targetNumberOfActiveBigLedgerPeers
                                 (Set.size $ activePeers'
                                             `Set.intersection`
                                             bigLedgerPeers)
                                 peeraddr]
                          else [TraceDemoteHotDone
                                 targetNumberOfActivePeers
                                 (Set.size $ activePeers'
                                      Set.\\ bigLedgerPeers)
                                 peeraddr],
          decisionState = st {
                            activePeers         = activePeers',
                            knownPeers          = knownPeers',
                            inProgressDemoteHot = Set.delete peeraddr
                                                    (inProgressDemoteHot st)
                          },
          decisionJobs  = []
        }
