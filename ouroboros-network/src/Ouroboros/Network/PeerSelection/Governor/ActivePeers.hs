{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.PeerSelection.Governor.ActivePeers
  ( belowTarget
  , aboveTarget
  , jobDemoteActivePeer
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Stack (HasCallStack)

import Control.Applicative (Alternative)
import Control.Concurrent.JobPool (Job (..))
import Control.Exception (SomeException, assert)
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import System.Random (randomR)

import Ouroboros.Network.PeerSelection.Governor.Types
import Ouroboros.Network.PeerSelection.LedgerPeers.Type (IsBigLedgerPeer (..))
import Ouroboros.Network.PeerSelection.PublicRootPeers qualified as PublicRootPeers
import Ouroboros.Network.PeerSelection.State.EstablishedPeers qualified as EstablishedPeers
import Ouroboros.Network.PeerSelection.State.KnownPeers (setTepidFlag)
import Ouroboros.Network.PeerSelection.State.KnownPeers qualified as KnownPeers
import Ouroboros.Network.PeerSelection.State.LocalRootPeers (HotValency (..))
import Ouroboros.Network.PeerSelection.State.LocalRootPeers qualified as LocalRootPeers


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
               , HasCallStack
               )
            => PeerSelectionActions peeraddr peerconn m
            -> MkGuardedDecision peeraddr peerconn m
belowTarget = belowTargetBigLedgerPeers
           <> belowTargetLocal
           <> belowTargetOther

-- | If we are below the target of /hot big ledger peers peers/ we promote some
-- of the /warm peers/ according to 'policyPickWarmPeersToPromote' policy.
--
-- It should be noted if the node is in bootstrap mode (i.e. in a sensitive
-- state) then this monitoring action will be disabled.
--
belowTargetBigLedgerPeers :: forall peeraddr peerconn m.
                             (MonadDelay m, MonadSTM m, Ord peeraddr)
                          => PeerSelectionActions peeraddr peerconn m
                          -> MkGuardedDecision peeraddr peerconn m
belowTargetBigLedgerPeers actions
                          policy@PeerSelectionPolicy {
                            policyPickWarmPeersToPromote
                          }
                          st@PeerSelectionState {
                            publicRootPeers,
                            establishedPeers,
                            activePeers,
                            inProgressPromoteWarm,
                            inProgressDemoteWarm,
                            inProgressDemoteToCold,
                            targets = PeerSelectionTargets {
                                        targetNumberOfActiveBigLedgerPeers
                                      },
                            ledgerStateJudgement,
                            bootstrapPeersFlag
                          }
    -- Are we below the target for number of active peers?
  | numActiveBigLedgerPeers + numPromoteInProgressBigLedgerPeers
    < targetNumberOfActiveBigLedgerPeers

    -- Are there any warm peers we could pick to promote?
  , let availableToPromote :: Set peeraddr
        availableToPromote = EstablishedPeers.readyPeers establishedPeers
                               `Set.intersection` bigLedgerPeersSet
                               Set.\\ activePeers
                               Set.\\ inProgressPromoteWarm
                               Set.\\ inProgressDemoteWarm
                               Set.\\ inProgressDemoteToCold
        numPeersToPromote = targetNumberOfActiveBigLedgerPeers
                          - numActiveBigLedgerPeers
                          - numPromoteInProgressBigLedgerPeers
  , not (Set.null availableToPromote)
  , numPeersToPromote > 0
  -- Are we in a insensitive state, i.e. using bootstrap peers?
  , not (requiresBootstrapPeers bootstrapPeersFlag ledgerStateJudgement)
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
        decisionJobs  = [ jobPromoteWarmPeer actions policy peeraddr IsBigLedgerPeer peerconn
                        | (peeraddr, peerconn) <- Map.assocs selectedToPromote' ]
      }

    -- If we could promote except that there are no peers currently available
    -- then we return the next wakeup time (if any)
  | numActiveBigLedgerPeers + numPromoteInProgressBigLedgerPeers < targetNumberOfActiveBigLedgerPeers
  = GuardedSkip (EstablishedPeers.minActivateTime establishedPeers (`Set.member` bigLedgerPeersSet))

  | otherwise
  = GuardedSkip Nothing
  where
    bigLedgerPeersSet = PublicRootPeers.getBigLedgerPeers publicRootPeers
    PeerSelectionCounters {
        numberOfActiveBigLedgerPeers         = numActiveBigLedgerPeers,
        numberOfWarmBigLedgerPeersPromotions = numPromoteInProgressBigLedgerPeers
      }
      =
      peerSelectionStateToCounters st


belowTargetLocal :: forall peeraddr peerconn m.
                    (MonadDelay m, MonadSTM m, Ord peeraddr, HasCallStack)
                 => PeerSelectionActions peeraddr peerconn m
                 -> MkGuardedDecision peeraddr peerconn m
belowTargetLocal actions
                 policy@PeerSelectionPolicy {
                   policyPickWarmPeersToPromote
                 }
                 st@PeerSelectionState {
                   publicRootPeers,
                   localRootPeers,
                   establishedPeers,
                   activePeers,
                   inProgressPromoteWarm,
                   inProgressDemoteWarm,
                   inProgressDemoteToCold
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
                     Set.\\ inProgressDemoteToCold
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
        decisionJobs  = [ jobPromoteWarmPeer actions policy peeraddr IsNotBigLedgerPeer peerconn
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
  = GuardedSkip (EstablishedPeers.minActivateTime establishedPeers (`Set.notMember` bigLedgerPeersSet))

  | otherwise
  = GuardedSkip Nothing
  where
    bigLedgerPeersSet = PublicRootPeers.getBigLedgerPeers publicRootPeers
    groupsBelowTarget =
      [ (hotValency, members, membersActive)
      | (hotValency, _, members) <- LocalRootPeers.toGroupSets localRootPeers
      , let membersActive = members `Set.intersection` activePeers
      , Set.size membersActive < getHotValency hotValency
      ]

belowTargetOther :: forall peeraddr peerconn m.
                    (MonadDelay m, MonadSTM m, Ord peeraddr,
                     HasCallStack)
                 => PeerSelectionActions peeraddr peerconn m
                 -> MkGuardedDecision peeraddr peerconn m
belowTargetOther actions
                 policy@PeerSelectionPolicy {
                   policyPickWarmPeersToPromote
                 }
                 st@PeerSelectionState {
                   localRootPeers,
                   establishedPeers,
                   activePeers,
                   inProgressPromoteWarm,
                   inProgressDemoteToCold,
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
                               Set.\\ inProgressDemoteToCold
                               Set.\\ LocalRootPeers.keysSet localRootPeers
                               Set.\\ bigLedgerPeersSet
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
        decisionJobs  = [ jobPromoteWarmPeer actions policy peeraddr IsNotBigLedgerPeer peerconn
                        | (peeraddr, peerconn) <- Map.assocs selectedToPromote' ]
      }

    -- If we could promote except that there are no peers currently available
    -- then we return the next wakeup time (if any)
  | numActivePeers + numPromoteInProgress < targetNumberOfActivePeers
  = GuardedSkip (EstablishedPeers.minActivateTime establishedPeers (`Set.notMember` bigLedgerPeersSet))

  | otherwise
  = GuardedSkip Nothing
  where
    PeerSelectionView {
        viewActivePeers         = (_, numActivePeers),
        viewWarmPeersPromotions = (_, numPromoteInProgress),
        viewKnownBigLedgerPeers = (bigLedgerPeersSet, _)
      }
      =
      peerSelectionStateToView st


jobPromoteWarmPeer :: forall peeraddr peerconn m.
                      (MonadDelay m, Ord peeraddr)
                   => PeerSelectionActions peeraddr peerconn m
                   -> PeerSelectionPolicy peeraddr m
                   -> peeraddr
                   -> IsBigLedgerPeer
                   -> peerconn
                   -> Job () m (Completion m peeraddr peerconn)
jobPromoteWarmPeer PeerSelectionActions{peerStateActions = PeerStateActions {activatePeerConnection}}
                   PeerSelectionPolicy { policyErrorDelay }
                   peeraddr isBigLedgerPeer peerconn =
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
                                 publicRootPeers,
                                 activePeers,
                                 establishedPeers,
                                 knownPeers,
                                 stdGen,
                                 targets = PeerSelectionTargets {
                                             targetNumberOfActivePeers,
                                             targetNumberOfActiveBigLedgerPeers
                                           }
                               }
                      now ->
          -- TODO: this is a temporary fix, which will by addressed by
          -- #3460
          let bigLedgerPeersSet = PublicRootPeers.getBigLedgerPeers publicRootPeers
           in if peeraddr `Set.member` inProgressPromoteWarm st
                 then let establishedPeers' = EstablishedPeers.delete peeraddr
                                                establishedPeers
                          (fuzz, stdGen')  = randomR (-2, 2 :: Double) stdGen
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
                                                   knownPeers
                       in
                      Decision {
                        decisionTrace = if peeraddr `Set.member` bigLedgerPeersSet
                                        then [TracePromoteWarmBigLedgerPeerFailed
                                               targetNumberOfActiveBigLedgerPeers
                                               (Set.size $ activePeers
                                                           `Set.intersection`
                                                           bigLedgerPeersSet)
                                               peeraddr e]
                                        else [TracePromoteWarmFailed
                                               targetNumberOfActivePeers
                                               (Set.size $ activePeers
                                                    Set.\\ bigLedgerPeersSet)
                                               peeraddr e],
                        decisionState = st {
                                          inProgressPromoteWarm = Set.delete peeraddr
                                                                    (inProgressPromoteWarm st),
                                          knownPeers            = knownPeers',
                                          establishedPeers      = establishedPeers',
                                          stdGen                = stdGen'
                                        },
                        decisionJobs  = []
                      }
                 else Decision {
                        decisionTrace = if peeraddr `Set.member` bigLedgerPeersSet
                                           then [TracePromoteWarmBigLedgerPeerAborted
                                                  targetNumberOfActiveBigLedgerPeers
                                                  (Set.size $ activePeers
                                                              `Set.intersection`
                                                              bigLedgerPeersSet)
                                                  peeraddr]
                                           else [TracePromoteWarmAborted
                                                  targetNumberOfActivePeers
                                                  (Set.size $ activePeers
                                                       Set.\\ bigLedgerPeersSet)
                                                  peeraddr],
                        decisionState = st,
                        decisionJobs  = []
                      }


    job :: m (Completion m peeraddr peerconn)
    job = do
      activatePeerConnection isBigLedgerPeer peerconn
      return $ Completion $ \st@PeerSelectionState {
                               publicRootPeers,
                               activePeers,
                               targets = PeerSelectionTargets {
                                           targetNumberOfActivePeers
                                         }
                             }
                           _now ->
        let bigLedgerPeersSet = PublicRootPeers.getBigLedgerPeers publicRootPeers
         in if peeraddr `EstablishedPeers.member` establishedPeers st
               then let activePeers' = Set.insert peeraddr activePeers in
                    Decision {
                      decisionTrace = if peeraddr `Set.member` bigLedgerPeersSet
                                      then [TracePromoteWarmBigLedgerPeerDone
                                             targetNumberOfActivePeers
                                             (Set.size $ activePeers'
                                                         `Set.intersection`
                                                         bigLedgerPeersSet)
                                             peeraddr]
                                      else [TracePromoteWarmDone
                                             targetNumberOfActivePeers
                                             (Set.size $ activePeers'
                                                  Set.\\ bigLedgerPeersSet)
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
                   decisionTrace = if peeraddr `Set.member` bigLedgerPeersSet
                                   then [TracePromoteWarmBigLedgerPeerAborted
                                          targetNumberOfActivePeers
                                          (Set.size $ activePeers
                                                      `Set.intersection`
                                                      bigLedgerPeersSet)
                                          peeraddr]
                                   else [TracePromoteWarmAborted
                                          targetNumberOfActivePeers
                                          (Set.size $ activePeers
                                               Set.\\ bigLedgerPeersSet)
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
               , HasCallStack
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
                          PeerSelectionPolicy {
                            policyPickHotPeersToDemote
                          }
                          st@PeerSelectionState {
                            publicRootPeers,
                            localRootPeers,
                            establishedPeers,
                            activePeers,
                            inProgressDemoteHot,
                            inProgressDemoteToCold,
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
                              `Set.intersection` bigLedgerPeersSet
                              Set.\\ inProgressDemoteHot
                              Set.\\ inProgressDemoteToCold
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
        decisionJobs  = [ jobDemoteActivePeer actions peeraddr peerconn
                        | (peeraddr, peerconn) <- Map.assocs selectedToDemote' ]
      }

  | otherwise
  = GuardedSkip Nothing
  where
    bigLedgerPeersSet = PublicRootPeers.getBigLedgerPeers publicRootPeers
    PeerSelectionCounters {
        numberOfActiveBigLedgerPeers          = numActiveBigLedgerPeers,
        numberOfActiveBigLedgerPeersDemotions = numDemoteInProgressBigLedgerPeers
      }
      =
      peerSelectionStateToCounters st


aboveTargetLocal :: forall peeraddr peerconn m.
                    (MonadSTM m, Ord peeraddr, HasCallStack)
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
                   inProgressDemoteHot,
                   inProgressDemoteToCold
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
                                       Set.\\ inProgressDemoteToCold
                numDemoteInProgress = Set.size inProgressDemoteHot
                                    + Set.size (inProgressDemoteToCold
                                                `Set.intersection`
                                                activePeers)
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
        decisionJobs  = [ jobDemoteActivePeer actions peeraddr peerconn
                        | (peeraddr, peerconn) <- Map.assocs selectedToDemote' ]
      }

  | otherwise
  = GuardedSkip Nothing


aboveTargetOther :: forall peeraddr peerconn m.
                    (MonadSTM m, Ord peeraddr, HasCallStack)
                 => PeerSelectionActions peeraddr peerconn m
                 -> MkGuardedDecision peeraddr peerconn m
aboveTargetOther actions
                 PeerSelectionPolicy {
                   policyPickHotPeersToDemote
                 }
                 st@PeerSelectionState {
                   publicRootPeers,
                   localRootPeers,
                   establishedPeers,
                   activePeers,
                   inProgressDemoteHot,
                   inProgressDemoteToCold,
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
                         - (Set.size inProgressDemoteToCold)
  , numPeersToDemote > 0

    -- Are there any hot peers we actually can pick to demote?
    -- For the moment we say we cannot demote local root peers.
    -- TODO: review this decision. If we want to be able to demote local root
    -- peers, e.g. for churn and improved selection, then we'll need an extra
    -- mechanism to avoid promotion/demotion loops for local peers.
  , let availableToDemote = activePeers
                              Set.\\ inProgressDemoteHot
                              Set.\\ LocalRootPeers.keysSet localRootPeers
                              Set.\\ bigLedgerPeersSet
                              Set.\\ inProgressDemoteToCold
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
        decisionJobs  = [ jobDemoteActivePeer actions peeraddr peerconn
                        | (peeraddr, peerconn) <- Map.assocs selectedToDemote' ]
      }

  | otherwise
  = GuardedSkip Nothing
  where
    bigLedgerPeersSet   = PublicRootPeers.getBigLedgerPeers publicRootPeers
    PeerSelectionCounters {
        numberOfActivePeers          = numActivePeers,
        numberOfActivePeersDemotions = numDemoteInProgress
      }
      =
      peerSelectionStateToCounters st


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
      -- It's quite bad if demoting fails. The peer is cooling so
      -- we can't remove it from the set of established and hot peers.
      --
      Completion $ \st@PeerSelectionState {
                      publicRootPeers,
                      activePeers,
                      inProgressDemoteHot,
                      targets = PeerSelectionTargets {
                                  targetNumberOfActivePeers,
                                  targetNumberOfActiveBigLedgerPeers
                                }
                    }
                    _ ->
        let
            inProgressDemoteHot'  = Set.delete peeraddr inProgressDemoteHot
            bigLedgerPeersSet     = PublicRootPeers.getBigLedgerPeers publicRootPeers
         in Decision {
              decisionTrace = if peeraddr `Set.member` bigLedgerPeersSet
                              then [TraceDemoteHotBigLedgerPeerFailed
                                     targetNumberOfActiveBigLedgerPeers
                                     (Set.size $ activePeers
                                                 `Set.intersection`
                                                 bigLedgerPeersSet)
                                     peeraddr e]
                              else [TraceDemoteHotFailed
                                     targetNumberOfActivePeers
                                     (Set.size $ activePeers
                                          Set.\\ bigLedgerPeersSet)
                                     peeraddr e],
              decisionState = st {
                                inProgressDemoteHot = inProgressDemoteHot'
                              },
              decisionJobs  = []
            }

    job :: m (Completion m peeraddr peerconn)
    job = do
      deactivatePeerConnection peerconn
      return $ Completion $ \st@PeerSelectionState {
                                publicRootPeers,
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
            knownPeers'  = setTepidFlag peeraddr knownPeers
            bigLedgerPeersSet = PublicRootPeers.getBigLedgerPeers publicRootPeers
         in Decision {
              decisionTrace = if peeraddr `Set.member` bigLedgerPeersSet
                              then [TraceDemoteHotBigLedgerPeerDone
                                     targetNumberOfActiveBigLedgerPeers
                                     (Set.size $ activePeers'
                                                 `Set.intersection`
                                                 bigLedgerPeersSet)
                                     peeraddr]
                              else [TraceDemoteHotDone
                                     targetNumberOfActivePeers
                                     (Set.size $ activePeers'
                                          Set.\\ bigLedgerPeersSet)
                                     peeraddr],
              decisionState = st {
                                activePeers         = activePeers',
                                knownPeers          = knownPeers',
                                inProgressDemoteHot = Set.delete peeraddr
                                                        (inProgressDemoteHot st)
                              },
              decisionJobs  = []
            }
