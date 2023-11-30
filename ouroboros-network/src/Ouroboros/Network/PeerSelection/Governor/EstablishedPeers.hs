{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.PeerSelection.Governor.EstablishedPeers
  ( belowTarget
  , aboveTarget
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set

import           Control.Applicative (Alternative)
import           Control.Concurrent.JobPool (Job (..))
import           Control.Exception (SomeException)
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTime.SI
import           System.Random (randomR)

import           Ouroboros.Network.PeerSelection.Governor.Types
import           Ouroboros.Network.PeerSelection.LedgerPeers.Type
                     (IsBigLedgerPeer (..))
import           Ouroboros.Network.PeerSelection.PeerAdvertise
                     (PeerAdvertise (..))
import           Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import qualified Ouroboros.Network.PeerSelection.State.EstablishedPeers as EstablishedPeers
import qualified Ouroboros.Network.PeerSelection.State.KnownPeers as KnownPeers
import           Ouroboros.Network.PeerSelection.State.LocalRootPeers
                     (WarmValency (..))
import qualified Ouroboros.Network.PeerSelection.State.LocalRootPeers as LocalRootPeers


---------------------------------
-- Established peers below target
--


-- | If we are below the target of /warm peers/ we promote /cold peers/
-- according to 'policyPickColdPeersToPromote'.
--
-- There are two targets we are trying to hit here:
--
-- 1. a target for the overall number of established peers; and
-- 2. the target that all local root peers are established peers.
--
-- These two targets overlap: the conditions and the actions overlap since local
-- root peers are also known peers. Since they overlap, the order in which we
-- consider these targets is important. We consider the local peers target
-- /before/ the target for promoting other peers.
--
-- We will /always/ try to establish connections to the local root peers, even
-- if that would put us over target for the number of established peers. If we
-- do go over target then the action to demote will be triggered. The demote
-- action never picks local root peers.
--
belowTarget :: forall peeraddr peerconn m.
               ( Alternative (STM m)
               , MonadSTM m
               , Ord peeraddr
               )
            => PeerSelectionActions peeraddr peerconn m
            -> MkGuardedDecision peeraddr peerconn m
belowTarget =  belowTargetBigLedgerPeers <> belowTargetLocal <> belowTargetOther


-- | For locally configured root peers we have the explicit target that comes from local
-- configuration.
--
belowTargetLocal :: forall peeraddr peerconn m.
                   (MonadSTM m, Ord peeraddr)
                 => PeerSelectionActions peeraddr peerconn m
                 -> MkGuardedDecision peeraddr peerconn m
belowTargetLocal actions
                 policy@PeerSelectionPolicy {
                   policyPickColdPeersToPromote
                 }
                 st@PeerSelectionState {
                   bigLedgerPeers,
                   localRootPeers,
                   knownPeers,
                   establishedPeers,
                   inProgressPromoteCold,
                   inProgressDemoteToCold
                 }

    -- Are there any groups of local peers that are below target?
  | not (null groupsBelowTarget)
    -- We need this detailed check because it is not enough to check we are
    -- below an aggregate target. We can be above target for some groups
    -- and below for others. We need to take into account peers which are being
    -- promoted to Warm, and peers which are being demoted to Cold.

    -- Are there any groups where we can pick members to promote?
  , let groupsAvailableToPromote =
          [ (numMembersToPromote, membersAvailableToPromote)
          | let availableToPromote =
                  localAvailableToConnect
                     Set.\\ localEstablishedPeers
                     Set.\\ localConnectInProgress
                     Set.\\ inProgressDemoteToCold
          , not (Set.null availableToPromote)
          , (WarmValency warmTarget, members, membersEstablished) <- groupsBelowTarget
          , let membersAvailableToPromote = Set.intersection members availableToPromote
                numMembersToPromote       = warmTarget
                                          - Set.size membersEstablished
                                          - numLocalConnectInProgress
          , not (Set.null membersAvailableToPromote)
          , numMembersToPromote > 0
          ]
  , not (null groupsAvailableToPromote)
  = Guarded Nothing $ do
      selectedToPromote <-
        Set.unions <$> sequence
          [ pickPeers st
              policyPickColdPeersToPromote
              membersAvailableToPromote
              numMembersToPromote
          | (numMembersToPromote,
             membersAvailableToPromote) <- groupsAvailableToPromote ]

      return $ \_now -> Decision {
        decisionTrace = [TracePromoteColdLocalPeers
                           [ (target, Set.size membersEstablished)
                           | (target, _, membersEstablished) <- groupsBelowTarget ]
                           selectedToPromote],
        decisionState = st {
                          inProgressPromoteCold = inProgressPromoteCold
                                               <> selectedToPromote
                        },
        decisionJobs  = [ jobPromoteColdPeer actions policy peer IsNotBigLedgerPeer
                        | peer <- Set.toList selectedToPromote ]
      }

    -- If we could promote except that there are no peers currently available
    -- then we return the next wakeup time (if any)
  | not (null groupsBelowTarget)
  , let potentialToPromote =
          -- These are local peers that are cold but not ready.
          localRootPeersSet
             Set.\\ localEstablishedPeers
             Set.\\ KnownPeers.availableToConnect knownPeers
  , not (Set.null potentialToPromote)
  = GuardedSkip (KnownPeers.minConnectTime knownPeers (`Set.notMember` bigLedgerPeers))

  | otherwise
  = GuardedSkip Nothing
  where
    groupsBelowTarget =
      [ (warmValency, members, membersEstablished)
      | (_, warmValency, members) <- LocalRootPeers.toGroupSets localRootPeers
      , let membersEstablished = members `Set.intersection` EstablishedPeers.toSet establishedPeers
      , Set.size membersEstablished < getWarmValency warmValency
      ]

    localRootPeersSet          = LocalRootPeers.keysSet localRootPeers
    localEstablishedPeers      = EstablishedPeers.toSet establishedPeers
                                  `Set.intersection` localRootPeersSet
    localAvailableToConnect    = KnownPeers.availableToConnect knownPeers
                                  `Set.intersection` localRootPeersSet
    localConnectInProgress     = inProgressPromoteCold
                                  `Set.intersection` localRootPeersSet
    numLocalConnectInProgress  = Set.size localConnectInProgress


belowTargetOther :: forall peeraddr peerconn m.
                    (MonadSTM m, Ord peeraddr)
                 => PeerSelectionActions peeraddr peerconn m
                 -> MkGuardedDecision peeraddr peerconn m
belowTargetOther actions
                 policy@PeerSelectionPolicy {
                   policyPickColdPeersToPromote
                 }
                 st@PeerSelectionState {
                   bigLedgerPeers,
                   knownPeers,
                   establishedPeers,
                   inProgressPromoteCold,
                   targets = PeerSelectionTargets {
                               targetNumberOfEstablishedPeers
                             }
                 }
    -- Are we below the target for number of established peers?
  | numEstablishedPeers + numConnectInProgress < targetNumberOfEstablishedPeers

    -- Are there any cold peers we could possibly pick to connect to?
    -- We can subtract the established ones because by definition they are
    -- not cold and our invariant is that they are always in the connect set.
    -- We can also subtract the in progress ones since they are also already
    -- in the connect set and we cannot pick them again.
  , numAvailableToConnect - numEstablishedPeers - numConnectInProgress > 0
  = Guarded Nothing $ do
      -- The availableToPromote here is non-empty due to the second guard.
      -- The known peers map restricted to the connect set is the same size as
      -- the connect set (because it is a subset). The establishedPeers is a
      -- subset of the connect set and we also know that there is no overlap
      -- between inProgressPromoteCold and establishedPeers. QED.
      --
      -- The numPeersToPromote is positive based on the first guard.
      --
      let availableToPromote :: Set peeraddr
          availableToPromote = availableToConnect
                                 Set.\\ EstablishedPeers.toSet establishedPeers
                                 Set.\\ inProgressPromoteCold
          numPeersToPromote  = targetNumberOfEstablishedPeers
                             - numEstablishedPeers
                             - numConnectInProgress
      selectedToPromote <- pickPeers st
                             policyPickColdPeersToPromote
                             availableToPromote
                             numPeersToPromote
      return $ \_now -> Decision {
        decisionTrace = [TracePromoteColdPeers
                           targetNumberOfEstablishedPeers
                           numEstablishedPeers
                           selectedToPromote],
        decisionState = st {
                          inProgressPromoteCold = inProgressPromoteCold
                                               <> selectedToPromote
                        },
        decisionJobs  = [ jobPromoteColdPeer actions policy peer IsNotBigLedgerPeer
                        | peer <- Set.toList selectedToPromote ]
      }

    -- If we could connect except that there are no peers currently available
    -- then we return the next wakeup time (if any)
  | numEstablishedPeers + numConnectInProgress < targetNumberOfEstablishedPeers
  = GuardedSkip (KnownPeers.minConnectTime knownPeers (`Set.notMember` bigLedgerPeers))

  | otherwise
  = GuardedSkip Nothing
  where
    numEstablishedPeers, numConnectInProgress :: Int
    numEstablishedPeers   = Set.size $ EstablishedPeers.toSet establishedPeers
                                Set.\\ bigLedgerPeers
    numConnectInProgress  = Set.size $ inProgressPromoteCold
                                Set.\\ bigLedgerPeers
    availableToConnect    = KnownPeers.availableToConnect knownPeers
                                Set.\\ bigLedgerPeers
    numAvailableToConnect = Set.size availableToConnect



belowTargetBigLedgerPeers :: forall peeraddr peerconn m.
                             (MonadSTM m, Ord peeraddr)
                          => PeerSelectionActions peeraddr peerconn m
                          -> MkGuardedDecision peeraddr peerconn m
belowTargetBigLedgerPeers actions
                          policy@PeerSelectionPolicy {
                            policyPickColdPeersToPromote
                          }
                          st@PeerSelectionState {
                            bigLedgerPeers,
                            knownPeers,
                            establishedPeers,
                            inProgressPromoteCold,
                            targets = PeerSelectionTargets {
                                        targetNumberOfEstablishedBigLedgerPeers
                                      }
                          }
    -- Are we below the target for number of established peers?
  | numEstablishedPeers + numConnectInProgress
      < targetNumberOfEstablishedBigLedgerPeers

    -- Are there any cold peers we could possibly pick to connect to?
    -- We can subtract the established ones because by definition they are
    -- not cold and our invariant is that they are always in the connect set.
    -- We can also subtract the in progress ones since they are also already
    -- in the connect set and we cannot pick them again.
  , numAvailableToConnect - numEstablishedPeers - numConnectInProgress > 0
  = Guarded Nothing $ do
      -- The availableToPromote here is non-empty due to the second guard.
      -- The known peers map restricted to the connect set is the same size as
      -- the connect set (because it is a subset). The establishedPeers is a
      -- subset of the connect set and we also know that there is no overlap
      -- between inProgressPromoteCold and establishedPeers. QED.
      --
      -- The numPeersToPromote is positive based on the first guard.
      --
      let availableToPromote :: Set peeraddr
          availableToPromote = availableToConnect
                                 Set.\\ EstablishedPeers.toSet establishedPeers
                                 Set.\\ inProgressPromoteCold
          numPeersToPromote  = targetNumberOfEstablishedBigLedgerPeers
                             - numEstablishedPeers
                             - numConnectInProgress
      selectedToPromote <- pickPeers st
                             policyPickColdPeersToPromote
                             availableToPromote
                             numPeersToPromote
      return $ \_now -> Decision {
        decisionTrace = [TracePromoteColdBigLedgerPeers
                           targetNumberOfEstablishedBigLedgerPeers
                           numEstablishedPeers
                           selectedToPromote],
        decisionState = st {
                          inProgressPromoteCold = inProgressPromoteCold
                                               <> selectedToPromote
                        },
        decisionJobs  = [ jobPromoteColdPeer actions policy peer IsBigLedgerPeer
                        | peer <- Set.toList selectedToPromote ]
      }

    -- If we could connect except that there are no peers currently available
    -- then we return the next wakeup time (if any)
  | numEstablishedPeers + numConnectInProgress
      < targetNumberOfEstablishedBigLedgerPeers
  = GuardedSkip (KnownPeers.minConnectTime knownPeers  (`Set.member` bigLedgerPeers))

  | otherwise
  = GuardedSkip Nothing
  where
    numEstablishedPeers, numConnectInProgress :: Int
    numEstablishedPeers  = Set.size $
                           EstablishedPeers.toSet establishedPeers
                           `Set.intersection`
                           bigLedgerPeers
    numConnectInProgress = Set.size $
                           inProgressPromoteCold
                           `Set.intersection`
                           bigLedgerPeers
    availableToConnect   = KnownPeers.availableToConnect knownPeers
                           `Set.intersection`
                           bigLedgerPeers
    numAvailableToConnect= Set.size availableToConnect


-- | Must be larger than '2' since we add a random value drawn from '(-2, 2)`.
--
baseColdPeerRetryDiffTime :: Int
baseColdPeerRetryDiffTime = 5

maxColdPeerRetryBackoff :: Int
maxColdPeerRetryBackoff = 5


jobPromoteColdPeer :: forall peeraddr peerconn m.
                       (Monad m, Ord peeraddr)
                   => PeerSelectionActions peeraddr peerconn m
                   -> PeerSelectionPolicy peeraddr m
                   -> peeraddr
                   -> IsBigLedgerPeer
                   -> Job () m (Completion m peeraddr peerconn)
jobPromoteColdPeer PeerSelectionActions {
                     peerStateActions = PeerStateActions {establishPeerConnection},
                     peerConnToPeerSharing
                   }
                   PeerSelectionPolicy { policyPeerShareActivationDelay }
                   peeraddr isBigLedgerPeer =
    Job job handler () "promoteColdPeer"
  where
    handler :: SomeException -> m (Completion m peeraddr peerconn)
    handler e = return $
      Completion $ \st@PeerSelectionState {
                      bigLedgerPeers,
                      establishedPeers,
                      fuzzRng,
                      targets = PeerSelectionTargets {
                                  targetNumberOfEstablishedPeers,
                                  targetNumberOfEstablishedBigLedgerPeers
                                }
                    }
                    now ->
        let (failCount, knownPeers') = KnownPeers.incrementFailCount
                                         peeraddr
                                         (knownPeers st)
            (fuzz, fuzzRng') = randomR (-2, 2 :: Double) fuzzRng

            -- exponential backoff: 5s, 10s, 20s, 40s, 80s, 160s.
            delay :: DiffTime
            delay = realToFrac fuzz
                  + fromIntegral
                      ( baseColdPeerRetryDiffTime
                      * 2 ^ (pred failCount `min` maxColdPeerRetryBackoff)
                      )
        in
          Decision {
            decisionTrace = if peeraddr `Set.member` bigLedgerPeers
                            then [TracePromoteColdBigLedgerPeerFailed
                                   targetNumberOfEstablishedBigLedgerPeers
                                   (Set.size $ EstablishedPeers.toSet establishedPeers
                                               `Set.intersection`
                                               bigLedgerPeers)
                                   peeraddr delay e]
                            else [TracePromoteColdFailed
                                   targetNumberOfEstablishedPeers
                                   (EstablishedPeers.size establishedPeers)
                                   peeraddr delay e],
            decisionState = st {
                              knownPeers            = KnownPeers.setConnectTimes
                                                        (Map.singleton
                                                          peeraddr
                                                          (delay `addTime` now))
                                                        knownPeers',
                              inProgressPromoteCold = Set.delete peeraddr
                                                        (inProgressPromoteCold st),
                              fuzzRng = fuzzRng'
                            },
            decisionJobs  = []
          }

    job :: m (Completion m peeraddr peerconn)
    job = do
      --TODO: decide if we should do timeouts here or if we should make that
      -- the responsibility of establishPeerConnection
      peerconn <- establishPeerConnection isBigLedgerPeer peeraddr
      let !peerSharing = peerConnToPeerSharing peerconn

      return $ Completion $ \st@PeerSelectionState {
                               bigLedgerPeers,
                               establishedPeers,
                               knownPeers,
                               targets = PeerSelectionTargets {
                                           targetNumberOfEstablishedPeers,
                                           targetNumberOfEstablishedBigLedgerPeers
                                         }
                             }
                             now ->
        let establishedPeers' = EstablishedPeers.insert peeraddr peerconn
                                                        (addTime policyPeerShareActivationDelay now)
                                                        establishedPeers
            advertise = case peerSharing of
                          PeerSharingEnabled  -> DoAdvertisePeer
                          PeerSharingDisabled -> DoNotAdvertisePeer
            -- Update PeerSharing value in KnownPeers
            knownPeers'       = KnownPeers.insert (Map.singleton peeraddr ( Just peerSharing
                                                                          , newDefaultValue peeraddr
                                                                                            advertise
                                                                                            knownPeers
                                                                          , Nothing))
                              $ KnownPeers.setSuccessfulConnectionFlag peeraddr
                              $ KnownPeers.clearTepidFlag peeraddr $
                                    KnownPeers.resetFailCount
                                        peeraddr
                                        knownPeers

        in Decision {
             decisionTrace = if peeraddr `Set.member` bigLedgerPeers
                             then [TracePromoteColdBigLedgerPeerDone
                                    targetNumberOfEstablishedBigLedgerPeers
                                    (Set.size $ EstablishedPeers.toSet establishedPeers'
                                                `Set.intersection`
                                                bigLedgerPeers)
                                    peeraddr]
                             else [TracePromoteColdDone
                                    targetNumberOfEstablishedPeers
                                    (Set.size $ EstablishedPeers.toSet establishedPeers'
                                         Set.\\ bigLedgerPeers)
                                    peeraddr],
             decisionState = st {
                               establishedPeers      = establishedPeers',
                               inProgressPromoteCold = Set.delete peeraddr
                                                         (inProgressPromoteCold st),
                               knownPeers            = knownPeers'
                             },
             decisionJobs  = []
           }


---------------------------------
-- Established peers above target
--
--


-- | If we are above the target of /established peers/ we demote some of the
-- /warm peers/ to the cold state, according to 'policyPickWarmPeersToDemote'.
--
aboveTarget :: forall peeraddr peerconn m.
               (Alternative (STM m), MonadSTM m, Ord peeraddr)
            => PeerSelectionActions peeraddr peerconn m
            -> MkGuardedDecision peeraddr peerconn m
aboveTarget =  aboveTargetBigLedgerPeers <> aboveTargetOther

aboveTargetOther :: forall peeraddr peerconn m.
               (MonadSTM m, Ord peeraddr)
            => PeerSelectionActions peeraddr peerconn m
            -> MkGuardedDecision peeraddr peerconn m
aboveTargetOther actions
                 policy@PeerSelectionPolicy {
                   policyPickWarmPeersToDemote
                 }
                 st@PeerSelectionState {
                   bigLedgerPeers,
                   localRootPeers,
                   establishedPeers,
                   activePeers,
                   inProgressDemoteWarm,
                   inProgressPromoteWarm,
                   inProgressDemoteToCold,
                   targets = PeerSelectionTargets {
                               targetNumberOfEstablishedPeers
                             }
                 }
    -- Are we above the target for number of established peers?
    -- Or more precisely, how many established peers could we demote?
    -- We only want to pick established peers that are not active, since for
    -- active one we need to demote them first.
  | let numEstablishedPeers, numActivePeers, numPeersToDemote :: Int
        numEstablishedPeers = Set.size $ EstablishedPeers.toSet establishedPeers
                                  Set.\\ bigLedgerPeers
        numActivePeers      = Set.size $ activePeers
                                  Set.\\ bigLedgerPeers
        numLocalWarmPeers   = Set.size localWarmPeers
        localWarmPeers      = LocalRootPeers.keysSet localRootPeers
           `Set.intersection` EstablishedPeers.toSet establishedPeers
                       Set.\\ activePeers
                       Set.\\ bigLedgerPeers
        -- One constraint on how many to demote is the difference in the
        -- number we have now vs the target. The other constraint is that
        -- we pick established peers that are not also active. These
        -- constraints combine by taking the minimum. We must also subtract
        -- the number we're demoting so we don't repeat the same work. And
        -- cannot demote ones we're in the process of promoting.
        numPeersToDemote    = min (numEstablishedPeers
                                   - targetNumberOfEstablishedPeers)
                                  (numEstablishedPeers
                                   - numLocalWarmPeers
                                   - numActivePeers)
                            - Set.size (inProgressDemoteWarm  Set.\\ bigLedgerPeers)
                            - Set.size (inProgressPromoteWarm Set.\\ bigLedgerPeers)

        availableToDemote :: Set peeraddr
        availableToDemote = EstablishedPeers.toSet establishedPeers
                              Set.\\ activePeers
                              Set.\\ LocalRootPeers.keysSet localRootPeers
                              Set.\\ bigLedgerPeers
                              Set.\\ inProgressDemoteWarm
                              Set.\\ inProgressPromoteWarm
                              Set.\\ inProgressDemoteToCold

  , numPeersToDemote > 0
  , not (Set.null availableToDemote)
  = Guarded Nothing $ do
      selectedToDemote <- pickPeers st
                            policyPickWarmPeersToDemote
                            availableToDemote
                            numPeersToDemote
      let selectedToDemote' :: Map peeraddr peerconn
          selectedToDemote' = EstablishedPeers.toMap establishedPeers
                                `Map.restrictKeys` selectedToDemote

      return $ \_now -> Decision {
        decisionTrace = [TraceDemoteWarmPeers
                           targetNumberOfEstablishedPeers
                           numEstablishedPeers
                           selectedToDemote],
        decisionState = st {
                          inProgressDemoteWarm = inProgressDemoteWarm
                                              <> selectedToDemote
                        },
        decisionJobs  = [ jobDemoteEstablishedPeer actions policy peeraddr peerconn
                        | (peeraddr, peerconn) <- Map.assocs selectedToDemote' ]
      }

  | otherwise
  = GuardedSkip Nothing


aboveTargetBigLedgerPeers :: forall peeraddr peerconn m.
                             (MonadSTM m, Ord peeraddr)
                          => PeerSelectionActions peeraddr peerconn m
                          -> MkGuardedDecision peeraddr peerconn m
aboveTargetBigLedgerPeers actions
                          policy@PeerSelectionPolicy {
                            policyPickWarmPeersToDemote
                          }
                          st@PeerSelectionState {
                            bigLedgerPeers,
                            establishedPeers,
                            activePeers,
                            inProgressDemoteWarm,
                            inProgressPromoteWarm,
                            inProgressDemoteToCold,
                            targets = PeerSelectionTargets {
                                        targetNumberOfEstablishedBigLedgerPeers
                                      }
                          }
    -- Are we above the target for number of established peers?
    -- Or more precisely, how many established peers could we demote?
    -- We only want to pick established peers that are not active, since for
    -- active one we need to demote them first.
  | let numEstablishedBigLedgerPeers, numBigLedgerPeersToDemote :: Int
        numEstablishedBigLedgerPeers = Set.size $ EstablishedPeers.toSet establishedPeers
                               `Set.intersection` bigLedgerPeers

        numActiveBigLedgerPeers      = Set.size $ activePeers
                               `Set.intersection` bigLedgerPeers

        -- We want to demote big ledger peers towards the target but we avoid to
        -- pick active peer.  The `min` is taken so that `pickPeers` is given
        -- consistent number of peers with the set of peers available to demote,
        -- i.e. `availableToDemote`.
        numBigLedgerPeersToDemote    = min ( numEstablishedBigLedgerPeers
                                           - targetNumberOfEstablishedBigLedgerPeers)
                                           ( numEstablishedBigLedgerPeers
                                           - numActiveBigLedgerPeers)
                                     - Set.size inProgressDemoteWarm
                                     - Set.size inProgressPromoteWarm

        availableToDemote :: Set peeraddr
        availableToDemote = EstablishedPeers.toSet establishedPeers
                             `Set.intersection` bigLedgerPeers
                              Set.\\ activePeers
                              Set.\\ inProgressDemoteWarm
                              Set.\\ inProgressPromoteWarm
                              Set.\\ inProgressDemoteToCold

  , numBigLedgerPeersToDemote > 0
  , not (Set.null availableToDemote)
  = Guarded Nothing $ do

      selectedToDemote <- pickPeers st
                            policyPickWarmPeersToDemote
                            availableToDemote
                            numBigLedgerPeersToDemote
      let selectedToDemote' :: Map peeraddr peerconn
          selectedToDemote' = EstablishedPeers.toMap establishedPeers
                                `Map.restrictKeys` selectedToDemote

      return $ \_now -> Decision {
        decisionTrace = [TraceDemoteWarmBigLedgerPeers
                           targetNumberOfEstablishedBigLedgerPeers
                           numEstablishedBigLedgerPeers
                           selectedToDemote],
        decisionState = st {
                          inProgressDemoteWarm = inProgressDemoteWarm
                                              <> selectedToDemote
                        },
        decisionJobs  = [ jobDemoteEstablishedPeer actions policy peeraddr peerconn
                        | (!peeraddr, !peerconn) <- Map.assocs selectedToDemote' ]
      }

  | otherwise
  = GuardedSkip Nothing


jobDemoteEstablishedPeer :: forall peeraddr peerconn m.
                            (Monad m, Ord peeraddr)
                         => PeerSelectionActions peeraddr peerconn m
                         -> PeerSelectionPolicy peeraddr m
                         -> peeraddr
                         -> peerconn
                         -> Job () m (Completion m peeraddr peerconn)
jobDemoteEstablishedPeer PeerSelectionActions{peerStateActions = PeerStateActions {closePeerConnection}}
                         PeerSelectionPolicy { policyErrorDelay }
                         peeraddr peerconn =
    Job job handler () "demoteEstablishedPeer"
  where
    handler :: SomeException -> m (Completion m peeraddr peerconn)
    handler e = return $
      -- It's quite bad if closing fails. The peer is cold so
      -- remove if from the set of established.
      Completion $ \st@PeerSelectionState {
                       bigLedgerPeers,
                       establishedPeers,
                       inProgressDemoteWarm,
                       knownPeers,
                       targets = PeerSelectionTargets {
                                   targetNumberOfEstablishedPeers,
                                   targetNumberOfEstablishedBigLedgerPeers
                                 },
                       fuzzRng
                     }
                     now ->
        let (rFuzz, fuzzRng')     = randomR (-2, 2 :: Double) fuzzRng
            peerSet               = Set.singleton peeraddr
            inProgressDemoteWarm' = Set.delete peeraddr inProgressDemoteWarm
            knownPeers'           = KnownPeers.setConnectTimes
                                      (Map.singleton
                                        peeraddr
                                        ((realToFrac rFuzz + policyErrorDelay)
                                         `addTime` now))
                                  . Set.foldr'
                                      ((snd .) . KnownPeers.incrementFailCount)
                                      knownPeers
                                  $ peerSet
            establishedPeers'     = EstablishedPeers.deletePeers
                                     peerSet
                                     establishedPeers in
        Decision {
          decisionTrace = if peeraddr `Set.member` bigLedgerPeers
                          then [TraceDemoteWarmBigLedgerPeerFailed
                                 targetNumberOfEstablishedBigLedgerPeers
                                 (Set.size $ EstablishedPeers.toSet establishedPeers
                                             `Set.intersection`
                                             bigLedgerPeers)
                                 peeraddr e]
                          else [TraceDemoteWarmFailed
                                 targetNumberOfEstablishedPeers
                                 (Set.size $ EstablishedPeers.toSet establishedPeers
                                      Set.\\ bigLedgerPeers)
                                 peeraddr e],
          decisionState = st {
                            inProgressDemoteWarm = inProgressDemoteWarm',
                            fuzzRng = fuzzRng',
                            knownPeers = knownPeers',
                            establishedPeers = establishedPeers'
                          },
          decisionJobs  = []
      }

    job :: m (Completion m peeraddr peerconn)
    job = do
      closePeerConnection peerconn
      return $ Completion $ \st@PeerSelectionState {
                               bigLedgerPeers,
                               establishedPeers,
                               targets = PeerSelectionTargets {
                                           targetNumberOfEstablishedPeers
                                         }
                             }
                             _now ->
        let establishedPeers' = EstablishedPeers.delete peeraddr
                                                        establishedPeers
        in Decision {
             decisionTrace = if peeraddr `Set.member` bigLedgerPeers
                             then [TraceDemoteWarmBigLedgerPeerDone
                                    targetNumberOfEstablishedPeers
                                    (Set.size $ EstablishedPeers.toSet establishedPeers'
                                                `Set.intersection`
                                                bigLedgerPeers)
                                    peeraddr]
                             else [TraceDemoteWarmDone
                                    targetNumberOfEstablishedPeers
                                    (Set.size $ EstablishedPeers.toSet establishedPeers'
                                         Set.\\ bigLedgerPeers)
                                    peeraddr],
             decisionState = st {
                               establishedPeers     = establishedPeers',
                               inProgressDemoteWarm = Set.delete peeraddr
                                                        (inProgressDemoteWarm st)
                             },
             decisionJobs  = []
           }
