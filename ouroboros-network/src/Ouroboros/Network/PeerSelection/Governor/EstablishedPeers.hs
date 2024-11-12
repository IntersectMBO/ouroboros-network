{-# LANGUAGE GADTs               #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.PeerSelection.Governor.EstablishedPeers
  ( belowTarget
  , aboveTarget
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Stack (HasCallStack)

import Control.Applicative (Alternative)
import Control.Concurrent.JobPool (Job (..))
import Control.Exception (SomeException)
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadTime.SI
import System.Random (randomR)

import Ouroboros.Network.PeerSelection.Bootstrap (requiresBootstrapPeers)
import Ouroboros.Network.PeerSelection.Governor.Types
import Ouroboros.Network.PeerSelection.LedgerPeers.Type (IsBigLedgerPeer (..))
import Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise (..))
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import Ouroboros.Network.PeerSelection.PublicRootPeers qualified as PublicRootPeers
import Ouroboros.Network.PeerSelection.State.EstablishedPeers qualified as EstablishedPeers
import Ouroboros.Network.PeerSelection.State.KnownPeers qualified as KnownPeers
import Ouroboros.Network.PeerSelection.State.LocalRootPeers (WarmValency (..))
import Ouroboros.Network.PeerSelection.State.LocalRootPeers qualified as LocalRootPeers


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
                    (MonadSTM m, Ord peeraddr, HasCallStack)
                 => PeerSelectionActions peeraddr peerconn m
                 -> MkGuardedDecision peeraddr peerconn m
belowTargetLocal actions
                 policy@PeerSelectionPolicy {
                   policyPickColdPeersToPromote
                 }
                 st@PeerSelectionState {
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
  = GuardedSkip (KnownPeers.minConnectTime knownPeers (`Set.notMember` bigLedgerPeersSet))

  | otherwise
  = GuardedSkip Nothing
  where
    groupsBelowTarget =
      [ (warmValency, members, membersEstablished)
      | (_, warmValency, members) <- LocalRootPeers.toGroupSets localRootPeers
      , let membersEstablished = members `Set.intersection` EstablishedPeers.toSet establishedPeers
      , Set.size membersEstablished < getWarmValency warmValency
      ]

    PeerSelectionView {
        viewKnownBigLedgerPeers              = (bigLedgerPeersSet, _),
        viewKnownLocalRootPeers              = (localRootPeersSet, _),
        viewEstablishedLocalRootPeers        = (localEstablishedPeers, _),
        viewAvailableToConnectLocalRootPeers = (localAvailableToConnect, _),
        viewColdLocalRootPeersPromotions     = (localConnectInProgress, numLocalConnectInProgress)
      } = peerSelectionStateToView st


belowTargetOther :: forall peeraddr peerconn m.
                    (MonadSTM m, Ord peeraddr, HasCallStack)
                 => PeerSelectionActions peeraddr peerconn m
                 -> MkGuardedDecision peeraddr peerconn m
belowTargetOther actions
                 policy@PeerSelectionPolicy {
                   policyPickColdPeersToPromote
                 }
                 st@PeerSelectionState {
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
  = GuardedSkip (KnownPeers.minConnectTime knownPeers (`Set.notMember` bigLedgerPeersSet))

  | otherwise
  = GuardedSkip Nothing
  where
    PeerSelectionView {
        viewKnownBigLedgerPeers     = (bigLedgerPeersSet, _),

        viewAvailableToConnectPeers = (availableToConnect, numAvailableToConnect),
        viewEstablishedPeers        = (_, numEstablishedPeers),
        viewColdPeersPromotions     = (_, numConnectInProgress)
      }
      =
      peerSelectionStateToView st


-- |
--
-- It should be noted if the node is in bootstrap mode (i.e. in a sensitive
-- state) then this monitoring action will be disabled.
--
belowTargetBigLedgerPeers :: forall peeraddr peerconn m.
                             (MonadSTM m, Ord peeraddr, HasCallStack)
                          => PeerSelectionActions peeraddr peerconn m
                          -> MkGuardedDecision peeraddr peerconn m
belowTargetBigLedgerPeers actions
                          policy@PeerSelectionPolicy {
                            policyPickColdPeersToPromote
                          }
                          st@PeerSelectionState {
                            knownPeers,
                            establishedPeers,
                            inProgressPromoteCold,
                            targets = PeerSelectionTargets {
                                        targetNumberOfEstablishedBigLedgerPeers
                                      },
                            ledgerStateJudgement,
                            bootstrapPeersFlag
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

    -- Are we in insensitive state, i.e. using bootstrap peers?
  , not (requiresBootstrapPeers bootstrapPeersFlag ledgerStateJudgement)
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
  = GuardedSkip (KnownPeers.minConnectTime knownPeers  (`Set.member` bigLedgerPeersSet))

  | otherwise
  = GuardedSkip Nothing
  where
    PeerSelectionView {
        viewKnownBigLedgerPeers              = (bigLedgerPeersSet, _),
        viewAvailableToConnectBigLedgerPeers = (availableToConnect, numAvailableToConnect),
        viewEstablishedBigLedgerPeers        = (_, numEstablishedPeers),
        viewColdBigLedgerPeersPromotions     = (_, numConnectInProgress)
      }
      =
      peerSelectionStateToView st


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
                      publicRootPeers,
                      stdGen,
                      targets = PeerSelectionTargets {
                                  targetNumberOfEstablishedPeers,
                                  targetNumberOfEstablishedBigLedgerPeers
                                }
                    }
                    now ->
        let (failCount, knownPeers') = KnownPeers.incrementFailCount
                                         peeraddr
                                         (knownPeers st)
            (fuzz, stdGen') = randomR (-2, 2 :: Double) stdGen

            -- exponential backoff: 5s, 10s, 20s, 40s, 80s, 160s.
            delay :: DiffTime
            delay = realToFrac fuzz
                  + fromIntegral
                      ( baseColdPeerRetryDiffTime
                      * 2 ^ (pred failCount `min` maxColdPeerRetryBackoff)
                      )
            bigLedgerPeersSet = PublicRootPeers.getBigLedgerPeers publicRootPeers

            st' = st { knownPeers            = KnownPeers.setConnectTimes
                                                 (Map.singleton
                                                   peeraddr
                                                   (delay `addTime` now))
                                                 knownPeers',
                       inProgressPromoteCold = Set.delete peeraddr
                                                 (inProgressPromoteCold st),
                       stdGen = stdGen'
                     }
            cs' = peerSelectionStateToCounters st'
        in
          Decision {
            decisionTrace = if peeraddr `Set.member` bigLedgerPeersSet
                            then [TracePromoteColdBigLedgerPeerFailed
                                   targetNumberOfEstablishedBigLedgerPeers
                                   (case cs' of
                                     PeerSelectionCounters { numberOfEstablishedBigLedgerPeers = a } -> a)
                                   peeraddr delay e]
                            else [TracePromoteColdFailed
                                   targetNumberOfEstablishedPeers
                                   (case cs' of
                                     PeerSelectionCounters { numberOfEstablishedPeers = a } -> a)
                                   peeraddr delay e],
            decisionState = st',
            decisionJobs  = []
          }

    job :: m (Completion m peeraddr peerconn)
    job = do
      --TODO: decide if we should do timeouts here or if we should make that
      -- the responsibility of establishPeerConnection
      peerconn <- establishPeerConnection isBigLedgerPeer peeraddr
      let !peerSharing = peerConnToPeerSharing peerconn

      return $ Completion $ \st@PeerSelectionState {
                               publicRootPeers,
                               establishedPeers,
                               knownPeers,
                               targets = PeerSelectionTargets {
                                           targetNumberOfEstablishedPeers,
                                           targetNumberOfEstablishedBigLedgerPeers
                                         }
                             }
                             now ->
        let psTime = case peerSharing of
                          PeerSharingEnabled  -> Just (addTime policyPeerShareActivationDelay now)
                          PeerSharingDisabled -> Nothing
            establishedPeers' = EstablishedPeers.insert peeraddr peerconn psTime establishedPeers
            advertise = case peerSharing of
                          PeerSharingEnabled  -> DoAdvertisePeer
                          PeerSharingDisabled -> DoNotAdvertisePeer
            -- Update PeerSharing value in KnownPeers
            knownPeers'       = KnownPeers.alter
                                  (\x -> case x of
                                    Nothing ->
                                      KnownPeers.alterKnownPeerInfo
                                        (Just peerSharing, Just advertise)
                                        x
                                    Just _ ->
                                      KnownPeers.alterKnownPeerInfo
                                        (Just peerSharing, Nothing)
                                        x
                                  )
                                  (Set.singleton peeraddr)
                              $ KnownPeers.setSuccessfulConnectionFlag (Set.singleton peeraddr)
                              $ KnownPeers.clearTepidFlag peeraddr $
                                    KnownPeers.resetFailCount
                                        peeraddr
                                        knownPeers
            bigLedgerPeersSet = PublicRootPeers.getBigLedgerPeers publicRootPeers

            st' = st { establishedPeers      = establishedPeers',
                       inProgressPromoteCold = Set.delete peeraddr
                                               (inProgressPromoteCold st),
                       knownPeers            = knownPeers'
                     }
            cs' = peerSelectionStateToCounters st'

        in Decision {
             decisionTrace = if peeraddr `Set.member` bigLedgerPeersSet
                             then [TracePromoteColdBigLedgerPeerDone
                                    targetNumberOfEstablishedBigLedgerPeers
                                    (case cs' of
                                      PeerSelectionCounters { numberOfEstablishedBigLedgerPeers = a } -> a)
                                    peeraddr]
                             else [TracePromoteColdDone
                                    targetNumberOfEstablishedPeers
                                    (case cs' of
                                      PeerSelectionCounters { numberOfEstablishedPeers = a } ->  a)
                                    peeraddr],
             decisionState = st',
             decisionJobs  = []
           }

-- jobPromoteColdPeer' :: forall peeraddr peerconn time m.
--                        (Monad m, Ord peeraddr)
--                    => PeerSelectionActions peeraddr peerconn m
--                    -> PeerSelectionPolicy peeraddr m
--                    -> peeraddr
--                    -> IsBigLedgerPeer
--                    -> Job () m (Completion' peeraddr peerconn)
-- jobPromoteColdPeer' PeerSelectionActions {
--                      peerStateActions = PeerStateActions {establishPeerConnection},
--                      peerConnToPeerSharing
--                    }
--                    PeerSelectionPolicy { policyPeerShareActivationDelay }
--                    peeraddr isBigLedgerPeer =
--     Job job handler () "promoteColdPeer"
--   where
--     -- handler :: SomeException -> m (Completion m peeraddr peerconn)
--     handler e = undefined -- return $
--       -- Completion $ \st@PeerSelectionState {
--       --                 publicRootPeers,
--       --                 stdGen,
--       --                 targets = PeerSelectionTargets {
--       --                             targetNumberOfEstablishedPeers,
--       --                             targetNumberOfEstablishedBigLedgerPeers
--       --                           }
--       --               }
--       --               now ->
--       --   let (failCount, knownPeers') = KnownPeers.incrementFailCount
--       --                                    peeraddr
--       --                                    (knownPeers st)
--       --       (fuzz, stdGen') = randomR (-2, 2 :: Double) stdGen

--       --       -- exponential backoff: 5s, 10s, 20s, 40s, 80s, 160s.
--       --       delay :: DiffTime
--       --       delay = realToFrac fuzz
--       --             + fromIntegral
--       --                 ( baseColdPeerRetryDiffTime
--       --                 * 2 ^ (pred failCount `min` maxColdPeerRetryBackoff)
--       --                 )
--       --       bigLedgerPeersSet = PublicRootPeers.getBigLedgerPeers publicRootPeers

--       --       st' = st { knownPeers            = KnownPeers.setConnectTimes
--       --                                            (Map.singleton
--       --                                              peeraddr
--       --                                              (delay `addTime` now))
--       --                                            knownPeers',
--       --                  inProgressPromoteCold = Set.delete peeraddr
--       --                                            (inProgressPromoteCold st),
--       --                  stdGen = stdGen'
--       --                }
--       --       cs' = peerSelectionStateToCounters st'
--       --   in
--       --     Decision {
--       --       decisionTrace = if peeraddr `Set.member` bigLedgerPeersSet
--       --                       then [TracePromoteColdBigLedgerPeerFailed
--       --                              targetNumberOfEstablishedBigLedgerPeers
--       --                              (case cs' of
--       --                                PeerSelectionCounters { numberOfEstablishedBigLedgerPeers = a } -> a)
--       --                              peeraddr delay e]
--       --                       else [TracePromoteColdFailed
--       --                              targetNumberOfEstablishedPeers
--       --                              (case cs' of
--       --                                PeerSelectionCounters { numberOfEstablishedPeers = a } -> a)
--       --                              peeraddr delay e],
--       --       decisionState = st',
--       --       decisionJobs  = []
--       --     }

--     job :: m (Completion' peeraddr peerconn)
--     job = do
--       --TODO: decide if we should do timeouts here or if we should make that
--       -- the responsibility of establishPeerConnection
--       peerconn <- establishPeerConnection isBigLedgerPeer peeraddr
--       let !peerSharing = peerConnToPeerSharing peerconn

--       return $ \st@PeerSelectionState {
--                                publicRootPeers,
--                                establishedPeers,
--                                knownPeers,
--                                targets = PeerSelectionTargets {
--                                            targetNumberOfEstablishedPeers,
--                                            targetNumberOfEstablishedBigLedgerPeers
--                                          }
--                              }
--                              ->
--         let psTime = case peerSharing of
--                           -- PeerSharingEnabled  -> Just (addTime policyPeerShareActivationDelay now)
--                           PeerSharingDisabled -> Nothing
--             establishedPeers' = EstablishedPeers.insert peeraddr peerconn psTime establishedPeers
--             advertise = case peerSharing of
--                           PeerSharingEnabled  -> DoAdvertisePeer
--                           PeerSharingDisabled -> DoNotAdvertisePeer
--             -- Update PeerSharing value in KnownPeers
--             knownPeers'       = KnownPeers.alter
--                                   (\x -> case x of
--                                     Nothing ->
--                                       KnownPeers.alterKnownPeerInfo
--                                         (Just peerSharing, Just advertise)
--                                         x
--                                     Just _ ->
--                                       KnownPeers.alterKnownPeerInfo
--                                         (Just peerSharing, Nothing)
--                                         x
--                                   )
--                                   (Set.singleton peeraddr)
--                               $ KnownPeers.setSuccessfulConnectionFlag (Set.singleton peeraddr)
--                               $ KnownPeers.clearTepidFlag peeraddr $
--                                     KnownPeers.resetFailCount
--                                         peeraddr
--                                         knownPeers
--             bigLedgerPeersSet = PublicRootPeers.getBigLedgerPeers publicRootPeers

--             st' = st { establishedPeers      = establishedPeers',
--                        inProgressPromoteCold = Set.delete peeraddr
--                                                (inProgressPromoteCold st),
--                        knownPeers            = knownPeers'
--                      }
--             cs' = peerSelectionStateToCounters st'

--         in undefined
--           -- Decision {
--           --    decisionTrace = if peeraddr `Set.member` bigLedgerPeersSet
--           --                    then [TracePromoteColdBigLedgerPeerDone
--           --                           targetNumberOfEstablishedBigLedgerPeers
--           --                           (case cs' of
--           --                             PeerSelectionCounters { numberOfEstablishedBigLedgerPeers = a } -> a)
--           --                           peeraddr]
--           --                    else [TracePromoteColdDone
--           --                           targetNumberOfEstablishedPeers
--           --                           (case cs' of
--           --                             PeerSelectionCounters { numberOfEstablishedPeers = a } ->  a)
--           --                           peeraddr],
--           --    decisionState = st',
--           --    decisionJobs  = []
--           --  }


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
               (MonadSTM m, Ord peeraddr, HasCallStack)
            => PeerSelectionActions peeraddr peerconn m
            -> MkGuardedDecision peeraddr peerconn m
aboveTargetOther actions
                 PeerSelectionPolicy {
                   policyPickWarmPeersToDemote
                 }
                 st@PeerSelectionState {
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
  | let peerSelectionView = peerSelectionStateToView st
        PeerSelectionView {
            viewKnownBigLedgerPeers = (bigLedgerPeersSet, _),
            viewEstablishedPeers    = (_, numEstablishedPeers),
            viewActivePeers         = (_, numActivePeers)
          }
          =
          peerSelectionView
        PeerSelectionCountersHWC {
            numberOfWarmLocalRootPeers = numLocalWarmPeers
          }
          =
          snd <$> peerSelectionView

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
                            - Set.size (inProgressDemoteWarm  Set.\\ bigLedgerPeersSet)
                            - Set.size (inProgressPromoteWarm Set.\\ bigLedgerPeersSet)

        availableToDemote :: Set peeraddr
        availableToDemote = EstablishedPeers.toSet establishedPeers
                              Set.\\ activePeers
                              Set.\\ LocalRootPeers.keysSet localRootPeers
                              Set.\\ bigLedgerPeersSet
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
        decisionJobs  = [ jobDemoteEstablishedPeer actions peeraddr peerconn
                        | (peeraddr, peerconn) <- Map.assocs selectedToDemote' ]
      }

  | otherwise
  = GuardedSkip Nothing


aboveTargetBigLedgerPeers :: forall peeraddr peerconn m.
                             (MonadSTM m, Ord peeraddr, HasCallStack)
                          => PeerSelectionActions peeraddr peerconn m
                          -> MkGuardedDecision peeraddr peerconn m
aboveTargetBigLedgerPeers actions
                          PeerSelectionPolicy {
                            policyPickWarmPeersToDemote
                          }
                          st@PeerSelectionState {
                            publicRootPeers,
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
  | let bigLedgerPeersSet = PublicRootPeers.getBigLedgerPeers publicRootPeers
        PeerSelectionCounters {
            numberOfEstablishedBigLedgerPeers = numEstablishedBigLedgerPeers,
            numberOfActiveBigLedgerPeers      = numActiveBigLedgerPeers
          }
          =
          peerSelectionStateToCounters st

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
                             `Set.intersection` bigLedgerPeersSet
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
        decisionJobs  = [ jobDemoteEstablishedPeer actions peeraddr peerconn
                        | (!peeraddr, !peerconn) <- Map.assocs selectedToDemote' ]
      }

  | otherwise
  = GuardedSkip Nothing


jobDemoteEstablishedPeer :: forall peeraddr peerconn m.
                            (Monad m, Ord peeraddr)
                         => PeerSelectionActions peeraddr peerconn m
                         -> peeraddr
                         -> peerconn
                         -> Job () m (Completion m peeraddr peerconn)
jobDemoteEstablishedPeer PeerSelectionActions{peerStateActions = PeerStateActions {closePeerConnection}}
                         peeraddr peerconn =
    Job job handler () "demoteEstablishedPeer"
  where
    handler :: SomeException -> m (Completion m peeraddr peerconn)
    handler e = return $
      -- It's quite bad if closing fails. The peer is cooling so
      -- we can't remove it from the set of established peers.
      --
      Completion $ \st@PeerSelectionState {
                       publicRootPeers,
                       establishedPeers,
                       inProgressDemoteWarm,
                       targets = PeerSelectionTargets {
                                   targetNumberOfEstablishedPeers,
                                   targetNumberOfEstablishedBigLedgerPeers
                                 }
                     }
                     _ ->
        let inProgressDemoteWarm' = Set.delete peeraddr inProgressDemoteWarm
            bigLedgerPeersSet = PublicRootPeers.getBigLedgerPeers publicRootPeers
         in
        Decision {
          decisionTrace = if peeraddr `Set.member` bigLedgerPeersSet
                          then [TraceDemoteWarmBigLedgerPeerFailed
                                 targetNumberOfEstablishedBigLedgerPeers
                                 (Set.size $ EstablishedPeers.toSet establishedPeers
                                             `Set.intersection`
                                             bigLedgerPeersSet)
                                 peeraddr e]
                          else [TraceDemoteWarmFailed
                                 targetNumberOfEstablishedPeers
                                 (Set.size $ EstablishedPeers.toSet establishedPeers
                                      Set.\\ bigLedgerPeersSet)
                                 peeraddr e],
          decisionState = st {
                            inProgressDemoteWarm = inProgressDemoteWarm'
                          },
          decisionJobs  = []
      }

    job :: m (Completion m peeraddr peerconn)
    job = do
      closePeerConnection peerconn
      return $ Completion $ \st@PeerSelectionState {
                               publicRootPeers,
                               establishedPeers,
                               targets = PeerSelectionTargets {
                                           targetNumberOfEstablishedPeers
                                         }
                             }
                             _now ->
        let establishedPeers' = EstablishedPeers.delete peeraddr
                                                        establishedPeers
            bigLedgerPeersSet = PublicRootPeers.getBigLedgerPeers publicRootPeers
        in Decision {
             decisionTrace = if peeraddr `Set.member` bigLedgerPeersSet
                             then [TraceDemoteWarmBigLedgerPeerDone
                                    targetNumberOfEstablishedPeers
                                    (Set.size $ EstablishedPeers.toSet establishedPeers'
                                                `Set.intersection`
                                                bigLedgerPeersSet)
                                    peeraddr]
                             else [TraceDemoteWarmDone
                                    targetNumberOfEstablishedPeers
                                    (Set.size $ EstablishedPeers.toSet establishedPeers'
                                         Set.\\ bigLedgerPeersSet)
                                    peeraddr],
             decisionState = st {
                               establishedPeers     = establishedPeers',
                               inProgressDemoteWarm = Set.delete peeraddr
                                                        (inProgressDemoteWarm st)
                             },
             decisionJobs  = []
           }
