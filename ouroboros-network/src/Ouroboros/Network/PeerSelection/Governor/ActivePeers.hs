{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.PeerSelection.Governor.ActivePeers
  ( belowTarget
  , aboveTarget

  , jobDemoteActivePeer
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           Control.Monad.Class.MonadSTM
import           Control.Concurrent.JobPool (Job(..))
import           Control.Exception (SomeException, assert)

import           Ouroboros.Network.PeerSelection.Types
import           Ouroboros.Network.PeerSelection.KnownPeers (KnownPeerInfo(..))
import qualified Ouroboros.Network.PeerSelection.KnownPeers as KnownPeers
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
belowTarget actions
            PeerSelectionPolicy {
              policyPickWarmPeersToPromote
            }
            st@PeerSelectionState {
              knownPeers,
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
  , numEstablishedPeers - numActivePeers
                        - numPromoteInProgress - numDemoteInProgress > 0
  = Guarded Nothing $ do
          -- The availableToPromote is non-empty due to the second guard.
          -- The numPeersToPromote is positive due to the first guard.
      let availableToPromote :: Map peeraddr KnownPeerInfo
          availableToPromote = KnownPeers.toMap knownPeers
                                `Map.intersection` establishedPeers
                                `Map.withoutKeys` activePeers
                                `Map.withoutKeys` inProgressPromoteWarm
                                `Map.withoutKeys` inProgressDemoteWarm
          numPeersToPromote  = targetNumberOfActivePeers
                             - numActivePeers
                             - numPromoteInProgress
      selectedToPromote <- pickPeers
                             policyPickWarmPeersToPromote
                             availableToPromote
                             numPeersToPromote
      let selectedToPromote' :: Map peeraddr peerconn
          selectedToPromote' = establishedPeers
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

  | otherwise
  = GuardedSkip Nothing
  where
    numEstablishedPeers, numActivePeers, numPromoteInProgress :: Int
    numEstablishedPeers  = Map.size establishedPeers
    numActivePeers       = Set.size activePeers
    numPromoteInProgress = Set.size inProgressPromoteWarm
    numDemoteInProgress  = Set.size inProgressDemoteWarm


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
      Completion $ \st _now -> Decision {
        decisionTrace = TracePromoteWarmFailed peeraddr e,
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
      return $ Completion $ \st _now ->
        assert (peeraddr `Map.member` establishedPeers st)
        Decision {
          decisionTrace = TracePromoteWarmDone peeraddr,
          decisionState = st {
                            activePeers           = Set.insert peeraddr
                                                      (activePeers st),
                            establishedStatus     = Map.insert peeraddr PeerHot
                                                      (establishedStatus st),
                            inProgressPromoteWarm = Set.delete peeraddr
                                                      (inProgressPromoteWarm st)
                          },
          decisionJobs  = []
        }


----------------------------
-- Active peers above target
--

-- | If we are above the target of /hot peers/ we demote some of the /warm
-- peers/, according to 'policyPickHotPeersToDemote'.
--
aboveTarget :: forall peeraddr peerconn m.
               (MonadSTM m, Ord peeraddr)
            => PeerSelectionActions peeraddr peerconn m
            -> MkGuardedDecision peeraddr peerconn m
aboveTarget actions
            PeerSelectionPolicy {
              policyPickHotPeersToDemote
            }
            st@PeerSelectionState {
              knownPeers,
              establishedPeers,
              activePeers,
              inProgressDemoteHot,
              targets = PeerSelectionTargets {
                          targetNumberOfActivePeers
                        }
            }
    -- Are we above the target for number of active peers?
    -- Or more precisely, how many active peers could we demote?
  | let numActivePeers, numPeersToDemote :: Int
        numActivePeers   = Set.size activePeers
        -- The main constraint on how many to demote is the difference in the
        -- number we have now vs the target. We must also subtract the number
        -- we're already demoting so we don't repeat the same work.
        numPeersToDemote = numActivePeers
                         - targetNumberOfActivePeers
                         - Set.size inProgressDemoteHot
  , numPeersToDemote > 0
  = Guarded Nothing $ do

      let availableToDemote :: Map peeraddr KnownPeerInfo
          availableToDemote = KnownPeers.toMap knownPeers
                               `Map.restrictKeys` activePeers
                               `Map.withoutKeys` inProgressDemoteHot
      selectedToDemote <- pickPeers
                            policyPickHotPeersToDemote
                            availableToDemote
                            numPeersToDemote
      let selectedToDemote' :: Map peeraddr peerconn
          selectedToDemote' = establishedPeers
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
      Completion $ \st _now -> Decision {
        decisionTrace = TraceDemoteHotFailed peeraddr e,
        decisionState = st {
                          inProgressDemoteHot = Set.delete peeraddr
                                                  (inProgressDemoteHot st)
                        },
        decisionJobs  = []
      }

    job :: m (Completion m peeraddr peerconn)
    job = do
      deactivatePeerConnection peerconn
      return $ Completion $ \st _now ->
        assert (peeraddr `Map.member` establishedPeers st)
        Decision {
          decisionTrace = TraceDemoteHotDone peeraddr,
          decisionState = st {
                            activePeers         = Set.delete peeraddr
                                                    (activePeers st),
                            establishedStatus   = Map.insert peeraddr PeerWarm
                                                    (establishedStatus st),
                            inProgressDemoteHot = Set.delete peeraddr
                                                    (inProgressDemoteHot st)
                          },
          decisionJobs  = []
        }
