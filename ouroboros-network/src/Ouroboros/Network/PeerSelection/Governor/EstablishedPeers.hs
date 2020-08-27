{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.PeerSelection.Governor.EstablishedPeers
  ( belowTarget
  , aboveTarget
  ) where

import           Data.Semigroup (Min(..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           Control.Concurrent.JobPool (Job(..))
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTime
import           Control.Exception (SomeException)

import           Ouroboros.Network.PeerSelection.Types
import           Ouroboros.Network.PeerSelection.KnownPeers (KnownPeerInfo(..))
import qualified Ouroboros.Network.PeerSelection.KnownPeers as KnownPeers
import           Ouroboros.Network.PeerSelection.Governor.Types


---------------------------------
-- Established peers below target
--


-- | If we are below the target of /warm peers/ we promote /cold peers/
-- according to 'policyPickColdPeersToPromote'.
--
belowTarget :: forall peeraddr peerconn m.
               (MonadSTM m, Ord peeraddr)
            => PeerSelectionActions peeraddr peerconn m
            -> MkGuardedDecision peeraddr peerconn m
belowTarget actions
            PeerSelectionPolicy {
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
  , Set.size availableToConnect - numEstablishedPeers - numConnectInProgress > 0
  = Guarded Nothing $ do
      -- The availableToPromote here is non-empty due to the second guard.
      -- The known peers map restricted to the connect set is the same size as
      -- the connect set (because it is a subset). The establishedPeers is a
      -- subset of the connect set and we also know that there is no overlap
      -- between inProgressPromoteCold and establishedPeers. QED.
      --
      -- The numPeersToPromote is positive based on the first guard.
      --
      let availableToPromote :: Map peeraddr KnownPeerInfo
          availableToPromote = KnownPeers.toMap knownPeers
                                `Map.restrictKeys` availableToConnect
                                 Map.\\ establishedPeers
                                `Map.withoutKeys` inProgressPromoteCold
          numPeersToPromote  = targetNumberOfEstablishedPeers
                             - numEstablishedPeers
                             - numConnectInProgress
      selectedToPromote <- pickPeers
                             policyPickColdPeersToPromote
                             availableToPromote
                             numPeersToPromote
      return Decision {
        decisionTrace = TracePromoteColdPeers
                          targetNumberOfEstablishedPeers
                          numEstablishedPeers
                          selectedToPromote,
        decisionState = st {
                          inProgressPromoteCold = inProgressPromoteCold
                                               <> selectedToPromote
                        },
        decisionJobs  = [ jobPromoteColdPeer actions peer
                        | peer <- Set.toList selectedToPromote ]
      }

    -- If we could connect except that there are no peers currently available
    -- then we return the next wakeup time (if any)
  | numEstablishedPeers + numConnectInProgress < targetNumberOfEstablishedPeers
  = GuardedSkip (Min <$> KnownPeers.minConnectTime knownPeers)

  | otherwise
  = GuardedSkip Nothing
  where
    numEstablishedPeers, numConnectInProgress :: Int
    numEstablishedPeers  = Map.size establishedPeers
    numConnectInProgress = Set.size inProgressPromoteCold
    availableToConnect   = KnownPeers.availableToConnect knownPeers


baseColdPeerRetryDiffTime :: Int
baseColdPeerRetryDiffTime = 5

maxColdPeerRetryBackoff :: Int
maxColdPeerRetryBackoff = 5


jobPromoteColdPeer :: forall peeraddr peerconn m.
                       (Monad m, Ord peeraddr)
                   => PeerSelectionActions peeraddr peerconn m
                   -> peeraddr
                   -> Job m (Completion m peeraddr peerconn)
jobPromoteColdPeer PeerSelectionActions{peerStateActions = PeerStateActions {establishPeerConnection}} peeraddr =
    Job job handler "promoteColdPeer"
  where
    handler :: SomeException -> Completion m peeraddr peerconn
    handler e =
      Completion $ \st now ->
        let (failCount, knownPeers') = KnownPeers.incrementFailCount
                                         peeraddr
                                         (knownPeers st)

            -- exponential backoff: 5s, 10s, 20s, 40s, 80s, 160s.
            delay :: DiffTime
            delay = fromIntegral $
              2 ^ (pred failCount `min` maxColdPeerRetryBackoff) * baseColdPeerRetryDiffTime
        in
          Decision {
            decisionTrace = TracePromoteColdFailed peeraddr delay e,
            decisionState = st {
                              knownPeers            = KnownPeers.setConnectTime
                                                        (Set.singleton peeraddr)
                                                        (delay `addTime` now)
                                                        knownPeers',
                              inProgressPromoteCold = Set.delete peeraddr
                                                        (inProgressPromoteCold st)
                            },
            decisionJobs  = []
          }

    job :: m (Completion m peeraddr peerconn)
    job = do
      --TODO: decide if we should do timeouts here or if we should make that
      -- the responsibility of establishPeerConnection
      peerconn <- establishPeerConnection peeraddr
      return $ Completion $ \st _now -> Decision {
        decisionTrace = TracePromoteColdDone peeraddr,
        decisionState = st {
                          establishedPeers      = Map.insert peeraddr peerconn
                                                    (establishedPeers st),
                          establishedStatus     = Map.insert peeraddr PeerWarm
                                                    (establishedStatus st),
                          inProgressPromoteCold = Set.delete peeraddr
                                                    (inProgressPromoteCold st)
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
               (MonadSTM m, Ord peeraddr)
            => PeerSelectionActions peeraddr peerconn m
            -> MkGuardedDecision peeraddr peerconn m
aboveTarget actions
            PeerSelectionPolicy {
              policyPickWarmPeersToDemote
            }
            st@PeerSelectionState {
              knownPeers,
              establishedPeers,
              activePeers,
              inProgressDemoteWarm,
              inProgressPromoteWarm,
              targets = PeerSelectionTargets {
                          targetNumberOfEstablishedPeers
                        }
            }
    -- Are we above the target for number of established peers?
    -- Or more precisely, how many established peers could we demote?
    -- We only want to pick established peers that are not active, since for
    -- active one we need to demote them first.
  | let numEstablishedPeers, numActivePeers, numPeersToDemote :: Int
        numEstablishedPeers = Map.size establishedPeers
        numActivePeers      = Set.size activePeers
        -- One constraint on how many to demote is the difference in the
        -- number we have now vs the target. The other constraint is that
        -- we pick established peers that are not also active. These
        -- constraints combine by taking the minimum. We must also subtract
        -- the number we're demoting so we don't repeat the same work. And
        -- cannot demote ones we're in the process of promoting.
        numPeersToDemote    = min (numEstablishedPeers
                                   - targetNumberOfEstablishedPeers)
                                  (numEstablishedPeers
                                   - numActivePeers)
                            - Set.size inProgressDemoteWarm
                            - Set.size inProgressPromoteWarm
  , numPeersToDemote > 0
  = Guarded Nothing $ do

      let availableToDemote :: Map peeraddr KnownPeerInfo
          availableToDemote = KnownPeers.toMap knownPeers
                               `Map.intersection` establishedPeers
                               `Map.withoutKeys` activePeers
                               `Map.withoutKeys` inProgressDemoteWarm
                               `Map.withoutKeys` inProgressPromoteWarm
      selectedToDemote <- pickPeers
                            policyPickWarmPeersToDemote
                            availableToDemote
                            numPeersToDemote
      let selectedToDemote' :: Map peeraddr peerconn
          selectedToDemote' = establishedPeers
                                `Map.restrictKeys` selectedToDemote

      return Decision {
        decisionTrace = TraceDemoteWarmPeers
                          targetNumberOfEstablishedPeers
                          numEstablishedPeers
                          selectedToDemote,
        decisionState = st {
                          inProgressDemoteWarm = inProgressDemoteWarm
                                              <> selectedToDemote
                        },
        decisionJobs  = [ jobDemoteEstablishedPeer actions peeraddr peerconn
                        | (peeraddr, peerconn) <- Map.assocs selectedToDemote' ]
      }

  | otherwise
  = GuardedSkip Nothing


jobDemoteEstablishedPeer :: forall peeraddr peerconn m.
                            (Monad m, Ord peeraddr)
                         => PeerSelectionActions peeraddr peerconn m
                         -> peeraddr
                         -> peerconn
                         -> Job m (Completion m peeraddr peerconn)
jobDemoteEstablishedPeer PeerSelectionActions{peerStateActions = PeerStateActions {closePeerConnection}}
                         peeraddr peerconn =
    Job job handler "demoteEstablishedPeer"
  where
    handler :: SomeException -> Completion m peeraddr peerconn
    handler e =
      -- It's quite bad if closing fails, but the best we can do is revert to
      -- the state where we believed this peer is still warm, since then we
      -- can have another go or perhaps it'll be closed for other reasons and
      -- our monitoring will notice it.
      Completion $ \st _now -> Decision {
        decisionTrace = TraceDemoteWarmFailed peeraddr e,
        decisionState = st {
                          inProgressDemoteWarm = Set.delete peeraddr
                                                   (inProgressDemoteWarm st)
                        },
        decisionJobs  = []
      }

    job :: m (Completion m peeraddr peerconn)
    job = do
      closePeerConnection peerconn
      return $ Completion $ \st _now -> Decision {
        decisionTrace = TraceDemoteWarmDone peeraddr,
        decisionState = st {
                          establishedPeers     = Map.delete peeraddr
                                                   (establishedPeers st),
                          establishedStatus    = Map.delete peeraddr
                                                   (establishedStatus st),
                          inProgressDemoteWarm = Set.delete peeraddr
                                                   (inProgressDemoteWarm st)
                        },
        decisionJobs  = []
      }
