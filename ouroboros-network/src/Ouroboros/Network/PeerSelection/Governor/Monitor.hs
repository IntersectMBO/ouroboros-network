{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module contains governor decisions for monitoring tasks:
--
-- * monitoring target peer changes
-- * monitoring results governor's job results
-- * monitoring connections
--
module Ouroboros.Network.PeerSelection.Governor.Monitor
  ( targetPeers
  , jobs
  , connections
  , localRoots
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Set (Set)

import           Control.Concurrent.JobPool (JobPool)
import qualified Control.Concurrent.JobPool as JobPool
import           Control.Monad.Class.MonadSTM
import           Control.Exception (assert)

import qualified Ouroboros.Network.PeerSelection.KnownPeers as KnownPeers
import           Ouroboros.Network.PeerSelection.Types
import           Ouroboros.Network.PeerSelection.Governor.Types
import           Ouroboros.Network.PeerSelection.Governor.ActivePeers (jobDemoteActivePeer)


-- | Monitor 'PeerSelectionTargets', if they change, we just need to update
-- 'PeerSelectionState', since we return it in a 'Decision' action it will be
-- picked by the governor's 'peerSelectionGovernorLoop'.
--
targetPeers :: MonadSTM m
            => PeerSelectionActions peeraddr peerconn m
            -> PeerSelectionState peeraddr peerconn
            -> Guarded (STM m) (TimedDecision m peeraddr peerconn)
targetPeers PeerSelectionActions{readPeerSelectionTargets}
            st@PeerSelectionState{
              localRootPeers,
              targets
            } =
    Guarded Nothing $ do
      targets' <- readPeerSelectionTargets
      check (targets' /= targets)

      -- We have to enforce the invariant that the number of root peers is
      -- not more than the target number of known peers. It's unlikely in
      -- practice so it's ok to resolve it arbitrarily using Map.take.
      let localRootPeers' = Map.take (targetNumberOfKnownPeers targets')
                                     localRootPeers

      return $ \_now -> Decision {
        decisionTrace = TraceTargetsChanged targets targets',
        decisionJobs  = [],
        decisionState = assert (sanePeerSelectionTargets targets')
                        st {
                          targets        = targets',
                          localRootPeers = localRootPeers'
                        }
      }


-- | Await for the first result from 'JobPool' and return its 'Decision'.
--
jobs :: MonadSTM m
     => JobPool m (Completion m peeraddr peerconn)
     -> PeerSelectionState peeraddr peerconn
     -> Guarded (STM m) (TimedDecision m peeraddr peerconn)
jobs jobPool st =
    -- This case is simple because the job pool returns a 'Completion' which is
    -- just a function from the current state to a new 'Decision'.
    Guarded Nothing $ do
      Completion completion <- JobPool.collect jobPool
      return (completion st)


-- | Monitor connections.
--
connections :: forall m peeraddr peerconn.
               (MonadSTM m, Ord peeraddr)
            => PeerSelectionActions peeraddr peerconn m
            -> PeerSelectionState peeraddr peerconn
            -> Guarded (STM m) (TimedDecision m peeraddr peerconn)
connections PeerSelectionActions{peerStateActions = PeerStateActions {monitorPeerConnection}}
            st@PeerSelectionState {
              activePeers,
              establishedPeers,
              establishedStatus,
              inProgressDemoteHot,
              inProgressDemoteWarm
            } =
    Guarded Nothing $ do
      establishedStatus' <- traverse monitorPeerConnection establishedPeers
      let demotions = asynchronousDemotions establishedStatus
                                            establishedStatus'
      check (not (Map.null demotions))
      let (demotedToWarm, demotedToCold) = Map.partition (==PeerWarm) demotions
      return $ \_now -> Decision {
        decisionTrace = TraceDemoteAsynchronous demotions,
        decisionJobs  = [],
        decisionState = st {
                          activePeers       = activePeers
                                                Set.\\ Map.keysSet demotions,
                          establishedPeers  = establishedPeers
                                                Map.\\ demotedToCold,

                          -- Note that we do not use establishedStatus' which
                          -- has the synchronous ones that are supposed to be
                          -- handled elsewhere. We just update the async ones:
                          establishedStatus = demotedToWarm
                                                <> establishedStatus
                                                Map.\\ demotedToCold
                        }
      }
  where
    -- Those demotions that occurred not as a result of action by the governor.
    -- They're further classified into demotions to warm, and demotions to cold.
    asynchronousDemotions :: Map peeraddr PeerStatus
                          -> Map peeraddr PeerStatus
                          -> Map peeraddr PeerStatus
    asynchronousDemotions old new =
      Map.mapMaybeWithKey asyncDemotion
        (Map.filter (uncurry (>))
           (Map.intersectionWith (,) old new))

    -- The asynchronous ones, those not directed by the governor, are:
    -- hot -> warm, warm -> cold and hot -> cold, other than the ones in the in
    -- relevant progress set.
    asyncDemotion :: peeraddr -> (PeerStatus, PeerStatus) -> Maybe PeerStatus
    asyncDemotion peeraddr (PeerHot, PeerWarm)
      | peeraddr `Set.notMember` inProgressDemoteHot  = Just PeerWarm
    asyncDemotion peeraddr (PeerWarm, PeerCold)
      | peeraddr `Set.notMember` inProgressDemoteWarm = Just PeerCold
    asyncDemotion _        (PeerHot, PeerCold)        = Just PeerCold
    asyncDemotion _        _                          = Nothing


--------------------------------
-- Local root peers below target
--


-- | Monitor local roots using 'readLocalRootPeers' 'STM' action.
--
localRoots :: forall peeraddr peerconn m.
              (MonadSTM m, Ord peeraddr)
           => PeerSelectionActions peeraddr peerconn m
           -> PeerSelectionState peeraddr peerconn
           -> Guarded (STM m) (TimedDecision m peeraddr peerconn)
localRoots actions@PeerSelectionActions{readLocalRootPeers}
           st@PeerSelectionState{
             localRootPeers,
             publicRootPeers,
             knownPeers,
             establishedPeers,
             activePeers,
             inProgressDemoteHot,
             targets = PeerSelectionTargets{targetNumberOfKnownPeers}
           } =
    Guarded Nothing $ do
      -- We have to enforce the invariant that the number of root peers is
      -- not more than the target number of known peers. It's unlikely in
      -- practice so it's ok to resolve it arbitrarily using Map.take.
      localRootPeers' <- Map.take targetNumberOfKnownPeers <$> readLocalRootPeers
      check (localRootPeers' /= localRootPeers)

      let added       = localRootPeers' Map.\\ localRootPeers
          removed     = localRootPeers  Map.\\ localRootPeers'
          addedSet    = Map.keysSet added
          removedSet  = Map.keysSet removed
          knownPeers' = KnownPeers.insert PeerSourceLocalRoot
                                          (added Map.!)
                                          addedSet

                        -- We do not immediately remove old ones from the
                        -- known peers set because we may have established
                        -- connections, but we mark them so that policy
                        -- functions can prioritise them to forget:
                      . KnownPeers.insert PeerSourceStaleRoot
                                          (const DoNotAdvertisePeer)
                                          removedSet
                      $ knownPeers

          -- We have to adjust the publicRootPeers to maintain the invariant
          -- that the local and public sets are non-overlapping.
          publicRootPeers' = publicRootPeers Set.\\ Map.keysSet localRootPeers'

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

          selectedToDemote  = activePeers `Set.intersection` removedSet
          selectedToDemote' = establishedPeers
                               `Map.restrictKeys` selectedToDemote
      return $ \_now -> Decision {
        decisionTrace = TraceLocalRootPeersChanged localRootPeers
                                                   localRootPeers',
        decisionState = st {
                          localRootPeers      = localRootPeers',
                          publicRootPeers     = publicRootPeers',
                          knownPeers          = knownPeers',
                          inProgressDemoteHot = inProgressDemoteHot
                                             <> selectedToDemote
                        },
        decisionJobs  = [ jobDemoteActivePeer actions peeraddr peerconn
                        | (peeraddr, peerconn) <- Map.assocs selectedToDemote' ]
      }
