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
import           Control.Monad.Class.MonadTime
import           Control.Exception (assert)

import qualified Ouroboros.Network.PeerSelection.EstablishedPeers as EstablishedPeers
import qualified Ouroboros.Network.PeerSelection.KnownPeers as KnownPeers
import qualified Ouroboros.Network.PeerSelection.LocalRootPeers as LocalRootPeers
import           Ouroboros.Network.PeerSelection.Types
import           Ouroboros.Network.PeerSelection.Governor.Types
import           Ouroboros.Network.PeerSelection.Governor.ActivePeers (jobDemoteActivePeer)


-- | Monitor 'PeerSelectionTargets', if they change, we just need to update
-- 'PeerSelectionState', since we return it in a 'Decision' action it will be
-- picked by the governor's 'peerSelectionGovernorLoop'.
--
targetPeers :: (MonadSTM m, Ord peeraddr)
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
      check (targets' /= targets && sanePeerSelectionTargets targets')
      -- We simply ignore target updates that are not "sane".

      -- We have to enforce the invariant that the number of root peers is
      -- not more than the target number of known peers. It's unlikely in
      -- practice so it's ok to resolve it arbitrarily using clampToLimit.
      let localRootPeers' = LocalRootPeers.clampToLimit
                              (targetNumberOfKnownPeers targets')
                              localRootPeers
      --TODO: trace when the clamping kicks in, and warn operators

      return $ \_now -> Decision {
        decisionTrace = TraceTargetsChanged targets targets',
        decisionJobs  = [],
        decisionState = st {
                          targets        = targets',
                          localRootPeers = localRootPeers'
                        }
      }


-- | Await for the first result from 'JobPool' and return its 'Decision'.
--
jobs :: MonadSTM m
     => JobPool () m (Completion m peeraddr peerconn)
     -> PeerSelectionState peeraddr peerconn
     -> Guarded (STM m) (TimedDecision m peeraddr peerconn)
jobs jobPool st =
    -- This case is simple because the job pool returns a 'Completion' which is
    -- just a function from the current state to a new 'Decision'.
    Guarded Nothing $ do
      Completion completion <- JobPool.collect jobPool
      return (completion st)


-- | Reconnect delay for peers which asynchronously transitioned to cold state.
--
reconnectDelay :: DiffTime
reconnectDelay = 10
--TODO: make this a policy param

-- | Activation delay after a peer was asynchronously demoted to warm state.
--
activateDelay :: DiffTime
activateDelay = 60
--TODO: make this a policy param


-- | Monitor connections.
--
connections :: forall m peeraddr peerconn.
               (MonadSTM m, Ord peeraddr)
            => PeerSelectionActions peeraddr peerconn m
            -> PeerSelectionState peeraddr peerconn
            -> Guarded (STM m) (TimedDecision m peeraddr peerconn)
connections PeerSelectionActions{
              peerStateActions = PeerStateActions {monitorPeerConnection}
            }
            st@PeerSelectionState {
              activePeers,
              establishedPeers,
              inProgressDemoteHot,
              inProgressDemoteWarm,
              inProgressPromoteWarm
            } =
    Guarded Nothing $ do
      monitorStatus <- traverse monitorPeerConnection
                                (EstablishedPeers.toMap establishedPeers)
      let demotions = asynchronousDemotions monitorStatus
      check (not (Map.null demotions))
      let (demotedToWarm, demotedToCold) = Map.partition (==PeerWarm) demotions
      return $ \now ->
        let activePeers'       = activePeers
                                  Set.\\ Map.keysSet demotions

            -- Note that we do not use establishedStatus' which
            -- has the synchronous ones that are supposed to be
            -- handled elsewhere. We just update the async ones:
            establishedPeers'  = EstablishedPeers.setActivateTime
                                  (Map.keysSet demotedToWarm)
                                  (activateDelay `addTime` now)
                               . EstablishedPeers.deletePeers
                                  (Map.keysSet demotedToCold)
                               $ establishedPeers

            -- Asynchronous transition to cold peer can only be
            -- a result of a failure.
            knownPeers'        = KnownPeers.setConnectTime
                                   (Map.keysSet demotedToCold)
                                   (reconnectDelay `addTime` now)
                               . Set.foldr'
                                   ((snd .) . KnownPeers.incrementFailCount)
                                   (knownPeers st)
                               $ (Map.keysSet demotedToCold)

        in assert
            (let establishedPeersSet' =
                   Map.keysSet (EstablishedPeers.toMap establishedPeers')
             in activePeers' `Set.isSubsetOf` establishedPeersSet'
             &&    Map.keysSet
                     (EstablishedPeers.toMap establishedPeers')
                == establishedPeersSet')

            Decision {
              decisionTrace = TraceDemoteAsynchronous demotions,
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
                                                      Set.\\ Map.keysSet demotedToCold

                                -- Note that we do not need to adjust
                                -- inProgressDemoteWarm or inProgressDemoteHot
                                -- here since we define the async demotions
                                -- to not include peers in those sets. Instead,
                                -- those ones will complete synchronously.
                              }
          }
  where
    -- Those demotions that occurred not as a result of action by the governor.
    -- They're further classified into demotions to warm, and demotions to cold.
    asynchronousDemotions :: Map peeraddr PeerStatus -> Map peeraddr PeerStatus
    asynchronousDemotions = Map.mapMaybeWithKey asyncDemotion

    -- The asynchronous ones, those not directed by the governor, are:
    -- hot -> warm, warm -> cold and hot -> cold, other than the ones in the in
    -- relevant progress set.
    asyncDemotion :: peeraddr -> PeerStatus -> Maybe PeerStatus

    -- a hot -> warm transition has occurred if it is now warm, and it was
    -- hot, but not in the set we were deliberately demoting synchronously
    asyncDemotion peeraddr PeerWarm
      | peeraddr `Set.member`    activePeers
      , peeraddr `Set.notMember` inProgressDemoteHot  = Just PeerWarm

    -- a warm -> cold transition has occurred if it is now cold, and it was
    -- warm, but not in the set we were deliberately demoting synchronously
    asyncDemotion peeraddr PeerCold
      | peeraddr `EstablishedPeers.member` establishedPeers
      , peeraddr `Set.notMember` activePeers
      , peeraddr `Set.notMember` inProgressDemoteWarm = Just PeerCold

    -- a hot -> cold transition has occurred if it is now cold, and it was hot
    asyncDemotion peeraddr PeerCold
      | peeraddr `Set.member`    activePeers          = Just PeerCold

    asyncDemotion _        _                          = Nothing


-----------------------------------------------
-- Monitoring changes to the local root peers
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
      -- practice so it's ok to resolve it arbitrarily using clampToLimit.
      localRootPeersRaw <- readLocalRootPeers
      let localRootPeers' = LocalRootPeers.clampToLimit
                              targetNumberOfKnownPeers
                          . LocalRootPeers.fromGroups
                          $ localRootPeersRaw
      check (localRootPeers' /= localRootPeers)
      --TODO: trace when the clamping kicks in, and warn operators

      let added       = LocalRootPeers.toMap localRootPeers' Map.\\
                        LocalRootPeers.toMap localRootPeers
          removed     = LocalRootPeers.toMap localRootPeers  Map.\\
                        LocalRootPeers.toMap localRootPeers'
          addedSet    = Map.keysSet added
          removedSet  = Map.keysSet removed
          knownPeers' = KnownPeers.insert addedSet
                        -- We do not immediately remove old ones from the
                        -- known peers set because we may have established
                        -- connections
                      $ knownPeers

          -- We have to adjust the publicRootPeers to maintain the invariant
          -- that the local and public sets are non-overlapping.
          publicRootPeers' = publicRootPeers Set.\\
                             LocalRootPeers.keysSet localRootPeers'

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
          selectedToDemote' = EstablishedPeers.toMap establishedPeers
                               `Map.restrictKeys` selectedToDemote
      return $ \_now ->

          assert (Set.isSubsetOf
                    publicRootPeers'
                   (KnownPeers.toSet knownPeers'))
        . assert (Set.isSubsetOf
                   (LocalRootPeers.keysSet localRootPeers')
                   (KnownPeers.toSet knownPeers'))

        $ Decision {
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
