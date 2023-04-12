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
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set

import           Control.Concurrent.JobPool (JobPool)
import qualified Control.Concurrent.JobPool as JobPool
import           Control.Exception (assert)
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTime
import           System.Random (randomR)

import           Ouroboros.Network.ExitPolicy (ReconnectDelay)
import qualified Ouroboros.Network.ExitPolicy as ExitPolicy
import qualified Ouroboros.Network.PeerSelection.EstablishedPeers as EstablishedPeers
import           Ouroboros.Network.PeerSelection.Governor.ActivePeers
                     (jobDemoteActivePeer)
import           Ouroboros.Network.PeerSelection.Governor.Types hiding
                     (PeerSelectionCounters (..))
import qualified Ouroboros.Network.PeerSelection.KnownPeers as KnownPeers
import           Ouroboros.Network.PeerSelection.LedgerPeers (IsLedgerPeer (..))
import qualified Ouroboros.Network.PeerSelection.LocalRootPeers as LocalRootPeers
import           Ouroboros.Network.PeerSelection.PeerSharing.Type
                     (PeerSharing (..))
import           Ouroboros.Network.PeerSelection.Types


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
        decisionTrace = [TraceTargetsChanged targets targets'],
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
      Completion completion <- JobPool.waitForJob jobPool
      return (completion st)


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
              localRootPeers,
              activePeers,
              establishedPeers,
              inProgressDemoteHot,
              inProgressDemoteWarm,
              inProgressPromoteWarm,
              fuzzRng
            } =
    Guarded Nothing $ do
      monitorStatus <- traverse monitorPeerConnection
                                (EstablishedPeers.toMap establishedPeers)
      let demotions = asynchronousDemotions monitorStatus
      check (not (Map.null demotions))
      let (demotedToWarm, demotedToCold) = Map.partition ((==PeerWarm) . fst) demotions
          -- fuzz reconnect delays
          (aFuzz, fuzzRng')  = randomR (-5, 5 :: Double) fuzzRng
          (rFuzz, fuzzRng'') = randomR (-2, 2 :: Double) fuzzRng'
          demotions' = (\a@(peerState, reconnectDelay) -> case peerState of
                         PeerHot  -> a
                         PeerWarm -> ( peerState
                                     , (\x -> (x + realToFrac aFuzz) `max` 0) <$> reconnectDelay
                                     )
                         PeerCold -> ( peerState
                                     , (\x -> (x + realToFrac rFuzz) `max` 0) <$> reconnectDelay
                                     )
                       ) <$> demotions
      return $ \now ->
        let -- Remove all asynchronous demotions from 'activePeers'
            activePeers'       = activePeers Set.\\ Map.keysSet demotions'

            -- Note that we do not use establishedStatus' which
            -- has the synchronous ones that are supposed to be
            -- handled elsewhere. We just update the async ones:
            establishedPeers'  = EstablishedPeers.setActivateTimes
                                   ( (\(_, a) -> ExitPolicy.reconnectDelay (fromMaybe 0 a) `addTime` now)
                                      -- 'monitorPeerConnection' returns
                                      -- 'Nothing' iff all mini-protocols are
                                      -- either still running or 'NotRunning'
                                      -- (e.g.  this possible for warm or hot
                                      -- peers).  In such case we don't want to
                                      -- `setActivateTimes`
                                      <$> Map.filter (\(_, a) -> a /= Nothing)
                                                     demotedToWarm
                                   )
                               . EstablishedPeers.deletePeers
                                  (Map.keysSet demotedToCold)
                               $ establishedPeers

            -- Asynchronous transition to cold peer can only be
            -- a result of a failure.
            knownPeers'        = KnownPeers.setConnectTimes
                                    ( (\(_, a) -> ExitPolicy.reconnectDelay (fromMaybe 0 a) `addTime` now)
                                      <$> demotedToCold
                                    )
                               . Set.foldr'
                                   ((snd .) . KnownPeers.incrementFailCount)
                                   (knownPeers st)
                               $ Map.keysSet demotedToCold
            (localDemotions, nonLocalDemotions) =
              Map.partitionWithKey
                (\peer _ -> peer `LocalRootPeers.member` localRootPeers)
                demotions'

        in assert (activePeers' `Set.isSubsetOf`
                     Map.keysSet (EstablishedPeers.toMap establishedPeers'))
            Decision {
              decisionTrace = [ TraceDemoteLocalAsynchronous localDemotions
                              , TraceDemoteAsynchronous nonLocalDemotions],
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
                                                      Set.\\ Map.keysSet demotedToCold,

                                -- Note that we do not need to adjust
                                -- inProgressDemoteWarm or inProgressDemoteHot
                                -- here since we define the async demotions
                                -- to not include peers in those sets. Instead,
                                -- those ones will complete synchronously.

                                fuzzRng = fuzzRng''
                              }
          }
  where
    -- Those demotions that occurred not as a result of action by the governor.
    -- They're further classified into demotions to warm, and demotions to cold.
    asynchronousDemotions :: Map peeraddr (PeerStatus, Maybe ReconnectDelay) -> Map peeraddr (PeerStatus, Maybe ReconnectDelay)
    asynchronousDemotions = Map.mapMaybeWithKey asyncDemotion

    -- The asynchronous ones, those not directed by the governor, are:
    -- hot -> warm, warm -> cold and hot -> cold, other than the ones in the in
    -- relevant progress set.
    asyncDemotion :: peeraddr -> (PeerStatus, Maybe ReconnectDelay) -> Maybe (PeerStatus, Maybe ReconnectDelay)

    -- a hot -> warm transition has occurred if it is now warm, and it was
    -- hot, but not in the set we were deliberately demoting synchronously
    asyncDemotion peeraddr (PeerWarm, returnCommand)
      | peeraddr `Set.member`    activePeers
      , peeraddr `Set.notMember` inProgressDemoteHot  = Just (PeerWarm, returnCommand)

    -- a warm -> cold transition has occurred if it is now cold, and it was
    -- warm, but not in the set we were deliberately demoting synchronously
    asyncDemotion peeraddr (PeerCold, returnCommand)
      | peeraddr `EstablishedPeers.member` establishedPeers
      , peeraddr `Set.notMember` activePeers
      , peeraddr `Set.notMember` inProgressDemoteWarm = Just (PeerCold, returnCommand)

    -- a hot -> cold transition has occurred if it is now cold, and it was hot
    asyncDemotion peeraddr (PeerCold, returnCommand)
      | peeraddr `Set.member`    activePeers
      , peeraddr `Set.notMember` inProgressDemoteHot  = Just (PeerCold, returnCommand)

    asyncDemotion _        _                          = Nothing


-----------------------------------------------
-- Monitoring changes to the local root peers
--


-- | Monitor local roots using 'readLocalRootPeers' 'STM' action.
--
localRoots :: forall peeraddr peerconn m.
              (MonadSTM m, Ord peeraddr)
           => PeerSelectionActions peeraddr peerconn m
           -> PeerSelectionPolicy peeraddr m
           -> PeerSelectionState peeraddr peerconn
           -> Guarded (STM m) (TimedDecision m peeraddr peerconn)
localRoots actions@PeerSelectionActions{ readLocalRootPeers
                                       }
           policy
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

      let added        = LocalRootPeers.toMap localRootPeers' Map.\\
                         LocalRootPeers.toMap localRootPeers
          removed      = LocalRootPeers.toMap localRootPeers  Map.\\
                         LocalRootPeers.toMap localRootPeers'
          -- LocalRoots are not ledger!
          addedInfoMap = Map.map (\a -> (NoPeerSharing, a, IsNotLedgerPeer)) added
          removedSet   = Map.keysSet removed
          knownPeers'  = KnownPeers.insert addedInfoMap knownPeers
                        -- We do not immediately remove old ones from the
                        -- known peers set because we may have established
                        -- connections

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
                               `Map.restrictKeys` (selectedToDemote Set.\\ inProgressDemoteHot)
      return $ \_now ->

          assert (Set.isSubsetOf
                    publicRootPeers'
                   (KnownPeers.toSet knownPeers'))
        . assert (Set.isSubsetOf
                   (LocalRootPeers.keysSet localRootPeers')
                   (KnownPeers.toSet knownPeers'))

        $ Decision {
            decisionTrace = [TraceLocalRootPeersChanged localRootPeers
                                                        localRootPeers'],
            decisionState = st {
                              localRootPeers      = localRootPeers',
                              publicRootPeers     = publicRootPeers',
                              knownPeers          = knownPeers',
                              inProgressDemoteHot = inProgressDemoteHot
                                                 <> selectedToDemote
                            },
            decisionJobs  = [ jobDemoteActivePeer actions policy peeraddr peerconn
                          | (peeraddr, peerconn) <- Map.assocs selectedToDemote' ]
          }
