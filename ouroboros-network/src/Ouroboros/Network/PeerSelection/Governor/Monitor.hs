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
  , inboundPeers
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
import           Control.Monad.Class.MonadTime.SI
import           System.Random (randomR)

import           Ouroboros.Network.ExitPolicy (ReconnectDelay)
import qualified Ouroboros.Network.ExitPolicy as ExitPolicy
import           Ouroboros.Network.PeerSelection.Governor.ActivePeers
                     (jobDemoteActivePeer)
import           Ouroboros.Network.PeerSelection.Governor.Types hiding
                     (PeerSelectionCounters (..))
import           Ouroboros.Network.PeerSelection.PeerAdvertise
                     (PeerAdvertise (..))
import qualified Ouroboros.Network.PeerSelection.State.EstablishedPeers as EstablishedPeers
import qualified Ouroboros.Network.PeerSelection.State.KnownPeers as KnownPeers
import qualified Ouroboros.Network.PeerSelection.State.LocalRootPeers as LocalRootPeers
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
              bigLedgerPeers,
              localRootPeers,
              targets
            } =
    Guarded Nothing $ do
      targets' <- readPeerSelectionTargets
      check (targets' /= targets && sanePeerSelectionTargets targets')
      -- We simply ignore target updates that are not "sane".

      let -- We have to enforce the invariant that the number of root peers is
          -- not more than the target number of known peers. It's unlikely in
          -- practice so it's ok to resolve it arbitrarily using clampToLimit.
          --
          -- TODO: we ought to add a warning if 'clampToLimit' modified local
          -- root peers, even though this is unexpected in the most common
          -- scenarios.
          localRootPeers' = LocalRootPeers.clampToLimit
                              (targetNumberOfKnownPeers targets')
                              localRootPeers

          -- We have to enforce that local and big ledger peers are disjoint.
          bigLedgerPeers' = bigLedgerPeers
                            Set.\\
                            LocalRootPeers.keysSet localRootPeers'

      return $ \_now -> Decision {
        decisionTrace = [TraceTargetsChanged targets targets'],
        decisionJobs  = [],
        decisionState = st {
                          targets        = targets',
                          localRootPeers = localRootPeers',
                          bigLedgerPeers = bigLedgerPeers'
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

-- | Monitor new inbound connections
--
inboundPeers :: forall m peeraddr peerconn.
                 (MonadSTM m, Ord peeraddr)
             => PeerSelectionActions peeraddr peerconn m
             -> PeerSelectionState peeraddr peerconn
             -> Guarded (STM m) (TimedDecision m peeraddr peerconn)
inboundPeers PeerSelectionActions{
               readNewInboundConnection
             }
             st@PeerSelectionState {
               knownPeers
             } =
  Guarded Nothing $ do
    (addr, ps) <- readNewInboundConnection
    return $ \_ ->
      let knownPeers' =
            KnownPeers.alter
              (\x -> case x of
                Nothing ->
                  KnownPeers.alterKnownPeerInfo
                    (Just ps, Just DoAdvertisePeer, Nothing)
                    x
                Just _ ->
                  KnownPeers.alterKnownPeerInfo
                    (Just ps, Nothing, Nothing)
                    x
              )
              (Set.singleton addr)
              knownPeers
       in Decision {
            decisionTrace = [TraceKnownInboundConnection addr ps],
            decisionJobs = [],
            decisionState = st { knownPeers = knownPeers' }
          }

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
              bigLedgerPeers,
              localRootPeers,
              activePeers,
              establishedPeers,
              inProgressDemoteHot,
              inProgressDemoteWarm,
              inProgressPromoteWarm,
              inProgressDemoteToCold,
              fuzzRng
            } =
    Guarded Nothing $ do
      -- Get previously cooling peers
      monitorStatus <- traverse monitorPeerConnection
                                (EstablishedPeers.toMap establishedPeers)
      let demotions = asynchronousDemotions monitorStatus
      check (not (Map.null demotions))
      let (demotedToWarm, demotedToCoolingOrCold) = Map.partition ((==PeerWarm) . fst) demotions
          (demotedToCold, demotedToCooling) = Map.partition ((==PeerCold) . fst) demotedToCoolingOrCold
          -- fuzz reconnect delays
          (aFuzz, fuzzRng')  = randomR (-5, 5 :: Double) fuzzRng
          (rFuzz, fuzzRng'') = randomR (-2, 2 :: Double) fuzzRng'
          demotions' = (\a@(peerState, reconnectDelay) -> case peerState of
                         PeerHot  -> a
                         PeerWarm ->
                           ( peerState
                           , (\x -> (x + realToFrac aFuzz) `max` 0) <$> reconnectDelay
                           )
                         PeerCooling ->
                           ( peerState
                           , (\x -> (x + realToFrac rFuzz) `max` 0) <$> reconnectDelay
                           )
                         PeerCold -> a
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

            publicRootDemotions     = nonLocalDemotions
                   `Map.withoutKeys`  bigLedgerPeers
            bigLedgerPeersDemotions = nonLocalDemotions
                   `Map.restrictKeys` bigLedgerPeers

            -- Peers in this state won't be able to be promoted nor demoted.
            inProgressDemoteToCold' =
              (inProgressDemoteToCold Set.\\ Map.keysSet demotedToCold )
               <> Map.keysSet demotedToCooling
        in assert (activePeers' `Set.isSubsetOf`
                     Map.keysSet (EstablishedPeers.toMap establishedPeers'))
            Decision {
              decisionTrace = [ TraceDemoteLocalAsynchronous localDemotions
                              | not $ null localDemotions ]
                           <> [ TraceDemoteAsynchronous publicRootDemotions
                              | not $ null publicRootDemotions ]
                           <> [ TraceDemoteBigLedgerPeersAsynchronous
                                  bigLedgerPeersDemotions
                              | not $ null bigLedgerPeersDemotions ],
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
                                                      Set.\\ Map.keysSet demotedToCoolingOrCold,

                                inProgressDemoteToCold =
                                  inProgressDemoteToCold',

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
    asynchronousDemotions :: Map peeraddr (PeerStatus, Maybe ReconnectDelay)
                          -> Map peeraddr (PeerStatus, Maybe ReconnectDelay)
    asynchronousDemotions = Map.mapMaybeWithKey asyncDemotion

    -- The asynchronous ones, those not directed by the governor, are:
    -- hot -> warm, warm -> cold and hot -> cold, other than the ones in the in
    -- relevant progress set.
    asyncDemotion :: peeraddr
                  -> (PeerStatus, Maybe ReconnectDelay)
                  -> Maybe (PeerStatus, Maybe ReconnectDelay)

    -- a hot -> warm transition has occurred if it is now warm, and it was
    -- hot, but not in the set we were deliberately demoting synchronously
    asyncDemotion peeraddr (PeerWarm, returnCommand)
      | peeraddr `Set.member`    activePeers
      , peeraddr `Set.notMember` inProgressDemoteHot  = Just (PeerWarm, returnCommand)

    -- a `{PeerHot,PeerWarm} -> PeerCooling` transition has occurred if it is
    -- now cooling, and it was warm, but not in the set of peers being demoted
    -- synchronously, e.g. `inProgressDemote{Hot,Warm}`.
    --
    -- If the peer is a member of the `inProgressDemoteToCold` set it means we
    -- already accounted it, since we are adding peers to
    -- `inProgressDemoteToCold` only if this function returns
    -- `Just (PeerCooling, ...)`.
    --
    -- A peer in the `PeerCooling` state is going to be a member of the established set
    -- until its connection is effectively terminated on the outbound side when
    -- it will become `PeerCold`. We check if the peer does not exist in the
    -- `inProgressDemoteToCold` to see if it is a new asynchronous demotion.
    --
    asyncDemotion peeraddr (PeerCooling, returnCommand)
      | peeraddr `EstablishedPeers.member` establishedPeers
      , peeraddr `Set.notMember` activePeers
      , peeraddr `Set.notMember` inProgressDemoteWarm
      , peeraddr `Set.notMember` inProgressDemoteToCold = Just (PeerCooling, returnCommand)

    -- a hot -> cooling transition has occurred if it is now cooling, and it was hot
    asyncDemotion peeraddr (PeerCooling, returnCommand)
      | peeraddr `Set.member`    activePeers
      , peeraddr `Set.notMember` inProgressDemoteHot
      , peeraddr `Set.notMember` inProgressDemoteToCold = Just (PeerCooling, returnCommand)

    -- a cooling -> cold transition has occurred if it is now cold, and it was cooling
    asyncDemotion peeraddr (PeerCold, returnCommand)
      | peeraddr `EstablishedPeers.member` establishedPeers || peeraddr `Set.member` activePeers
      , peeraddr `Set.notMember` inProgressDemoteWarm
      , peeraddr `Set.notMember` inProgressDemoteHot  = Just (PeerCold, returnCommand)
      -- Note:
      --
      -- We need to take care of direct transitions too `PeerCold` without going
      -- through `PeerCooling` which can be triggered by
      -- `deactivatePeerConnection`.
      --
      -- Also the peer might not be in `inProgressDemoteToCold`, that could
      -- happen in `outbound-governor` skipped seeing `PeerCooling`.  This can
      -- happen under load or we could be just unlucky.

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
             bigLedgerPeers,
             localRootPeers,
             publicRootPeers,
             knownPeers,
             establishedPeers,
             activePeers,
             inProgressDemoteHot,
             inProgressDemoteToCold,
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
          addedInfoMap = Map.map (\a -> (Nothing, Just a, Nothing)) added
          removedSet   = Map.keysSet removed
          knownPeers'  = KnownPeers.insert addedInfoMap knownPeers
                        -- We do not immediately remove old ones from the
                        -- known peers set because we may have established
                        -- connections

          localRootPeersSet = LocalRootPeers.keysSet localRootPeers'

          -- We have to adjust the publicRootPeers to maintain the invariant
          -- that the local and public sets are non-overlapping.
          publicRootPeers' = publicRootPeers Set.\\ localRootPeersSet

          -- We have to adjust the bigLedgerPeers to maintain the invariant that
          -- the local and big ledger peer sets are non-overlapping.
          bigLedgerPeers'  = bigLedgerPeers Set.\\ localRootPeersSet

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
                                Set.\\ inProgressDemoteToCold
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
            decisionTrace = [TraceLocalRootPeersChanged localRootPeers
                                                        localRootPeers'],
            decisionState = st {
                              bigLedgerPeers      = bigLedgerPeers',
                              localRootPeers      = localRootPeers',
                              publicRootPeers     = publicRootPeers',
                              knownPeers          = knownPeers',
                              inProgressDemoteHot = inProgressDemoteHot
                                                 <> selectedToDemote
                            },
            decisionJobs  = [ jobDemoteActivePeer actions policy peeraddr peerconn
                          | (peeraddr, peerconn) <- Map.assocs selectedToDemote' ]
          }
