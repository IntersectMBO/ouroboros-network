{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
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
  , monitorLedgerStateJudgement
  , monitorBootstrapPeersFlag
  , waitForSystemToQuiesce
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Set (Set)
import Data.Set qualified as Set

import Control.Concurrent.JobPool (JobPool)
import Control.Concurrent.JobPool qualified as JobPool
import Control.Exception (assert)
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadTime.SI
import System.Random (randomR)

import Ouroboros.Network.ExitPolicy (RepromoteDelay)
import Ouroboros.Network.ExitPolicy qualified as ExitPolicy
import Ouroboros.Network.PeerSelection.Bootstrap (isBootstrapPeersEnabled,
           isNodeAbleToMakeProgress, requiresBootstrapPeers, UseBootstrapPeers (..))
import Ouroboros.Network.PeerSelection.Governor.ActivePeers
           (jobDemoteActivePeer)
import Ouroboros.Network.PeerSelection.Governor.Types hiding
           (PeerSelectionCounters (..))
import Ouroboros.Network.PeerSelection.LedgerPeers.Type
           (LedgerStateJudgement (..))
import Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise (..))
import Ouroboros.Network.PeerSelection.PeerTrustable (PeerTrustable (..))
import Ouroboros.Network.PeerSelection.PublicRootPeers qualified as PublicRootPeers
import Ouroboros.Network.PeerSelection.State.EstablishedPeers qualified as EstablishedPeers
import Ouroboros.Network.PeerSelection.State.KnownPeers qualified as KnownPeers
import Ouroboros.Network.PeerSelection.State.LocalRootPeers qualified as LocalRootPeers
import Ouroboros.Network.PeerSelection.Types

-- | Used to set 'bootstrapPeersTimeout' for crashing the node in a critical
-- failure case
--
governor_BOOTSTRAP_PEERS_TIMEOUT :: DiffTime
governor_BOOTSTRAP_PEERS_TIMEOUT = 15 * 60

-- | Constants establishing target number of big ledger peers
-- in TooOld (stale chain) state when in Genesis mode
--
governor_GENESIS_KNOWN_BIG_LEDGER_PEERS :: Int
governor_GENESIS_KNOWN_BIG_LEDGER_PEERS = 50
governor_GENESIS_ESTABLISHED_BIG_LEDGER_PEERS :: Int
governor_GENESIS_ESTABLISHED_BIG_LEDGER_PEERS = 40
governor_GENESIS_ACTIVE_BIG_LEDGER_PEERS :: Int
governor_GENESIS_ACTIVE_BIG_LEDGER_PEERS = 30

-- | Monitor 'PeerSelectionTargets', if they change, we just need to update
-- 'PeerSelectionState', since we return it in a 'Decision' action it will be
-- picked by the governor's 'peerSelectionGovernorLoop'.
--
-- It should be noted if the node is in bootstrap mode (i.e. in a sensitive
-- state) then, until the node reaches a clean state, this monitoring action
-- will be disabled and thus churning will be disabled as well.
--
targetPeers :: (MonadSTM m, Ord peeraddr)
            => PeerSelectionActions peeraddr peerconn m
            -> PeerSelectionState peeraddr peerconn
            -> Guarded (STM m) (TimedDecision m peeraddr peerconn)
targetPeers PeerSelectionActions{readPeerSelectionTargets}
            st@PeerSelectionState{
              publicRootPeers,
              localRootPeers,
              targets,
              ledgerStateJudgement,
              bootstrapPeersFlag,
              hasOnlyBootstrapPeers
            } =
    Guarded Nothing $ do
      targets' <- readPeerSelectionTargets
      check ( isNodeAbleToMakeProgress bootstrapPeersFlag
                                       ledgerStateJudgement
                                       hasOnlyBootstrapPeers
            && targets' /= targets
            && sanePeerSelectionTargets targets'
            )
      -- We simply ignore target updates that are not "sane".

      let usingBootstrapPeers = requiresBootstrapPeers bootstrapPeersFlag
                                                       ledgerStateJudgement
          -- We have to enforce the invariant that the number of root peers is
          -- not more than the target number of known peers. It's unlikely in
          -- practice so it's ok to resolve it arbitrarily using clampToLimit.
          --
          -- Here we need to make sure that we prioritise the trustable peers
          -- by clamping to trustable first.
          --
          -- TODO: we ought to add a warning if 'clampToLimit' modified local
          -- root peers, even though this is unexpected in the most common
          -- scenarios.
          localRootPeers' =
              LocalRootPeers.clampToLimit
                              (targetNumberOfKnownPeers targets')
            $ (if usingBootstrapPeers
                  then LocalRootPeers.clampToTrustable
                  else id)
            $ localRootPeers

          -- We have to enforce that local and big ledger peers are disjoint.
          publicRootPeers' = publicRootPeers
                             `PublicRootPeers.difference`
                             LocalRootPeers.keysSet localRootPeers'

      return $ \_now -> Decision {
        decisionTrace = [TraceTargetsChanged targets targets'],
        decisionJobs  = [],
        decisionState = st {
                          targets        = targets',
                          localRootPeers = localRootPeers',
                          publicRootPeers = publicRootPeers'
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
-- It should be noted if the node is in bootstrap mode (i.e. in a sensitive
-- state) then this monitoring action will be disabled.
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
             , ledgerStateJudgement
             , bootstrapPeersFlag
             }
  -- If we are in a sensitive state we can't let any non-trustable peer into our
  -- knownPeers set
  | not (requiresBootstrapPeers bootstrapPeersFlag ledgerStateJudgement) =
    Guarded Nothing $ do
      (addr, ps) <- readNewInboundConnection
      return $ \_ ->
        let knownPeers' =
              KnownPeers.setSuccessfulConnectionFlag addr $
              KnownPeers.alter
                (\x -> case x of
                  Nothing ->
                    KnownPeers.alterKnownPeerInfo
                      (Just ps, Just DoAdvertisePeer)
                      x
                  Just _ ->
                    KnownPeers.alterKnownPeerInfo
                      (Just ps, Nothing)
                      x
                )
                (Set.singleton addr)
                knownPeers
         in Decision {
              decisionTrace = [TraceKnownInboundConnection addr ps],
              decisionJobs = [],
              decisionState = st { knownPeers = knownPeers' }
            }
  | otherwise = GuardedSkip Nothing

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
              publicRootPeers,
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
          (aFuzz, fuzzRng')  = randomR (0.1, 10 :: Double) fuzzRng
          (rFuzz, fuzzRng'') = randomR (0.1, 4  :: Double) fuzzRng'
          demotions' = (\a@(peerState, repromoteDelay) -> case peerState of
                         PeerHot  -> a
                         PeerWarm ->
                           ( peerState
                           , (\x -> (x + realToFrac aFuzz) `max` 0) <$> repromoteDelay
                           )
                         PeerCooling -> a
                         PeerCold ->
                           ( peerState
                           , (\x -> (x + realToFrac rFuzz) `max` 0) <$> repromoteDelay
                           )
                       ) <$> demotions
      return $ \now ->
        let -- Remove all asynchronous demotions from 'activePeers'
            activePeers'       = activePeers Set.\\ Map.keysSet demotions'

            -- Note that we do not use establishedStatus' which
            -- has the synchronous ones that are supposed to be
            -- handled elsewhere. We just update the async ones:
            establishedPeers'  = EstablishedPeers.setActivateTimes
                                   ( (\(_, a) -> ExitPolicy.repromoteDelay (fromMaybe 0 a) `addTime` now)
                                      -- 'monitorPeerConnection' returns
                                      -- 'Nothing' iff all mini-protocols are
                                      -- either still running or 'NotStarted'
                                      -- (e.g.  this possible for warm or hot
                                      -- peers).  In such case we don't want to
                                      -- `setActivateTimes`
                                      <$> Map.filter (isJust . snd) demotedToWarm
                                   )
                               . EstablishedPeers.deletePeers
                                  (Map.keysSet demotedToCold)
                               $ establishedPeers

            -- Asynchronous transition to cold peer can only be
            -- a result of a failure.
            knownPeers'        = KnownPeers.setConnectTimes
                                    ( (\(_, a) -> ExitPolicy.repromoteDelay (fromMaybe 0 a) `addTime` now)
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
                   `Map.withoutKeys`  bigLedgerPeersSet
            bigLedgerPeersDemotions = nonLocalDemotions
                   `Map.restrictKeys` bigLedgerPeersSet

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
    bigLedgerPeersSet = PublicRootPeers.getBigLedgerPeers publicRootPeers
    -- Those demotions that occurred not as a result of action by the governor.
    -- They're further classified into demotions to warm, and demotions to cold.
    asynchronousDemotions :: Map peeraddr (PeerStatus, Maybe RepromoteDelay)
                          -> Map peeraddr (PeerStatus, Maybe RepromoteDelay)
    asynchronousDemotions = Map.mapMaybeWithKey asyncDemotion

    -- The asynchronous ones, those not directed by the governor, are:
    -- hot -> warm, warm -> cold and hot -> cold, other than the ones in the in
    -- relevant progress set.
    asyncDemotion :: peeraddr
                  -> (PeerStatus, Maybe RepromoteDelay)
                  -> Maybe (PeerStatus, Maybe RepromoteDelay)

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
-- If the current ledger state is TooOld we can only trust our trustable local
-- root peers, this means that if we remove any local root peer we
-- might no longer abide by the invariant that we are only connected to
-- trusted peers. E.g. Local peers = A, B*, C* (* means trusted peer), if
-- the node is in bootstrap mode and decided to reconfigure the local root
-- peers to the following set: A*, B, C*, D*, E. Notice that B is no longer
-- trusted, however we will keep a connection to it until the outbound
-- governor notices it and disconnects from it.
localRoots :: forall peeraddr peerconn m.
              (MonadSTM m, Ord peeraddr)
           => PeerSelectionActions peeraddr peerconn m
           -> PeerSelectionState peeraddr peerconn
           -> Guarded (STM m) (TimedDecision m peeraddr peerconn)
localRoots actions@PeerSelectionActions{ readLocalRootPeers
                                       }
           st@PeerSelectionState{
             localRootPeers,
             publicRootPeers,
             knownPeers,
             establishedPeers,
             activePeers,
             inProgressDemoteHot,
             inProgressDemoteToCold,
             targets = PeerSelectionTargets{targetNumberOfKnownPeers},
             ledgerStateJudgement,
             bootstrapPeersFlag,
             hasOnlyBootstrapPeers
           }
  | isNodeAbleToMakeProgress bootstrapPeersFlag ledgerStateJudgement hasOnlyBootstrapPeers =
    Guarded Nothing $ do
      -- We have to enforce the invariant that the number of root peers is
      -- not more than the target number of known peers. It's unlikely in
      -- practice so it's ok to resolve it arbitrarily using clampToLimit.
      --
      -- Here we need to make sure that we prioritise the trustable peers
      -- by clamping to trustable first.
      localRootPeersRaw <- readLocalRootPeers
      let inSensitiveState = requiresBootstrapPeers bootstrapPeersFlag ledgerStateJudgement
          localRootPeers' = LocalRootPeers.clampToLimit
                              targetNumberOfKnownPeers
                          . (if inSensitiveState
                                then LocalRootPeers.clampToTrustable
                                else id)
                          . LocalRootPeers.fromGroups
                          $ localRootPeersRaw
      check (localRootPeers' /= localRootPeers)

      --TODO: trace when the clamping kicks in, and warn operators

      let added        = LocalRootPeers.toMap localRootPeers' Map.\\
                         LocalRootPeers.toMap localRootPeers
          removed      = LocalRootPeers.toMap localRootPeers  Map.\\
                         LocalRootPeers.toMap localRootPeers'
          -- LocalRoots are not ledger!
          addedInfoMap = Map.map (\(pa, _) -> (Nothing, Just pa)) added
          removedSet   = Map.keysSet removed
          knownPeers'  = KnownPeers.insert addedInfoMap knownPeers
                        -- We do not immediately remove old ones from the
                        -- known peers set because we may have established
                        -- connections

          localRootPeersSet = LocalRootPeers.keysSet localRootPeers'

          -- We have to adjust the publicRootPeers to maintain the invariant
          -- that the local and public sets are non-overlapping.
          --
          publicRootPeers' = publicRootPeers
                             `PublicRootPeers.difference`
                             localRootPeersSet

          -- Non trustable peers that the outbound governor might keep. These
          -- should be demoted forgot as soon as possible. In order to do that
          -- we set 'hasOnlyBootstrapPeers' to False and
          -- 'ledgerStateJudgement' to TooOld in order to force clean state
          -- reconnections.
          hasOnlyBootstrapPeers' =
            Set.null $ KnownPeers.toSet knownPeers'
                       `Set.difference`
                       (  LocalRootPeers.keysSet localRootPeers'
                       <> PublicRootPeers.getBootstrapPeers publicRootPeers')

          -- If the node is in a vulnerable position, i.e. connected to a
          -- local root that is no longer deemed trustable we have to
          -- force clean state reconnections. We do this by setting the
          -- 'ledgerStateJudgement' value to 'YoungEnough' which
          -- 'monitorLedgerStateJudgement' will then read the original
          -- 'TooOld' value and trigger the clean state protocol.
          --
          -- Note that if the state actually changes to 'YoungEnough' it
          -- doesn't really matter.
          --
          ledgerStateJudgement' =
            if requiresBootstrapPeers bootstrapPeersFlag ledgerStateJudgement && not hasOnlyBootstrapPeers'
            then YoungEnough
            else ledgerStateJudgement

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
                    (PublicRootPeers.toSet publicRootPeers')
                   (KnownPeers.toSet knownPeers'))
        . assert (Set.isSubsetOf
                   (LocalRootPeers.keysSet localRootPeers')
                   (KnownPeers.toSet knownPeers'))

        $ Decision {
            decisionTrace = [TraceLocalRootPeersChanged localRootPeers
                                                        localRootPeers']
                         ++ if not hasOnlyBootstrapPeers'
                               then [TraceLedgerStateJudgementChanged YoungEnough]
                               else [],
            decisionState = st {
                              localRootPeers      = localRootPeers',
                              publicRootPeers     = publicRootPeers',
                              knownPeers          = knownPeers',
                              inProgressDemoteHot = inProgressDemoteHot
                                                 <> selectedToDemote,
                              hasOnlyBootstrapPeers = hasOnlyBootstrapPeers',
                              ledgerStateJudgement  = ledgerStateJudgement'
                            },
            decisionJobs  = [ jobDemoteActivePeer actions peeraddr peerconn
                          | (peeraddr, peerconn) <- Map.assocs selectedToDemote' ]
          }
  | otherwise = GuardedSkip Nothing


-- | Monitor 'UseBootstrapPeers' flag.
--
-- The user might reconfigure the node at any point to change the value of
-- UseBootstrapPeers. Essentially the user can enable or disable bootstrap
-- peers at any time. Since 'monitorLedgerStateJudgement' will act on the
-- ledger state judgement value changing, this monitoring action should only
-- be responsible for either disabling bootstrap peers (in case the user
-- disables this flag) or enabling the 'monitorLedgerStateJudgement' action to
-- work correctly in the case the node finds itself in bootstrap mode. In
-- order to achieve this behavior, when bootstrap peers are disabled we should
-- update the ledger state judgement value to 'YoungEnough'; and
-- 'hasOnlyBootstrapPeers' to 'False'.
--
-- Here's a brief explanation why this works. There's 4 scenarios to consider:
--
-- 1. The node is in 'YoungEnough' state and the user
--  1.1. Enables bootstrap peers: In this case since the node is caught up
--  nothing should happen, so setting the LSJ to YoungEnough state is
--  idempotent.
--  1.2. Disables bootstrap peers: In this case, since the node is caught up,
--  its functioning can't really be distinguished from that of a node that has
--  bootstrap peers disabled. So changing the LSJ and 'hasOnlyBootstrapPeers'
--  flag is idempotent.
-- 2. The node is in 'TooOld' state and the user
--  2.1. Enables bootstrap peers: If the node is behind, enabling bootstrap
--  peers will enable 'monitorLedgerStateJudgement'. So if we set the LSJ to
--  be in 'YoungEnough' state it is going to make sure 'monitorLedgerStateJudgement'
--  observes the 'TooOld' state, triggering the right measures to be taken.
--  2.2. Disables bootstrap peers: If this is the case, we want to let the
--  peer connect to non-trusted peers, so just updating the boostrap peers
--  flag will enable the previously disabled monitoring actions.
--
monitorBootstrapPeersFlag :: ( MonadSTM m
                             , Ord peeraddr
                             )
                          => PeerSelectionActions peeraddr peerconn m
                          -> PeerSelectionState peeraddr peerconn
                          -> Guarded (STM m) (TimedDecision m peeraddr peerconn)
monitorBootstrapPeersFlag PeerSelectionActions { readUseBootstrapPeers }
                          st@PeerSelectionState { bootstrapPeersFlag
                                                , knownPeers
                                                , establishedPeers
                                                , publicRootPeers
                                                , inProgressPromoteCold
                                                , inProgressPromoteWarm
                                                , useGenesisFlag
                                                }
  | useGenesisFlag = GuardedSkip Nothing
  | otherwise =
  Guarded Nothing $ do
    ubp <- readUseBootstrapPeers
    check (ubp /= bootstrapPeersFlag)
    let nonEstablishedBootstrapPeers =
          PublicRootPeers.getBootstrapPeers publicRootPeers
          Set.\\
          EstablishedPeers.toSet establishedPeers
          Set.\\
          (inProgressPromoteCold <> inProgressPromoteWarm)
    return $ \_now ->
      Decision {
        decisionTrace = [TraceUseBootstrapPeersChanged ubp],
        decisionJobs  = [],
        decisionState =
          st { bootstrapPeersFlag    = ubp
             , ledgerStateJudgement  = YoungEnough
             , hasOnlyBootstrapPeers = False
             , bootstrapPeersTimeout = Nothing
             , knownPeers =
                 KnownPeers.delete
                   nonEstablishedBootstrapPeers
                   knownPeers
             , publicRootPeers =
                 PublicRootPeers.difference
                   publicRootPeers
                   nonEstablishedBootstrapPeers
             }
      }

-- | Monitor 'LedgerStateJudgement', if it changes, depending on the value we
-- just need to update 'PeerSelectionTargets'. If the ledger state changed to
-- 'TooOld' we set all other targets to 0 and the governor waits for all
-- active connections to drop and then set the targets to sensible values for
-- getting caught up again.
-- However if the state changes to 'YoungEnough' we reset the targets back to
-- their original values.
--
-- It should be noted if the node has bootstrap peers disabled then this
-- monitoring action will be disabled.
--
-- It should also be noted that churning is ignored until the node converges
-- to a clean state. I.e., it will disconnect from the targets source of truth.
--
monitorLedgerStateJudgement :: ( MonadSTM m
                               , Ord peeraddr
                               )
                            => PeerSelectionActions peeraddr peerconn m
                            -> PeerSelectionState peeraddr peerconn
                            -> Guarded (STM m) (TimedDecision m peeraddr peerconn)
monitorLedgerStateJudgement PeerSelectionActions{ readLedgerStateJudgement }
                            st@PeerSelectionState{ bootstrapPeersFlag,
                                                   publicRootPeers,
                                                   knownPeers,
                                                   establishedPeers,
                                                   inProgressPromoteCold,
                                                   inProgressPromoteWarm,
                                                   ledgerStateJudgement,
                                                   useGenesisFlag,
                                                   targets
                                                 }
  | isBootstrapPeersEnabled bootstrapPeersFlag || useGenesisFlag =
    Guarded Nothing $ do
      lsj <- readLedgerStateJudgement
      check (lsj /= ledgerStateJudgement)
      st' <- if | useGenesisFlag && lsj == TooOld ->
                  -- todo: ledgerStateJudgement is initially TooOld, and if
                  -- lsj is TooOld from the start, then we will not reach here
                  return $ const st { ledgerStateJudgement = lsj,
                                      targets = targets {
                                        targetNumberOfActivePeers = 0,
                                        targetNumberOfKnownBigLedgerPeers = governor_GENESIS_KNOWN_BIG_LEDGER_PEERS,
                                        targetNumberOfEstablishedBigLedgerPeers = governor_GENESIS_ESTABLISHED_BIG_LEDGER_PEERS,
                                        targetNumberOfActiveBigLedgerPeers = governor_GENESIS_ACTIVE_BIG_LEDGER_PEERS } }
                | not useGenesisFlag && lsj == YoungEnough ->
                  let bootstrapPeers = PublicRootPeers.getBootstrapPeers publicRootPeers
                      nonEstablishedBootstrapPeers =
                        bootstrapPeers
                        Set.\\
                        EstablishedPeers.toSet establishedPeers
                        Set.\\
                        (inProgressPromoteCold <> inProgressPromoteWarm) 
                  in return $ const st { ledgerStateJudgement = lsj,
                                         knownPeers = KnownPeers.delete
                                                        nonEstablishedBootstrapPeers
                                                        knownPeers,
                                         publicRootPeers = PublicRootPeers.difference
                                                             publicRootPeers
                                                             nonEstablishedBootstrapPeers }
                | not useGenesisFlag && lsj == TooOld  ->
                  return $ \now ->
                    st { ledgerStateJudgement = lsj,
                         targets = PeerSelectionTargets {
                           targetNumberOfRootPeers                 = 0,
                           targetNumberOfKnownPeers                = 0,
                           targetNumberOfEstablishedPeers          = 0,
                           targetNumberOfActivePeers               = 0,
                           targetNumberOfKnownBigLedgerPeers       = 0,
                           targetNumberOfEstablishedBigLedgerPeers = 0,
                           targetNumberOfActiveBigLedgerPeers      = 0 },
                         -- We have to enforce the invariant that the number of root peers is
                         -- not more than the target number of known peers. It's unlikely in
                         -- practice so it's ok to resolve it arbitrarily using clampToLimit.
                         localRootPeers = LocalRootPeers.empty,
                         bootstrapPeersTimeout = Just (addTime governor_BOOTSTRAP_PEERS_TIMEOUT now),
                         hasOnlyBootstrapPeers = False }
                | otherwise -> return $ const st { ledgerStateJudgement = lsj }
      return $ \now ->
        Decision {
          decisionTrace = [TraceLedgerStateJudgementChanged lsj],
          decisionJobs  = [],
          decisionState = st' now
        }       
  | otherwise = GuardedSkip Nothing

-- | If the node just got in the TooOld state, the node just had its targets
-- adjusted to get rid of all peers. This jobs monitors the node state and when
-- it has arrived to a clean (quiesced) state it sets the 'hasOnlyBootstrapPeers'
-- flag on which will unblock the 'localRoots' and 'targetPeers' monitoring actions,
-- allowing the node to make progress by only connecting to trusted peers.
--
-- It should be noted if the node is _not_ in bootstrap mode (i.e. _not_ in a
-- sensitive state) then this monitoring action will be disabled.
--
-- If the node takes more than 15 minutes to converge to a clean state the
-- node will crash itself so it can be brought back on again in a clean state.
-- If the node takes more than 15 minutes to converge to a clean state it
-- means something really bad must be going on, such a global network outage,
-- DNS issues, or there could be an actual bug in the code. In any case we'll
-- detect that and have a way to observe such cases.
--
waitForSystemToQuiesce :: ( MonadSTM m
                          , Ord peeraddr
                          )
                       => PeerSelectionState peeraddr peerconn
                       -> Guarded (STM m) (TimedDecision m peeraddr peerconn)
waitForSystemToQuiesce st@PeerSelectionState{
                            ledgerStateJudgement
                          , bootstrapPeersFlag
                          , knownPeers
                          , localRootPeers
                          , publicRootPeers
                          , inProgressPromoteCold
                          , inProgressPromoteWarm
                          , inProgressPeerShareReqs
                          , inProgressPublicRootsReq
                          , inProgressBigLedgerPeersReq
                          , hasOnlyBootstrapPeers
                          }
  -- Is the node in sensitive state?
  | requiresBootstrapPeers bootstrapPeersFlag ledgerStateJudgement
  -- Has the node still haven't reached a clean state
  , not hasOnlyBootstrapPeers
  -- Are the local root peers all trustable?
  , all (\case
            (_, IsTrustable) -> True
            _                -> False
        )
        (LocalRootPeers.toMap localRootPeers)
  -- Are the known peers all trustable or all in progress to be demoted?
  , KnownPeers.toSet knownPeers `Set.isSubsetOf`
    (  PublicRootPeers.getBootstrapPeers publicRootPeers
    <> LocalRootPeers.keysSet (LocalRootPeers.clampToTrustable localRootPeers)
    )

  -- Are there still any in progress promotion jobs?
  , Set.null inProgressPromoteCold
  , Set.null inProgressPromoteWarm
  , inProgressPeerShareReqs == 0
  , not inProgressBigLedgerPeersReq
  , not inProgressPublicRootsReq =
    Guarded Nothing $ do
      return $ \_now ->
        Decision { decisionTrace = [TraceOnlyBootstrapPeers],
                   decisionJobs  = [],
                   decisionState = st { hasOnlyBootstrapPeers = True
                                      , bootstrapPeersTimeout = Nothing
                                      }
        }
  | otherwise = GuardedSkip Nothing
