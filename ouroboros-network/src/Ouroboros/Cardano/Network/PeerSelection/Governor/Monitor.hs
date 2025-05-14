{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module contains governor decisions for monitoring tasks:
--
-- * monitoring local root peer config changes
-- * monitoring changes to the peer target numbers
-- * monitoring the completion of asynchronous governor job
-- * monitoring connections
--
module Ouroboros.Cardano.Network.PeerSelection.Governor.Monitor
  ( targetPeers
  , localRoots
  , monitorLedgerStateJudgement
  , monitorBootstrapPeersFlag
  , waitForSystemToQuiesce
  ) where

import Data.Set qualified as Set

import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI

import Cardano.Network.ConsensusMode
import Cardano.Network.PeerSelection.Bootstrap (isBootstrapPeersEnabled,
           isNodeAbleToMakeProgress, requiresBootstrapPeers)
import Cardano.Network.PeerSelection.PeerTrustable (PeerTrustable (..))
import Cardano.Network.Types (LedgerStateJudgement (..))
import Control.Exception (assert)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Ouroboros.Cardano.Network.LedgerPeerConsensusInterface qualified as Cardano
import Ouroboros.Cardano.Network.PeerSelection.Governor.PeerSelectionActions qualified as Cardano
import Ouroboros.Cardano.Network.PeerSelection.Governor.PeerSelectionState qualified as Cardano
import Ouroboros.Cardano.Network.PublicRootPeers qualified as Cardano
import Ouroboros.Network.PeerSelection.Governor.ActivePeers
           (jobDemoteActivePeer)
import Ouroboros.Network.PeerSelection.Governor.Monitor (jobVerifyPeerSnapshot)
import Ouroboros.Network.PeerSelection.Governor.Types hiding
           (PeerSelectionCounters)
import Ouroboros.Network.PeerSelection.LedgerPeers.Type
           (LedgerPeersConsensusInterface (..))
import Ouroboros.Network.PeerSelection.PublicRootPeers qualified as PublicRootPeers
import Ouroboros.Network.PeerSelection.State.EstablishedPeers qualified as EstablishedPeers
import Ouroboros.Network.PeerSelection.State.KnownPeers qualified as KnownPeers
import Ouroboros.Network.PeerSelection.State.LocalRootPeers
           (LocalRootConfig (..))
import Ouroboros.Network.PeerSelection.State.LocalRootPeers qualified as LocalRootPeers
import Ouroboros.Network.PeerSelection.Types


-- | Used to set 'bootstrapPeersTimeout' for crashing the node in a critical
-- failure case
--
governor_BOOTSTRAP_PEERS_TIMEOUT :: DiffTime
governor_BOOTSTRAP_PEERS_TIMEOUT = 15 * 60

-- | Monitor 'PeerSelectionTargets', if they change, we just need to update
-- 'PeerSelectionState', since we return it in a 'Decision' action it will be
-- picked by the governor's 'peerSelectionGovernorLoop'.
--
-- It should be noted if the node is in bootstrap mode (i.e. in a sensitive
-- state) then, until the node reaches a clean state, this monitoring action
-- will be disabled and thus churning will be disabled as well.
--
-- On the other hand, if Genesis mode is on for the node, this action responds
-- to changes in ledger state judgement monitoring actions to change the static
-- set of target peers.
targetPeers
  :: (MonadSTM m, Ord peeraddr)
  => Cardano.ExtraPeerSelectionActions m
  -> PeerSelectionActions
      Cardano.ExtraState
      extraFlags
      extraPeers
      extraAPI
      extraCounters
      peeraddr
      peerconn
      m
  -> PeerSelectionState
      Cardano.ExtraState
      PeerTrustable
      extraPeers
      peeraddr
      peerconn
  -> Guarded (STM m)
            (TimedDecision m Cardano.ExtraState extraDebugState PeerTrustable
                           extraPeers peeraddr peerconn)
targetPeers Cardano.ExtraPeerSelectionActions {
              Cardano.genesisPeerTargets
            }
            PeerSelectionActions {
              peerSelectionTargets,
              readPeerSelectionTargets,
              extraPeersAPI
            }
            st@PeerSelectionState{
              publicRootPeers,
              localRootPeers,
              targets,
              extraState = Cardano.ExtraState {
                Cardano.bootstrapPeersFlag,
                Cardano.hasOnlyBootstrapPeers,
                Cardano.ledgerStateJudgement,
                Cardano.consensusMode
              }
            } =
    Guarded Nothing $ do
      churnTargets <- readPeerSelectionTargets
      -- Genesis consensus mode:
      -- we check if targets proposed by churn are stale
      -- in the sense that they are the targets for
      -- opposite value of the current ledger state judgement.
      -- This indicates that we aren't churning currently, and
      -- furthermore it means that the ledger state has flipped since
      -- we last churned. Therefore we can't set the targets from
      -- the TVar, and instead we set the appropriate targets
      -- for the mode we are in.
      let targets' =
            case (ledgerStateJudgement, consensusMode) of
              (YoungEnough, GenesisMode)
                | churnTargets == genesisPeerTargets ->
                  peerSelectionTargets
              (TooOld, GenesisMode)
                | churnTargets == peerSelectionTargets ->
                  genesisPeerTargets
              _otherwise -> churnTargets

      -- nb. first check is redundant in Genesis mode
      check (   isNodeAbleToMakeProgress bootstrapPeersFlag
                                         ledgerStateJudgement
                                         hasOnlyBootstrapPeers
             && targets /= targets'
             && sanePeerSelectionTargets targets')
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
          publicRootPeers' =
            PublicRootPeers.difference (differenceExtraPeers extraPeersAPI)
              publicRootPeers (LocalRootPeers.keysSet localRootPeers')

      return $ \_now -> Decision {
        decisionTrace = [TraceTargetsChanged targets targets'],
        decisionJobs  = [],
        decisionState = st {
                          targets        = targets',
                          localRootPeers = localRootPeers',
                          publicRootPeers = publicRootPeers'
                        } }


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
localRoots
  :: forall extraDebugState extraAPI extraCounters peeraddr peerconn m.
    (MonadTimer m, Ord peeraddr)
  => PeerSelectionActions
      Cardano.ExtraState
      PeerTrustable
      (Cardano.ExtraPeers peeraddr)
      extraAPI
      extraCounters
      peeraddr
      peerconn
      m
  -> PeerSelectionState
      Cardano.ExtraState
      PeerTrustable
      (Cardano.ExtraPeers peeraddr)
      peeraddr
      peerconn
  -> Guarded (STM m)
            (TimedDecision m Cardano.ExtraState extraDebugState
                           PeerTrustable (Cardano.ExtraPeers peeraddr)
                           peeraddr peerconn)
localRoots actions@PeerSelectionActions{ readLocalRootPeers
                                       , extraPeersAPI
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
             extraState = cpst@Cardano.ExtraState {
               Cardano.bootstrapPeersFlag,
               Cardano.hasOnlyBootstrapPeers,
               Cardano.ledgerStateJudgement
             }
           }
  | isNodeAbleToMakeProgress bootstrapPeersFlag
                             ledgerStateJudgement
                             hasOnlyBootstrapPeers =
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
          addedInfoMap = Map.map
                           (\LocalRootConfig { peerAdvertise } ->
                             (Nothing, Just peerAdvertise))
                           added
          removedSet   = Map.keysSet removed
          knownPeers'  = KnownPeers.insert addedInfoMap knownPeers
                        -- We do not immediately remove old ones from the
                        -- known peers set because we may have established
                        -- connections

          localRootPeersSet = LocalRootPeers.keysSet localRootPeers'

          -- We have to adjust the publicRootPeers to maintain the invariant
          -- that the local and public sets are non-overlapping.
          --
          publicRootPeers' =
            PublicRootPeers.difference (differenceExtraPeers extraPeersAPI)
              publicRootPeers
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
            if    requiresBootstrapPeers bootstrapPeersFlag ledgerStateJudgement
               && not hasOnlyBootstrapPeers'
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
                    (PublicRootPeers.toSet (extraPeersToSet extraPeersAPI)
                                           publicRootPeers')
                   (KnownPeers.toSet knownPeers'))
        . assert (Set.isSubsetOf
                   (LocalRootPeers.keysSet localRootPeers')
                   (KnownPeers.toSet knownPeers'))

        $ Decision {
            decisionTrace = TraceLocalRootPeersChanged localRootPeers localRootPeers'
                          : [ TraceLedgerStateJudgementChanged YoungEnough
                            | ledgerStateJudgement /= ledgerStateJudgement'
                            ],
            decisionState = st {
                              localRootPeers      = localRootPeers',
                              publicRootPeers     = publicRootPeers',
                              knownPeers          = knownPeers',
                              inProgressDemoteHot = inProgressDemoteHot
                                                 <> selectedToDemote,
                              extraState = cpst {
                                Cardano.hasOnlyBootstrapPeers = hasOnlyBootstrapPeers',
                                Cardano.ledgerStateJudgement  = ledgerStateJudgement'
                              }
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
monitorBootstrapPeersFlag
  :: ( MonadSTM m
     , Ord peeraddr
     )
  => Cardano.ExtraPeerSelectionActions m
  -> PeerSelectionActions
      Cardano.ExtraState

      extraFlags
      (Cardano.ExtraPeers peeraddr)
      extraAPI
      extraCounters
      peeraddr
      peerconn
      m
  -> PeerSelectionState
      Cardano.ExtraState
      extraFlags
      (Cardano.ExtraPeers peeraddr)
      peeraddr
      peerconn
  -> Guarded (STM m)
            (TimedDecision m Cardano.ExtraState extraDebugState extraFlags
                           (Cardano.ExtraPeers peeraddr) peeraddr peerconn)
monitorBootstrapPeersFlag Cardano.ExtraPeerSelectionActions { Cardano.readUseBootstrapPeers }
                          PeerSelectionActions { extraPeersAPI }
                          st@PeerSelectionState { knownPeers
                                                , establishedPeers
                                                , publicRootPeers
                                                , inProgressPromoteCold
                                                , inProgressPromoteWarm
                                                , extraState = cpst@Cardano.ExtraState {
                                                    Cardano.bootstrapPeersFlag,
                                                    Cardano.consensusMode
                                                  }
                                                }
  | GenesisMode <- consensusMode = GuardedSkip Nothing
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
          st { knownPeers =
                 KnownPeers.delete
                   nonEstablishedBootstrapPeers
                   knownPeers
             , publicRootPeers =
                 PublicRootPeers.difference (differenceExtraPeers extraPeersAPI)
                   publicRootPeers
                   nonEstablishedBootstrapPeers
             , extraState = cpst {
                 Cardano.bootstrapPeersFlag    = ubp
               , Cardano.ledgerStateJudgement  = YoungEnough
               , Cardano.hasOnlyBootstrapPeers = False
               , Cardano.bootstrapPeersTimeout = Nothing
               }
             }
      }

-- | Monitor 'LedgerStateJudgement', if it changes,
--
-- For Praos mode:
-- If bootstrap peers are enabled, we need to update 'PeerSelectionTargets'.
-- If the ledger state changed to 'TooOld' we set all other targets to 0
-- and the governor waits for all active connections to drop and then set
-- the targets to sensible values for getting caught up again.
-- However if the state changes to 'YoungEnough' we reset the targets back to
-- their original values.
--
-- It should be noted if the node has bootstrap peers disabled then this
-- monitoring action will be disabled.
--
-- It should also be noted that churning is ignored until the node converges
-- to a clean state. I.e., it will disconnect from the targets source of truth.
--
monitorLedgerStateJudgement
  :: ( MonadSTM m
     , Ord peeraddr
     )
  => PeerSelectionActions
      Cardano.ExtraState
      extraFlags
      (Cardano.ExtraPeers peeraddr)
      (Cardano.LedgerPeersConsensusInterface m)
      extraCounters
      peeraddr
      peerconn
      m
  -> PeerSelectionState
      Cardano.ExtraState
      extraFlags
      (Cardano.ExtraPeers peeraddr)
      peeraddr
      peerconn
  -> Guarded (STM m)
            (TimedDecision m Cardano.ExtraState extraDebugState extraFlags
                           (Cardano.ExtraPeers peeraddr) peeraddr peerconn)
monitorLedgerStateJudgement PeerSelectionActions{
                              getLedgerStateCtx = ledgerCtx@LedgerPeersConsensusInterface {
                                lpExtraAPI = Cardano.LedgerPeersConsensusInterface {
                                  Cardano.getLedgerStateJudgement = readLedgerStateJudgement
                                }
                              }
                            , extraPeersAPI
                            }
                            st@PeerSelectionState{ publicRootPeers,
                                                   knownPeers,
                                                   establishedPeers,
                                                   inProgressPromoteCold,
                                                   inProgressPromoteWarm,
                                                   ledgerPeerSnapshot,
                                                   extraState = cpst@Cardano.ExtraState {
                                                     Cardano.bootstrapPeersFlag,
                                                     Cardano.ledgerStateJudgement,
                                                     Cardano.consensusMode
                                                   }
                                                 }
  | GenesisMode <- consensusMode =
    Guarded Nothing $ do
      lsj <- readLedgerStateJudgement
      check (lsj /= ledgerStateJudgement)

      return $ \_now ->
        Decision {
          decisionTrace = [TraceLedgerStateJudgementChanged lsj],
          decisionJobs = case (lsj, ledgerPeerSnapshot) of
                           (TooOld, Just ledgerPeerSnapshot') ->
                             [jobVerifyPeerSnapshot ledgerPeerSnapshot' ledgerCtx]
                           _otherwise -> [],
          decisionState = st {
            extraState = cpst {
              Cardano.ledgerStateJudgement = lsj
            }
          }
        }

  | PraosMode <- consensusMode
  , isBootstrapPeersEnabled bootstrapPeersFlag =
    Guarded Nothing $ do
      lsj <- readLedgerStateJudgement
      check (lsj /= ledgerStateJudgement)
      st' <- case lsj of
        TooOld      -> do
          return (\now -> st
            { targets =
                PeerSelectionTargets
                  { targetNumberOfRootPeers                 = 0
                  , targetNumberOfKnownPeers                = 0
                  , targetNumberOfEstablishedPeers          = 0
                  , targetNumberOfActivePeers               = 0
                  , targetNumberOfKnownBigLedgerPeers       = 0
                  , targetNumberOfEstablishedBigLedgerPeers = 0
                  , targetNumberOfActiveBigLedgerPeers      = 0
                  }
            -- We have to enforce the invariant that the number of root peers is
            -- not more than the target number of known peers. It's unlikely in
            -- practice so it's ok to resolve it arbitrarily using clampToLimit.
            , localRootPeers = LocalRootPeers.empty
            , publicRootBackoffs = 0
            , publicRootRetryTime = now
            , extraState = cpst {
                Cardano.ledgerStateJudgement = lsj
              , Cardano.hasOnlyBootstrapPeers = False
              , Cardano.bootstrapPeersTimeout = Just (addTime governor_BOOTSTRAP_PEERS_TIMEOUT now)
              }
            })
        YoungEnough -> do
          let nonEstablishedBootstrapPeers =
                PublicRootPeers.getBootstrapPeers publicRootPeers
                Set.\\
                EstablishedPeers.toSet establishedPeers
                Set.\\
                (inProgressPromoteCold <> inProgressPromoteWarm)
          return (\now -> st
            { knownPeers =
                KnownPeers.delete
                  nonEstablishedBootstrapPeers
                  knownPeers
            , publicRootPeers =
                PublicRootPeers.difference (differenceExtraPeers extraPeersAPI)
                  publicRootPeers
                  nonEstablishedBootstrapPeers
            , publicRootBackoffs = 0
            , publicRootRetryTime = now
            , extraState = cpst {
                Cardano.ledgerStateJudgement  = lsj
              , Cardano.hasOnlyBootstrapPeers = False
              , Cardano.bootstrapPeersTimeout = Nothing
              }
            })
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
waitForSystemToQuiesce
  :: ( MonadSTM m
     , Ord peeraddr
     )
  => PeerSelectionState
      Cardano.ExtraState
      PeerTrustable
      (Cardano.ExtraPeers peeraddr)
      peeraddr
      peerconn
  -> Guarded (STM m)
            (TimedDecision m Cardano.ExtraState extraDebugState PeerTrustable
                             (Cardano.ExtraPeers peeraddr) peeraddr peerconn)
waitForSystemToQuiesce st@PeerSelectionState{
                            knownPeers
                          , localRootPeers
                          , publicRootPeers
                          , inProgressPromoteCold
                          , inProgressPromoteWarm
                          , inProgressPeerShareReqs
                          , inProgressPublicRootsReq
                          , inProgressBigLedgerPeersReq
                          , extraState = cpst@Cardano.ExtraState {
                              Cardano.ledgerStateJudgement
                            , Cardano.bootstrapPeersFlag
                            , Cardano.hasOnlyBootstrapPeers
                            }
                          }
  -- Is the node in sensitive state?
  | requiresBootstrapPeers bootstrapPeersFlag ledgerStateJudgement
  -- Has the node still haven't reached a clean state
  , not hasOnlyBootstrapPeers
  -- Are the local root peers all trustable?
  , all (\case
          LocalRootConfig { extraFlags = IsTrustable }
            -> True
          _ -> False
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
                   decisionState = st { extraState = cpst {
                                          Cardano.hasOnlyBootstrapPeers = True
                                        , Cardano.bootstrapPeersTimeout = Nothing
                                        }
                                      }
        }
  | otherwise = GuardedSkip Nothing
