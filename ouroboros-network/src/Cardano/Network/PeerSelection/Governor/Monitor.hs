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
module Cardano.Network.PeerSelection.Governor.Monitor
  ( monitorLedgerStateJudgement
  , monitorBootstrapPeersFlag
  , waitForSystemToQuiesce
  ) where

import Data.Set qualified as Set

import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadTime.SI

import Cardano.Network.ConsensusMode
import Cardano.Network.LedgerPeerConsensusInterface
           (CardanoLedgerPeersConsensusInterface (..))
import Cardano.Network.PeerSelection.Bootstrap (isBootstrapPeersEnabled,
           requiresBootstrapPeers)
import Cardano.Network.PeerSelection.Governor.PeerSelectionActions
           (CardanoPeerSelectionActions (..))
import Cardano.Network.PeerSelection.Governor.PeerSelectionState
           (CardanoPeerSelectionState (..))
import Cardano.Network.PeerSelection.PeerTrustable (PeerTrustable (..))
import Cardano.Network.PublicRootPeers (CardanoPublicRootPeers)
import Cardano.Network.Types (LedgerStateJudgement (..))
import Ouroboros.Network.PeerSelection.Governor.Monitor (jobVerifyPeerSnapshot)
import Ouroboros.Network.PeerSelection.Governor.Types hiding
           (PeerSelectionCounters)
import Ouroboros.Network.PeerSelection.LedgerPeers.Type
           (LedgerPeersConsensusInterface (..))
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
                          => PeerSelectionActions CardanoPeerSelectionState (CardanoPeerSelectionActions m) (CardanoPublicRootPeers peeraddr) extraFlags extraAPI extraCounters peeraddr peerconn m
                          -> PeerSelectionState CardanoPeerSelectionState extraFlags (CardanoPublicRootPeers peeraddr) peeraddr peerconn
                          -> Guarded (STM m) (TimedDecision m CardanoPeerSelectionState extraFlags (CardanoPublicRootPeers peeraddr) peeraddr peerconn)
monitorBootstrapPeersFlag PeerSelectionActions { extraActions = CardanoPeerSelectionActions { cpsaReadUseBootstrapPeers }
                                               , extraPeersActions
                                               }
                          st@PeerSelectionState { knownPeers
                                                , establishedPeers
                                                , publicRootPeers
                                                , inProgressPromoteCold
                                                , inProgressPromoteWarm
                                                , extraState = cpst@CardanoPeerSelectionState {
                                                    cpstBootstrapPeersFlag,
                                                    cpstConsensusMode
                                                  }
                                                }
  | GenesisMode <- cpstConsensusMode = GuardedSkip Nothing
  | otherwise =
  Guarded Nothing $ do
    ubp <- cpsaReadUseBootstrapPeers
    check (ubp /= cpstBootstrapPeersFlag)
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
                 PublicRootPeers.difference (differenceExtraPeers extraPeersActions)
                   publicRootPeers
                   nonEstablishedBootstrapPeers
             , extraState = cpst {
                 cpstBootstrapPeersFlag    = ubp
               , cpstLedgerStateJudgement  = YoungEnough
               , cpstHasOnlyBootstrapPeers = False
               , cpstBootstrapPeersTimeout = Nothing
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
monitorLedgerStateJudgement :: ( MonadSTM m
                               , Ord peeraddr
                               )
                            => PeerSelectionActions CardanoPeerSelectionState (CardanoPeerSelectionActions m) (CardanoPublicRootPeers peeraddr) extraFlags (CardanoLedgerPeersConsensusInterface m) extraCounters peeraddr peerconn m
                            -> PeerSelectionState CardanoPeerSelectionState extraFlags (CardanoPublicRootPeers peeraddr) peeraddr peerconn
                            -> Guarded (STM m) (TimedDecision m CardanoPeerSelectionState extraFlags (CardanoPublicRootPeers peeraddr) peeraddr peerconn)
monitorLedgerStateJudgement PeerSelectionActions{
                              getLedgerStateCtx = ledgerCtx@LedgerPeersConsensusInterface {
                                lpExtraAPI = CardanoLedgerPeersConsensusInterface {
                                  clpciGetLedgerStateJudgement = readLedgerStateJudgement
                                }
                              }
                            , extraPeersActions
                            }
                            st@PeerSelectionState{ publicRootPeers,
                                                   knownPeers,
                                                   establishedPeers,
                                                   inProgressPromoteCold,
                                                   inProgressPromoteWarm,
                                                   ledgerPeerSnapshot,
                                                   extraState = cpst@CardanoPeerSelectionState {
                                                     cpstBootstrapPeersFlag,
                                                     cpstLedgerStateJudgement,
                                                     cpstConsensusMode
                                                   }
                                                 }
  | GenesisMode <- cpstConsensusMode =
    Guarded Nothing $ do
      lsj <- readLedgerStateJudgement
      check (lsj /= cpstLedgerStateJudgement)

      return $ \_now ->
        Decision {
          decisionTrace = [TraceLedgerStateJudgementChanged lsj],
          decisionJobs = case (lsj, ledgerPeerSnapshot) of
                           (TooOld, Just ledgerPeerSnapshot') ->
                             [jobVerifyPeerSnapshot ledgerPeerSnapshot' ledgerCtx]
                           _otherwise -> [],
          decisionState = st {
            extraState = cpst {
              cpstLedgerStateJudgement = lsj
            }
          }
        }

  | PraosMode <- cpstConsensusMode
  , isBootstrapPeersEnabled cpstBootstrapPeersFlag =
    Guarded Nothing $ do
      lsj <- readLedgerStateJudgement
      check (lsj /= cpstLedgerStateJudgement)
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
                cpstLedgerStateJudgement = lsj
              , cpstHasOnlyBootstrapPeers = False
              , cpstBootstrapPeersTimeout = Just (addTime governor_BOOTSTRAP_PEERS_TIMEOUT now)
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
                PublicRootPeers.difference (differenceExtraPeers extraPeersActions)
                  publicRootPeers
                  nonEstablishedBootstrapPeers
            , publicRootBackoffs = 0
            , publicRootRetryTime = now
            , extraState = cpst {
                cpstLedgerStateJudgement  = lsj
              , cpstHasOnlyBootstrapPeers = False
              , cpstBootstrapPeersTimeout = Nothing
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
waitForSystemToQuiesce :: ( MonadSTM m
                          , Ord peeraddr
                          )
                       => PeerSelectionState CardanoPeerSelectionState PeerTrustable (CardanoPublicRootPeers peeraddr) peeraddr peerconn
                       -> Guarded (STM m) (TimedDecision m CardanoPeerSelectionState PeerTrustable (CardanoPublicRootPeers peeraddr) peeraddr peerconn)
waitForSystemToQuiesce st@PeerSelectionState{
                            knownPeers
                          , localRootPeers
                          , publicRootPeers
                          , inProgressPromoteCold
                          , inProgressPromoteWarm
                          , inProgressPeerShareReqs
                          , inProgressPublicRootsReq
                          , inProgressBigLedgerPeersReq
                          , extraState = cpst@CardanoPeerSelectionState {
                              cpstLedgerStateJudgement
                            , cpstBootstrapPeersFlag
                            , cpstHasOnlyBootstrapPeers
                            }
                          }
  -- Is the node in sensitive state?
  | requiresBootstrapPeers cpstBootstrapPeersFlag cpstLedgerStateJudgement
  -- Has the node still haven't reached a clean state
  , not cpstHasOnlyBootstrapPeers
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
                   decisionState = st { extraState = cpst {
                                          cpstHasOnlyBootstrapPeers = True
                                        , cpstBootstrapPeersTimeout = Nothing
                                        }
                                      }
        }
  | otherwise = GuardedSkip Nothing
