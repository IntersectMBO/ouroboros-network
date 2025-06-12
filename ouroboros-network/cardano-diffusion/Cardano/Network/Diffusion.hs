{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}

-- | This module is expected to be imported qualified (it will clash
-- with the "Ouroboros.Network.Diffusion.NonP2P").
--
module Cardano.Network.Diffusion
  ( module Cardano.Network.Diffusion.Types
  , run
  ) where

import Control.Monad.Class.MonadThrow
import Control.Tracer (traceWith)
import Data.Set qualified as Set
import Data.Void (Void)
import System.Exit (ExitCode)

import Cardano.Network.Diffusion.Handlers qualified as Cardano
import Cardano.Network.Diffusion.Types
import Cardano.Network.LedgerPeerConsensusInterface qualified as Cardano
import Cardano.Network.PeerSelection.Churn qualified as Cardano.Churn
import Cardano.Network.PeerSelection.ExtraRootPeers qualified as Cardano
import Cardano.Network.PeerSelection.Governor.PeerSelectionActions qualified as Cardano
import Cardano.Network.PeerSelection.Governor.PeerSelectionState qualified as Cardano.PeerSelectionState
import Cardano.Network.PeerSelection.Governor.Types qualified as Cardano.Types
import Cardano.Network.PeerSelection.PeerSelectionActions qualified as Cardano

import Ouroboros.Network.Diffusion qualified as Diffusion
import Ouroboros.Network.IOManager
import Ouroboros.Network.NodeToClient qualified as NodeToClient
import Ouroboros.Network.NodeToNode (NodeToNodeVersionData (..), RemoteAddress,
           ntnDataFlow)
import Ouroboros.Network.NodeToNode qualified as NodeToNode
import Ouroboros.Network.PeerSelection.LedgerPeers.Type
           (LedgerPeersConsensusInterface (..))
import Ouroboros.Network.Protocol.Handshake

-- | Main entry point for Cardano data diffusion service.  It allows to:
--
-- * connect to upstream peers;
-- * accept connection from downstream peers, if run in
--  'InitiatorAndResponderDiffusionMode'.
-- * runs a local service which allows to use node-to-client protocol to obtain
--   information from the running system.  This is used by 'cardano-cli' or
--   a wallet and a like local services.
--
run :: CardanoNodeArguments IO
    -- ^ node API: instantiated in `cardano-node`.
    -> CardanoConsensusArguments RemoteAddress IO
    -- ^ consensus API; instantiated in `ouroboros-consensus-diffusion` (with
    -- exception of `readUseBootstrapPeers` field).
    -> CardanoTracers IO
    -- ^ generic diffusion tracers; instantiated in `cardano-node`.
    -> CardanoConfiguration IO
    -- ^ generic diffusion configuration; instantiated in `cardano-node`.
    -> CardanoApplications IO a
    -- ^ cardano specific applications; instantiated in
    -- `ouroboros-consensus-diffusion`.
    -> IO Void
run CardanoNodeArguments {
      consensusMode,
      genesisPeerTargets,
      minNumOfBigLedgerPeers,
      tracerChurnMode
    }
    CardanoConsensusArguments {
      churnModeVar,
      churnMetrics,
      ledgerPeersAPI,
      readUseBootstrapPeers
    }
    tracers config apps = do
    let tracer = Diffusion.dtDiffusionTracer tracers
        daNtnHandshakeArguments =
          HandshakeArguments {
              haHandshakeTracer = Diffusion.dtHandshakeTracer tracers,
              haHandshakeCodec  = NodeToNode.nodeToNodeHandshakeCodec,
              haVersionDataCodec =
                cborTermVersionDataCodec
                  NodeToNode.nodeToNodeCodecCBORTerm,
              haAcceptVersion = acceptableVersion,
              haQueryVersion = queryVersion,
              haTimeLimits = timeLimitsHandshake
            }
        daNtcHandshakeArguments =
          HandshakeArguments {
              haHandshakeTracer  = Diffusion.dtLocalHandshakeTracer tracers,
              haHandshakeCodec   = NodeToClient.nodeToClientHandshakeCodec,
              haVersionDataCodec =
                cborTermVersionDataCodec
                  NodeToClient.nodeToClientCodecCBORTerm,
              haAcceptVersion = acceptableVersion,
              haQueryVersion = queryVersion,
              haTimeLimits = noTimeLimitsHandshake
            }

    -- We run two services: for /node-to-node/ and /node-to-client/.  The
    -- naming convention is that we use /local/ prefix for /node-to-client/
    -- related terms, as this is a local only service running over a unix
    -- socket / windows named pipe.
    handleJust (\e -> case fromException e :: Maybe ExitCode of
                  Nothing -> Just e
                  Just {} -> Nothing)
               (\e -> traceWith tracer (Diffusion.DiffusionErrored e)
                   >> throwIO (Diffusion.DiffusionError e))
         $ withIOManager $ \iocp -> do
             interfaces <- Diffusion.mkInterfaces iocp tracer (Diffusion.dcEgressPollInterval config)
             Diffusion.runM
               interfaces
               tracers
               Diffusion.Arguments {
                  daNtnDataFlow    = ntnDataFlow,
                  daNtnPeerSharing = peerSharing,
                  daUpdateVersionData = \versionData diffusionMode -> versionData { diffusionMode },
                  daNtnHandshakeArguments,
                  daNtcHandshakeArguments,
                  daLedgerPeersCtx                    = ledgerPeersAPI,
                  daEmptyExtraState                   =
                    Cardano.PeerSelectionState.empty
                      consensusMode
                      minNumOfBigLedgerPeers,
                  daEmptyExtraCounters                = Cardano.Types.empty,
                  daExtraPeersAPI                     = Cardano.cardanoPublicRootPeersAPI,
                  daInstallSigUSR1Handler             =
                    Cardano.sigUSR1Handler
                      tracers
                      (Diffusion.dcReadUseLedgerPeers config)
                      (Diffusion.dcPeerSharing config)
                      readUseBootstrapPeers
                      (Cardano.getLedgerStateJudgement (lpExtraAPI ledgerPeersAPI))
                      churnMetrics,
                  daPeerSelectionGovernorArgs         =
                    Cardano.Types.cardanoPeerSelectionGovernorArgs
                      Cardano.ExtraPeerSelectionActions {
                        Cardano.genesisPeerTargets    = genesisPeerTargets,
                        Cardano.readUseBootstrapPeers = readUseBootstrapPeers
                      },
                  daPeerSelectionStateToExtraCounters = Cardano.Types.cardanoPeerSelectionStatetoCounters,
                  daToExtraPeers                      = flip Cardano.ExtraPeers Set.empty,
                  daRequestPublicRootPeers            =
                      Just $ Cardano.requestPublicRootPeers
                               (Diffusion.dtTracePublicRootPeersTracer tracers)
                               readUseBootstrapPeers
                               (Cardano.getLedgerStateJudgement (lpExtraAPI ledgerPeersAPI))
                               (Diffusion.dcReadPublicRootPeers config),
                  daPeerChurnGovernor                 = Cardano.Churn.peerChurnGovernor,
                  daExtraChurnArgs                    =
                    Cardano.Churn.ExtraArguments {
                      Cardano.Churn.modeVar            = churnModeVar,
                      Cardano.Churn.genesisPeerTargets = genesisPeerTargets,
                      Cardano.Churn.readUseBootstrap   = readUseBootstrapPeers,
                      Cardano.Churn.consensusMode      = consensusMode,
                      Cardano.Churn.tracerChurnMode    = tracerChurnMode
                    }
                }
               config apps
