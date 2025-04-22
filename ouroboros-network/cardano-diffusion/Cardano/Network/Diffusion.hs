{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeOperators            #-}

-- | This module is expected to be imported qualified (it will clash
-- with the "Ouroboros.Network.Diffusion.NonP2P").
--
module Cardano.Network.Diffusion (run) where


import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (IOException)
import Control.Monad.Class.MonadThrow
import Control.Tracer (Tracer, traceWith)
import Data.Void (Void)
import System.Exit (ExitCode)

import Network.Socket (Socket)

import Cardano.Network.PeerSelection.PeerTrustable (PeerTrustable)

import Data.Set qualified as Set

import Cardano.Network.Diffusion.Configuration qualified as Cardano.LC
import Cardano.Network.Diffusion.Handlers qualified as Cardano
import Cardano.Network.LedgerPeerConsensusInterface qualified as Cardano
import Cardano.Network.PeerSelection.Churn qualified as Cardano.Churn
import Cardano.Network.PeerSelection.ExtraRootPeers qualified as Cardano
import Cardano.Network.PeerSelection.Governor.PeerSelectionActions qualified as Cardano
import Cardano.Network.PeerSelection.Governor.PeerSelectionState qualified as Cardano
import Cardano.Network.PeerSelection.Governor.PeerSelectionState qualified as Cardano.PeerSelectionState
import Cardano.Network.PeerSelection.Governor.Types qualified as Cardano
import Cardano.Network.PeerSelection.Governor.Types qualified as Cardano.Types
import Cardano.Network.PeerSelection.PeerSelectionActions qualified as Cardano
import Ouroboros.Network.Diffusion qualified as Diffusion
import Ouroboros.Network.IOManager
import Ouroboros.Network.NodeToClient (NodeToClientVersion (..),
           NodeToClientVersionData)
import Ouroboros.Network.NodeToClient qualified as NodeToClient
import Ouroboros.Network.NodeToNode (NodeToNodeVersion (..),
           NodeToNodeVersionData (..), RemoteAddress, ntnDataFlow)
import Ouroboros.Network.NodeToNode qualified as NodeToNode
import Ouroboros.Network.PeerSelection.LedgerPeers.Type
           (LedgerPeersConsensusInterface (..))
import Ouroboros.Network.Protocol.Handshake
import Ouroboros.Network.Protocol.Handshake.Codec
import Ouroboros.Network.Protocol.Handshake.Version
import Ouroboros.Network.Snocket (LocalAddress, LocalSocket (..))

-- | Main entry point for Cardano data diffusion service.  It allows to:
--
-- * connect to upstream peers;
-- * accept connection from downstream peers, if run in
--  'InitiatorAndResponderDiffusionMode'.
-- * runs a local service which allows to use node-to-client protocol to obtain
--   information from the running system.  This is used by 'cardano-cli' or
--   a wallet and a like local services.
--
run :: LedgerPeersConsensusInterface (Cardano.LedgerPeersConsensusInterface IO) IO
    -> Tracer IO Cardano.Churn.TracerChurnMode
    -> Cardano.LC.LocalConfiguration IO
    -> Diffusion.Tracers
        RemoteAddress
        NodeToNodeVersion
        NodeToNodeVersionData
        LocalAddress
        NodeToClientVersion
        NodeToClientVersionData
        IOException
        Cardano.ExtraState
        Cardano.DebugPeerSelectionState
        PeerTrustable
        (Cardano.ExtraPeers RemoteAddress)
        (Cardano.ExtraPeerSelectionSetsWithSizes RemoteAddress)
        IO
    -> Diffusion.Configuration
        PeerTrustable
        IO
        Socket
        RemoteAddress
        LocalSocket
        LocalAddress
    -> Diffusion.Applications
        RemoteAddress
        NodeToNodeVersion
        NodeToNodeVersionData
        LocalAddress
        NodeToClientVersion
        NodeToClientVersionData
        IO
        a
    -> IO Void
run lpci tracerChurnMode localConfig tracers args apps = do
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

    churnModeVar <- newTVarIO Cardano.Churn.ChurnModeNormal

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
             interfaces <- Diffusion.mkInterfaces iocp tracer
             Diffusion.runM
               interfaces
               tracers
               Diffusion.Arguments {
                  daNtnDataFlow    = ntnDataFlow,
                  daNtnPeerSharing = peerSharing,
                  daUpdateVersionData = \versionData diffusionMode -> versionData { diffusionMode },
                  daNtnHandshakeArguments,
                  daNtcHandshakeArguments,
                  daLedgerPeersCtx                    = lpci,
                  daEmptyExtraState                   =
                    Cardano.PeerSelectionState.empty
                      (Cardano.LC.consensusMode localConfig)
                      (Cardano.LC.numBigLedgerPeers localConfig),
                  daEmptyExtraCounters                = Cardano.Types.empty,
                  daExtraPeersAPI                     = Cardano.cardanoPublicRootPeersAPI,
                  daInstallSigUSR1Handler             =
                    Cardano.sigUSR1Handler
                      tracers
                      (Diffusion.dcReadUseLedgerPeers args)
                      (Diffusion.dcOwnPeerSharing args)
                      (Cardano.LC.readUseBootstrapPeers localConfig)
                      (Cardano.getLedgerStateJudgement (lpExtraAPI lpci)),
                  daPeerSelectionGovernorArgs         =
                    Cardano.Types.cardanoPeerSelectionGovernorArgs
                      Cardano.ExtraPeerSelectionActions {
                        Cardano.genesisPeerTargets    = Cardano.LC.genesisPeerTargets localConfig,
                        Cardano.readUseBootstrapPeers = Cardano.LC.readUseBootstrapPeers localConfig
                      },
                  daPeerSelectionStateToExtraCounters = Cardano.Types.cardanoPeerSelectionStatetoCounters,
                  daToExtraPeers                      = flip Cardano.ExtraPeers Set.empty,
                  daRequestPublicRootPeers            =
                      Just $ Cardano.requestPublicRootPeers
                               (Diffusion.dtTracePublicRootPeersTracer tracers)
                               (Cardano.LC.readUseBootstrapPeers localConfig)
                               (Cardano.getLedgerStateJudgement (lpExtraAPI lpci))
                               (Diffusion.dcReadPublicRootPeers args),
                  daPeerChurnGovernor                 = Cardano.Churn.peerChurnGovernor,
                  daExtraChurnArgs                    =
                    Cardano.Churn.ExtraArguments {
                      Cardano.Churn.modeVar            = churnModeVar,
                      Cardano.Churn.genesisPeerTargets = Cardano.LC.genesisPeerTargets localConfig,
                      Cardano.Churn.readUseBootstrap   = Cardano.LC.readUseBootstrapPeers localConfig,
                      Cardano.Churn.consensusMode      = Cardano.LC.consensusMode localConfig,
                      Cardano.Churn.tracerChurnMode    = tracerChurnMode
                    }
                }
               args apps
