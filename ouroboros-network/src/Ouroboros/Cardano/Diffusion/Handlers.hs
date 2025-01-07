{-# LANGUAGE CPP #-}

#if !defined(mingw32_HOST_OS)
#define POSIX
#endif

module Ouroboros.Cardano.Diffusion.Handlers where

import Cardano.Network.PeerSelection.Bootstrap (UseBootstrapPeers)
import Cardano.Network.Types (LedgerStateJudgement)
import Control.Concurrent.Class.MonadSTM.Strict
import Ouroboros.Cardano.Network.PeerSelection.Governor.PeerSelectionState
import Ouroboros.Network.ConnectionManager.Types
import Ouroboros.Network.Diffusion.Common
import Ouroboros.Network.PeerSelection.Governor
import Ouroboros.Network.PeerSelection.LedgerPeers.Type (UseLedgerPeers)
import Ouroboros.Network.PeerSelection.PeerMetric
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing)
#ifdef POSIX
import Control.Monad.Class.MonadTime.SI
import Control.Tracer (traceWith)
import Ouroboros.Network.ConnectionManager.Core (Trace (..))
import Ouroboros.Network.PeerSelection.Governor.Types
           (makeDebugPeerSelectionState)
import System.Posix.Signals qualified as Signals
#endif

#ifdef POSIX
sigUSR1Handler
  :: Ord ntnAddr
  => TracersExtra ntnAddr ntnVersion ntnVersionData
                  ntcAddr ntcVersion ntcVersionData
                  resolverError extraState
                  CardanoDebugPeerSelectionState
                  extraFlags extraPeers extraCounters
                  IO
  -> STM IO UseLedgerPeers
  -> PeerSharing
  -> STM IO UseBootstrapPeers
  -> STM IO LedgerStateJudgement
  -> ConnectionManager muxMode socket ntnAddr
                       handle handleError IO
  -> StrictTVar IO (PeerSelectionState
                   CardanoPeerSelectionState
                   extraFlags extraPeers ntnAddr
                   peerconn)
  -> PeerMetrics IO ntnAddr
  -> IO ()
sigUSR1Handler tracersExtra getUseLedgerPeers ownPeerSharing getBootstrapPeers
               getLedgerStateJudgement connectionManager dbgStateVar metrics = do
  _ <- Signals.installHandler
         Signals.sigUSR1
         (Signals.Catch
           (do (state, useBootstrapPeers) <- atomically $ do
                  (,) <$> readState connectionManager <*> getBootstrapPeers
               traceWith (dtConnectionManagerTracer tracersExtra)
                         (TrState state)
               ps <- readTVarIO dbgStateVar
               now <- getMonotonicTime
               (up, bp, lsj, am) <- atomically $
                 (,,,) <$> upstreamyness metrics
                       <*> fetchynessBlocks metrics
                       <*> (CardanoDebugPeerSelectionState <$> getLedgerStateJudgement)
                       <*> readAssociationMode
                             getUseLedgerPeers
                             ownPeerSharing
                             useBootstrapPeers
               let dbgState = makeDebugPeerSelectionState ps up bp lsj am
               traceWith (dtTracePeerSelectionTracer tracersExtra)
                         (TraceDebugState now dbgState)
           )
         )
         Nothing
  return ()
#else
sigUSR1Handler
  :: TracersExtra ntnAddr ntnVersion ntnVersionData
                  ntcAddr ntcVersion ntcVersionData
                  resolverError extraState
                  CardanoDebugPeerSelectionState
                  extraFlags extraPeers extraCounters
                  IO
  -> STM IO UseLedgerPeers
  -> PeerSharing
  -> UseBootstrapPeers
  -> STM IO LedgerStateJudgement
  -> ConnectionManager muxMode socket ntnAddr
                       handle handleError IO
  -> StrictTVar IO (PeerSelectionState
                   CardanoPeerSelectionState
                   extraFlags extraPeers ntnAddr
                   peerconn)
  -> PeerMetrics IO ntnAddr
  -> IO ()
sigUSR1Handler _ _ _ _ _ _ _ _ = pure ()
#endif
