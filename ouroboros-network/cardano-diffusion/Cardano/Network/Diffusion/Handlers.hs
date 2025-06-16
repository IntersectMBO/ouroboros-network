{-# LANGUAGE CPP #-}

#if !defined(mingw32_HOST_OS)
#define POSIX
#else
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
#endif

module Cardano.Network.Diffusion.Handlers where

import Cardano.Network.PeerSelection.Bootstrap (UseBootstrapPeers)
import Cardano.Network.PeerSelection.Governor.PeerSelectionState qualified as Cardano
import Cardano.Network.Types (LedgerStateJudgement)
import Control.Concurrent.Class.MonadSTM.Strict
import Ouroboros.Network.ConnectionManager.Types
import Ouroboros.Network.Diffusion.Types (Tracers (..))
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

sigUSR1Handler
  :: Ord ntnAddr
  => Tracers ntnAddr ntnVersion ntnVersionData
             ntcAddr ntcVersion ntcVersionData
             extraState
             Cardano.DebugPeerSelectionState
             extraFlags extraPeers extraCounters
             IO
  -> STM IO UseLedgerPeers
  -> PeerSharing
  -> STM IO UseBootstrapPeers
  -> STM IO LedgerStateJudgement
  -> PeerMetrics IO ntnAddr
  -> ConnectionManager muxMode socket ntnAddr
                       handle handleError IO
  -> StrictTVar IO (PeerSelectionState
                   Cardano.ExtraState
                   extraFlags extraPeers ntnAddr
                   peerconn)
  -> IO ()
#ifdef POSIX
sigUSR1Handler tracersExtra getUseLedgerPeers ownPeerSharing getBootstrapPeers
               getLedgerStateJudgement metrics connectionManager dbgStateVar = do
  _ <- Signals.installHandler
         Signals.sigUSR1
         (Signals.Catch
           (do now <- getMonotonicTime
               (state, up, bp, lsj, am, ps) <- atomically $ do
                  useBootstrapPeers <- getBootstrapPeers
                  (,,,,,) <$> readState connectionManager
                          <*> upstreamyness metrics
                          <*> fetchynessBlocks metrics
                          <*> (Cardano.DebugPeerSelectionState <$> getLedgerStateJudgement)
                          <*> readAssociationMode
                                getUseLedgerPeers
                                ownPeerSharing
                                useBootstrapPeers
                          <*> readTVar dbgStateVar

               let dbgState = makeDebugPeerSelectionState ps up bp lsj am

               traceWith (dtConnectionManagerTracer tracersExtra)
                         (TrState state)
               traceWith (dtTracePeerSelectionTracer tracersExtra)
                         (TraceDebugState now dbgState)
           )
         )
         Nothing
  return ()
#else
sigUSR1Handler _ _ _ _ _ _ _ _ = pure ()
#endif
