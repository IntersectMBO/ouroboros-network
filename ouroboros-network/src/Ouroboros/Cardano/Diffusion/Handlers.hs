{-# LANGUAGE CPP #-}

#if !defined(mingw32_HOST_OS)
#define POSIX
#endif

module Ouroboros.Cardano.Diffusion.Handlers where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadTime.SI
import Control.Tracer (traceWith)
import Ouroboros.Network.ConnectionManager.Types
import Ouroboros.Network.Diffusion.Common
#ifdef POSIX
import Ouroboros.Cardano.Network.LedgerPeerConsensusInterface
import Ouroboros.Cardano.Network.PeerSelection.Governor.PeerSelectionState
           (CardanoDebugPeerSelectionState (..), CardanoPeerSelectionState (..))
import Ouroboros.Network.ConnectionManager.Core (Trace (..))
import Ouroboros.Network.PeerSelection.Governor
import Ouroboros.Network.PeerSelection.Governor.Types
           (makeDebugPeerSelectionState)
import Ouroboros.Network.PeerSelection.LedgerPeers.Type
import Ouroboros.Network.PeerSelection.PeerMetric (PeerMetrics,
           fetchynessBlocks, upstreamyness)
import System.Posix.Signals qualified as Signals
#else
import Ouroboros.Network.PeerSelection.LedgerPeers.Type (LedgerPeerSnapshot,
           MinBigLedgerPeersForTrustedState, UseLedgerPeers)
import Ouroboros.Network.PeerSelection.PeerMetric (PeerMetrics)
#endif

#ifdef POSIX
sigUSR1Handler
  :: Ord ntnAddr
  => TracersExtra ntnAddr ntnVersion ntnVersionData ntcAddr ntcVersion
     ntcVersionData resolverError CardanoPeerSelectionState
     CardanoDebugPeerSelectionState extraFlags extraPeers extraCounters
     IO
  -> ArgumentsExtra extraArgs CardanoPeerSelectionState extraActions
     (CardanoLedgerPeersConsensusInterface IO) extraPeers extraFlags
     extraChurnArgs extraCounters exception peeraddr IO
  -> Applications ntnAddr ntnVersion ntnVersionData ntcAddr ntcVersion
     ntcVersionData (CardanoLedgerPeersConsensusInterface IO) IO a
  -> ConnectionManager muxMode socket ntnAddr handle handleError IO
  -> StrictTVar IO (PeerSelectionState CardanoPeerSelectionState
     extraFlags extraPeers ntnAddr peerconn)
  -> PeerMetrics IO ntnAddr
  -> IO ()
sigUSR1Handler tracersExtra argsExtra apps connectionManager
                        dbgStateVar metrics = do
  _ <- Signals.installHandler
         Signals.sigUSR1
         (Signals.Catch
           (do state <- atomically $ readState connectionManager
               traceWith (dtConnectionManagerTracer tracersExtra)
                         (TrState state)
               ps <- readTVarIO dbgStateVar
               now <- getMonotonicTime
               (up, bp, lsj, am) <- atomically $
                 (,,,) <$> upstreamyness metrics
                       <*> fetchynessBlocks metrics
                       <*> (CardanoDebugPeerSelectionState <$>
                             clpciGetLedgerStateJudgement
                               (lpExtraAPI (daLedgerPeersCtx apps)))
                       <*> readAssociationMode
                             (daReadUseLedgerPeers argsExtra)
                             (daOwnPeerSharing argsExtra)
                             (cpstBootstrapPeersFlag (extraState ps))
               let dbgState = makeDebugPeerSelectionState ps up bp lsj am
               traceWith (dtTracePeerSelectionTracer tracersExtra)
                         (TraceDebugState now dbgState)
           )
         )
         Nothing
  return ()
#else
sigUSR1Handler _ _ _ _ _ _ = pure ()
#endif
