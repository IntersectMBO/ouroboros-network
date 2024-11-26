{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Diffusion
  ( -- * Common API
    P2P (..)
  , DiffusionTracer (..)
  , Tracers (..)
  , nullTracers
  , ExtraTracers (..)
  , Failure (..)
  , Arguments (..)
  , ExtraArguments (..)
  , Applications (..)
  , ExtraApplications (..)
    -- * Run data diffusion
  , run
    -- * Re-exports
  , P2P.AbstractTransitionTrace
  , PublicPeerSelectionState
  , makePublicPeerSelectionStateVar
  ) where

import Control.Concurrent.Class.MonadSTM.Strict (StrictTVar)
import Control.Exception (Exception, IOException)
import Data.Functor (void)
import Network.Mux qualified as Mx
import Network.Socket (Socket)
import Ouroboros.Network.Diffusion.Common (Arguments,
           NodeToNodeConnectionManager, NodeToNodePeerConnectionHandle, Tracers)
import Ouroboros.Network.Diffusion.Common qualified as Common
import Ouroboros.Network.PeerSelection.Governor.Types (PeerSelectionState)
import Ouroboros.Network.PeerSelection.PeerMetric (PeerMetrics)
import Ouroboros.Network.Diffusion.NonP2P qualified as NonP2P
import Ouroboros.Network.NodeToClient (LocalAddress, LocalSocket,
           NodeToClientVersion, NodeToClientVersionData)
import Ouroboros.Network.NodeToNode (NodeToNodeVersion, NodeToNodeVersionData,
           RemoteAddress)

-- | Promoted data types.
--
data P2P = P2P | NonP2P

-- | Tracers which depend on p2p mode.
--
data ExtraTracers (p2p :: P2P) where
  P2PTracers
    :: Common.TracersExtra
           RemoteAddress NodeToNodeVersion         NodeToNodeVersionData
           LocalAddress  NodeToClientVersion       NodeToClientVersionData
           IOException   extraState extraState extraFlags extraPeers m
           IOException   extraState extraState extraFlags extraPeers extraCounters m
    -> ExtraTracers 'P2P extraState extraFlags extraPeers extraCounters m
  P2PCardanoTracers
    :: Common.TracersExtra
           RemoteAddress NodeToNodeVersion         NodeToNodeVersionData
           LocalAddress  NodeToClientVersion       NodeToClientVersionData
           IOException   CardanoPeerSelectionState CardanoPeerSelectionState
           PeerTrustable (CardanoPublicRootPeers RemoteAddress) m
           PeerTrustable (CardanoPublicRootPeers RemoteAddress)
           (CardanoPeerSelectionView RemoteAddress) m
    -> ExtraTracers 'P2PCardano CardanoPeerSelectionState PeerTrustable (CardanoPublicRootPeers RemoteAddress) (CardanoPeerSelectionView RemoteAddress) m
    :: Common.ArgumentsExtra extraArgs extraState extraActions extraAPI
                            extraPeers extraFlags extraChurnArgs
                            extraCounters exception ntnAddr m
    -> ArgumentsExtra 'P2P extraArgs extraState extraActions extraAPI
                           extraPeers extraFlags extraChurnArgs
                           extraCounters exception ntnAddr m
-- | Diffusion arguments which depend on p2p mode.
--
data ExtraArguments (p2p :: P2P) m where
  P2PArguments
    :: Common.ArgumentsExtra extraArgs extraPeers m
    -> ArgumentsExtra 'P2P extraArgs extraPeers m

  P2PCardanoArguments
    :: Common.ArgumentsExtra (CardanoArgumentsExtra m) PeerTrustable m
    -> ArgumentsExtra 'P2PCardano (CardanoArgumentsExtra m) PeerTrustable m

  NonP2PArguments
    :: NonP2P.ArgumentsExtra
    -> ExtraArguments 'NonP2P m


-- | Application data which depend on p2p mode.
--
data ExtraApplications (p2p :: P2P) ntnAddr m a where
  P2PApplications
    :: P2P.ApplicationsExtra ntnAddr m a
    -> ExtraApplications 'P2P ntnAddr m a

  NonP2PApplications
    :: NonP2P.ApplicationsExtra
    -> ExtraApplications 'NonP2P ntnAddr m a


-- | Run data diffusion in either 'P2P' or 'NonP2P' mode.
--
run :: forall (p2p :: P2P) a.
       Tracers
         RemoteAddress NodeToNodeVersion
         LocalAddress  NodeToClientVersion
         IO
    -> ExtraTracers p2p
    -> Arguments
         IO
         Socket      RemoteAddress
    -> ArgumentsExtra p2p extraArgs extraState extraActions extraAPI
       extraPeers extraFlags extraChurnArgs extraCounters exception
       RemoteAddress IO
    -> ArgumentsExtra p2p extraArgs extraFlags IO
    -> Applications p2p extraAPI IO a

    -> ApplicationsExtra p2p RemoteAddress IO a
    -> IO ()
run tracers (P2PTracers tracersExtra)
            args (P2PArguments argsExtra)
            apps (P2PApplications appsExtra) =
    void $
    P2P.run tracers tracersExtra
            args argsExtra
            apps appsExtra
run tracers (NonP2PTracers tracersExtra)
            args (NonP2PArguments argsExtra)
            (NonP2PApplications apps)
            (NonP2PApplicationsExtra appsExtra) = do
    NonP2P.run tracers tracersExtra
               args argsExtra
               apps appsExtra
