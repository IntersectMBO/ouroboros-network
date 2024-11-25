{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Diffusion
  ( -- * Common API
    P2P (..)
  , ExtraTracers (..)
  , ArgumentsExtra (..)
  , Applications (..)
  , ApplicationsExtra (..)
    -- * Run data diffusion
  , run
  ) where

import Control.Concurrent.Class.MonadSTM.Strict (StrictTVar)
import Control.Exception (Exception, IOException)
import Data.Functor (void)
import Network.Socket (Socket)
import Ouroboros.Network.Diffusion.Common (Arguments,
           NodeToNodeConnectionManager, NodeToNodePeerConnectionHandle, Tracers)
import Ouroboros.Network.Diffusion.Common qualified as Common
import Ouroboros.Network.Diffusion.MinimalP2P qualified as MinimalP2P
import Ouroboros.Network.Diffusion.NonP2P qualified as NonP2P
import Ouroboros.Network.NodeToClient (LocalAddress, LocalSocket,
           NodeToClientVersion, NodeToClientVersionData)
import Ouroboros.Network.NodeToNode (NodeToNodeVersion, NodeToNodeVersionData,
           RemoteAddress)
import Ouroboros.Network.PeerSelection.Governor.Types (PeerSelectionState)
import Ouroboros.Network.PeerSelection.PeerMetric (PeerMetrics)
import qualified Network.Mux as Mx

-- | Promoted data types.
--
data P2P = P2P        -- ^ General P2P mode. Can be instantiated with custom
                      -- data types
         | NonP2P     -- ^ Cardano non-P2P mode. Deprecated

-- | Tracers which depend on p2p mode.
--
data ExtraTracers (p2p :: P2P) extraState extraFlags extraPeers extraCounters m where
  P2PTracers
    :: Common.TracersExtra
           RemoteAddress NodeToNodeVersion         NodeToNodeVersionData
           LocalAddress  NodeToClientVersion       NodeToClientVersionData
           IOException   extraState extraState extraFlags extraPeers
           extraCounters m
    -> ExtraTracers 'P2P extraState extraFlags extraPeers extraCounters m

  NonP2PTracers
    :: NonP2P.TracersExtra
    -> ExtraTracers 'NonP2P extraState extraFlags extraPeers extraCounters m


-- | Diffusion arguments which depend on p2p mode.
--
data ArgumentsExtra
       (p2p :: P2P) extraArgs extraState extraActions extraAPI
       extraPeers extraFlags extraChurnArgs extraCounters exception ntnAddr m where
  P2PArguments
    :: Common.ArgumentsExtra extraArgs extraState extraActions extraAPI
                            extraPeers extraFlags extraChurnArgs
                            extraCounters exception ntnAddr m
    -> ArgumentsExtra 'P2P extraArgs extraState extraActions extraAPI
                           extraPeers extraFlags extraChurnArgs
                           extraCounters exception ntnAddr m

  NonP2PArguments
    :: NonP2P.ArgumentsExtra
    -> ArgumentsExtra 'NonP2P extraArgs extraState extraActions extraAPI
                              extraPeers extraFlags extraChurnArgs
                              extraCounters exception ntnAddr m

-- | Application data which depend on p2p mode.
--
data Applications (p2p :: P2P) extraAPI m a where
  P2PApplications
    :: Common.Applications
         RemoteAddress  NodeToNodeVersion   NodeToNodeVersionData
         LocalAddress   NodeToClientVersion NodeToClientVersionData
         extraAPI m a
    -> Applications 'P2P extraAPI m a

  NonP2PApplications
    :: Common.Applications
         RemoteAddress  NodeToNodeVersion   NodeToNodeVersionData
         LocalAddress   NodeToClientVersion NodeToClientVersionData
         () m a
    -> Applications 'NonP2P () m a

-- | Application data which depend on p2p mode.
--
data ApplicationsExtra (p2p :: P2P) ntnAddr m a where
  P2PApplicationsExtra
    :: Common.ApplicationsExtra ntnAddr m a
    -> ApplicationsExtra 'P2P ntnAddr m a

  NonP2PApplicationsExtra
    :: NonP2P.ApplicationsExtra
    -> ApplicationsExtra 'NonP2P ntnAddr m a


-- | Run data diffusion in either 'P2P' or 'NonP2P' mode.
--
run :: forall (p2p :: P2P) extraArgs extraState extraActions extraFlags
             extraPeers extraAPI extraChurnArgs extraCounters exception a.
      ( Monoid extraPeers
      , Eq extraCounters
      , Eq extraFlags
      , Exception exception
      )
    => (forall (mode :: Mx.Mode) x y.
        NodeToNodeConnectionManager
          mode Socket RemoteAddress NodeToNodeVersionData NodeToNodeVersion IO x y
        -> StrictTVar
             IO
             (PeerSelectionState
                extraState
                extraFlags
                extraPeers
                RemoteAddress
                (NodeToNodePeerConnectionHandle
                   mode RemoteAddress NodeToNodeVersionData IO x y))
        -> PeerMetrics IO RemoteAddress
        -> IO ())
    -> Tracers
         RemoteAddress NodeToNodeVersion
         LocalAddress  NodeToClientVersion
         IO
    -> ExtraTracers p2p extraState extraFlags extraPeers extraCounters IO
    -> Arguments
         IO
         Socket      RemoteAddress
         LocalSocket LocalAddress
    -> ArgumentsExtra p2p extraArgs extraState extraActions extraAPI
       extraPeers extraFlags extraChurnArgs extraCounters exception
       RemoteAddress IO
    -> Applications p2p extraAPI IO a

    -> ApplicationsExtra p2p RemoteAddress IO a
    -> IO ()
run sigUSR1Signal
    tracers (P2PTracers tracersExtra)
            args (P2PArguments argsExtra)
            (P2PApplications apps)
            (P2PApplicationsExtra appsExtra) =
    void $
    MinimalP2P.run
      sigUSR1Signal tracers tracersExtra
      args argsExtra apps appsExtra
run _
    tracers (NonP2PTracers tracersExtra)
            args (NonP2PArguments argsExtra)
            (NonP2PApplications apps)
            (NonP2PApplicationsExtra appsExtra) = do
    NonP2P.run tracers tracersExtra
               args argsExtra
               apps appsExtra
