{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Diffusion
  ( -- * Common API
    P2P (..)
  , P2PDecision (..)
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
import Network.DNS (Resolver)
import Network.Socket (Socket)
import Ouroboros.Network.Diffusion.Common (Applications (..), Arguments,
           Tracers)
import Ouroboros.Network.Diffusion.NonP2P qualified as NonP2P
import Ouroboros.Network.Diffusion.P2P qualified as P2P
import Ouroboros.Network.NodeToClient (LocalAddress, LocalSocket,
           NodeToClientVersion, NodeToClientVersionData)
import Ouroboros.Network.NodeToNode (NodeToNodeVersion, NodeToNodeVersionData,
           RemoteAddress)
import Ouroboros.Network.PeerSelection.Governor.Types (PeerSelectionState)
import Ouroboros.Network.PeerSelection.PeerMetric (PeerMetrics)

-- | Promoted data types.
--
data P2P = P2P        -- ^ General P2P mode. Can be instantiated with custom
                      -- data types
         | NonP2P     -- ^ Cardano non-P2P mode. Deprecated

-- | Auxiliary type to define arbitrary decision types based on type level
-- P2P
--
data P2PDecision (p2p :: P2P) a b where
  P2PDecision :: a
              -> P2PDecision 'P2P a b
  NonP2PDecision :: b
              -> P2PDecision 'NonP2P a b


-- | Tracers which depend on p2p mode.
--
data ExtraTracers (p2p :: P2P) extraState extraDebugState extraFlags extraPeers extraCounters m where
  P2PTracers
    :: P2P.TracersExtra
           RemoteAddress NodeToNodeVersion         NodeToNodeVersionData
           LocalAddress  NodeToClientVersion       NodeToClientVersionData
           IOException   extraState extraDebugState extraFlags extraPeers
           extraCounters m
    -> ExtraTracers 'P2P extraState extraDebugState extraFlags extraPeers extraCounters m

  NonP2PTracers
    :: NonP2P.TracersExtra
    -> ExtraTracers 'NonP2P extraState extraDebugState extraFlags extraPeers extraCounters m


-- | Diffusion arguments which depend on p2p mode.
--
-- TODO: do we need both `exception` and `resolverError`?
data ArgumentsExtra
       (p2p :: P2P)
       extraArgs extraState extraDebugState
       extraFlags extraPeers extraAPI
       extraChurnArgs extraCounters exception
       ntnAddr ntcAddr
       resolver resolverError m where
  P2PArguments
    :: P2P.ArgumentsExtra extraState extraDebugState
                          extraFlags extraPeers extraAPI extraChurnArgs
                          extraCounters exception ntnAddr ntcAddr resolver resolverError m
    -> ArgumentsExtra 'P2P extraArgs extraState extraDebugState
                           extraFlags extraPeers extraAPI extraChurnArgs
                           extraCounters exception ntnAddr ntcAddr resolver resolverError m

  NonP2PArguments
    :: NonP2P.ArgumentsExtra
    -> ArgumentsExtra 'NonP2P extraArgs extraState extraDebugState
                              extraFlags extraPeers extraAPI extraChurnArgs
                              extraCounters exception ntnAddr ntcAddr resolver resolverError m


-- | Application data which depend on p2p mode.
--
data ApplicationsExtra (p2p :: P2P) ntnAddr m a where
  P2PApplicationsExtra
    :: P2P.ApplicationsExtra ntnAddr m a
    -> ApplicationsExtra 'P2P ntnAddr m a

  NonP2PApplicationsExtra
    :: NonP2P.ApplicationsExtra
    -> ApplicationsExtra 'NonP2P ntnAddr m a


-- | Run data diffusion in either 'P2P' or 'NonP2P' mode.
--
run :: forall (p2p :: P2P) extraArgs extraState extraDebugState extraFlags
             extraPeers extraAPI extraChurnArgs extraCounters exception a.
      ( Monoid extraPeers
      , Eq extraCounters
      , Eq extraFlags
      , Exception exception
      )
    => (forall mode x y.
           P2P.NodeToNodeConnectionManager mode Socket
                                           RemoteAddress NodeToNodeVersionData
                                           NodeToNodeVersion IO x y
        -> StrictTVar IO
             (PeerSelectionState extraState extraFlags extraPeers
                                 RemoteAddress
                                 (P2P.NodeToNodePeerConnectionHandle
                                      mode RemoteAddress
                                      NodeToNodeVersionData IO x y))
        -> PeerMetrics IO RemoteAddress
        -> IO ())
    -> Tracers
         RemoteAddress NodeToNodeVersion
         LocalAddress  NodeToClientVersion
         IO
    -> ExtraTracers p2p extraState extraDebugState extraFlags extraPeers extraCounters IO
    -> Arguments
         IO
         Socket      RemoteAddress
         LocalSocket LocalAddress
    -> ArgumentsExtra p2p
         extraArgs extraState extraDebugState
         extraFlags extraPeers extraAPI
         extraChurnArgs extraCounters exception
         RemoteAddress LocalAddress Resolver IOException IO
    -> Applications RemoteAddress NodeToNodeVersion   NodeToNodeVersionData
                    LocalAddress  NodeToClientVersion NodeToClientVersionData
                    extraAPI IO a
    -> ApplicationsExtra p2p RemoteAddress IO a
    -> IO ()
run sigUSR1Signal
    tracers (P2PTracers tracersExtra)
            args (P2PArguments argsExtra)
            apps
            (P2PApplicationsExtra appsExtra) =
    void $
    P2P.run
      sigUSR1Signal tracers tracersExtra
      args argsExtra apps appsExtra
run _
    tracers (NonP2PTracers tracersExtra)
            args (NonP2PArguments argsExtra)
            apps
            (NonP2PApplicationsExtra appsExtra) = do
    NonP2P.run tracers tracersExtra
               args argsExtra
               apps appsExtra
