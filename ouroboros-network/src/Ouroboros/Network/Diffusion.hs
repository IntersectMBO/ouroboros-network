{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
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

import Cardano.Diffusion.P2P qualified as P2P
import Cardano.Node.ArgumentsExtra (CardanoArgumentsExtra)
import Cardano.Node.LedgerPeerConsensusInterface
           (CardanoLedgerPeersConsensusInterface)
import Cardano.Node.PeerSelection.Governor.PeerSelectionState
           (CardanoPeerSelectionState)
import Cardano.Node.PeerSelection.PeerTrustable (PeerTrustable)
import Cardano.Node.PublicRootPeers (CardanoPublicRootPeers)
import Control.Exception (IOException)
import Data.Functor (void)
import Network.Socket (Socket)
import Ouroboros.Network.Diffusion.Common (Arguments, Tracers)
import Ouroboros.Network.Diffusion.Common qualified as Common
import Ouroboros.Network.Diffusion.NonP2P qualified as NonP2P
import Ouroboros.Network.NodeToClient (LocalAddress, LocalSocket,
           NodeToClientVersion, NodeToClientVersionData)
import Ouroboros.Network.NodeToNode (NodeToNodeVersion, NodeToNodeVersionData,
           RemoteAddress)

-- | Promoted data types.
--
data P2P = P2P        -- ^ General P2P mode. Can be instantiated with custom
                      -- data types
         | NonP2P     -- ^ Cardano non-P2P mode. Deprecated
         | P2PCardano -- ^ Cardano P2P mode.

-- | Tracers which depend on p2p mode.
--
data ExtraTracers (p2p :: P2P) extraState extraFlags extraPeers m where
  P2PTracers
    :: Common.TracersExtra
           RemoteAddress NodeToNodeVersion         NodeToNodeVersionData
           LocalAddress  NodeToClientVersion       NodeToClientVersionData
           IOException   extraState extraState extraFlags extraPeers m
    -> ExtraTracers 'P2P extraState extraFlags extraPeers m

  P2PCardanoTracers
    :: Common.TracersExtra
           RemoteAddress NodeToNodeVersion         NodeToNodeVersionData
           LocalAddress  NodeToClientVersion       NodeToClientVersionData
           IOException   CardanoPeerSelectionState CardanoPeerSelectionState
           PeerTrustable (CardanoPublicRootPeers RemoteAddress) m
    -> ExtraTracers 'P2PCardano CardanoPeerSelectionState PeerTrustable (CardanoPublicRootPeers RemoteAddress) m

  NonP2PTracers
    :: NonP2P.TracersExtra
    -> ExtraTracers 'NonP2P extraState extraFlags extraPeers m


-- | Diffusion arguments which depend on p2p mode.
--
data ArgumentsExtra (p2p :: P2P) extraArgs extraPeers m where
  P2PArguments
    :: Common.ArgumentsExtra extraArgs extraPeers m
    -> ArgumentsExtra 'P2P extraArgs extraPeers m

  P2PCardanoArguments
    :: Common.ArgumentsExtra (CardanoArgumentsExtra m) PeerTrustable m
    -> ArgumentsExtra 'P2PCardano (CardanoArgumentsExtra m) PeerTrustable m

  NonP2PArguments
    :: NonP2P.ArgumentsExtra
    -> ArgumentsExtra 'NonP2P extraArgs extraPeers m

-- | Application data which depend on p2p mode.
--
data Applications (p2p :: P2P) extraAPI m a where
  P2PApplications
    :: Common.Applications
         RemoteAddress  NodeToNodeVersion   NodeToNodeVersionData
         LocalAddress   NodeToClientVersion NodeToClientVersionData
         (CardanoLedgerPeersConsensusInterface m) m a
    -> Applications 'P2P extraAPI m a

  P2PCardanoApplications
    :: Common.Applications
         RemoteAddress  NodeToNodeVersion   NodeToNodeVersionData
         LocalAddress   NodeToClientVersion NodeToClientVersionData
         (CardanoLedgerPeersConsensusInterface m) m a
    -> Applications 'P2PCardano (CardanoLedgerPeersConsensusInterface m) m a

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

  P2PCardanoApplicationsExtra
    :: Common.ApplicationsExtra ntnAddr m a
    -> ApplicationsExtra 'P2PCardano ntnAddr m a

  NonP2PApplicationsExtra
    :: NonP2P.ApplicationsExtra
    -> ApplicationsExtra 'NonP2P ntnAddr m a


-- | Run data diffusion in either 'P2P' or 'NonP2P' mode.
--
run :: forall (p2p :: P2P) extraArgs extraState extraFlags extraPeers extraAPI a.
       Tracers
         RemoteAddress NodeToNodeVersion
         LocalAddress  NodeToClientVersion
         IO
    -> ExtraTracers p2p extraState extraFlags extraPeers IO
    -> Arguments
         IO
         Socket      RemoteAddress
         LocalSocket LocalAddress
    -> ArgumentsExtra p2p extraArgs extraFlags IO
    -> Applications p2p extraAPI IO a

    -> ApplicationsExtra p2p RemoteAddress IO a
    -> IO ()
run _ (P2PTracers _)
            _ (P2PArguments _)
            (P2PApplications _)
            (P2PApplicationsExtra _) =
    undefined
run tracers (P2PCardanoTracers tracersExtra)
            args (P2PCardanoArguments argsExtra)
            (P2PCardanoApplications apps)
            (P2PCardanoApplicationsExtra appsExtra) =
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
