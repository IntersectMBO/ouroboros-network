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

import Control.Exception (IOException)
import Data.Functor (void)

import Network.Socket (Socket)

import Ouroboros.Network.NodeToClient (LocalAddress, LocalSocket,
           NodeToClientVersion, NodeToClientVersionData)
import Ouroboros.Network.NodeToNode (NodeToNodeVersion, NodeToNodeVersionData,
           RemoteAddress)
import Ouroboros.Network.PeerSelection.Governor.Types

import Cardano.Node.ArgumentsExtra (CardanoArgumentsExtra)
import Cardano.Node.LedgerPeerConsensusInterface
           (CardanoLedgerPeersConsensusInterface)
import Cardano.Node.PeerSelection.Governor.PeerSelectionState
           (CardanoPeerSelectionState)
import Cardano.Node.PeerSelection.PeerTrustable (PeerTrustable)
import Cardano.Node.PublicRootPeers (CardanoPublicRootPeers)
import Ouroboros.Network.Diffusion.Common as Common
import Ouroboros.Network.Diffusion.NonP2P qualified as NonP2P
import Ouroboros.Network.Diffusion.P2P qualified as P2P
import Ouroboros.Network.PeerSelection.LedgerPeers.Type (mapExtraAPI)

-- | Promoted data types.
--
data P2P = P2P | NonP2P

-- | Tracers which depend on p2p mode.
--
data ExtraTracers (p2p :: P2P) where
  P2PTracers
    :: P2P.TracersExtra
           RemoteAddress NodeToNodeVersion         NodeToNodeVersionData
           LocalAddress  NodeToClientVersion       NodeToClientVersionData
           IOException   CardanoPeerSelectionState CardanoPeerSelectionState
           PeerTrustable (CardanoPublicRootPeers RemoteAddress) IO
    -> ExtraTracers 'P2P

  NonP2PTracers
    :: NonP2P.TracersExtra
    -> ExtraTracers 'NonP2P


-- | Diffusion arguments which depend on p2p mode.
--
data ExtraArguments (p2p :: P2P) m where
  P2PArguments
    :: P2P.ArgumentsExtra (CardanoArgumentsExtra m) PeerTrustable m
    -> ExtraArguments 'P2P m

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
         LocalSocket LocalAddress
    -> ExtraArguments p2p IO
    -> Applications
         RemoteAddress  NodeToNodeVersion   NodeToNodeVersionData
         LocalAddress   NodeToClientVersion NodeToClientVersionData
         (CardanoLedgerPeersConsensusInterface IO) IO a
    -> ExtraApplications p2p RemoteAddress IO a
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
            apps (NonP2PApplications appsExtra) = do
    let appsUnit = apps {
          daLedgerPeersCtx = mapExtraAPI (const ()) (daLedgerPeersCtx apps)
        }
    NonP2P.run tracers tracersExtra
               args argsExtra
               appsUnit appsExtra
