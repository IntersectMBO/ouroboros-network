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

import Ouroboros.Network.NodeToClient qualified as NodeToClient
import Ouroboros.Network.NodeToNode qualified as NodeToNode
import Ouroboros.Network.PeerSelection.Governor.Types

import Ouroboros.Network.Diffusion.Common as Common
import Ouroboros.Network.Diffusion.NonP2P qualified as NonP2P
import Ouroboros.Network.Diffusion.P2P qualified as P2P

-- | Promoted data types.
--
data P2P = P2P | NonP2P

-- | Tracers which depend on p2p mode.
--
data ExtraTracers (p2p :: P2P) where
  P2PTracers
    :: P2P.TracersExtra
           NodeToNode.Address  NodeToNode.Version   NodeToNode.VersionData
           NodeToClient.Address   NodeToClient.Version NodeToClient.VersionData
           IOException IO
    -> ExtraTracers 'P2P

  NonP2PTracers
    :: NonP2P.TracersExtra
    -> ExtraTracers 'NonP2P


-- | Diffusion arguments which depend on p2p mode.
--
data ExtraArguments (p2p :: P2P) m where
  P2PArguments
    :: P2P.ArgumentsExtra m
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
         NodeToNode.Address   NodeToNode.Version
         NodeToClient.Address NodeToClient.Version
         IO
    -> ExtraTracers p2p
    -> Arguments
         IO
         NodeToNode.Socket        NodeToNode.Address
         NodeToClient.LocalSocket NodeToClient.Address
    -> ExtraArguments p2p IO
    -> Applications
         NodeToNode.Address   NodeToNode.Version   NodeToNode.VersionData
         NodeToClient.Address NodeToClient.Version NodeToClient.VersionData
         IO a
    -> ExtraApplications p2p NodeToNode.Address IO a
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
            apps (NonP2PApplications appsExtra) =
    NonP2P.run tracers tracersExtra
               args argsExtra
               apps appsExtra
