{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Diffusion
  (
  -- * Common API
    P2P (..)
  , InitializationTracer(..)
  , Tracers (..)
  , nullTracers
  , ExtraTracers (..)
  , Failure
  , Arguments (..)
  , ExtraArguments (..)
  , Applications (..)
  , ExtraApplications (..)
  -- * Run data diffusion
  , run
  )
  where

import           Data.Functor (void)

import           Ouroboros.Network.Diffusion.Common as Common
import qualified Ouroboros.Network.Diffusion.P2P    as P2P
import qualified Ouroboros.Network.Diffusion.NonP2P as NonP2P

-- | Promoted data types.
--
data P2P = P2P | NonP2P


-- | Tracers which depend on p2p mode.
--
data ExtraTracers (p2p :: P2P) where
  P2PTracers
    :: P2P.TracersExtra
    -> ExtraTracers 'P2P

  NonP2PTracers
    :: NonP2P.TracersExtra
    -> ExtraTracers 'NonP2P


-- | Diffusion arguments which depend on p2p mode.
--
data ExtraArguments (p2p :: P2P) where
  P2PArguments
    :: P2P.ArgumentsExtra
    -> ExtraArguments 'P2P

  NonP2PArguments
    :: NonP2P.ArgumentsExtra
    -> ExtraArguments 'NonP2P


-- | Application data which depend on p2p mode.
--
data ExtraApplications (p2p :: P2P) where
  P2PApplications
    :: P2P.ApplicationsExtra
    -> ExtraApplications 'P2P

  NonP2PApplications
    :: NonP2P.ApplicationsExtra
    -> ExtraApplications 'NonP2P


-- | Run data diffusion in either 'P2P' or 'NonP2P' mode.
--
run :: forall (p2p :: P2P).
       Tracers
    -> ExtraTracers p2p
    -> Arguments
    -> ExtraArguments p2p
    -> Applications
    -> ExtraApplications p2p
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

