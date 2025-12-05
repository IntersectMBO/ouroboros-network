{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Cardano.Network.Protocol.Limits where

import Control.Monad.Class.MonadTime.SI
import System.Random (StdGen)

import Network.TypedProtocol.Core

import Cardano.Network.PeerSelection.PeerTrustable (PeerTrustable)


newtype ProtocolTimeLimitsWithRnd ps = ProtocolTimeLimitsWithRnd {
      timeLimitForStateWithRnd :: forall (st :: ps). ActiveState st
                               => PeerTrustable -> StateToken st -> StdGen -> (Maybe DiffTime, StdGen)
    }
