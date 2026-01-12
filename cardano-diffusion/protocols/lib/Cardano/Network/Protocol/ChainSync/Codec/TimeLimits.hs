{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Network.Protocol.ChainSync.Codec.TimeLimits (timeLimitsChainSync) where

import Control.Monad.Class.MonadTime.SI

import Network.TypedProtocol.Codec.CBOR hiding (decode, encode)

import Ouroboros.Network.Protocol.ChainSync.Codec
import Ouroboros.Network.Protocol.ChainSync.Type
import Ouroboros.Network.Protocol.Limits

import Cardano.Network.PeerSelection.PeerTrustable (PeerTrustable (..))

import Data.Bifunctor (first)
import Data.Kind (Type)
import System.Random (StdGen, randomR)

-- | Time Limits
--
-- +----------------+----------------------------+-------------------------------------------------------------+
-- | Trustable peer | ChainSync State            | timeout (s)                                                 |
-- +================+============================+=============================================================+
-- |                | @'StIdle'@                 | corresponds to 'ChainSyncIdleTimeout'                       |
-- +----------------+----------------------------+-------------------------------------------------------------+
-- |                | @'StNext' 'StCanAwait'@    | 'shortWait'                                                 |
-- +----------------+----------------------------+-------------------------------------------------------------+
-- | IsNotTrustable | @'StNext' 'StMustReply'@   | randomly picked using uniform distribution from             |
-- |                |                            | the range @('minChainSyncTimeout', 'maxChainSyncTimeout')@, |
-- |                |                            | which corresponds to a chance of an empty streak of slots   |
-- |                |                            | between `0.0001%` and `1%` probability.                     |
-- +----------------+----------------------------+-------------------------------------------------------------+
-- | IsTrustable    | @'StNext' 'StMustReply'@   | 'waitForever' (i.e. never times out)                        |
-- +----------------+----------------------------+-------------------------------------------------------------+
-- |                | @'StIntersect'@            | 'shortWait'                                                 |
-- +----------------+----------------------------+-------------------------------------------------------------+
--
timeLimitsChainSync :: forall (header :: Type) (point :: Type) (tip :: Type).
                       ChainSyncIdleTimeout
                    -- ^ idle timeout, the default value
                    -- `Configuration.defaultChainSyncIdleTimeout`.
                    -> PeerTrustable
                    -> ProtocolTimeLimitsWithRnd (ChainSync header point tip)
timeLimitsChainSync idleTimeout peerTrustable = ProtocolTimeLimitsWithRnd stateToLimit
  where
    stateToLimit :: forall (st :: ChainSync header point tip).
                    ActiveState st
                 => StateToken st -> StdGen -> (Maybe DiffTime, StdGen)
    stateToLimit SingIdle                 rnd | ChainSyncIdleTimeout timeout <- idleTimeout
                                              = (Just timeout, rnd)
                                              | otherwise
                                              = (Nothing, rnd)
    stateToLimit SingIntersect            rnd = (shortWait, rnd)
    stateToLimit (SingNext SingCanAwait)  rnd = (shortWait, rnd)
    stateToLimit (SingNext SingMustReply) rnd =
      case peerTrustable of
        IsTrustable    -> (Nothing, rnd)
        IsNotTrustable ->
          -- We draw from a range for which streaks of empty slots ranges
          -- from 0.0001% up to 1% probability.
          -- t = T_s [log (1-Y) / log (1-f)]
          -- Y = [0.99, 0.999...]
          -- T_s = slot length of 1s.
          -- f = 0.05
          -- The timeout is randomly picked per state to avoid all peers go down at
          -- the same time in case of a long streak of empty slots, and thus to
          -- avoid global synchronisation.  The timeout is picked uniformly from
          -- the interval 135 - 269, which corresponds to 99.9% to
          -- 99.9999% thresholds.
          let timeout :: DiffTime
              (timeout, rnd') = first realToFrac
                              . randomR ( realToFrac minChainSyncTimeout :: Double
                                        , realToFrac maxChainSyncTimeout :: Double
                                        )
                              $ rnd
          in (Just timeout, rnd')
    stateToLimit a@SingDone rnd = (notActiveState a, rnd)

