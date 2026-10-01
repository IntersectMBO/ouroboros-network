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
-- |                |                            | between `4.1e-14` and `5.1e-21`.                            |
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
                    -- ^ NOTE:
                    -- * All inbound peers are `IsNotTrustable` in
                    --   `ouroboros-consensus-diffusion`
                    -- * Only outbound peers declared in the topology as
                    --   trustable have `IsTrustable` set.
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
          -- The timeout bounds how long we wait at the tip of the chain for
          -- the next block.  With a slot length of `T_s = 1s` and an active
          -- slot coefficient of `f = 0.05`, the chance that no block is
          -- produced for `t` slots is `(1-f)^t`, hence a timeout of `t`
          -- seconds corresponds to a threshold `Y`:
          --
          --   t = T_s [log (1-Y) / log (1-f)]
          --
          -- The timeout is randomly picked per state to avoid all peers go down at
          -- the same time in case of a long streak of empty slots, and thus to
          -- avoid global synchronisation.  It is picked uniformly from the
          -- interval `minChainSyncTimeout` - `maxChainSyncTimeout`, currently
          -- 601 - 911, which corresponds to the thresholds `1 - 4.1e-14` and
          -- `1 - 5.1e-21`, i.e. to streaks of empty slots of `4.1e-14` down to
          -- `5.1e-21` probability.  At the lower end of the interval a false
          -- positive is expected roughly once in `1.5e7` years.
          --
          -- NOTE: keep these figures in sync with `minChainSyncTimeout` and
          -- `maxChainSyncTimeout`.
          let timeout :: DiffTime
              (timeout, rnd') = first realToFrac
                              . randomR ( realToFrac minChainSyncTimeout :: Double
                                        , realToFrac maxChainSyncTimeout :: Double
                                        )
                              $ rnd
          in (Just timeout, rnd')
    stateToLimit a@SingDone rnd = (notActiveState a, rnd)

