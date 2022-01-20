{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

module Ouroboros.Network.BlockFetch.DeltaQ
  ( GSV
  , Distribution
  , DeltaQ
  , PeerGSV (..)
  , SizeInBytes
  , PeerFetchInFlightLimits (..)
  , calculatePeerFetchInFlightLimits
  , estimateResponseDeadlineProbability
  , estimateExpectedResponseDuration
  , comparePeerGSV
  , comparePeerGSV'
  ) where

import           Control.Monad.Class.MonadTime
import           Data.Fixed as Fixed (Pico)
import           Data.Hashable
import           Data.Set (Set)
import qualified Data.Set as Set

import           Ouroboros.Network.DeltaQ


data PeerFetchInFlightLimits = PeerFetchInFlightLimits {
       inFlightBytesHighWatermark :: SizeInBytes,
       inFlightBytesLowWatermark  :: SizeInBytes
     }
  deriving Show

-- | Order two PeerGSVs based on `g`.
-- Incase the g values are within +/- 5% of each other `peer` is used as a tie breaker.
-- The salt is unique per running node, which avoids all nodes prefering the same peer in case of
-- a tie.
comparePeerGSV :: forall peer.
      ( Hashable peer
      , Ord peer
      )
      => Set peer
      -> Int
      -> (PeerGSV, peer)
      -> (PeerGSV, peer)
      -> Ordering
comparePeerGSV activePeers salt (a, a_p) (b, b_p) =
    let gs_a = if isActive a_p then activeAdvantage * gs a
                               else gs a
        gs_b = if isActive b_p then activeAdvantage * gs b
                               else gs b in
    if abs (gs_a - gs_b) < 0.05 * max gs_a gs_b
       then compare (hashWithSalt salt a_p) (hashWithSalt salt b_p)
       else compare gs_a gs_b
  where
    -- In order to avoid switching between peers with similar g we give
    -- active peers a slight advantage.
    activeAdvantage :: DiffTime
    activeAdvantage = 0.8

    isActive :: peer -> Bool
    isActive p = Set.member p activePeers

    gs :: PeerGSV -> DiffTime
    gs PeerGSV { outboundGSV = GSV g_out _s_out _v_out,
                 inboundGSV  = GSV g_in  _s_in  _v_in
               } = g_out + g_in

-- | Order two PeerGSVs based on `g`.
-- Like comparePeerGSV but doesn't take active status into account
comparePeerGSV' :: forall peer.
      ( Hashable peer
      , Ord peer
      )
      => Int
      -> (PeerGSV, peer)
      -> (PeerGSV, peer)
      -> Ordering
comparePeerGSV' = comparePeerGSV Set.empty


calculatePeerFetchInFlightLimits :: PeerGSV -> PeerFetchInFlightLimits
calculatePeerFetchInFlightLimits PeerGSV {
                                   outboundGSV = GSV g_out _s_out _v_out,
                                   inboundGSV  = GSV g_in   s_in  _v_in
                                 } =
    PeerFetchInFlightLimits {
      inFlightBytesLowWatermark,
      inFlightBytesHighWatermark
    }
  where
    -- To keep the remote end busy at all times, we want to make sure that by
    -- the time it finishes sending its last response that there's another
    -- request at the remote end that's ready to go. So we must initiate another
    -- request g_out seconds before the remote end becomes idle.
    --
    -- Now it turns out to be more convenient to measure this not in time, but
    -- based on the number of bytes of requests that are in-flight. This of
    -- course directly corresponds to time, via S_in.
    --
    -- The units of S_in is seconds / octet. We need to calculate the number
    -- of bytes that can be in flight during a time interval t. So we need
    -- octets / second * seconds-in-flight = octets.
    --
    -- > (1/s_in) * t   or equivalently  t/s_in
    --
    -- So for a remote peer, g_out seconds before it becomes idle, it will have
    -- \( g_in\/s_in \) bytes inbound. Our request will arrive after g_out
    -- seconds, we should request at minimum \( g_in\/s_in \) bytes.
    --
    -- We should also account for the fact that we do not have perfect
    -- scheduling and cannot initiate the request at exactly the right time, so
    -- we should request it slightly early (and thus request correspondingly
    -- more). Lets say our maximum schedule delay is @d@ seconds.
    --
    inFlightBytesLowWatermark =
        max minLowWaterMark (ceiling (seconds (g_out + g_in + d) / seconds (s_in 1)))
      where
        -- To ensure that blockfetch can do pipelining we enforce a minimal
        -- low water mark of at least 3 64k blocks
        minLowWaterMark :: SizeInBytes
        minLowWaterMark = 3 * 64 * 1024

        seconds :: DiffTime -> Fixed.Pico
        seconds = realToFrac
      --FIXME: s is now a function of bytes, not unit seconds / octet

    d = 2e-2 -- 20 milliseconds, we desire to make a new descison every 10ms.
             -- This gives us some margin.

    -- But note that the minimum here is based on the assumption that we can
    -- react as the /leading/ edge of the low watermark arrives, but in fact
    -- we can only react when the /trailing/ edge arrives. So when we

    -- The high watermark is a bit arbitrary. It's just about making sure we
    -- have a bit of a buffer so we can ask for more in one go, rather than
    -- asking for lots of small requests very frequently.
    inFlightBytesHighWatermark = inFlightBytesLowWatermark * 2


-- | Given the 'PeerGSV', the bytes already in flight and the size of new
-- blocks to download, estimate the probability of the download completing
-- within the deadline.
--
-- This is an appropriate estimator to use in a situation where meeting a
-- known deadline is the goal.
--
estimateResponseDeadlineProbability :: PeerGSV
                                    -> SizeInBytes
                                    -> SizeInBytes
                                    -> DiffTime
                                    -> Double
estimateResponseDeadlineProbability PeerGSV{outboundGSV, inboundGSV}
                                    bytesInFlight bytesRequested deadline =
    deltaqProbabilityMassBeforeDeadline deadline $
        gsvTrailingEdgeArrive outboundGSV reqSize
     <> gsvTrailingEdgeArrive inboundGSV respSize
  where
    reqSize  = 100 -- TODO not exact, but it's small
    respSize = bytesInFlight + bytesRequested


-- | Given the 'PeerGSV', the bytes already in flight and the size of new
-- blocks to download, estimate the expected (mean) time to complete the
-- download.
--
-- This is an appropriate estimator to use when trying to minimising the
-- expected overall download time case in the long run (rather than optimising
-- for the worst case in the short term).
--
estimateExpectedResponseDuration :: PeerGSV
                                 -> SizeInBytes -- ^ Request size
                                 -> SizeInBytes -- ^ Expected response size
                                 -> DiffTime
estimateExpectedResponseDuration PeerGSV{outboundGSV, inboundGSV}
                           bytesInFlight bytesRequested =
    deltaqQ50thPercentile $
        gsvTrailingEdgeArrive outboundGSV reqSize
     <> gsvTrailingEdgeArrive inboundGSV respSize
  where
    reqSize  = 100 -- TODO not exact, but it's small
    respSize = bytesInFlight + bytesRequested

{-
estimateBlockFetchResponse :: PeerGSV
                           -> PeerFetchInFlight header
                           -> [SizeInBytes]
                           -> DiffTime
estimateBlockFetchResponse gsvs
                           PeerFetchInFlight{peerFetchBytesInFlight}
                           blockSizes =
    gsvRequestResponseDuration gsvs reqSize respSize
  where
    reqSize  = 100 -- not exact, but it's small
    respSize = peerFetchBytesInFlight + sum blockSizes


-- | The /trailing/ edge arrival schedule for a bunch of blocks.
--
blockArrivalShedule :: PeerGSV
                    -> PeerFetchInFlight header
                    -> [SizeInBytes]
                    -> [DiffTime]
blockArrivalShedule gsvs
                    PeerFetchInFlight{peerFetchBytesInFlight}
                    blockSizes =
    [ gsvRequestResponseDuration gsvs reqSize respSize
    | respSize <- cumulativeSumFrom peerFetchBytesInFlight blockSizes
    ]
  where
    reqSize = 100 -- not exact, but it's small

    cumulativeSumFrom n = tail . scanl (+) n
-}
