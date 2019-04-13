{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Ouroboros.Network.BlockFetch.DeltaQ (
    GSV,
    Distribution,
    DeltaQ,
    PeerGSV(..),
    SizeInBytes,
    estimateBlockFetchResponse,
    blockArrivalShedule,
    PeerFetchInFlightLimits(..),
    calculatePeerFetchInFlightLimits,
  ) where

import           Data.Fixed as Fixed (Pico)
import           Control.Monad.Class.MonadTime

import           Ouroboros.Network.DeltaQ
import           Ouroboros.Network.BlockFetch.ClientState
                   ( PeerFetchInFlight(..) )


data PeerFetchInFlightLimits = PeerFetchInFlightLimits {
       inFlightBytesHighWatermark :: SizeInBytes,
       inFlightBytesLowWatermark  :: SizeInBytes
     }
  deriving Show

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
        ceiling (seconds (g_out + g_in + d) / seconds (s_in 1))
      where
        seconds :: DiffTime -> Fixed.Pico
        seconds = realToFrac
      --FIXME: s is now a function of bytes, not unit seconds / octet

    d = 2e-3 -- 2 milliseconds
    -- But note that the minimum here is based on the assumption that we can
    -- react as the /leading/ edge of the low watermark arrives, but in fact
    -- we can only react when the /trailing/ edge arrives. So when we 

    -- The high watermark is a bit arbitrary. It's just about making sure we
    -- have a bit of a buffer so we can ask for more in one go, rather than
    -- asking for lots of small requests very frequently.
    inFlightBytesHighWatermark = inFlightBytesLowWatermark * 2


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

