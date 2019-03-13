{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Ouroboros.Network.BlockFetch.DeltaQ (
    GSV(..),
    Distribution(..),
    DeltaQ,
    PeerGSV(..),
    estimateBlockFetchResponse,
    blockArrivalShedule,
    PeerFetchInFlightLimits(..),
    calculatePeerFetchInFlightLimits,
  ) where

import           Data.Semigroup ((<>))
import           Control.Monad.Class.MonadTimer

import           Ouroboros.Network.BlockFetch.Types
                   ( SizeInBytes, PeerFetchInFlight(..) )


data Distribution n = Distribution n
-- This is a totally bogus representation. It's just a PoC.
-- This says that there's a single value that it takes with probability 1.

instance Num n => Semigroup (Distribution n) where
  (<>) = convolveDistribution

convolveDistribution :: Num n => Distribution n -> Distribution n -> Distribution n
convolveDistribution (Distribution d) (Distribution d') = Distribution (d+d')
-- Again, totally bogus.

shiftDistribution :: Num n => n -> Distribution n -> Distribution n
shiftDistribution n (Distribution d) = Distribution (n+d)
-- Again, totally bogus.

data GSV t = GSV (Duration t)                -- G as seconds
                 (Duration t)                -- S as seconds / octet
                 (Distribution (Duration t)) -- V as distribution

instance TimeMeasure t => Semigroup (GSV t) where
  GSV g1 s1 v1 <> GSV g2 s2 v2 = GSV (g1+g2) (s1+s2) (v1 <> v2)

newtype DeltaQ t = DeltaQ (Distribution (Duration t))

deriving instance TimeMeasure t => Semigroup (DeltaQ t)

gsvLeadingEdgeArrive  :: TimeMeasure t => GSV t ->                DeltaQ t
gsvTrailingEdgeDepart :: TimeMeasure t => GSV t -> SizeInBytes -> DeltaQ t  -- perhaps a bit dubious
gsvTrailingEdgeArrive :: TimeMeasure t => GSV t -> SizeInBytes -> DeltaQ t

gsvLeadingEdgeArrive (GSV g _s v) =
  DeltaQ (shiftDistribution g v)

gsvTrailingEdgeDepart (GSV _g s v) bytes =
  DeltaQ (shiftDistribution (s * fromIntegral bytes) v)

gsvTrailingEdgeArrive (GSV g s v) bytes =
  DeltaQ (shiftDistribution (g + s * fromIntegral bytes) v)


estimateDetltaQ99thPercentile :: DeltaQ t -> Duration t
estimateDetltaQ99thPercentile (DeltaQ (Distribution t)) = t
-- Again, totally bogus.

estimateProbabilityMassBeforeDeadline :: TimeMeasure t
                                      => DeltaQ t -> Duration t -> Double
estimateProbabilityMassBeforeDeadline (DeltaQ (Distribution t)) d
  | t < d     = 1
  | otherwise = 0
  -- Again, totally bogus.

data PeerGSV time = PeerGSV {
                      outboundGSV :: !(GSV time),
                      inboundGSV  :: !(GSV time)
                    }

data PeerFetchInFlightLimits = PeerFetchInFlightLimits {
       inFlightBytesHighWatermark :: SizeInBytes,
       inFlightBytesLowWatermark  :: SizeInBytes
     }
  deriving Show

calculatePeerFetchInFlightLimits :: TimeMeasure time
                                 => PeerGSV time
                                 -> PeerFetchInFlightLimits
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
      ceiling (toRational (g_out + g_in + d) / toRational s_in)

    d = 2000 --TODO: switch to concrete type that's in RealFrac
    -- But note that the minimum here is based on the assumption that we can
    -- react as the /leading/ edge of the low watermark arrives, but in fact
    -- we can only react when the /trailing/ edge arrives. So when we 

    -- The high watermark is a bit arbitrary. It's just about making sure we
    -- have a bit of a buffer so we can ask for more in one go, rather than
    -- asking for lots of small requests very frequently.
    inFlightBytesHighWatermark = inFlightBytesLowWatermark * 2

{-
blockArrivalShedule :: TimeMeasure time
                    => (block -> Size)
                    -> GSV
                    -> time
                    -> [block]
                    -> [(Point block, time)]
blockArrivalShedule blockSize deltaq now blocks =
    [ (blockPoint b, eta)
    | b <- blocks
    , let eta = estimateETA deltaq (blockSize b) `addTime` now
    ]
-}

{-
submitFetchReqs :: ChainRange block
                -> [(Point block, time)]
                -> FetchTrackingState
                -> TMVar m (FetchRequestMsg block)
                -> m ()
submitFetchReqs range etaschedule FetchTrackingState{..} outQ =
    atomically $ do
      putTMVar outQ (FetchRequestMsg range)
      mapM_ (writeTBQueue blocksInFlight) etaschedule
      modifyTVar' bytesInFlight (+ sum (map (blockSize . fst) etaschedule))
-}


--  If I were to send this request now, when would the leading and trailing
-- edges of the response come back?
--
-- The leading edge is useful for setting a timeout to give us early indication
-- that we're highly likely to miss our response time. The trailing edge is
-- the time we are ultimately interested in, and a timeout on that ensures we
-- are not waiting too long before re-evaluating our decisions.
--
    -- Obviously the response times depend not just on the outbound and inbound
    -- Delta Qs, and the request and response sizes, but it also depends on
    -- the requests that the remote end is already processing.
    --
    -- We could keep track of the detail of the queue on the remote side, but
    -- this quickly gets complicated. Instead we make the following simplifying
    -- and conservative assumption. We only keep track of the amount of data
    -- we have requested that we have not yet received, and we pretend that we
    -- are now asking for that plus the new data we're really asking for.
    --
    -- This over-estimates the response time, but by at most one round trip.
    -- To see that this is the case, consider the most extreme case of response
    -- data that it just arriving at the receiver. If instead the leading edge
    -- is back at the initial request, that is a full round trip away. Note
    -- that to limit this to one round trip we need to update the in-flight
    -- data as it arrives, not just at the trailing edge of block messages.

{-
data FetchTrackingState header time = FetchTrackingState {
       fetchRequestsInFlight      :: Queue (FetchRequestBatch header time),
       fetchRequestsBytesInFlight :: Size
     }

type Queue = Q.BankersDequeue

data FetchRequestBatch header time =
     FetchRequestBatch
       !(ChainRange header)         -- the requested range
       [(BlockInfo header, time)]   -- blocks and trailing edge timeouts

data BlockInfo header = BlockInfo !(Point header) !Size
-}



estimateBlockFetchResponse :: TimeMeasure time
                           => PeerGSV time
                           -> PeerFetchInFlight header
                           -> [SizeInBytes]
                           -> Duration time
estimateBlockFetchResponse PeerGSV{outboundGSV, inboundGSV}
                           PeerFetchInFlight{peerFetchBytesInFlight}
                           blockSizes =
    estimateDetltaQ99thPercentile $
         gsvTrailingEdgeArrive outboundGSV reqSize
      <> gsvTrailingEdgeArrive inboundGSV  rspSize
  where
    reqSize = 100 -- not exact, but it's small
    rspSize = peerFetchBytesInFlight + sum blockSizes

-- | The /trailing/ edge arrival schedule for a bunch of blocks.
--
blockArrivalShedule :: TimeMeasure time
                    => PeerGSV time
                    -> PeerFetchInFlight header
                    -> [SizeInBytes]
                    -> [Duration time]
blockArrivalShedule PeerGSV{outboundGSV, inboundGSV}
                    PeerFetchInFlight{peerFetchBytesInFlight}
                    blockSizes =
    [ estimateDetltaQ99thPercentile $
           gsvTrailingEdgeArrive outboundGSV reqSize
        <> gsvTrailingEdgeArrive inboundGSV  rspSize
    | rspSize <- cumulativeSumFrom peerFetchBytesInFlight blockSizes
    ]
  where
    reqSize = 100 -- not exact, but it's small

    cumulativeSumFrom n = tail . scanl (+) n

{-
newFetchRequestBatch :: TimeMeasure time
                     => time
                     -> PeerGSV time
                     -> PeerFetchInFlight header
                     -> ChainRange header
                     -> [BlockInfo header]
                     -> FetchRequestBatch header time
newFetchRequestBatch now outboundGSV inboundGSV fetchTrackingState fetchRange fetchBlocks =
    FetchRequestBatch
      fetchRange
      [ (block, addTime trailingEdge now)
      | (block, trailingEdge) <- zip fetchBlocks blockTrailingEdges ]
  where
    blockTrailingEdges =
      blockArrivalShedule outboundGSV inboundGSV
                          fetchTrackingState
                          fetchBlocks
-}
{-
updateForNewRequest :: DeltaQ
                    -> DeltaQ
                    -> time
                    -> BlockInfo
                    -> FetchTrackingState
                    -> FetchTrackingState
updateForNewRequest outboundDeltaQ inboundDeltaQ now RemoteIdle =
    RemoteActive {
      _ = 
      _ =
      _ = Q.empty
    }
  where
    -- Point in time where our request would arrive on the remote side.
    est1 = estimateTrailingEdgeArrive outboundDeltaQ 100

    -- Point in time where the response would 
    est2 = estimateTrailingEdgeArrive inboundDeltaQ (blockSize b)

    est3 = estimateTrailingEdgeDepart inboundDeltaQ (blockSize b)


updateForNewRequest RemoteActive{..} =

updateForBlockArrival :: Point
                      -> FetchTrackingState
                      -> FetchTrackingState
-- 
-}

