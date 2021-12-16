{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE StandaloneDeriving         #-}

-- | 𝚫Q representation and primitives.
--
-- See also <https://www.ofcom.org.uk/__data/assets/pdf_file/0024/71682/traffic-management-detection.pdf>
-- appendix A.
--
module Ouroboros.Network.DeltaQ
  ( -- * DeltaQ
    DeltaQ (..)
  , deltaqQ99thPercentile
  , deltaqQ50thPercentile
  , deltaqProbabilityMassBeforeDeadline
    -- * GSV
  , GSV (..)
  , SizeInBytes
  , ballisticGSV
  , gsvLeadingEdgeArrive
  , gsvTrailingEdgeDepart
  , gsvTrailingEdgeArrive
    -- * Distribution
    -- | Needed to construct a 'DeltaQ' or 'GSV'
  , Distribution
  , degenerateDistribution
    -- * Bi-directional GSV.
    -- | 'gsvRequestResponseDuration' is provided as an example of the 'GSV'
    -- and 'DeltaQ' primitives.
  , PeerGSV (..)
  , gsvRequestResponseDuration
  , defaultGSV
  , fromSample
  ) where

import           Control.Monad.Class.MonadTime (DiffTime, Time (..), diffTime)
import           Data.Word (Word32)


--
-- Sketch of API for DeltaQ
--

-- | A \"𝚫Q\" is a probability distribution on the duration between two events.
-- It is an \"improper\" probability distribution in that it may not integrate
-- to 1. The \"missing\" probability mass represents failure. This allows both
-- timing and failure to be represented in one mathematical object.
--
-- In the case of networks a 𝚫Q can be used for example distributions such as
-- the time for a leading edge or trailing edge of a packet to traverse a
-- network (or failing to do so), and many others besides.
--
newtype DeltaQ = DeltaQ (Distribution DiffTime)

-- | DeltaQ distributions (as independent random variables) are semi-groups
-- by convolution.
--
deriving instance Semigroup DeltaQ

-- | The point in time in the distribution for which 99% of the probability
-- mass is before that time.
--
-- This tells us how long we would have to wait to have a 99% chance of the
-- end event having happened. Of course 99% is somewhat arbitrary and other
-- points in the distribution could be of interest.
--
-- TODO: this needs to be specified better for improper distributions.
--
deltaqQ99thPercentile :: DeltaQ -> DiffTime
deltaqQ99thPercentile (DeltaQ (DegenerateDistribution t)) = t

deltaqQ50thPercentile :: DeltaQ -> DiffTime
deltaqQ50thPercentile (DeltaQ (DegenerateDistribution t)) = t

-- | This is another way of looking at a 𝚫Q distribution. Instead of giving
-- a fraction of the probability mass (like 99%) and asking how long we would
-- have to wait, we can say how long we are prepared to wait and ask what
-- fraction of the probability mass is before that time.
--
-- So this tells us the chance of the end event happening within the time we
-- are prepared to wait. This is useful for evaluating different options for
-- which has the greatest probability of success within a deadline.
--
deltaqProbabilityMassBeforeDeadline :: DiffTime
                                    -> DeltaQ
                                    -> Double
deltaqProbabilityMassBeforeDeadline deadline (DeltaQ (DegenerateDistribution t))
  | t < deadline = 1
  | otherwise    = 0


--
-- Sketch of API for Distribution
--

-- | An improper probability distribution over some underlying type (such as
-- time durations).
--
-- The current representation only covers the case of degenerate distributions,
-- that take a single value with probability 1. This is just a proof of concept
-- to illustrate the API.
--
data Distribution n = DegenerateDistribution n

-- | Distributions are semi-groups by convolution.
--
instance Num n => Semigroup (Distribution n) where
  (<>) = convolveDistribution


-- | Make a degenerate distribution.
--
-- <https://en.wikipedia.org/wiki/Degenerate_distribution>
--
degenerateDistribution :: n -> Distribution n
degenerateDistribution = DegenerateDistribution


-- | The convolution of two (improper) probability distributions.
--
-- <https://en.wikipedia.org/wiki/Convolution>
--
-- For now the implementation only covers degenerate distributions.
--
convolveDistribution :: Num n
                     => Distribution n -> Distribution n -> Distribution n
convolveDistribution (DegenerateDistribution d)
                     (DegenerateDistribution d') =
    DegenerateDistribution (d+d')


-- | Shift a distribution by a constant. This is equivalent to convolution
-- with a degenerate distribution.
--
-- > shiftDistribution n d = degenerateDistribution n `convolveDistribution` d
--
shiftDistribution :: Num n => n -> Distribution n -> Distribution n
shiftDistribution n (DegenerateDistribution d) = DegenerateDistribution (n+d)


--
-- Sketch of API for GSV
--

-- | A \"GSV\" corresponds to a 𝚫Q that is a function of the size of a data
-- unit to be transmitted over a network. That is, it gives the 𝚫Q of the
-- transmission time for different sizes of data in 'SizeInBytes'.
--
-- The 𝚫Q is broken out into three separate 𝚫Q distributions, 𝚫Q∣G, 𝚫Q∣S and
-- 𝚫Q∣V, with the overall 𝚫Q being the convolution of the three components.
-- The G and S components captures the /structural/ aspects of networks, while
-- the V captures the /variable/ aspects:
--
-- [/G/]: the /geographical/ component of network delay. This is the minimum
--        time to transmit a hypothetical zero-sized data unit. This component
--        of the distribution does not depend on the data unit size. It is
--        a degenerate distribution, taking only one value.
--
-- [/S/]: the /serialisation/ component of network delay. This is time to
--        serialise a data unit as it is being transmitted. This is of course
--        a function of the data unit size. For each size it is a degenerate
--        distribution, taking only one value.
--
-- [/V/]: the /variable/ aspect of network delay. This captures the variability
--        in network delay due to issues such as congestion. This does not
--        depend on the data unit size, and is /not/ a degenerate disruption.
--
-- For ballistic transmission of packets, /S/ is typically directly proportional
-- to the size. Thus the combination of /G/ and /S/ is simply a linear function
-- of the size.
--
data GSV = GSV !DiffTime                  -- G as seconds
               !(SizeInBytes -> DiffTime) -- S as seconds for size
               !(Distribution DiffTime)   -- V as distribution

-- | GSVs are semi-groups by convolution on the three individual components.
--
instance Semigroup GSV where
  GSV g1 s1 v1 <> GSV g2 s2 v2 = GSV (g1+g2) (\sz -> s1 sz + s2 sz) (v1 <> v2)


instance Show GSV where
    show (GSV g s (DegenerateDistribution v)) =
        "GSV g " ++ show g ++ " s " ++ show (s 1) ++ " v " ++ show v

-- | The case of ballistic packet transmission where the /S/ is directly
-- proportional to the packet size.
--
ballisticGSV :: DiffTime               -- ^ /G/
             -> DiffTime               -- ^ /S/ as time per byte.
             -> Distribution DiffTime  -- ^ /V/ distribution
             -> GSV
ballisticGSV g s v = GSV g (\sz -> s * fromIntegral sz) v


--
-- Basic calculations based on GSV
--

type SizeInBytes = Word32

-- | The 𝚫Q for when the leading edge of a transmission unit arrives at the
-- destination. This is just the convolution of the /G/ and /V/ components.
--
gsvLeadingEdgeArrive  :: GSV ->                DeltaQ

-- | The 𝚫Q for when the trailing edge of a transmission unit departs the
-- sending end. This is just the convolution of the /S/ and /V/ components.
--
-- Since it involves /S/ then it depends on the 'SizeInBytes' of the
-- transmission unit.
--
gsvTrailingEdgeDepart :: GSV -> SizeInBytes -> DeltaQ

-- | The 𝚫Q for when the trailing edge of a transmission unit arrives at the
-- destination. This is the convolution of the /G/, /S/ and /V/ components.
--
-- Since it involves /S/ then it depends on the 'SizeInBytes' of the
-- transmission unit.
--
gsvTrailingEdgeArrive :: GSV -> SizeInBytes -> DeltaQ

gsvLeadingEdgeArrive (GSV g _s v) =
  DeltaQ (shiftDistribution g v) -- dubious for anything other than ballistic

gsvTrailingEdgeDepart (GSV _g s v) bytes =
  DeltaQ (shiftDistribution (s bytes) v)

gsvTrailingEdgeArrive (GSV g s v) bytes =
  DeltaQ (shiftDistribution (g + s bytes) v)


--
-- Derived calculations for bi-directional GSV
--

-- | The 'GSV' for both directions with a peer, outbound and inbound.
--
data PeerGSV = PeerGSV {
                 sampleTime  :: !Time,
                 outboundGSV :: !GSV,
                 inboundGSV  :: !GSV
               }
               deriving Show

-- | The current tracking model is based on an EWMA
--  (https://en.wikipedia.org/wiki/Moving_average#Exponential_moving_average).
--  Typically implementations of EWMA assume a regular update, but EWMA is based
--  on Exponential Smoothing
--  (https://en.wikipedia.org/wiki/Exponential_smoothing). Such smoothing has a
--  time constant, which captures the time for a unit impulse to decay to 1 -
--  1/e (~ 63.2%), the &#x1D6FC (smoothing factor) is a function of relative
--  frequency of the sample interval and this time constant.
--
-- The approach being taken here is one that does not assume a fixed sample
-- interval (and hence a fixed &#x1D6FC), instead we calculate, given the
-- interval from when the last sample was taken, the &#x1D6FC needed to ensure
-- that the old value has sufficiently decayed.
--
-- The exact calcuation involves exponentiation, however where the number of
-- samples within the time constant is sufficiently large a simple ratio of the
-- sample's interval over the time constant will suffice. The relative error of
-- this numerical approximation is, for our use case, small. Eg 1/50 (20s
-- between samples with a 1000s time constant) has a relative error of 1%. The
-- expected typical range of this relative error is between 5% (ratio of 1/10),
-- to 0.5% (1/100).
--
-- Given the inherent measurement noise in this measurement, the use of the
-- approximation is well justified. We choose (reaonably aribtarily) 1000s as
-- the time constant, it is unclear if this should be a configuration variable
-- or not.
-- Note that this semigroup is  is non-commutative. The new value must come first.
instance Semigroup PeerGSV where
  (<>) a b = let timeConstant = 1000 :: DiffTime
                 sampleInterval = sampleTime a `diffTime` sampleTime b
                 alpha = (sampleInterval / timeConstant) `min` 1
                 updateG (GSV g1 s v) (GSV g0 _ _)
                   = GSV (g0 + alpha * (g1 - g0)) s v
             in PeerGSV { sampleTime  = sampleTime a
                        , outboundGSV = updateG (outboundGSV a) (outboundGSV b)
                        , inboundGSV  = updateG (inboundGSV  a) (inboundGSV  b)
                        }

-- | This is an example derived operation using the other 'GSV' and 'DeltaQ'
-- primitives.
--
-- It calculates the 𝚫Q for the time to send a request of a certain size and
-- receive a reply of an expected size. It then takes the 99% percentile as an
-- approximation of the maximum time we might be prepared to wait.
--
-- > deltaqQ99thPercentile $
-- >     gsvTrailingEdgeArrive outboundGSV reqSize
-- >  <> gsvTrailingEdgeArrive inboundGSV respSize
--
--
-- This is not realistic in that it omits processing time, but that could be
-- added as yet another 'DeltaQ' value, if there's any estimate for it:
--
-- > deltaqQ99thPercentile $
-- >     gsvTrailingEdgeArrive outboundGSV reqSize
-- >  <> gsvTrailingEdgeArrive inboundGSV respSize
-- >  <> processingDeltaQ
--
gsvRequestResponseDuration :: PeerGSV
                           -> SizeInBytes -- ^ Request size
                           -> SizeInBytes -- ^ Expected response size
                           -> DiffTime
gsvRequestResponseDuration PeerGSV{outboundGSV, inboundGSV}
                           reqSize respSize =
    deltaqQ99thPercentile $
        gsvTrailingEdgeArrive outboundGSV reqSize
     <> gsvTrailingEdgeArrive inboundGSV respSize


defaultGSV :: PeerGSV
defaultGSV = PeerGSV {sampleTime, outboundGSV, inboundGSV }
  where
    default_g = 500e-3 -- Old hardcoded default value. Only available value when running without KeepAlive.
    default_s = 2e-6 -- 4Mbps.
    inboundGSV  = ballisticGSV default_g default_s (degenerateDistribution 0)
    outboundGSV = inboundGSV
    sampleTime  = Time 0

fromSample :: Time -> Time -> SizeInBytes -> PeerGSV
fromSample t@(Time start) (Time end) _size =
    PeerGSV  {sampleTime, outboundGSV, inboundGSV }
  where
    g =  (end - start) / 2

    sampleTime = t
    inboundGSV = ballisticGSV g 2e-6 (degenerateDistribution 0)
    outboundGSV  = inboundGSV
