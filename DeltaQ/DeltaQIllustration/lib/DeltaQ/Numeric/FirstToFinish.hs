module DeltaQ.Numeric.FirstToFinish
where

import System.IO.Unsafe (unsafePerformIO)
import System.Random.MWC (GenIO)
import Control.Lens ((^.))


import DeltaQ.Numeric.CDF
import DeltaQ.RationalProbabilityDoubleDelay

-- | A first-to-finish synchronisation between a (non-preemptive)
--   timeout for retrying requests and a (preemptive) timeout
--   for propagating the exception.
ftfNonPreemptWithinTimeout ::( GenIO
                             , Int)
                           -- ^
                           --   - The generator for the random numbers.
                           --   - The number of samples to be used when
                           --     creating the `EmpiricalCDF` from
                           --     the given `DeltaQ` below.
                           -> ( Double
                              , Double)
                           -- ^
                           --   - Preemptive timeout interval.
                           --   - Non-preemptive timeout (retry interval).
                           -> DeltaQ
                           -- ^ The ΔQ of the underlying bearer service
                           -> (EmpiricalCDF
                             , Int
                             , Double -> (Int, Rational)
                             )
                           -- ^
                           --
                           --  - The `EmpiricalCDF` of the underlying
                           --    bearer service.
                           --
                           --  - The maximum number of potential
                           --    attempts made against the underlying
                           --    service (measure of maximum offered
                           --    load / retry count).
                           --
                           --  - Function that returns the the number
                           --    of attempts made and the probabilty
                           --    mass (an improper CDF) for a given
                           --    duration.
ftfNonPreemptWithinTimeout (gen,count) (response'within, ack'timeout) dq
  = (dq'ecdf, length rs, eval'ftf rs)
  where
    rs = residuals 0
    dq'ecdf
      = unsafePerformIO $ empiricalCDF gen count dq
    residuals t
      | t < response'within
          = (t, dq'ecdf) : residuals (t + ack'timeout)
      | otherwise = []
    eval'ftf [] _ = (0,0)
    eval'ftf ((x'o,x'dq):xs) y
      | y <= x'o  = (0,0)
      | otherwise
        = let p'a       = (x'dq ^. ecdf) (y - x'o)
              (p'w,p'b) = eval'ftf xs y
          in (1+p'w,p'a + p'b - (p'a * p'b))
