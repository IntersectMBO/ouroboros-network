module Network.Mux.DeltaQTraceStatsSupport
where

-- This module is making use of hackage libararies. They are not the
-- most efficicient approches for this particlular use case, and they
-- will increase the package dependencies for the final binaries.
--
-- It may well be worthwhile constructing specialsed version for the
-- specific use case, but building those and creating the associated
-- test suite was not deemed a good use of time (at the time of
-- creation).
--
-- Definite space/time optimisation task here.

import Network.Mux.DeltaQTraceTypes

estimateGS :: [(Int, SISec)] -> (Double, Double, Double)
estimateGS = undefined
