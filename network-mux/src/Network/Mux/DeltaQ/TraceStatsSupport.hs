module Network.Mux.DeltaQ.TraceStatsSupport where

-- This module is making use of hackage statisitical libraries. They
-- are not the most efficicient approches for this particlular use
-- case, and they may increase the package dependencies for the final
-- binaries (they have a lot of dependencies).
--
-- It may well be worthwhile constructing specialsed version for the
-- specific use case, but building those and creating the associated
-- test suite was not deemed a good use of time (at the time of
-- creation).
--
-- Definite space/time optimisation task here.

import           Network.Mux.DeltaQ.TraceTypes

import qualified Data.Vector.Unboxed as V
import           Statistics.LinearRegression

estimateGS :: [(Int, SISec)] -> (Double, Double, Double)
estimateGS xys
  = let (xs', ys') = unzip xys
        xs = V.fromList $ map fromIntegral xs'
        ys = V.fromList $ map (\(S x) -> fromRational . toRational $ x) ys'
    in linearRegressionRSqr xs ys
