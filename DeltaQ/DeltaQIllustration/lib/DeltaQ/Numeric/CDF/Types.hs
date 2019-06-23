{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module DeltaQ.Numeric.CDF.Types
where

import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Default
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Numeric.IEEE
import           Text.Show.Functions ()

-- | An empirical CDF for improper random variable.
data EmpiricalCDF = EmpiricalCDF
  { _ecdf         :: !(Double -> Rational)
    -- ^ The empirical CDF, may not reach 1. It is undefined if the
    --   `ecdfMass` is zero.
  , _ecdfInverse  :: !(Rational -> Maybe Double)
    -- ^ Returns values over [0,`ecdfMass`] and `Nothing` between
    --   (`ecdMass`, 1]. The inverse of the above.
  , _ecdfMass     :: !Rational
    -- ^ The tangible propabilty mass.
  , _ecdfSamples  :: !Int
    -- ^ The total number of samples taken for the construction of
    --   `EmpiricalCDF`.
  , _ecdfMin      :: !Double
    -- ^ The minimum sampled value. Value is +∞ if `ecdfMass` is zero.
  , _ecdfMax      :: !Double
    -- ^ The minimum sampled value. Value is -∞ if `ecdfMass` is zero.
  , _ecdfMean     :: !Double
    -- ^ The mean of the sampled tangible distribution. Zero if
    --   `ecdfMass` is zero.
  , _ecdfVariance :: !Double
    -- ^ The variance of the sampled tangible distribution. Is NaN if
    --   the number samples is two or less.
  }
    deriving Show
makeLenses ''EmpiricalCDF

-- | Given a list of IRV samples construct an empirical improper CDF
--   along with some additional statistics.
makeEmpiricalCDF :: [Maybe Double] -> EmpiricalCDF
makeEmpiricalCDF ys = run (start >> step ys >> finalise)
  where
    -- execute the evalation loop
    run  = (flip evalState) def
    -- set some more suitable default values
    start = _4 .= infinity >> _5 .= (negate infinity)
    -- loop over the input stream
    step [] = return ()
    step (Nothing:zs)
      = _2 += 1 >> step zs -- just increment the total count
    step ((Just z):zs) = do
      -- insert in the value to occurance map
      _1 %= M.insertWith (+) z 1
      -- increment the total count
      _2 += 1
      -- increment the tangible mass count
      _3 += (1 :: Int)
      -- update the minimum value
      _4 %= min z
      -- update the maximum value
      _5 %= max z
      -- step the online mean and variance algorithm
      do n      <- fmap (fromRational . toRational) $ use _3
         delta  <- fmap (z -) $ use _6
         _6     += delta / n
         delta2 <- fmap (z -) $ use _6
         _7     += delta * delta2
      -- and loop
      step zs
    finalise = do
      (!m',!n,!t,!l,!u,!a,!b) <- get
          -- finalise the variance
      let v | t > 2     = b / fromIntegral (t - 1)
            | otherwise = nan
          -- normalise the occurance map into cumulative probability
          m = normalise n m'
          -- consruct the inverse map
          i = M.fromList . map (\(x,y) -> (y,x)) . M.toAscList $ m
          -- lookup the total tangible probablity mass
          p = M.findWithDefault 0 u m
          -- construct a continuous iCDF from the occurance map
          f x | M.null m  = error "makeEmpiricalCDF: no tangible mass"
              | x <= l    = 0
              | x >= u    = p
              | otherwise = snd . fromJust $ M.lookupLE x m
          -- construct the inverse iCDF defined over the range of the
          -- tangible mass
          g x | M.null i  = error "makeEmpiricalCDF: no tangible mass"
              | x <  0    = error "makeEmpiricalCDF: negative probability"
              | x >  1    = error "makeEmpiricalCDF: > unit probability"
              | x >  p    = Nothing
              | otherwise = fmap snd $  M.lookupLE x i
      return $ i `seq` EmpiricalCDF f g p n l u a v
    normalise :: Int -> M.Map Double Int -> M.Map Double Rational
    normalise n
      = snd
      . M.mapAccum (\a b -> let c = a + toRational b in (c, c / toRational n)) 0
