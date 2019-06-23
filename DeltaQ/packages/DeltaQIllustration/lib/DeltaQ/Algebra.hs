{-# LANGUAGE FlexibleContexts #-}
module DeltaQ.Algebra
  ( module DeltaQ.Algebra.Class
  , perfection
  , bottom
  , choose
  , convolve
  , mmmv
  , intangibleMass
  )
where

import DeltaQ.Algebra.Class

-- | No quality attenutation, zero ∆Q.
perfection :: DeltaQ p d n
perfection = Unit

-- | total quality attentuation, unbounded ∆Q.
bottom :: DeltaQ p d n
bottom = Bottom

-- | choice operator, choose the left over the right with a given probability.
choose :: (Show p, Real p)
       => p                     -- ^ probability of choice of left over right
       -> DeltaQ p d n          -- ^ left ∆Q
       -> DeltaQ p d n          -- ^ right ∆Q
       -> DeltaQ p d n
choose p
  | toRational p >= 0 &&
    toRational p <= 1     = ProbChoice p
  | otherwise             = error $ "choose: invalid probability: " ++ show p

-- | convolve operator, convolve two ∆Q's together.
convolve :: DeltaQ p d n -> DeltaQ p d n -> DeltaQ p d n
convolve = Convolve

-- | extract the min, max, mean and variance from DeltaQ
mmmv :: ( Fractional p, Real p, Fractional n, DelayModel d n
        , SimpleStatDesc d n) =>
        DeltaQ p d n -> Maybe (MinMaxMeanVar n)
mmmv = fmap simpleStatDesc . tangibleRandomness
{-# SPECIALIZE mmmv :: (DelayModel d Double, SimpleStatDesc d Double) =>
                        DeltaQ Rational d Double -> Maybe (MinMaxMeanVar Double) #-}

-- | The intangilble probability mass for a given ∆Q
intangibleMass :: (Fractional p, Real p, DelayModel d n) => DeltaQ p d n -> Double
intangibleMass = fromRational . (flip subtract 1) . toRational . tangibleMass
