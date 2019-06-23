{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module DeltaQ.Algebra.DelayModel.SimpleUniform
  ( SimpleUniform (..)
  )
where

import DeltaQ.Algebra.Class

-- | the working assumption here is that a, more ususal,
-- representation of the uniform distribution (Uniform[a,b]) can be
-- expressed as a Dirac δ convolved with a ⊓ (Uniform) - namely δ(a) ⊕
-- ⊓(b-a).
--
-- $proof

data SimpleUniform t where
    DiracDelta :: (Num t, Ord t) => t
                -> SimpleUniform t
    UniformD    :: t -- uniform distribution [0, x]
                -> SimpleUniform t

instance (Num t, Ord t) => DelayModel SimpleUniform t where
    simplifyDelay = simplifyC
    fixed         = fixedDelay
    uniform0      = uniform0Delay

-- | Dirac delta function at time t. Unicode \u03b4
fixedDelay :: (Num t, Ord t) => t -> DeltaQ p SimpleUniform t
fixedDelay x
  | x == 0 = Unit
  | x <  0 = error "δ: Can't have a delay of < 0"
  | otherwise = Delay . DiracDelta $ x
{-# SPECIALISE  fixedDelay :: Rational -> DeltaQ Rational SimpleUniform Rational #-}
{-# SPECIALISE  fixedDelay :: Double -> DeltaQ Rational SimpleUniform Double #-}


-- | Uniform distribution between [0, t]. Unicode \u2293
uniform0Delay :: (Ord t, Num t) => t -> DeltaQ p SimpleUniform t
uniform0Delay x
  | x == 0 = Unit
  | x <  0 = error "⊓: Can't have a delay of < 0"
  | otherwise = Delay . UniformD  $ x
{-# SPECIALISE  uniform0Delay :: Rational -> DeltaQ Rational SimpleUniform Rational #-}
{-# SPECIALISE  uniform0Delay :: Double -> DeltaQ Rational SimpleUniform Double #-}


instance (Show t) => Show (SimpleUniform t) where
    showsPrec n dl
        = case dl of
            DiracDelta t     -> showParen (n > 10) $ showString "δ" . showChar '⎣'
                                . shows t . showChar '⎤'
            UniformD   d     -> showParen (n > 10) $ showString "⊓[0,"
                                . shows d . showChar ']'

-- $proof
-- We assert that min, max, mean and variance properties of
-- @Uniform[a,b]@ remain the same under the transformation to
-- @δ(a)⊕⊓(b-a)@.
--
-- Noting that means and variances both sum under convolution.
--
-- properties of δ:
--       min:  δ(x) = x
--       max:  δ(x) = x
--       mean: δ(x) = x
--       var:  δ(x) = 0
--
-- properties of ⊓:
--       min:  ⊓(x) = 0
--       max:  ⊓(x) = x
--       mean: ⊓(x) = x/2
--       var:  ⊓(x) = (x^2)/12
--
-- properies under ⊕:
--       min(x)  ⊕ min(y)  = min(x)  + min(y)
--       max(x)  ⊕ max(y)  = max(x)  + max(y)
--       mean(x) ⊕ mean(y) = mean(x) + mean(y)
--       var(x)  ⊕ var(y)  = var(x)  + var(y)
--          assuming co-variance is zero, i.e. independent
--
-- properties of δ(a)⊕⊓(b-a) - compared with Uniform[a,b]
--       min(δ(a)⊕⊓(b-a))  = a + 0          = a          = min(Uniform[a,b])
--       max(δ(a)⊕⊓(b-a))  = a + (b-a)      = b          = max(Uniform[a,b])
--       mean(δ(a)⊕⊓(b-a)) = a + (b-a)/2    = (a+b)/2    = mean(Uniform[a,b])
--       var(δ(a)⊕⊓(b-a))  = 0 + (b-a)^2/12 = (b-a)^2/12 = var(Uniform[a,b])
--
instance (Num t, Fractional t) => SimpleStatDesc SimpleUniform t where
    simpleStatDesc (DiracDelta t) = MMMV t t t 0
    simpleStatDesc (UniformD   t) = MMMV 0 t (t/2) ((t*t)/12)

-- simplification brings dirac deltas to the left, combining where
-- possible for convolution, also simplifies probbalistics choice

-- only a few are done here at present - plenty of scope for more

simplifyC :: (Num p) =>
             (DeltaQ p SimpleUniform t
                  -> DeltaQ p SimpleUniform t)
          -> DeltaQ p SimpleUniform t
          -> DeltaQ p SimpleUniform t
simplifyC  _ x@(Delay (DiracDelta a))
    | a <= 0    = Unit
    | otherwise = x

simplifyC _ (Convolve (Delay (DiracDelta a)) (Delay (DiracDelta b)))
    = Delay . DiracDelta $! a + b

simplifyC norm (Convolve x@(Delay _) y@(Delay (DiracDelta _)))
    = simplifyC norm $ Convolve y x

simplifyC norm (Convolve (Delay (DiracDelta a))
                            (Convolve (Delay (DiracDelta b)) y))
    = simplifyC norm $ Convolve (Delay . DiracDelta $! a + b) y

simplifyC norm (Convolve x@(Delay _) y)
  = Convolve x (norm y)

simplifyC _norm z@(ProbChoice _p _x@(Delay (DiracDelta _a)) _y@(Delay (DiracDelta _b)))
--    | a == b = Delay . DiracDelta $ a
--    | a < b  = Convolve (Delay a) (ProbChoice p Unit (Delay . DiracDelta $! b - a))
--    | a > b  = simplifyC norm $ ProbChoice (1-p) y x
   = z -- more needs to be done


simplifyC _ x = x




