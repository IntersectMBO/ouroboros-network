{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- This is an algebraic model for ∆Q - quality attenutation.  It is
-- parameterised in the both the propability and delay model so that
-- various instantiations of such can be accomodated within the same
-- framework.

module DeltaQ.Algebra.Class
  ( DeltaQ (..)
  , DelayModel(..)
  , ImproperRandomVar(..)
  , SimpleStatDesc(..)
  , MinMaxMeanVar(..)
  , (⊥)
  , (∅)
  , (⇋)
  , (⊕)
  , δ
  , (⊓)
  , uniform
  , simpleShow
  )
where

import DeltaQ.Algebra.Type
import DeltaQ.Algebra.Simplification

-- | Improper random variables are ones whose CDFs tend to 1 - epsilon
-- as x tends to infininty.
--
-- following invariants should hold
--  @ tangibleMass x <= 1@
--  @ tangibleMass (tangibleRandomess x) == 1@
--
class ImproperRandomVar m where
    tangibleMass       :: m -> m -- ^ <= unit probability. 1 - epsilon
    tangibleRandomness :: m -> Maybe m -- ^ a proper random variable
                                       -- of unit probability mass, if
                                       -- there is any tangible
                                       -- randomness

-- | instances of ∆Q, being improper random variables in themseleves,
-- can also support the following operations.
class SimpleStatDesc (a :: * -> *) n where
   simpleStatDesc :: a n -> MinMaxMeanVar n

-- | Divergence, unbounded delay, total loss. Unicode \u22a5 - maximum ∆Q
(⊥) :: DeltaQ p d n
(⊥) = Bottom

-- | Unit in the algebra, no quality attenutation. Unicode \u2205 - "empty" ∆Q
(∅) :: DeltaQ p d n
(∅) = Unit

-- | Probabalistic weighted choice. Unicode \u21cb, chooses the first
-- ∆Q with probability p. Shown as a ratio of choices between the two arms.
(⇋) :: p -> DeltaQ p d n -> DeltaQ p d n-> DeltaQ p d n
(⇋) = ProbChoice

-- | Convolution of two ∆(\u2206)Q's. Unicode \u2295
(⊕) :: DeltaQ p d n -> DeltaQ p d n -> DeltaQ p d n
(⊕) = Convolve



instance (Fractional p, Real p, DelayModel d n) =>
    ImproperRandomVar (DeltaQ p d n) where
    tangibleMass Bottom             = Bottom
    tangibleMass Unit               = Unit
    tangibleMass (Delay _)          = Unit
    tangibleMass (ProbChoice p a b)
        = fromRational $ (toRational p) * (toRational $ tangibleMass a)
          + (1-(toRational p)) * (toRational $ tangibleMass b)
    tangibleMass (Convolve a b)
        = tangibleMass a * tangibleMass b

    tangibleRandomness Bottom    = Nothing
    tangibleRandomness Unit      = Just Unit
    tangibleRandomness (Delay d) = Just (Delay d)
    tangibleRandomness (ProbChoice p a b)
        = case canonicaliseDeltaQ (ProbChoice p (norm a) (norm b)) of
            (ProbChoice _ Bottom Bottom) -> Nothing
            (ProbChoice _ Bottom y)      -> Just y
            (ProbChoice _ x Bottom)      -> Just x
            x                            -> Just x
          where
            norm = maybe Bottom canonicaliseDeltaQ . tangibleRandomness
    tangibleRandomness (Convolve a b)
        = case canonicaliseDeltaQ (Convolve (norm a) (norm b)) of
            (Convolve Bottom _) -> Nothing
            (Convolve _ Bottom) -> Nothing
            x                   -> Just x
          where
            norm = maybe Bottom canonicaliseDeltaQ . tangibleRandomness


-- | The output from `simpleStatDesc` - the min, max, mean and
-- variance of the underlying delay model (which is a proper random
-- variable).
data MinMaxMeanVar a = MMMV {
      mmmvMin    :: a
    , mmmvMax    :: a
    , mmmvMean   :: a
    , mmmvVar    :: a
    } deriving (Show)

mmmvChoice :: (Fractional p, Real p, Ord a, Fractional a) =>
              p -> MinMaxMeanVar a -> MinMaxMeanVar a
           -> MinMaxMeanVar a
mmmvChoice p a b
    = MMMV { mmmvMin  = (mmmvMin a) `min` (mmmvMin b)
           , mmmvMax  = (mmmvMax a) `max` (mmmvMax b)
           , mmmvMean = (tfr p) * (mmmvMean a) + (tfr $ 1-p) * (mmmvMean b)
           , mmmvVar  = (tfr p) * (mmmvVar a) + (tfr $ 1-p) * (mmmvVar b)
           }
      where
        tfr = fromRational . toRational

mmmvConv :: (Num a) =>
              MinMaxMeanVar a -> MinMaxMeanVar a
           -> MinMaxMeanVar a
mmmvConv a b
    = MMMV { mmmvMin  = (mmmvMin a) + (mmmvMin b)
           , mmmvMax  = (mmmvMax a) + (mmmvMax b)
           , mmmvMean = (mmmvMean a) + (mmmvMean b)
           , mmmvVar  = (mmmvVar a) + (mmmvVar b)
           }

instance (Fractional p, Real p
         , Num n, Ord n, Fractional n
         , SimpleStatDesc d n) => SimpleStatDesc (DeltaQ p d) n where
    simpleStatDesc Unit
        = MMMV 0 0 0 0
    simpleStatDesc Bottom
        = error "simpleStatDesc: unefined for '⊥'"
    simpleStatDesc (Delay x)
        = simpleStatDesc x
    simpleStatDesc (ProbChoice p a b)
        = mmmvChoice p (simpleStatDesc a) (simpleStatDesc b)
    simpleStatDesc (Convolve a b)
        = mmmvConv (simpleStatDesc a) (simpleStatDesc b)
