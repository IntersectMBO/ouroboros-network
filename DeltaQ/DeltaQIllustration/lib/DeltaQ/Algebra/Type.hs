{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module DeltaQ.Algebra.Type
  ( DeltaQ(..)
  , DelayModel(..)
  , δ
  , (⊓)
  , uniform
  , ImproperRandomVar(..)
  , simpleShow
  )
where

import Data.Ratio (numerator, denominator)

import {-# SOURCE #-} DeltaQ.Algebra.Class (ImproperRandomVar(tangibleMass))

-- | In addition to the structual simplifier that is common to all
-- reifications of the general algebra. Each delay sub-model needs to
-- supply a simplifier for that model's specific properties. The
-- default is that no simplification is performed.
class (Num n, Ord n) => DelayModel (d :: * -> *) n where
    simplifyDelay :: Num p => ((DeltaQ p d n) -> (DeltaQ p d n))
                  -> DeltaQ p d n
                  -> DeltaQ p d n
    simplifyDelay _ = id

    fixed    :: n -> DeltaQ p d n
    uniform0 :: n -> DeltaQ p d n

-- | Dirac delta function at time t. Unicode 0x03b4
δ :: (DelayModel d n) => n -> DeltaQ p d n
δ = fixed

-- | Uniform distribution between [0, t]. Unicode 0x2293
(⊓) :: (DelayModel d n) => n -> DeltaQ p d n
(⊓) = uniform0


-- | Uniform distribution between [a,b].
uniform :: (DelayModel d n) => n -> n -> DeltaQ p d n
uniform  a b
  | b < a  = error "uniform: the upper bound has to exceed the lower bound"
  | a == b = δ a
  | a == 0 = (⊓) b
  | otherwise
      =  (δ a) `Convolve` ((⊓) $! b - a)

-- | The basic structural representation of DeltaQ
data DeltaQ p (d :: * -> *) n  where
  Bottom     :: DeltaQ p d n

  Unit       :: DeltaQ p d n

  Delay      :: DelayModel d n =>
                (d n)
             -> DeltaQ p d n

  ProbChoice :: p
             -> DeltaQ p d n
             -> DeltaQ p d n
             -> DeltaQ p d n

  Convolve   :: DeltaQ p d n
             -> DeltaQ p d n
             -> DeltaQ p d n
-- there could be other ways of representing ⇋'s associated
-- probabailites, thats "for future study"
instance (Real p, Show p, Show (d n)) => Show (DeltaQ p d n) where
  showsPrec n dq
    = case dq of
        Bottom           -> showChar '⊥'
        Unit             -> showChar '∅'
        Delay d          -> shows d
        ProbChoice p a b -> let r = toRational p
                            in showsPrec n a . showString " ("
                               .  shows (numerator r) . showChar '⇋'
                               . shows (denominator r - numerator r)
                               . showString ") " . showsPrec n b
        Convolve a b     -> showsPrec n a . showChar ' '
                            . showChar '⊕' . showChar ' '
                            . showsPrec n b

-- | a means of showing DeltaQ where the delay model is not an
-- instance of show
simpleShow :: Real p => DeltaQ p d n -> String
simpleShow dq = f dq ""
    where
      f x = case x of
              Bottom           -> showChar '⊥'
              Unit             -> showChar '∅'
              Delay _          -> showString "<delay>"
              ProbChoice p a b -> let r = toRational p
                                  in f a . showString " ("
                                       . shows (numerator r) . showChar '⇋'
                                       . shows (denominator r - numerator r)
                                       . showString ") " . f b
              Convolve a b     -> f a . showChar ' '
                                  . showChar '⊕' . showChar ' '
                                  . f b


instance (Fractional p, Real p
         , DelayModel d n
         , ImproperRandomVar (DeltaQ p d n)) => Fractional (DeltaQ p d n) where
   (/)   = error "(/) not defined for DeltaQ"
   recip = error "recip not defined for DeltaQ"
   fromRational r
       | r < 0 || r > 1
           = error $ "DeltaQ is a probability, <0 or >1 undefined - given '"
                     ++ show r ++ "'"
       | r == 0 = Bottom
       | r == 1 = Unit
       | otherwise = ProbChoice (fromRational (1- r)) Bottom Unit -- normal form

instance ( Fractional p, Real p, DelayModel d n
         , ImproperRandomVar(DeltaQ p d n)) => Real (DeltaQ p d n) where
    toRational x = case tangibleMass x of
                     Bottom                     -> 0
                     Unit                       -> 1
                     (ProbChoice p Bottom Unit) -> toRational (1 - p)
                     y -> error $ "un-normalised tangibleMass representation '" ++
                             simpleShow y ++ "' - hence toRational is not defined"

instance (Fractional p, Real p
        , DelayModel d n
        , ImproperRandomVar (DeltaQ p d n)) => Eq (DeltaQ p d n) where
  a == b = toRational a == toRational b

instance (Fractional p, Real p
         , DelayModel d n
         , ImproperRandomVar (DeltaQ p d n)) => Ord (DeltaQ p d n) where
  a `compare` b = (toRational a) `compare` (toRational b)

instance (Fractional p, Real p
         , DelayModel d n
         , ImproperRandomVar (DeltaQ p d n)) => Num (DeltaQ p d n) where
  (+) = dq'lift2 (+)
        where
          dq'lift2 f a b = fromRational $ (toRational a) `f` (toRational b)
  (*) = dq'lift2 (*)
        where
          dq'lift2 f a b = fromRational $ (toRational a) `f` (toRational b)
  abs = error "abs not defined for DeltaQ"
  signum = error "signum not defined for DeltaQ"
  negate = error "negate not defined for DeltaQ"
  fromInteger n
      | n == 0 = Bottom
      | n == 1 = Unit
      | otherwise = error "fromInteger is only defined for 0 and 1 for DeltaQ"
  (-) = dq'lift2 (-)
        where
          dq'lift2 f a b = fromRational $ (toRational a) `f` (toRational b)
