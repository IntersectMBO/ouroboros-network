module DeltaQ.Algebra.Simplification
where

import DeltaQ.Algebra.Type


{- Normal form
  * promotes ⊥ to up and to the left
  * demotes ∅ down and to the right
  in any expression.
-}

canonicaliseDeltaQ:: (Ord p,Fractional p, DelayModel d n)
                  => DeltaQ p d n -> DeltaQ p d n

-- eliminate redundant choices when both terms are ⊥ or ∅
canonicaliseDeltaQ (ProbChoice _ Bottom Bottom)
  = Bottom
canonicaliseDeltaQ (ProbChoice _ Unit Unit)
  = Unit

-- eliminate redundant branching if probability is 0 or 1.  use of <=
-- and >= is to permit (however unwisely) Real versions of probability
canonicaliseDeltaQ (ProbChoice prob a b)
  | prob <= 0  = canonicaliseDeltaQ b -- a has no influence
  | prob >= 1  = canonicaliseDeltaQ a -- b has no influence

-- normalise the position of ⊥ and ∅
canonicaliseDeltaQ (ProbChoice p a Bottom)
  = let a' = canonicaliseDeltaQ a
    in canonicaliseDeltaQ $ ProbChoice (1-p) Bottom a'
canonicaliseDeltaQ (ProbChoice p Unit b)
  = let b' = canonicaliseDeltaQ b
    in canonicaliseDeltaQ $ ProbChoice (1-p) b' Unit

-- structural re-writes for ProbChoice
-- ⊥ concatenation
canonicaliseDeltaQ (ProbChoice p Bottom (ProbChoice q Bottom x))
  = let x' = canonicaliseDeltaQ x
    in canonicaliseDeltaQ $ ProbChoice ( p + (1-p) * q) Bottom x'
--
-- visualisation of ⊥ concatenation:
--       A                                                 A
--   (p)/ \(1-p)     noting that               (p+(1-p)*q)/ \((1-p)*(1-q))
--     ⊥   B                       becomes               ⊥  x
--     (q)/ \(1-q)   the ⊥ branch
--       ⊥   x       B probability             and that 1 - (p+(1-p*q) = (1-p)*(1-q)
--                   is (1-p)*q

-- ∅ demotion
canonicaliseDeltaQ (ProbChoice q (ProbChoice p x Unit) y)
    = let x' = canonicaliseDeltaQ x
          y' = canonicaliseDeltaQ y
      in canonicaliseDeltaQ $ ProbChoice (p*q) x' (ProbChoice ((1 - q)/(1 - p*q)) y' Unit)

-- operational identities for ⊕
canonicaliseDeltaQ (Convolve Bottom _)
  = Bottom
canonicaliseDeltaQ (Convolve _ Bottom)
  = Bottom
canonicaliseDeltaQ (Convolve Unit y)
  = canonicaliseDeltaQ y
canonicaliseDeltaQ (Convolve x Unit)
  = canonicaliseDeltaQ x

-- ⊥ promotion
canonicaliseDeltaQ (Convolve (ProbChoice p Bottom x) y)
  = canonicaliseDeltaQ $ ProbChoice p Bottom (Convolve x y)
canonicaliseDeltaQ (Convolve x (ProbChoice p Bottom y))
  = canonicaliseDeltaQ $ ProbChoice p Bottom (Convolve x y)

-- ∅ elimination
canonicaliseDeltaQ (Convolve (ProbChoice p x Unit) y)
  = canonicaliseDeltaQ $ ProbChoice p (Convolve x y) y
canonicaliseDeltaQ (Convolve x (ProbChoice p y Unit))
  = canonicaliseDeltaQ $ ProbChoice p (Convolve x y) x

-- delay model simplification
canonicaliseDeltaQ x@(Convolve (Delay _) (Delay _))
 = simplifyDelay canonicaliseDeltaQ x

canonicaliseDeltaQ x = x
