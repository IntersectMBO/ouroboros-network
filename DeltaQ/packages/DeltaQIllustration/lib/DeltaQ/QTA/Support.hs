module DeltaQ.QTA.Support
where

import DeltaQ.Algebra

import Data.Foldable (foldl')

-- | An outcome's performance objective (an intent) is a strictly
--   monotonic sequence of (probability, delay) pairs. Given such a
--   sequence `fromQTA` returns the equivalent ∆Q.
fromQTA :: (DelayModel d n, Real p, Fractional p)
        => [(p, n)] -> DeltaQ p d n
fromQTA qs
  = check qs `seq` convert
  where
    convert
      = f qs
      where
        f xs
         | r'l > 0
           =   (⊥) `p'alt` (g (0, 1 - r'l) xs)
         | otherwise
           = g (0,1) xs
         where
           p'alt = (⇋) $ fromRational r'l
           r'l = toRational $ 1 - (fst . last $ qs)
        g _ [(_,v)]
          = δ v
        g (a,b) ((w0,v0):xs')
          = let w'    = (toRational w0 - a) / b
                p'alt = (⇋) $ fromRational w'
            in  (δ v0) `p'alt` (g (toRational w0, b * (1 - w')) xs')
        g _ _ = error "fromQTA: internal error (convert)"

    check [] = error "fromQTA: empty intent"
    check xs = feasible xs `seq` monotonic xs

    feasible = foldl' f'check ()
      where
        f'check () (a,b)
          | a >= 0 && a <= 1 && b >= 0
            = ()
          | otherwise
            = error "fromQTA: infeasible intent"

    monotonic [_] = ()
    monotonic ((x0,x1):ys@((y0,y1):_))
      | y0 > x0 && y1 > x1 = monotonic ys
      | otherwise = error "fromQTA: intent not strictly monotonic"
    monotonic _ = error "fromQTA: internal error (monotonic)"

