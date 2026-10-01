{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}

module Test.Ouroboros.Network.Diffusion.PoolAllowances (tests) where

import Control.Monad.Class.MonadTime.SI (Time (..))
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

import Ouroboros.Network.Diffusion.PoolAllowances

tests :: TestTree
tests =
  testGroup "Ouroboros.Network.Diffusion.PoolAllowances"
    [ testProperty "agrees with the token bucket after every step" prop_oracle
    , testProperty "credit within bounds and monotone in fresh"     prop_bounds
    , testProperty "nothing charged at zero credit"                  prop_zero
    , testProperty "a reset bucket reads the capacity"               prop_reset
    , testProperty "the clock reading starts at zero and grows"      prop_clock
    ]

-- | What happens to a bucket: fresh bytes are announced, or bytes are served.
data Op = Advance Int | Charge Int
  deriving Show

data Run = Run {
    rAllowance :: Allowance,
    rOps       :: [Op]
  }
  deriving Show

instance Arbitrary Run where
  arbitrary = do
    fmax <- choose (1_000, 20_000_000)
    n    <- choose (0, 200)
    ops  <- vectorOf n $ frequency
              [ (3, Advance <$> choose (1, fmax))        -- up to one object at a time
              , (3, Charge  <$> choose (1, 2 * fmax)) ]  -- up to a whole capacity at a time
    return Run { rAllowance = Allowance fmax, rOps = ops }
  shrink Run { rAllowance = Allowance fmax, rOps } =
       [ Run (Allowance fmax) ops' | ops' <- shrinkList shrinkOp rOps ]
    ++ [ Run (Allowance f) rOps    | f <- shrink fmax, f >= 1 ]
    where
      shrinkOp (Advance d) = [ Advance d' | d' <- shrink d, d' >= 1 ]
      shrinkOp (Charge n)  = [ Charge n'  | n' <- shrink n, n' >= 1 ]

-- | The oracle: an explicit token bucket. Tokens start full, an advance adds
-- κ of the bytes capped at the capacity, a charge subtracts while tokens are
-- positive and may run them into debt, and the credit shown is the tokens
-- floored at zero. Refills follow κ of the cumulative reading so the rounding
-- is the implementation's.
oracle :: Allowance -> [Op] -> [Int]
oracle al = go (capacity al) (Fresh 0)
  where
    go _ _ [] = []
    go tokens fresh (op : ops) =
      case op of
           Advance d ->
             let fresh'  = Fresh (unFresh fresh + d)
                 tokens' = min (capacity al) (tokens + kappa fresh' - kappa fresh)
             in max 0 tokens' : go tokens' fresh' ops
           Charge n ->
             let tokens' = if tokens > 0 then tokens - n else tokens
             in max 0 tokens' : go tokens' fresh ops

-- | The implementation, stepped the same way.
impl :: Allowance -> [Op] -> [Int]
impl al = go (Fresh 0) (resetCharged al (Fresh 0))
  where
    go _ _ [] = []
    go fresh ch (op : ops) =
      case op of
           Advance d ->
             let fresh' = Fresh (unFresh fresh + d)
             in credit al fresh' ch : go fresh' ch ops
           Charge n ->
             let ch' = charge al fresh n ch
             in credit al fresh ch' : go fresh ch' ops

prop_oracle :: Run -> Property
prop_oracle Run { rAllowance, rOps } =
  let got = impl rAllowance rOps
      zeros = length (filter (== 0) got)
  in classify (zeros > 0) "runs dry at some point"
   . classify (zeros > length got `div` 2) "dry more than half the time"
   . classify (any (== capacity rAllowance) got) "reads the capacity at some point"
   $ got === oracle rAllowance rOps

-- | A reading and a charged total, unrelated: any point of the state space.
data Point = Point Allowance Fresh Charged
  deriving Show

instance Arbitrary Point where
  arbitrary = do
    fmax <- choose (1_000, 20_000_000)
    f    <- choose (0, 100 * fmax)
    c    <- choose (-10 * fmax, 200 * fmax)
    return (Point (Allowance fmax) (Fresh f) (Charged c))
  shrink (Point (Allowance fmax) (Fresh f) (Charged c)) =
       [ Point (Allowance fmax) (Fresh f') (Charged c) | f' <- shrink f, f' >= 0 ]
    ++ [ Point (Allowance fmax) (Fresh f) (Charged c') | c' <- shrink c ]

prop_bounds :: Point -> NonNegative Int -> Property
prop_bounds (Point al fresh ch) (NonNegative d) =
  let now   = credit al fresh ch
      later = credit al (Fresh (unFresh fresh + d)) ch
  in counterexample (show (now, later))
   $ now >= 0 .&&. now <= capacity al .&&. later >= now

prop_zero :: Point -> Positive Int -> Positive Int -> Property
prop_zero (Point al fresh _) (Positive k) (Positive n) =
  let ch = Charged (kappa fresh + k)          -- past what was granted: dry
  in credit al fresh ch === 0 .&&. charge al fresh n ch === ch

prop_reset :: Point -> Property
prop_reset (Point al fresh _) =
  credit al fresh (resetCharged al fresh) === capacity al

prop_clock :: Positive Double -> NonNegative Double -> NonNegative Double -> Property
prop_clock (Positive rate) (NonNegative t0) (NonNegative dt) =
  let origin = Time (realToFrac t0)
      later  = Time (realToFrac (t0 + dt))
  in freshAt rate origin origin === Fresh 0 .&&. freshAt rate origin later >= Fresh 0
     .&&. freshAt rate origin later >= freshAt rate origin origin
