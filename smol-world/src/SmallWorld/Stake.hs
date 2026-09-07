-- | Heavy-tailed (log-normal) stake distribution, a stand-in for the skewed
-- Cardano mainnet stake.  Larger sigma => heavier tail / more concentration.
module SmallWorld.Stake (lognormalStakes) where

import System.Random (RandomGen, randomR)

-- | One standard normal via Box–Muller (uses the cosine branch; discards the
-- paired sine value — fine for our purposes).
boxMuller :: RandomGen g => g -> (Double, g)
boxMuller g0 =
  let (u1, g1) = randomR (1e-12, 1.0) g0   -- clamp away from 0 for log
      (u2, g2) = randomR (0.0, 1.0) g1
  in (sqrt (-2 * log u1) * cos (2 * pi * u2), g2)

-- | @lognormalStakes n sigma g@ draws @n@ log-normal weights with underlying
-- normal dispersion @sigma@ and normalizes them to sum to 1.
lognormalStakes :: RandomGen g => Int -> Double -> g -> ([Double], g)
lognormalStakes n sigma g0 =
  let go 0 g acc = (acc, g)
      go k g acc = let (z, g') = boxMuller g
                   in go (k - 1) g' (exp (sigma * z) : acc)
      (raw, g1) = go n g0 []
      total     = sum raw
  in (map (/ total) raw, g1)
