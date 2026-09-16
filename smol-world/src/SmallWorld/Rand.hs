-- | Tiny deterministic hashing used for reproducible pseudo-randomness (RTT
-- fuzz, per-round jitter) without threading a generator or breaking the DES's
-- determinism: everything is a pure function of integer keys.
module SmallWorld.Rand
  ( hashUnit
  , mix
  ) where

import Data.Bits (shiftR, xor)
import Data.List (foldl')
import Data.Word (Word64)

-- | Deterministic pseudo-uniform in [0,1) from an integer key (splitmix64
-- finalizer).
hashUnit :: Int -> Double
hashUnit key =
  let x0 = fromIntegral key + 0x9E3779B97F4A7C15 :: Word64
      x1 = (x0 `xor` (x0 `shiftR` 30)) * 0xBF58476D1CE4E5B9
      x2 = (x1 `xor` (x1 `shiftR` 27)) * 0x94D049BB133111EB
      x3 = x2 `xor` (x2 `shiftR` 31)
  in fromIntegral (x3 `shiftR` 11) / 9007199254740992  -- / 2^53

-- | Fold a list of ints into one hash key (FNV-style); order matters, so callers
-- can distinguish draws by appending a salt.
mix :: [Int] -> Int
mix = foldl' (\a x -> (a * 1000003) `xor` x) 2166136261
