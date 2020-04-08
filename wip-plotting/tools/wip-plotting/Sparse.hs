module Sparse (module Sparse) where

import qualified Data.Vector.Unboxed as U

-- | An @n + 1@ by @n + 1@ matrix with a very specific structure
--
-- The first column is unrestricted. Each other column has exactly two non-zero
-- components, and neither can be the first component.
data Sparse2 = MkSparse2
  !(U.Vector Double)
  -- the @n + 1@ summands for the 0th column
  !(U.Vector ((Int, Double), (Int, Double)))
  -- the two summands for i-th non-zero column;
  --
  -- There are @n - 1@ entries and each @Int@ is from @0@ to @n - 1@
  deriving (Eq, Ord, Show)

mkSparse2 ::
    Int {- ^ @n@ -} ->
    (Int -> Double) -> (Int -> ((Int, Double), (Int, Double))) ->
    Sparse2
{-# INLINE mkSparse2 #-}
mkSparse2 n f g = MkSparse2 (U.generate (n + 1) f) (U.generate n g)

vmat :: U.Vector Double -> Sparse2 -> U.Vector Double
{-# INLINE vmat #-}
vmat v (MkSparse2 z m) =
    U.generate (U.length v) $ \i ->
      if 0 == i then U.sum (U.zipWith (*) v z) else
      let ((i1, w1), (i2, w2)) = m U.! (i - 1)
      in
      w1 * (v U.! (i1 + 1)) + w2 * (v U.! (i2 + 1))
