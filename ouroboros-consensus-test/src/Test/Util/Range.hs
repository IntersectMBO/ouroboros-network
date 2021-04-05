module Test.Util.Range (
    Range (..)
  , RangeK (..)
  , range
  , rangeK
  ) where

import qualified Data.List as L
import           Data.Word

import           Ouroboros.Consensus.Config.SecurityParam

{-------------------------------------------------------------------------------
  Range of values related to K
-------------------------------------------------------------------------------}

-- | Rough indication of the size of a value in relation to @k@
--
-- Useful for labelling tests.
data RangeK =
    -- | The value was equal to K
    Range_Eq_K SecurityParam

    -- | The value was just below to K
    --
    -- We indicate how far off from K it was
  | Range_Just_Below_K SecurityParam Word64

    -- | The value was just above K
    --
    -- We indicate how far off from K it was
  | Range_Just_Above_K SecurityParam Word64

    -- | The value was almost zero
    --
    -- If there is a choice between 'Range_Just_Below_K' and 'Range_Near_Zero',
    -- the constructor with the smaller argument should be used.
  | Range_Near_Zero SecurityParam Word64

    -- | Less than k (but not near k and not near 0)
    --
    -- We round to the neareast multiple of (k / 10)
  | Range_Less_Than_K SecurityParam Word64

    -- | More than k (but not near k)
    --
    -- We round to the first power of two above k that is equal to above the value.
  | Range_More_Than_K SecurityParam Word64
  deriving (Eq)

instance Show RangeK where
  show r =
      case r of
        Range_Eq_K         (SecurityParam k)   -> "= (k = " ++ show k ++ ")"
        Range_Just_Below_K (SecurityParam k) n -> "= (k = " ++ show k ++ ")" ++ " >  " ++ show (k - n)
        Range_Just_Above_K (SecurityParam k) n -> "= (k = " ++ show k ++ ")" ++ " <  " ++ show (k + n)
        Range_Near_Zero    (SecurityParam k) n -> "= (k = " ++ show k ++ ")" ++ " >> " ++ show n
        Range_Less_Than_K  (SecurityParam k) n -> "≈ (k = " ++ show k ++ ")" ++ " >> " ++ show n
        Range_More_Than_K  (SecurityParam k) n -> "≈ (k = " ++ show k ++ ")" ++ " << " ++ show n

rangeK :: Integral a => SecurityParam -> a -> RangeK
rangeK (SecurityParam k) a
  | n == k    = Range_Eq_K      (SecurityParam k)
  | n < nearK = Range_Near_Zero (SecurityParam k) n
  | n < k     = if belowK <= nearK
                  then Range_Just_Below_K (SecurityParam k) n
                  else Range_Less_Than_K  (SecurityParam k) (n `div` bandSize)
  | otherwise = if aboveK <= nearK
                  then Range_Just_Above_K (SecurityParam k) n
                  else Range_More_Than_K  (SecurityParam k) (head (dropWhile (< n) powers))
  where
    n      = fromIntegral a
    belowK = k - n
    aboveK = n - k
    powers = [k + 2 ^ i | i <- [0..] :: [Int]]

    -- threshold for determining if a value is near k
    nearK = 5

    -- bands for summarizing values less than k
    bandSize = max 1 (k `div` 10)

{-------------------------------------------------------------------------------
  Summarize values not related to K
-------------------------------------------------------------------------------}

data Range n = R_Eq n | R_Btwn (n, n) | R_Gt n
  deriving (Show, Eq)

range :: (Ord n, Show n, Num n) => n -> Range n
range n
  | n > limit       = R_Gt limit
  | n `L.elem` vals = R_Eq n
  | otherwise       =
      case L.find (\(lo, hi) -> lo <= n && n <= hi) rngs of
        Nothing  -> error $ "range: unable to classify " ++ show n
        Just rng -> R_Btwn rng
  where
    vals  = [0, 1, 2, 3, 4]
    rngs  = [(0, 1), (1, 2), (2, 3), (3, 4), (4, 5), (5, 10), (10, 20)]
    limit = 20
