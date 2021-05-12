
-- | QuickCheck utils
--
module Test.QuickCheck.Utils (

    -- * Generator and shrinker utils
    arbitrarySubset,
    shrinkListElems,

    -- * Reporting utils
    renderRanges,

  ) where

import           Data.Set (Set)
import qualified Data.Set as Set

import           Test.QuickCheck

-- | Pick a subset of a set, using a 50:50 chance for each set element.
--
arbitrarySubset :: Ord a => Set a -> Gen (Set a)
arbitrarySubset s = do
    picks <- vectorOf (Set.size s) (arbitrary :: Gen Bool)
    let s' = Set.fromList
           . map snd
           . filter fst
           . zip picks
           . Set.toList
           $ s
    return s'


-- | Like 'shrinkList' but only shrink the elems, don't drop elements.
--
-- Useful when you want a custom strategy for dropping elements.
--
shrinkListElems :: (a -> [a]) -> [a] -> [[a]]
shrinkListElems _   []     = []
shrinkListElems shr (x:xs) = [ x':xs | x'  <- shr x ]
                          ++ [ x:xs' | xs' <- shrinkListElems shr xs ]


-- | Use in 'tabulate' to help summarise data into buckets.
--
renderRanges :: Int -> Int -> String
renderRanges r n = show lower ++ " -- " ++ show upper
  where
    lower = n - n `mod` r
    upper = lower + (r-1)
