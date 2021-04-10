
-- | QuickCheck utils
--
module Test.QuickCheck.Utils (

    -- * Generator and shrinker utils
    shrinkListElems,

    -- * Reporting utils
    renderRanges,

  ) where


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
