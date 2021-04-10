
-- | QuickCheck utils
--
module Test.QuickCheck.Utils (

    -- * Reporting utils
    renderRanges,

  ) where


-- | Use in 'tabulate' to help summarise data into buckets.
--
renderRanges :: Int -> Int -> String
renderRanges r n = show lower ++ " -- " ++ show upper
  where
    lower = n - n `mod` r
    upper = lower + (r-1)
