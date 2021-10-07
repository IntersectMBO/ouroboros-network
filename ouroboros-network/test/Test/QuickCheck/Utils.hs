
-- | QuickCheck utils
--
module Test.QuickCheck.Utils (

    -- * Generator and shrinker utils
    arbitrarySubset,
    shrinkListElems,
    prop_shrink_valid,
    prop_shrink_nonequal,

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


-- | Check that each shrink satisfies some invariant or validity condition.
--
prop_shrink_valid :: (Arbitrary a, Eq a, Show a)
                  => (a -> Bool) -> Fixed a -> Property
prop_shrink_valid valid (Fixed x) =
    let invalid = [ x' | x' <- shrink x, not (valid x') ]
     in case invalid of
          []     -> property True
          (x':_) -> counterexample ("shrink result invalid:\n" ++ show x') $
                    property False


-- | The 'shrink' function needs to give a valid value that is /smaller/ than
-- the original, otherwise the shrinking procedure is not well-founded and can
-- cycle.
--
-- This property does not check size, as that would need significant extra
-- infrastructure to define an appropriate measure. Instead this property
-- simply checks each shrink is not the same as the original. This catches
-- simple 1-cycles, but not bigger cycles. These are fortunately the most
-- common case, so it is still a useful property in practice.
--
prop_shrink_nonequal :: (Arbitrary a, Eq a) => Fixed a -> Property
prop_shrink_nonequal (Fixed x) =
    counterexample "A shrink result equals as the original.\n" $
    counterexample "This will cause non-termination for shrinking." $
    all (x /=) (shrink x)


-- | Use in 'tabulate' to help summarise data into buckets.
--
renderRanges :: Int -> Int -> String
renderRanges r n = show lower ++ " -- " ++ show upper
  where
    lower = n - n `mod` r
    upper = lower + (r-1)
