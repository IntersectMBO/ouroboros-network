module Ouroboros.Consensus.Util.Tests (tests) where

import           Ouroboros.Consensus.Util

import           Test.Tasty
import           Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "Ouroboros.Consensus.Util" $
    [ testProperty "split" prop_split
    ]

prop_split :: Fun Int Bool -> [Int] -> Property
prop_split (Fun _ p) as =
    concat (split p as) === filter (not . p) as .&&.
    length (split p as) === length (filter p as) + 1
