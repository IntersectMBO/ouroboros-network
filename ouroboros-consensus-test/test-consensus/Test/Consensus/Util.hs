module Test.Consensus.Util (tests) where

import           Ouroboros.Consensus.Util (chunks)
import           Test.Tasty
import           Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "Utils" [
    testGroup "chunks" [
        testProperty "chunks have at most size n" prop_chunksHaveAtMostSizeN
                       ]
    ]


prop_chunksHaveAtMostSizeN :: Positive Int -> [Int] -> Property
prop_chunksHaveAtMostSizeN (Positive n) xs = property $ length chunked <= length xs
  where chunked = chunks n xs

