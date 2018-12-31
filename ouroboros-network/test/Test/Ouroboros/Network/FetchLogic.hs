{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeOperators    #-}

module Test.FetchLogic (tests) where

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Test.Chain hiding (tests)

import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain (Chain (..), Point (..))
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.Testing.ConcreteBlock


--
-- The list of all tests
--

tests :: TestTree
tests = testGroup "FetchLogic"
  [ testGroup "generators"
    [ testProperty "arbitrary for TestChainAndRanges" prop_arbitrary_TestChainAndRanges
    , testProperty "shrink for TestChainAndRanges"    prop_shrink_TestChainAndRanges
    ]

  , testProperty "blockServer" prop_blockServer
  ]

--
-- Properties
--

prop_blockServer :: TestChainAndRanges -> Bool
prop_blockServer _ = False

--
-- Generator for chains and ranges on the chain
--

-- | A test generator for a chain and a range defined by a pair of points.
-- In most cases the range is on the chain, but it also covers at least 5% of
-- cases where the point is not on the chain.
--
data TestChainAndRanges = TestChainAndRanges (Chain Block) [(Point Block, Point Block)]
  deriving Show

instance Arbitrary TestChainAndRanges where
  arbitrary = do
    TestBlockChain chain <- arbitrary
    Positive n <- arbitrary
    ranges <- vectorOf n $
              frequency [ (10, genRangeOnChain chain)
                        , (1, (,) <$> genPoint <*> genPoint) ]
    return (TestChainAndRanges chain ranges)

  shrink (TestChainAndRanges c rs) =
    [ TestChainAndRanges c' (fixupRanges c' rs)
    | TestBlockChain c' <- shrink (TestBlockChain c)]

fixupRange :: HasHeader block
           => Chain block
           -> (Point block, Point block)
           -> (Point block, Point block)
fixupRange c (p1, p2) = (fixupPoint c p1, fixupPoint c p2)

fixupRanges :: HasHeader block
            => Chain block
            -> [(Point block, Point block)]
            -> [(Point block, Point block)]
fixupRanges chain = map (fixupRange chain)

prop_arbitrary_TestChainAndRanges :: TestChainAndRanges -> Bool
prop_arbitrary_TestChainAndRanges (TestChainAndRanges c rs) =
    Chain.valid c
 && and [ let onChain = Chain.pointOnChain p1 c && Chain.pointOnChain p2 c in
          onChain `implies` pointSlot p2 >= pointSlot p1
        | (p1, p2) <- rs ]

prop_shrink_TestChainAndRanges :: TestChainAndRanges -> Bool
prop_shrink_TestChainAndRanges crs@(TestChainAndRanges c _) =
  and [    Chain.valid c'
        && and [ Chain.pointOnChain p1 c && Chain.pointOnChain p2 c
                 `implies`
                 Chain.pointOnChain p1 c' && Chain.pointOnChain p2 c'
               | (p1, p2) <- rs ]
      | TestChainAndRanges c' rs <- shrink crs ]

implies :: Bool -> Bool -> Bool
a `implies` b = not a || b

infix 1 `implies`

