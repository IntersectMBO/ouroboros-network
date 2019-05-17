{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Arbitrary generators for chains, headers and blocks
--
module Test.ChainGenerators
  ( -- * Arbitrary Point BlockBody and BlockHeader
    -- These instances are useful for testing codec.
    ArbitraryPoint (..)
  , ArbitraryChainRange (..)
  , ArbitraryBlockBody (..)
  , ArbitraryBlockHeader (..)

    -- * Arbitrary chains generators
    -- These generators are used to test various scenarios that require
    -- a chain: e.g. appending a block to chain, arbitrary updates
    -- (rollforwards \/ backwards), chain forks.
  , TestAddBlock (..)
  , TestBlockChainAndUpdates (..)
  , TestBlockChain (..)
  , TestHeaderChain (..)
  , TestChainAndPoint (..)
  , TestChainAndRange (..)
  , TestChainAndPoints (..)
  , TestChainFork (..)

    -- * Utility functions
  , genNonNegative
  , genSlotGap
  , addSlotGap
  , genHeaderChain
  , mkPartialBlock
  , mkRollbackPoint
  , genPoint

    -- * Tests of the generators
  , tests
  )
  where

import qualified Data.List as L
import           Data.Maybe (fromJust, catMaybes, listToMaybe)
import qualified Data.ByteString.Char8 as BSC

import           Ouroboros.Network.Testing.ConcreteBlock
import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain (Chain (..))
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.Protocol.BlockFetch.Type (ChainRange (..))

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)


--
-- The tests for the generators themselves
--

tests :: TestTree
tests = testGroup "Chain"
  [ testProperty "arbitrary for TestBlockChain" $
    -- It's important we don't generate too many trivial test cases here
    -- so check the coverage to enforce it.
                 checkCoverage prop_arbitrary_TestBlockChain
  , testProperty "shrink for TestBlockChain"     prop_shrink_TestBlockChain

  , testProperty "arbitrary for TestHeaderChain" prop_arbitrary_TestHeaderChain
  , testProperty "shrink for TestHeaderChain"    prop_shrink_TestHeaderChain

  , testProperty "arbitrary for TestAddBlock" prop_arbitrary_TestAddBlock
  , testProperty "shrink for TestAddBlock"    prop_shrink_TestAddBlock

  , testProperty "arbitrary for TestBlockChainAndUpdates" $
    -- Same deal here applies here with generating trivial test cases.
                 checkCoverage prop_arbitrary_TestBlockChainAndUpdates

  , testProperty "arbitrary for TestChainAndPoint" $
                                     checkCoverage prop_arbitrary_TestChainAndPoint
  , testProperty "shrink for TestChainAndPoint"    prop_shrink_TestChainAndPoint

  , testProperty "arbitrary for TestChainAndRange" $
                                     checkCoverage prop_arbitrary_TestChainAndRange
  , testProperty "shrink for TestChainAndRange"    prop_shrink_TestChainAndRange

  , testProperty "arbitrary for TestChainAndPoints" $
                                      checkCoverage prop_arbitrary_TestChainAndPoints
  , testProperty "shrink for TestChainAndPoints"    prop_shrink_TestChainAndPoints

  , testProperty "arbitrary for TestChainFork" prop_arbitrary_TestChainFork
  , testProperty "shrink for TestChainFork"
                             (mapSize (min 40) prop_shrink_TestChainFork)
  ]


newtype ArbitraryPoint = ArbitraryPoint {
    getArbitraryPoint :: Point BlockHeader
  }
  deriving (Show, Eq)

instance Arbitrary ArbitraryPoint where
  arbitrary = do
    slot <- SlotNo <$> arbitrary
    hash <- HeaderHash <$> arbitrary
    return $ ArbitraryPoint $ Point slot (BlockHash hash)

newtype ArbitraryChainRange = ArbitraryChainRange {
    getArbitraryChainRange :: ChainRange BlockHeader
  }

instance Arbitrary ArbitraryChainRange where
  arbitrary = fmap ArbitraryChainRange . ChainRange  <$> (getArbitraryPoint <$> arbitrary) <*> (getArbitraryPoint <$> arbitrary)

newtype ArbitraryBlockBody = ArbitraryBlockBody {
    getArbitraryBlockBody :: BlockBody
  }
  deriving (Show, Eq)

instance Arbitrary ArbitraryBlockBody where
    arbitrary =
      ArbitraryBlockBody . BlockBody . BSC.pack <$>
        -- Sometimes pick a common block so some are equal
        frequency [ (1, pure "EMPTY")
                  , (4, vectorOf 4 (choose ('A', 'Z'))) ]
    -- probably no need for shrink, the content is arbitrary and opaque
    -- if we add one, it might be to shrink to an empty block

newtype ArbitraryBlockHeader = ArbitraryBlockHeader {
    getArbitraryBlockHeader :: BlockHeader
  }
  deriving (Show, Eq)

instance Arbitrary ArbitraryBlockHeader where
  arbitrary = ArbitraryBlockHeader . blockHeader . fromJust . Chain.head <$> genBlockChain 1

-- Note that we do not provide any instance Arbitrary Block
-- because it's of little help with making chains.

-- We do provide CoArbitrary instances however, for (Block -> _) functions
-- We use default implementations using generics.
instance CoArbitrary Block
instance CoArbitrary BlockHeader
instance CoArbitrary SlotNo
instance CoArbitrary BlockNo
instance CoArbitrary BlockSigner
instance CoArbitrary BodyHash
instance CoArbitrary (ChainHash BlockHeader)
instance CoArbitrary ConcreteHeaderHash

instance CoArbitrary BlockBody where
  coarbitrary (BlockBody b) = coarbitrary (BSC.unpack b)

-- | The 'NonNegative' generator produces a large proportion of 0s, so we use
-- this one instead for now.
--
-- https://github.com/nick8325/quickcheck/issues/229
--
genNonNegative :: Gen Int
genNonNegative = (abs <$> arbitrary) `suchThat` (>= 0)

genBlockChain :: Int -> Gen (Chain Block)
genBlockChain n = do
    bodies <- map getArbitraryBlockBody <$> vector n
    slots  <- mkSlots <$> vectorOf n genSlotGap
    return (mkChain (zip slots bodies))
  where
    mkSlots :: [Int] -> [SlotNo]
    mkSlots = map toEnum . tail . scanl (+) 0

genSlotGap :: Gen Int
genSlotGap = frequency [(25, pure 1), (5, pure 2), (1, pure 3)]

addSlotGap :: Int -> SlotNo -> SlotNo
addSlotGap g (SlotNo n) = SlotNo (n + fromIntegral g)

genHeaderChain :: Int -> Gen (Chain BlockHeader)
genHeaderChain = fmap (fmap blockHeader) . genBlockChain


-- | The Ouroboros K paramater. This is also the maximum rollback length.
--
k :: Int
k = 5

--
-- Generator for chain and single block
--

-- | A test generator for a chain and a block that can be appended to it.
--
data TestAddBlock = TestAddBlock (Chain Block) Block
  deriving Show

instance Arbitrary TestAddBlock where
  arbitrary = do
    TestBlockChain chain <- arbitrary
    block <- genAddBlock chain
    return (TestAddBlock chain block)

  shrink (TestAddBlock c b) =
    [ TestAddBlock c' b'
    | TestBlockChain c' <- shrink (TestBlockChain c)
    , let b' = fixupBlock (Chain.head c') b
    ]

genAddBlock :: (HasHeader block, HeaderHash block ~ ConcreteHeaderHash)
            => Chain block -> Gen Block
genAddBlock chain = do
    slotGap <- genSlotGap
    body    <- getArbitraryBlockBody <$> arbitrary
    let pb = mkPartialBlock (addSlotGap slotGap (Chain.headSlot chain)) body
        b  = fixupBlock (Chain.head chain) pb
    return b

prop_arbitrary_TestAddBlock :: TestAddBlock -> Bool
prop_arbitrary_TestAddBlock (TestAddBlock c b) =
    Chain.valid (c :> b)

prop_shrink_TestAddBlock :: TestAddBlock -> Bool
prop_shrink_TestAddBlock t =
    and [ Chain.valid (c :> b) | TestAddBlock c b <- shrink t ]


--
-- Generator for chain updates
--

-- | A test generator for a chain and a sequence of updates that can be applied
-- to it.
--
data TestBlockChainAndUpdates =
       TestBlockChainAndUpdates (Chain Block) [ChainUpdate Block]
  deriving Show

instance Arbitrary TestBlockChainAndUpdates where
  arbitrary = do
    TestBlockChain chain <- arbitrary
    m <- genNonNegative
    updates <- genChainUpdates chain m
    return (TestBlockChainAndUpdates chain updates)

genChainUpdate :: Chain Block
               -> Gen (ChainUpdate Block)
genChainUpdate chain =
    frequency $
      -- To ensure we make progress on average w must ensure the weight of
      -- adding one block is more than the expected rollback length. If we
      -- used expectedRollbackLength then we would on average make no
      -- progress. We slightly arbitrarily weight 2:1 for forward progress.
      [ (expectedRollbackLength * 2, AddBlock <$> genAddBlock chain) ]
   ++ L.take (Chain.length chain)
        [ (freq, pure (RollBack (mkRollbackPoint chain len)))
        | (freq, len) <- rollbackLengthDistribution
        ]
  where
    -- This is the un-normalised expected value since the 'frequency'
    -- combinator normalises everything anyway.
    expectedRollbackLength :: Int
    expectedRollbackLength =
        sum [ freq * n | (freq, n) <- rollbackLengthDistribution ]

    rollbackLengthDistribution :: [(Int,Int)]
    rollbackLengthDistribution =
      (1, 0) :
      [ let freq = (k+1-n); len = n
         in (freq, len)
      | n <- [1..k] ]

mkRollbackPoint :: HasHeader block => Chain block -> Int -> Point block
mkRollbackPoint chain n = Chain.headPoint $ Chain.drop n chain

genChainUpdates :: Chain Block
                -> Int
                -> Gen [ChainUpdate Block]
genChainUpdates _     0 = return []
genChainUpdates chain n = do
    update  <- genChainUpdate chain
    let Just chain' = Chain.applyChainUpdate update chain
    updates <- genChainUpdates chain' (n-1)
    return (update : updates)

prop_arbitrary_TestBlockChainAndUpdates :: TestBlockChainAndUpdates -> Property
prop_arbitrary_TestBlockChainAndUpdates (TestBlockChainAndUpdates c us) =
    cover 1.5 (     null us ) "empty updates"     $
    cover 95  (not (null us)) "non-empty updates" $
    tabulate "ChainUpdate" (map updateKind us) $
    tabulate "Growth" [hist (countChainUpdateNetProgress c us)] $

    Chain.valid c
 && case Chain.applyChainUpdates us c of
      Nothing -> False
      Just c' -> Chain.valid c'
  where
    hist n = show lower ++ " to " ++ show upper
      where
        lower = (n `div` 10)     * 10
        upper = (n `div` 10 + 1) * 10 - 1

    updateKind AddBlock{} = "AddBlock"
    updateKind RollBack{} = "RollBack"

-- | Count the number of blocks forward - the number of blocks backward.
--
countChainUpdateNetProgress :: HasHeader block
                            => Chain block
                            -> [ChainUpdate block]
                            -> Int
countChainUpdateNetProgress = go 0
  where
    go n _c []     = n
    go n c  (u:us) = go n' c' us
      where
        Just c' = Chain.applyChainUpdate u c
        n'      = n + fromEnum (Chain.headBlockNo c')
                    - fromEnum (Chain.headBlockNo c)


--
-- Generators for chains
--

-- | A test generator for a valid chain of blocks.
--
newtype TestBlockChain = TestBlockChain { getTestBlockChain :: Chain Block }
    deriving (Eq, Show)

-- | A test generator for a valid chain of block headers.
--
newtype TestHeaderChain = TestHeaderChain (Chain BlockHeader)
    deriving (Eq, Show)

instance Arbitrary TestBlockChain where
    arbitrary = do
        n <- genNonNegative
        TestBlockChain <$> genBlockChain n

    shrink (TestBlockChain c) =
        [ TestBlockChain (fixupChain fixupBlock c')
        | c' <- shrinkList (const []) (Chain.toNewestFirst c) ]

instance Arbitrary TestHeaderChain where
    arbitrary = do
        n <- genNonNegative
        TestHeaderChain <$> genHeaderChain n

    shrink (TestHeaderChain c) =
        [ TestHeaderChain (fixupChain fixupBlockHeader c')
        | c' <- shrinkList (const []) (Chain.toNewestFirst c) ]

prop_arbitrary_TestBlockChain :: TestBlockChain -> Property
prop_arbitrary_TestBlockChain (TestBlockChain c) =
    -- check we get some but not too many zero-length chains
    cover 95   (not (Chain.null c)) "non-null" $
    cover 1.5       (Chain.null c)  "null"     $
    Chain.valid c

prop_arbitrary_TestHeaderChain :: TestHeaderChain -> Bool
prop_arbitrary_TestHeaderChain (TestHeaderChain c) =
    Chain.valid c

prop_shrink_TestBlockChain :: TestBlockChain -> Bool
prop_shrink_TestBlockChain c =
    and [ Chain.valid c' | TestBlockChain c' <- shrink c ]

prop_shrink_TestHeaderChain :: TestHeaderChain -> Bool
prop_shrink_TestHeaderChain c =
    and [ Chain.valid c' | TestHeaderChain c' <- shrink c ]


--
-- Generator for chain and single point on the chain
--

-- | A test generator for a chain and a points. In most cases the point is
-- on the chain, but it also covers at least 5% of cases where the point is
-- not on the chain.
--
data TestChainAndPoint = TestChainAndPoint (Chain Block) (Point Block)
  deriving Show

instance Arbitrary TestChainAndPoint where
  arbitrary = do
    TestBlockChain chain <- arbitrary
    -- either choose point from the chain or a few off the chain!
    point <- frequency [ (10, genPointOnChain chain), (1, genPoint) ]
    return (TestChainAndPoint chain point)

  shrink (TestChainAndPoint c p) =
    [ TestChainAndPoint c' (fixupPoint c' p)
    | TestBlockChain c' <- shrink (TestBlockChain c) ]

genPointOnChain :: HasHeader block => Chain block -> Gen (Point block)
genPointOnChain chain =
    frequency
      [ (1, return (Chain.headPoint chain))
      , (1, return (mkRollbackPoint chain len))
      , (8, mkRollbackPoint chain <$> choose (1, len - 1))
      ]
  where
    len = Chain.length chain

genPoint :: Gen (Point Block)
genPoint = (\s h -> Point (SlotNo s) (BlockHash (HeaderHash h))) <$> arbitrary <*> arbitrary

fixupPoint :: HasHeader block => Chain block -> Point block -> Point block
fixupPoint c p =
  case Chain.lookupBySlot c (pointSlot p) of
    Just b  -> Chain.blockPoint b
    Nothing -> Chain.headPoint c

prop_arbitrary_TestChainAndPoint :: TestChainAndPoint -> Property
prop_arbitrary_TestChainAndPoint (TestChainAndPoint c p) =
  let onChain = Chain.pointOnChain p c in
  cover  85 onChain       "point on chain" $
  cover   5 (not onChain) "point not on chain" $
    Chain.valid c

prop_shrink_TestChainAndPoint :: TestChainAndPoint -> Bool
prop_shrink_TestChainAndPoint cp@(TestChainAndPoint c _) =
  and [     Chain.valid c'
        && (Chain.pointOnChain p c `implies` Chain.pointOnChain p c')
      | TestChainAndPoint c' p <- shrink cp ]

implies :: Bool -> Bool -> Bool
a `implies` b = not a || b

infix 1 `implies`


--
-- Generator for chain and range on the chain
--

-- | A test generator for a chain and a range defined by a pair of points.
-- In most cases the range is on the chain, but it also covers at least 5% of
-- cases where the point is not on the chain.
--
data TestChainAndRange = TestChainAndRange (Chain Block) (Point Block) (Point Block)
  deriving Show

instance Arbitrary TestChainAndRange where
  arbitrary = do
    TestBlockChain chain <- arbitrary
    -- either choose range from the chain or a few off the chain!
    (point1, point2) <- frequency [ (10, genRangeOnChain chain)
                                  , (1, (,) <$> genPoint <*> genPoint) ]
    return (TestChainAndRange chain point1 point2)

  shrink (TestChainAndRange c p1 p2) =
    [ TestChainAndRange c' (fixupPoint c' p1) (fixupPoint c' p2)
    | TestBlockChain c' <- shrink (TestBlockChain c) ]

genRangeOnChain :: HasHeader block
                => Chain block
                -> Gen (Point block, Point block)
genRangeOnChain chain = do
    point1 <- genPointOnChain chain
    let Just point1Depth = (\c -> Chain.length chain - Chain.length c) <$>
                           Chain.rollback point1 chain
    point2 <- frequency $
        [ (1, return (Chain.headPoint chain))
        , (1, return (mkRollbackPoint chain point1Depth))
        , (8, mkRollbackPoint chain <$> choose (0, point1Depth))
        ]
    return (point1, point2)

prop_arbitrary_TestChainAndRange :: TestChainAndRange -> Property
prop_arbitrary_TestChainAndRange (TestChainAndRange c p1 p2) =
  let onChain = Chain.pointOnChain p1 c && Chain.pointOnChain p2 c in
  cover 85 onChain               "points on chain" $
  cover  5 (onChain && p1 == p2) "empty range" $
  cover  5 (not onChain)         "points not on chain" $
    Chain.valid c
 && onChain `implies` pointSlot p2 >= pointSlot p1

prop_shrink_TestChainAndRange :: TestChainAndRange -> Bool
prop_shrink_TestChainAndRange cp@(TestChainAndRange c _ _) =
  and [    Chain.valid c'
        && (Chain.pointOnChain p1 c && Chain.pointOnChain p2 c
            `implies`
            Chain.pointOnChain p1 c' && Chain.pointOnChain p2 c')
      | TestChainAndRange c' p1 p2 <- shrink cp ]


-- | A test generator for a chain and a list of points, some of which may not be
-- on the chain.  Only 50% of the blocks are selected, one fifth of selected
-- ones are not on the chain.  Points which come from the chain are given in the
-- newest to oldest order, but the intermediate points which are not in the
-- chain might break the order.
--
data TestChainAndPoints = TestChainAndPoints (Chain Block) [Point Block]
  deriving Show

instance Arbitrary TestChainAndPoints where
  arbitrary = do
    TestBlockChain chain <- arbitrary
    let fn p = frequency
          [ (4, return $ Just p)
          , (1, Just <$> genPoint)
          , (5, return Nothing)
          ]
        points = map Chain.blockPoint (Chain.chainToList chain)
                  ++ [Chain.genesisPoint]
    points' <- catMaybes <$> mapM fn points
    return $ TestChainAndPoints chain points'

  shrink (TestChainAndPoints chain points) =
    [ TestChainAndPoints chain' points'
    | TestBlockChain chain' <- shrink (TestBlockChain chain)
      -- Leave only points that are on the @chain'@ or the ones that where not on the
      -- original @chain@.
    , let points' = filter (\p ->        p `Chain.pointOnChain` chain'
                                 || not (p `Chain.pointOnChain` chain)) points
    ] ++
    [ TestChainAndPoints chain points'
    | points' <- shrinkList shrinkNothing points
    ]

prop_arbitrary_TestChainAndPoints :: TestChainAndPoints -> Property
prop_arbitrary_TestChainAndPoints (TestChainAndPoints c ps) =
  cover 85 (any (`Chain.pointOnChain` c) ps)       "any points on chain"     $
  cover 65 (not (all (`Chain.pointOnChain` c) ps)) "not all points on chain" $
  cover 90 (not (null ps))                         "some points"             $
    Chain.valid c

prop_shrink_TestChainAndPoints :: TestChainAndPoints -> Bool
prop_shrink_TestChainAndPoints cps@(TestChainAndPoints c _) =
  -- can't really say much about the points without duplicating the logic above
  and [    Chain.valid c'
        && all (\p ->      p `Chain.pointOnChain` c'
                   || not (p `Chain.pointOnChain` c)) ps'
      | TestChainAndPoints c' ps' <- shrink cps ]


--
-- Generator for chain forks sharing a common prefix
--

-- | A test generator for two chains sharing a common prefix.
--
data TestChainFork = TestChainFork (Chain Block) -- common prefix
                                   (Chain Block) -- left fork
                                   (Chain Block) -- right fork

instance Show TestChainFork where
  show (TestChainFork c f1 f2)
    = let nl  = "\n    "
          nnl = "\n" ++ nl
      in "TestChainFork" ++ nl ++
      Chain.prettyPrintChain nl show c  ++ nnl ++
      Chain.prettyPrintChain nl show f1 ++ nnl ++
      Chain.prettyPrintChain nl show f2

instance Arbitrary TestChainFork where
  arbitrary = do
    TestBlockChain chain <- arbitrary
    -- at least 5% of forks should be equal
    equalChains <- frequency [(1, pure True), (19, pure False)]
    if equalChains
      then return (TestChainFork chain chain chain)
      else do
        l <- genNonNegative
        r <- genNonNegative
        chainL <- genAddBlocks l chain Nothing
        let exL = L.drop (Chain.length chain) (Chain.toOldestFirst chainL)
        chainR <- genAddBlocks r chain (listToMaybe exL)
        return (TestChainFork chain chainL chainR)

    where
      genAddBlocks :: Int
                   -> Chain Block
                   -> Maybe Block
                   -> Gen (Chain Block)
      genAddBlocks 0 c _       = return c
      genAddBlocks n c Nothing = do
          b <- genAddBlock c
          genAddBlocks (n-1) (Chain.addBlock b c) Nothing

      -- But we want to avoid the extensions starting off equal which would
      -- mean the longest common prefix was not the declared common prefix.
      -- So we optionally take the first block to avoid and use that in the
      -- second fork we generate.
      genAddBlocks n c (Just forbiddenBlock) = do
          b <- genAddBlock c `suchThat` (/= forbiddenBlock)
          genAddBlocks (n-1) (Chain.addBlock b c) Nothing


  shrink (TestChainFork common l r) =
        -- shrink the common prefix
      [ TestChainFork (fixupChain fixupBlock common')
                      (fixupChain fixupBlock (exl ++ common'))
                      (fixupChain fixupBlock (exr ++ common'))
      | let exl = extensionFragment common l
            exr = extensionFragment common r
      , common' <- shrinkList (const []) (Chain.toNewestFirst common)
      ]
        -- shrink the left fork
   ++ [ TestChainFork common l' r
      | let exl = extensionFragment common l
      , exl' <- shrinkList (const []) exl
      , let l' = fixupChain fixupBlock (exl' ++ Chain.toNewestFirst common)
      , isLongestCommonPrefix l' r
      ]
        -- shrink the right fork
   ++ [ TestChainFork common l r'
      | let exr = extensionFragment common r
      , exr' <- shrinkList (const []) exr
      , let r' = fixupChain fixupBlock (exr' ++ Chain.toNewestFirst common)
      , isLongestCommonPrefix l r'
      ]
    where
      extensionFragment :: Chain Block -> Chain Block -> [Block]
      extensionFragment c = reverse . L.drop (Chain.length c) . Chain.toOldestFirst

      -- Need to make sure that when we shrink that we don't make the longest
      -- common prefix be a strict extension of the original common prefix.
      isLongestCommonPrefix l' r' =
        case (L.drop (Chain.length common) (Chain.toOldestFirst l'),
              L.drop (Chain.length common) (Chain.toOldestFirst r')) of
          (lhead : _, rhead : _) -> lhead /= rhead
          _                      -> True


prop_arbitrary_TestChainFork :: TestChainFork -> Bool
prop_arbitrary_TestChainFork (TestChainFork c l r) =
    Chain.valid c && Chain.valid l && Chain.valid r
 && c `Chain.isPrefixOf` l
 && c `Chain.isPrefixOf` r
    -- And c is not just a common prefix, but the maximum common prefix
 && case (L.drop (Chain.length c) (Chain.toOldestFirst l),
          L.drop (Chain.length c) (Chain.toOldestFirst r)) of
      (lhead : _, rhead : _) -> lhead /= rhead
      _                      -> True


prop_shrink_TestChainFork :: TestChainFork -> Bool
prop_shrink_TestChainFork forks =
  and [    prop_arbitrary_TestChainFork forks'
        && measure forks' < mforks
      | let mforks = measure forks
      , forks' <- shrink forks ]
  where
    measure (TestChainFork c l r) = Chain.length c
                                  + Chain.length l
                                  + Chain.length r
