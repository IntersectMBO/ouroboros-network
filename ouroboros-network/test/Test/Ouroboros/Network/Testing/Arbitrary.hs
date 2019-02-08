{-# LANGUAGE GADTs #-}
-- | Arbitrary generators for chains, headers and blocks
--
module Test.Ouroboros.Network.Testing.Arbitrary
  ( -- * Arbitrary Point BlockBody and BlockHeader
    -- These instances are useful for testing codec.
    ArbitraryPoint (..)
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
  , TestChainAndPoints (..)
  , TestChainAndRange (..)
  , TestChainFork (..)

    -- * Utility functions
  , genNonNegative
  , genSlotGap
  , addSlotGap
  , genHeaderChain
  , mkPartialBlock
  , mkRollbackPoint
  , genPoint
  , implies
  )
  where

import qualified Data.List as L
import           Data.Maybe (catMaybes, fromJust)

import           Ouroboros.Network.Testing.ConcreteBlock
import           Ouroboros.Network.Block (Slot (..), Hash (..) , HasHeader (..))
import           Ouroboros.Network.Chain (Chain (..), ChainUpdate (..), Point (..))
import qualified Ouroboros.Network.Chain as Chain

import           Test.QuickCheck

newtype ArbitraryPoint = ArbitraryPoint {
    getArbitraryPoint :: Point BlockHeader
  }
  deriving (Show, Eq)

instance Arbitrary ArbitraryPoint where
  arbitrary = do
    slot <- Slot <$> arbitrary
    hash <- HeaderHash <$> arbitrary
    return $ ArbitraryPoint $ Point slot (BlockHash hash)

newtype ArbitraryBlockBody = ArbitraryBlockBody {
    getArbitraryBlockBody :: BlockBody
  }
  deriving (Show, Eq)

instance Arbitrary ArbitraryBlockBody where
    arbitrary = ArbitraryBlockBody . BlockBody <$> vectorOf 4 (choose ('A', 'Z'))
    -- probably no need for shrink, the content is arbitrary and opaque
    -- if we add one, it might be to shrink to an empty block

newtype ArbitraryBlockHeader = ArbitraryBlockHeader {
    getArbitraryBlockHeader :: BlockHeader
  }
  deriving (Show, Eq)

instance Arbitrary ArbitraryBlockHeader where
  arbitrary = ArbitraryBlockHeader . blockHeader . fromJust . Chain.head <$> genBlockChain 1

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
    return (mkChain slots bodies)
  where
    mkSlots :: [Int] -> [Slot]
    mkSlots = map toEnum . tail . scanl (+) 0

    mkChain :: [Slot] -> [BlockBody] -> Chain Block
    mkChain slots bodies =
        fromListFixupBlocks
      . reverse
      $ zipWith mkPartialBlock slots bodies

genSlotGap :: Gen Int
genSlotGap = frequency [(25, pure 1), (5, pure 2), (1, pure 3)]

addSlotGap :: Int -> Slot -> Slot
addSlotGap g (Slot n) = Slot (n + fromIntegral g)

genHeaderChain :: Int -> Gen (Chain BlockHeader)
genHeaderChain = fmap (fmap blockHeader) . genBlockChain

mkPartialBlock :: Slot -> BlockBody -> Block
mkPartialBlock sl body =
    Block {
      blockHeader = BlockHeader {
        headerSlot     = sl,
        headerSigner   = expectedBFTSigner sl,
        headerHash     = partialField "headerHash",
        headerPrevHash = partialField "headerPrevHash",
        headerBlockNo  = partialField "headerBlockNo",
        headerBodyHash = hashBody body
      }
    , blockBody = body
    }
  where
    partialField n = error ("mkPartialBlock: you didn't fill in field " ++ n)

    expectedBFTSigner :: Slot -> BlockSigner
    expectedBFTSigner (Slot n) = BlockSigner (n `mod` 7)


-- | To help with chain construction and shrinking it's handy to recalculate
-- all the hashes.
--
fromListFixupBlocks :: [Block] -> Chain Block
fromListFixupBlocks []      = Genesis
fromListFixupBlocks (b : c) = c' :> b'
  where
    c' = fromListFixupBlocks c
    b' = fixupBlock c' b

fromListFixupHeaders :: [BlockHeader] -> Chain BlockHeader
fromListFixupHeaders []      = Genesis
fromListFixupHeaders (b : c) = c' :> b'
  where
    c' = fromListFixupHeaders c
    b' = fixupBlockHeader c' (headerBodyHash b) b

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
    , let b' = fixupBlock c' b
    ]

genAddBlock :: (HasHeader block, HeaderHash block ~ ConcreteHeaderHash)
            => Chain block -> Gen Block
genAddBlock chain = do
    slotGap <- genSlotGap
    body    <- getArbitraryBlockBody <$> arbitrary
    let pb = mkPartialBlock (addSlotGap slotGap (Chain.headSlot chain)) body
        b  = fixupBlock chain pb
    return b


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
        [ TestBlockChain (fromListFixupBlocks c')
        | c' <- shrinkList (const []) (Chain.toNewestFirst c) ]

instance Arbitrary TestHeaderChain where
    arbitrary = do
        n <- genNonNegative
        TestHeaderChain <$> genHeaderChain n

    shrink (TestHeaderChain c) =
        [ TestHeaderChain (fromListFixupHeaders c')
        | c' <- shrinkList (const []) (Chain.toNewestFirst c) ]

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

  shrink (TestChainAndPoint c p)
    | Chain.pointOnChain p c
    = [ TestChainAndPoint c' (fixupPoint c' p)
    | TestBlockChain c' <- shrink (TestBlockChain c)]
    | otherwise
    = [ TestChainAndPoint c' p
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

-- | A test generator for a chain and a list of points some of which may not be
-- on the chain.  Only 50% of the blocks are selected, one fifth of selected
-- ones is not on the chain.  Points which come from the chain are given in the
-- newest to oldest order, but the intermittent points which are not in the
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
        points = map Chain.blockPoint (Chain.chainToList chain) ++ [Chain.genesisPoint]
    points' <- catMaybes <$> mapM fn points
    return $ TestChainAndPoints chain points'

  shrink (TestChainAndPoints chain points) =
    [ TestChainAndPoints chain' points'
    | TestBlockChain chain' <- shrink (TestBlockChain chain)
      -- Leave only points that are on the @chain'@ or the ones that where not on the
      -- original @chain@.
    , let points' = filter (\p -> p `Chain.pointOnChain` chain' || not (p `Chain.pointOnChain` chain)) points
    ] ++
    [ TestChainAndPoints chain points'
    | points' <- shrinkList shrinkNothing points
    ]

genPoint :: Gen (Point Block)
genPoint = mkPoint <$> arbitrary <*> arbitrary
  where
    mkPoint s h = Point (Slot s) (BlockHash (HeaderHash h))

fixupPoint :: HasHeader block => Chain block -> Point block -> Point block
fixupPoint c p =
  case Chain.lookupBySlot c (pointSlot p) of
    Just b  -> Chain.blockPoint b
    Nothing -> Chain.headPoint c

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
        chainL <- genAddBlocks l chain
        chainR <- genAddBlocks r chain
        return (TestChainFork chain chainL chainR)

    where
      genAddBlocks :: Int
                   -> Chain Block
                   -> Gen (Chain Block)
      genAddBlocks 0 c = return c
      genAddBlocks n c = do
          b <- genAddBlock c
          genAddBlocks (n-1) (Chain.addBlock b c)


  shrink (TestChainFork common l r) =
        -- shrink the common prefix
      [ TestChainFork (fromListFixupBlocks common')
                      (fromListFixupBlocks (exl ++ common'))
                      (fromListFixupBlocks (exr ++ common'))
      | let exl = extensionFragment common l
            exr = extensionFragment common r
      , common' <- shrinkList (const []) (Chain.toNewestFirst common)
      ]
        -- shrink the left fork
   ++ [ TestChainFork common l' r
      | let exl = extensionFragment common l
      , exl' <- shrinkList (const []) exl
      , let l' = fromListFixupBlocks (exl' ++ Chain.toNewestFirst common)
      ]
        -- shrink the right fork
   ++ [ TestChainFork common l r'
      | let exr = extensionFragment common r
      , exr' <- shrinkList (const []) exr
      , let r' = fromListFixupBlocks (exr' ++ Chain.toNewestFirst common)
      ]
    where
      extensionFragment :: Chain Block -> Chain Block -> [Block]
      extensionFragment c = reverse . L.drop (Chain.length c) . Chain.toOldestFirst

implies :: Bool -> Bool -> Bool
a `implies` b = not a || b

infix 1 `implies`

--
-- Generator for chain and single point on the chain
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
