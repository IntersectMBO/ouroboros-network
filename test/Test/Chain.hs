{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeOperators    #-}

module Test.Chain
  ( tests
  , TestBlockChainAndUpdates(..)
  , TestBlockChain(..)
  , TestChainFork(..)
  , mkRollbackPoint
  , genBlockChain
  , genHeaderChain
  ) where

import           Block
import           Chain                 (Chain (..), ChainUpdate (..),
                                        Point (..), genesisPoint)
import qualified Chain
import           Ouroboros
import           Serialise             (prop_serialise)

import qualified Data.List             as L
import           Data.Maybe            (listToMaybe)

import           Util.Singletons       (Dict (..))

import           Test.QuickCheck
import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Test.DepFn
import           Test.Ouroboros

--
-- The list of all tests
--

tests :: TestTree
tests = testGroup "Chain"
  [ testGroup "generators"
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

    , testProperty "arbitrary for TestChainAndPoint" prop_arbitrary_TestChainAndPoint
    , testProperty "shrink for TestChainAndPoint"    prop_shrink_TestChainAndPoint

    , testProperty "arbitrary for TestChainFork" prop_arbitrary_TestChainFork
    , testProperty "shrink for TestChainFork"
                               (mapSize (min 40) prop_shrink_TestChainFork)
    ]

  , testProperty "length/Genesis"  prop_length_genesis
  , testProperty "drop/Genesis"    prop_drop_genesis
  , testProperty "fromList/toList" prop_fromList_toList
  , testProperty "toList/head"     prop_toList_head
  , testProperty "drop"            prop_drop
  , testProperty "addBlock"        prop_addBlock
  , testProperty "rollback"        prop_rollback
  , testProperty "rollback/head"   prop_rollback_head
  , testProperty "successorBlock"  prop_successorBlock
  , testProperty "lookupBySlot"    prop_lookupBySlot
  , testProperty "intersectChains" prop_intersectChains
  , testProperty "serialise chain" prop_serialise_chain
  ]

--
-- Properties
--

prop_length_genesis :: Bool
prop_length_genesis = Chain.length Genesis == 0

prop_drop_genesis :: TestBlockChain :-> Bool
prop_drop_genesis = simpleProp $ \_ (TestBlockChain chain) ->
    Chain.drop (Chain.length chain) chain == Genesis

prop_fromList_toList :: TestBlockChain :-> Bool
prop_fromList_toList = simpleProp $ \_ (TestBlockChain chain) ->
    (Chain.fromNewestFirst . Chain.toNewestFirst) chain == chain

-- The list comes out in reverse order, most recent block at the head
prop_toList_head :: TestBlockChain :-> Bool
prop_toList_head = simpleProp $ \_ (TestBlockChain chain) ->
    (listToMaybe . Chain.toNewestFirst) chain == Chain.head chain

prop_drop :: TestBlockChain :-> Bool
prop_drop = simpleProp $ \_ (TestBlockChain chain) ->
    let blocks = Chain.toNewestFirst chain in
    and [ Chain.drop n chain == Chain.fromNewestFirst (L.drop n blocks)
        | n <- [0..Prelude.length blocks] ]

prop_addBlock :: TestAddBlock :-> Bool
prop_addBlock = simpleProp $ \_ (TestAddBlock c b) ->
 let c' = Chain.addBlock b c in
    -- after adding a block, that block is at the head
    Chain.headPoint c' == Chain.blockPoint b
    -- chain is still valid
 && Chain.valid c'
    -- removing the block gives the original
 && Chain.rollback (Chain.headPoint c) c' == Just c
 && Chain.drop 1 c' == c
    -- chain is one longer
 && Chain.length c' == Chain.length c + 1

prop_rollback :: TestChainAndPoint :-> Bool
prop_rollback = simpleProp $ \_ (TestChainAndPoint c p) ->
    case Chain.rollback p c of
      Nothing -> True
      Just c' ->
        -- chain is a prefix of original
             Chain.isPrefixOf c' c
        -- chain head point is the rollback point
        && Chain.headPoint c' == p

prop_rollback_head :: TestBlockChain :-> Bool
prop_rollback_head = simpleProp $ \_ (TestBlockChain c) ->
    Chain.rollback (Chain.headPoint c) c == Just c

prop_successorBlock :: TestChainAndPoint :-> Property
prop_successorBlock = simpleProp $ \_ (TestChainAndPoint c p) ->
  Chain.pointOnChain p c ==>
  case Chain.successorBlock p c of
    Nothing -> Chain.headPoint c === p
    Just b  -> property $ Chain.pointOnChain (Chain.blockPoint b) c

prop_lookupBySlot :: TestChainAndPoint :-> Bool
prop_lookupBySlot = simpleProp $ \_ (TestChainAndPoint c p) ->
  case Chain.lookupBySlot c (pointSlot p) of
    Just b  -> Chain.pointOnChain (Chain.blockPoint b) c
    Nothing | p == genesisPoint -> True
            | otherwise         -> not (Chain.pointOnChain p c)

prop_intersectChains :: TestChainFork :-> Bool
prop_intersectChains = simpleProp $ \_ (TestChainFork c l r) ->
  case Chain.intersectChains l r of
    Nothing -> c == Genesis && L.intersect (Chain.toNewestFirst l) (Chain.toNewestFirst r) == []
    Just p  -> Chain.headPoint c == p
            && Chain.pointOnChain p l
            && Chain.pointOnChain p r

prop_serialise_chain :: TestBlockChain :-> Bool
prop_serialise_chain = simpleProp $ \_ (TestBlockChain chain) ->
  prop_serialise chain

--
-- Generators for chains
--

-- | A test generator for a valid chain of blocks.
--
newtype TestBlockChain p = TestBlockChain { getTestBlockChain :: Chain (Block 'TestLedgerDomain p) }
    deriving (Eq, Show)

instance SingShow TestBlockChain where
  singShow s = case dictKnownOuroborosProtocol s of Dict -> show

-- | A test generator for a valid chain of block headers.
--
newtype TestHeaderChain p = TestHeaderChain (Chain (BlockHeader p))
    deriving (Eq, Show)

instance SingShow TestHeaderChain where
  singShow s = case dictKnownOuroborosProtocol s of Dict -> show

instance SingArbitrary TestBlockChain where
    singArbitrary _ = do
        n <- genNonNegative
        TestBlockChain <$> genBlockChain n

    singShrink _ (TestBlockChain c) =
        [ TestBlockChain (fromListFixupBlocks c')
        | c' <- shrinkList (const []) (Chain.toNewestFirst c) ]

instance SingArbitrary TestHeaderChain where
    singArbitrary _ = do
        n <- genNonNegative
        TestHeaderChain <$> genHeaderChain n

    singShrink _ (TestHeaderChain c) =
        [ TestHeaderChain (fromListFixupHeaders c')
        | c' <- shrinkList (const []) (Chain.toNewestFirst c) ]

prop_arbitrary_TestBlockChain :: TestBlockChain :-> Property
prop_arbitrary_TestBlockChain = simpleProp $ \_ (TestBlockChain c) ->
    -- check we get some but not too many zero-length chains
    cover 95   (not (Chain.null c)) "non-null" $
    cover 1.5       (Chain.null c)  "null"     $
    Chain.valid c

prop_arbitrary_TestHeaderChain :: TestHeaderChain :-> Bool
prop_arbitrary_TestHeaderChain = simpleProp $ \_ (TestHeaderChain c) ->
    Chain.valid c

prop_shrink_TestBlockChain :: TestBlockChain :-> Bool
prop_shrink_TestBlockChain = simpleProp $ \p c ->
    and [ Chain.valid c' | TestBlockChain c' <- singShrink p c ]

prop_shrink_TestHeaderChain :: TestHeaderChain :-> Bool
prop_shrink_TestHeaderChain = simpleProp $ \p c ->
    and [ Chain.valid c' | TestHeaderChain c' <- singShrink p c ]

-- | The 'NonNegative' generator produces a large proportion of 0s, so we use
-- this one instead for now.
--
-- https://github.com/nick8325/quickcheck/issues/229
--
genNonNegative :: Gen Int
genNonNegative = (abs <$> arbitrary) `suchThat` (>= 0)

genBlockChain :: Int -> Gen (Chain (Block 'TestLedgerDomain p))
genBlockChain n = do
    bodies <- vector n
    slots  <- mkSlots <$> vectorOf n genSlotGap
    return (mkChain slots bodies)
  where
    mkSlots :: [Int] -> [Slot]
    mkSlots = map toEnum . tail . scanl (+) 0

    mkChain :: [Slot] -> [BlockBody 'TestLedgerDomain] -> Chain (Block 'TestLedgerDomain p)
    mkChain slots bodies =
        fromListFixupBlocks
      . reverse
      $ zipWith mkPartialBlock slots bodies

genSlotGap :: Gen Int
genSlotGap = frequency [(25, pure 1), (5, pure 2), (1, pure 3)]

addSlotGap :: Int -> Slot -> Slot
addSlotGap g (Slot n) = Slot (n + fromIntegral g)

genHeaderChain :: Int -> Gen (Chain (BlockHeader p))
genHeaderChain = fmap (fmap blockHeader) . genBlockChain

mkPartialBlock :: Slot -> BlockBody 'TestLedgerDomain -> Block 'TestLedgerDomain p
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
fromListFixupBlocks :: (HasHeader (Block dom), KnownLedgerDomain dom)
                    => [Block dom p] -> Chain (Block dom p)
fromListFixupBlocks []      = Genesis
fromListFixupBlocks (b : c) = c' :> b'
  where
    c' = fromListFixupBlocks c
    b' = Chain.fixupBlock c' b

fromListFixupHeaders :: [BlockHeader p] -> Chain (BlockHeader p)
fromListFixupHeaders []      = Genesis
fromListFixupHeaders (b : c) = c' :> b'
  where
    c' = fromListFixupHeaders c
    b' = Chain.fixupBlockHeader c' (headerBodyHash b) b

-- | The Ouroboros K paramater. This is also the maximum rollback length.
--
k :: Int
k = 5

--
-- Generator for chain and single block
--

-- | A test generator for a chain and a block that can be appended to it.
--
data TestAddBlock p = TestAddBlock (Chain (Block 'TestLedgerDomain p)) (Block 'TestLedgerDomain p)
  deriving Show

instance SingShow TestAddBlock where
  singShow s = case dictKnownOuroborosProtocol s of Dict -> show

instance SingArbitrary TestAddBlock where
  singArbitrary p = do
    TestBlockChain chain <- singArbitrary p
    block <- genAddBlock chain
    return (TestAddBlock chain block)

  singShrink p (TestAddBlock c b) =
    [ TestAddBlock c' b'
    | TestBlockChain c' <- singShrink p (TestBlockChain c)
    , let b' = Chain.fixupBlock c' b
    ]

genAddBlock :: HasHeader block => Chain (block p) -> Gen (Block 'TestLedgerDomain p)
genAddBlock chain = do
    slotGap <- genSlotGap
    body    <- arbitrary
    let pb = mkPartialBlock (addSlotGap slotGap (Chain.headSlot chain)) body
        b  = Chain.fixupBlock chain pb
    return b

prop_arbitrary_TestAddBlock :: TestAddBlock :-> Bool
prop_arbitrary_TestAddBlock = simpleProp $ \_ (TestAddBlock c b) ->
    Chain.valid (c :> b)

prop_shrink_TestAddBlock :: TestAddBlock :-> Bool
prop_shrink_TestAddBlock = simpleProp $ \p t ->
    and [ Chain.valid (c :> b) | TestAddBlock c b <- singShrink p t ]

--
-- Generator for chain updates
--

-- | A test generator for a chain and a sequence of updates that can be applied
-- to it.
--
data TestBlockChainAndUpdates p =
       TestBlockChainAndUpdates (Chain (Block 'TestLedgerDomain p)) [ChainUpdate (Block 'TestLedgerDomain p)]
  deriving Show

instance SingShow TestBlockChainAndUpdates where
  singShow s = case dictKnownOuroborosProtocol s of Dict -> show

instance SingArbitrary TestBlockChainAndUpdates where
  singArbitrary p = do
    TestBlockChain chain <- singArbitrary p
    m <- genNonNegative
    updates <- genChainUpdates chain m
    return (TestBlockChainAndUpdates chain updates)

genChainUpdate :: Chain (Block 'TestLedgerDomain p)
               -> Gen (ChainUpdate (Block 'TestLedgerDomain p))
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

mkRollbackPoint :: HasHeader block => Chain (block p) -> Int -> Point
mkRollbackPoint chain n = Chain.headPoint $ Chain.drop n chain

genChainUpdates :: Chain (Block 'TestLedgerDomain p)
                -> Int
                -> Gen [ChainUpdate (Block 'TestLedgerDomain p)]
genChainUpdates _     0 = return []
genChainUpdates chain n = do
    update  <- genChainUpdate chain
    let Just chain' = Chain.applyChainUpdate update chain
    updates <- genChainUpdates chain' (n-1)
    return (update : updates)

prop_arbitrary_TestBlockChainAndUpdates :: TestBlockChainAndUpdates :-> Property
prop_arbitrary_TestBlockChainAndUpdates = simpleProp $ \_ (TestBlockChainAndUpdates c us) ->
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
                            => Chain (block p)
                            -> [ChainUpdate (block p)]
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
-- Generator for chain and single point on the chain
--

-- | A test generator for a chain and a points. In most cases the point is
-- on the chain, but it also covers at least 5% of cases where the point is
-- not on the chain.
--
data TestChainAndPoint p = TestChainAndPoint (Chain (Block 'TestLedgerDomain p)) Point
  deriving Show

instance SingShow TestChainAndPoint where
  singShow s = case dictKnownOuroborosProtocol s of Dict -> show

instance SingArbitrary TestChainAndPoint where
  singArbitrary p = do
    TestBlockChain chain <- singArbitrary p
    let len = Chain.length chain
    -- either choose point from the chain
    point <- frequency
      [ (2, return (Chain.headPoint chain))
      , (2, return (mkRollbackPoint chain len))
      , (8, mkRollbackPoint chain <$> choose (1, len - 1))
      -- or a few off the chain!
      , (1, genPoint)
      ]
    return (TestChainAndPoint chain point)

  singShrink protocol (TestChainAndPoint c p)
    | Chain.pointOnChain p c
    = [ TestChainAndPoint c' (fixupPoint c' p)
    | TestBlockChain c' <- singShrink protocol (TestBlockChain c)]
    | otherwise
    = [ TestChainAndPoint c' p
      | TestBlockChain c' <- singShrink protocol (TestBlockChain c) ]

genPoint :: Gen Point
genPoint = (\s h -> Point (Slot s) (HeaderHash h)) <$> arbitrary <*> arbitrary

fixupPoint :: HasHeader block => Chain (block p) -> Point -> Point
fixupPoint c p =
  case Chain.lookupBySlot c (pointSlot p) of
    Just b  -> Chain.blockPoint b
    Nothing -> Chain.headPoint c

prop_arbitrary_TestChainAndPoint :: TestChainAndPoint :-> Property
prop_arbitrary_TestChainAndPoint = simpleProp $ \_ (TestChainAndPoint c p) ->
  let onChain = Chain.pointOnChain p c in
  cover (85/100) onChain       "point on chain" $
  cover ( 5/100) (not onChain) "point not on chain" $
    Chain.valid c

prop_shrink_TestChainAndPoint :: TestChainAndPoint :-> Bool
prop_shrink_TestChainAndPoint = simpleProp $ \protocol cp@(TestChainAndPoint c _) ->
  and [ Chain.valid c' && (not (Chain.pointOnChain p c) || Chain.pointOnChain p c')
      | TestChainAndPoint c' p <- singShrink protocol cp ]

--
-- Generator for chain forks sharing a common prefix
--

-- | A test generator for two chains sharing a common prefix.
--
data TestChainFork p = TestChainFork (Chain (Block 'TestLedgerDomain p)) -- common prefix
                                     (Chain (Block 'TestLedgerDomain p)) -- left fork
                                     (Chain (Block 'TestLedgerDomain p)) -- right fork

instance KnownOuroborosProtocol p => Show (TestChainFork p) where
  show (TestChainFork c f1 f2)
    = let nl  = "\n    "
          nnl = "\n" ++ nl
      in "TestChainFork" ++ nl ++
      Chain.prettyPrintChain nl show c  ++ nnl ++
      Chain.prettyPrintChain nl show f1 ++ nnl ++
      Chain.prettyPrintChain nl show f2

instance SingShow TestChainFork where
  singShow s = case dictKnownOuroborosProtocol s of Dict -> show

instance SingArbitrary TestChainFork where
  singArbitrary p = do
    TestBlockChain chain <- singArbitrary p
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
                   -> Chain (Block 'TestLedgerDomain p)
                   -> Gen (Chain (Block 'TestLedgerDomain p))
      genAddBlocks 0 c = return c
      genAddBlocks n c = do
          b <- genAddBlock c
          genAddBlocks (n-1) (Chain.addBlock b c)


  singShrink _ (TestChainFork common l r) =
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
      extensionFragment :: Chain (Block dom p) -> Chain (Block dom p) -> [Block dom p]
      extensionFragment c = reverse . L.drop (Chain.length c) . Chain.toOldestFirst

prop_arbitrary_TestChainFork :: TestChainFork :-> Bool
prop_arbitrary_TestChainFork = simpleProp $ \_ (TestChainFork c l r) ->
    Chain.valid c && Chain.valid l && Chain.valid r
 && c `Chain.isPrefixOf` l
 && c `Chain.isPrefixOf` r

prop_shrink_TestChainFork :: TestChainFork :-> Bool
prop_shrink_TestChainFork = simpleProp $ \p forks ->
  and [    applyDepFn prop_arbitrary_TestChainFork p (forks' :* Nil)
        && measure forks' < mforks
      | let mforks = measure forks
      , forks' <- singShrink p forks ]
  where
    measure (TestChainFork c l r) = Chain.length c
                                  + Chain.length l
                                  + Chain.length r
