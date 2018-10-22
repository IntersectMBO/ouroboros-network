{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Test.Chain
  ( tests
  , TestBlockChainAndUpdates(..)
  , TestBlockChain(..)
  , TestChainFork(..)
  , mkRollbackPoint
  ) where

import           Block
import           Chain ( Chain (..), Point (..), ChainUpdate (..), genesisPoint)
import qualified Chain
import           Ouroboros

import Control.Monad.State
import Data.Functor.Identity
import Data.Maybe (listToMaybe)
import qualified Data.List as L

import Test.QuickCheck
import Test.QuickCheck.Property (Property(..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Test.GlobalState
import Test.ArbitrarySt

--
-- The list of all tests
--

tests :: TestTree
tests =
  testGroup "Chain"
  [ testGroup "generators"
    [ testProperty "arbitrary for TestBlockChain" $
      -- It's important we don't generate too many trivial test cases here
      -- so check the coverage to enforce it.
                   checkCoverage (withBft (prop_arbitrary_TestBlockChain @'OuroborosBFT))
    , testProperty "shrink for TestBlockChain" (withBft (prop_shrink_TestBlockChain @'OuroborosBFT))

    , testProperty "arbitrary for TestHeaderChain" (withBft $ prop_arbitrary_TestHeaderChain @'OuroborosBFT)
    , testProperty "shrink for TestHeaderChain"    (withBft $ prop_shrink_TestHeaderChain @'OuroborosBFT)

    , testProperty "arbitrary for TestAddBlock" (withBft $ prop_arbitrary_TestAddBlock @'OuroborosBFT)
    , testProperty "shrink for TestAddBlock"    (withBft $ prop_shrink_TestAddBlock @'OuroborosBFT)

    , testProperty "arbitrary for TestBlockChainAndUpdates" $
      -- Same deal here applies here with generating trivial test cases.
                   checkCoverage (withBft $ prop_arbitrary_TestBlockChainAndUpdates @'OuroborosBFT)

    , testProperty "arbitrary for TestChainAndPoint" (withBft $ prop_arbitrary_TestChainAndPoint @'OuroborosBFT)
    , testProperty "shrink for TestChainAndPoint"    (withBft $ prop_shrink_TestChainAndPoint @'OuroborosBFT)

    , testProperty "arbitrary for TestChainFork" (withBft $ prop_arbitrary_TestChainFork @'OuroborosBFT)
    , testProperty "shrink for TestChainFork"
                               (mapSize (min 40) (withBft $ prop_shrink_TestChainFork @'OuroborosBFT))
    ]

  , testProperty "length/Genesis"  prop_length_genesis
  , testProperty "drop/Genesis"    (withBft (prop_drop_genesis @'OuroborosBFT))
  , testProperty "fromList/toList" (withBft (prop_fromList_toList @'OuroborosBFT))
  , testProperty "toList/head"     (withBft (prop_toList_head @'OuroborosBFT))
  , testProperty "drop"            (withBft (prop_drop @'OuroborosBFT))
  , testProperty "addBlock"        (withBft (prop_addBlock @'OuroborosBFT))
  , testProperty "rollback"        (withBft (prop_rollback @'OuroborosBFT))
  , testProperty "rollback/head"   (withBft (prop_rollback_head @'OuroborosBFT))
  , testProperty "successorBlock"  (withBft (prop_successorBlock @'OuroborosBFT))
  , testProperty "lookupBySlot"    (withBft (prop_lookupBySlot @'OuroborosBFT))
  , testProperty "intersectChains" (withBft (prop_intersectChains @'OuroborosBFT))
  ]


--
-- Properties
--

prop_length_genesis :: Bool
prop_length_genesis = Chain.length Genesis == 0

prop_drop_genesis :: KnownOuroborosProtocol p => TestBlockChain p -> Bool
prop_drop_genesis (TestBlockChain chain) =
    Chain.drop (Chain.length chain) chain == Genesis

prop_fromList_toList :: KnownOuroborosProtocol p => TestBlockChain p -> Bool
prop_fromList_toList (TestBlockChain chain) =
    (Chain.fromList . Chain.toList) chain == chain

-- The list comes out in reverse order, most recent block at the head
prop_toList_head :: KnownOuroborosProtocol p => TestBlockChain p -> Bool
prop_toList_head (TestBlockChain chain) =
    (listToMaybe . Chain.toList) chain == Chain.head chain

prop_drop :: KnownOuroborosProtocol p => TestBlockChain p -> Bool
prop_drop (TestBlockChain chain) =
    and [ Chain.drop n chain == Chain.fromList (L.drop n blocks)
        | n <- [0..Prelude.length blocks] ]
  where
    blocks = Chain.toList chain

prop_addBlock :: KnownOuroborosProtocol p => TestAddBlock p -> Bool
prop_addBlock (TestAddBlock c b) =
    -- after adding a block, that block is at the head
    Chain.headPoint c' == Chain.blockPoint b
    -- chain is still valid
 && Chain.valid c'
    -- removing the block gives the original
 && Chain.rollback (Chain.headPoint c) c' == Just c
 && Chain.drop 1 c' == c
    -- chain is one longer
 && Chain.length c' == Chain.length c + 1
  where
    c' = Chain.addBlock b c

prop_rollback :: KnownOuroborosProtocol p => TestChainAndPoint p -> Bool
prop_rollback (TestChainAndPoint c p) =
    case Chain.rollback p c of
      Nothing -> True
      Just c' ->
        -- chain is a prefix of original
             Chain.isPrefixOf c' c
        -- chain head point is the rollback point
        && Chain.headPoint c' == p

prop_rollback_head :: KnownOuroborosProtocol p => TestBlockChain p -> Bool
prop_rollback_head (TestBlockChain c) =
    Chain.rollback (Chain.headPoint c) c == Just c

prop_successorBlock :: TestChainAndPoint p -> Property
prop_successorBlock (TestChainAndPoint c p) =
  Chain.pointOnChain p c ==>
  case Chain.successorBlock p c of
    Nothing -> Chain.headPoint c === p
    Just b  -> property $ Chain.pointOnChain (Chain.blockPoint b) c

prop_lookupBySlot :: TestChainAndPoint p -> Bool
prop_lookupBySlot (TestChainAndPoint c p) =
  case Chain.lookupBySlot c (pointSlot p) of
    Just b  -> Chain.pointOnChain (Chain.blockPoint b) c
    Nothing | p == genesisPoint -> True
            | otherwise         -> not (Chain.pointOnChain p c)

prop_intersectChains :: KnownOuroborosProtocol p => TestChainFork p -> Bool
prop_intersectChains (TestChainFork c l r) =
  case Chain.intersectChains l r of
    Nothing -> c == Genesis && L.intersect (Chain.toList l) (Chain.toList r) == []
    Just p  -> Chain.headPoint c == p
            && Chain.pointOnChain p l
            && Chain.pointOnChain p r


--
-- Generators for chains
--

-- | A test generator for a valid chain of blocks.
--
newtype TestBlockChain p = TestBlockChain (Chain (Block p))
    deriving (Eq, Show)

-- | A test generator for a valid chain of block headers.
--
newtype TestHeaderChain p = TestHeaderChain (Chain (BlockHeader p))
    deriving (Eq, Show)

instance KnownOuroborosProtocol p => ArbitrarySt p (TestBlockChain p) where
    arbitrarySt = do
        n <- lift genNonNegative
        TestBlockChain <$> genBlockChain n

    shrinkSt (TestBlockChain c) =
        [ TestBlockChain (fromListFixupBlocks c')
        | c' <- shrinkList (const []) (Chain.toList c) ]

instance KnownOuroborosProtocol p => ArbitrarySt p (TestHeaderChain p) where
    arbitrarySt = do
        n <- lift genNonNegative
        TestHeaderChain <$> genHeaderChain n

    shrinkSt (TestHeaderChain c) =
        [ TestHeaderChain (fromListFixupHeaders c')
        | c' <- shrinkList (const []) (Chain.toList c) ]

prop_arbitrary_TestBlockChain :: KnownOuroborosProtocol p => TestBlockChain p -> Property
prop_arbitrary_TestBlockChain (TestBlockChain c) =
    -- check we get some but not too many zero-length chains
    cover 95   (not (Chain.null c)) "non-null" $
    cover 1.5       (Chain.null c)  "null"     $
    Chain.valid c

prop_arbitrary_TestHeaderChain :: KnownOuroborosProtocol p => TestHeaderChain p -> Bool
prop_arbitrary_TestHeaderChain (TestHeaderChain c) = Chain.valid c

prop_shrink_TestBlockChain :: KnownOuroborosProtocol p => TestBlockChain p -> Bool
prop_shrink_TestBlockChain c =
    and [ Chain.valid c' | TestBlockChain c' <- shrinkSt c ]

prop_shrink_TestHeaderChain :: KnownOuroborosProtocol p => TestHeaderChain p -> Bool
prop_shrink_TestHeaderChain c =
    and [ Chain.valid c' | TestHeaderChain c' <- shrinkSt c ]

-- | The 'NonNegative' generator produces a large proportion of 0s, so we use
-- this one instead for now.
--
-- https://github.com/nick8325/quickcheck/issues/229
--
genNonNegative :: Gen Int
genNonNegative = (abs <$> arbitrary) `suchThat` (>= 0)

genBlockChain :: forall p. KnownOuroborosProtocol p
              => Int -> GenSt p (Chain (Block p))
genBlockChain n = do
    bodies <- lift $ vector n
    slots  <- lift $ mkSlots <$> vectorOf n genSlotGap
    mkChain slots bodies
  where
    mkSlots :: [Int] -> [Slot]
    mkSlots = map toEnum . tail . scanl (+) 0

    mkChain :: [Slot] -> [BlockBody] -> GenSt p (Chain (Block p))
    mkChain slots bodies =
        fmap (fromListFixupBlocks . reverse)
      $ zipWithM mkPartialBlock slots bodies

genSlotGap :: Gen Int
genSlotGap = frequency [(25, pure 1), (5, pure 2), (1, pure 3)]

addSlotGap :: Int -> Slot -> Slot
addSlotGap g (Slot n) = Slot (n + fromIntegral g)

genHeaderChain :: KnownOuroborosProtocol p => Int -> GenSt p (Chain (BlockHeader p))
genHeaderChain = fmap (fmap blockHeader) . genBlockChain

mkPartialBlock :: (KnownOuroborosProtocol p, MonadState (GlobalState p) m)
               => Slot -> BlockBody -> m (Block p)
mkPartialBlock sl body = do
    fmap runIdentity $ forLeaders sl pickOne $ \isLeader -> do
      ouroborosPayload <- mkOuroborosPayload isLeader
      return Block {
          blockHeader = BlockHeader {
            headerSlot      = sl,
            headerOuroboros = ouroborosPayload,
            headerHash      = partialField "headerHash",
            headerPrevHash  = partialField "headerPrevHash",
            headerBlockNo   = partialField "headerBlockNo",
            headerBodyHash  = hashBody body
          }
        , blockBody = body
        }
  where
    -- TODO: Deal with the case where we might have multiple or no leaders
    pickOne :: [(NodeId, ProofIsLeader p)] -> Identity (NodeId, ProofIsLeader p)
    pickOne = Identity . head

    partialField n = error ("mkPartialBlock: you didn't fill in field " ++ n)



-- | To help with chain construction and shrinking it's handy to recalculate
-- all the hashes.
--
fromListFixupBlocks :: KnownOuroborosProtocol p => [Block p] -> Chain (Block p)
fromListFixupBlocks []      = Genesis
fromListFixupBlocks (b : c) = c' :> b'
  where
    c' = fromListFixupBlocks c
    b' = Chain.fixupBlock c' b

fromListFixupHeaders :: KnownOuroborosProtocol p => [BlockHeader p] -> Chain (BlockHeader p)
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
data TestAddBlock p = TestAddBlock (Chain (Block p)) (Block p)
  deriving Show

instance KnownOuroborosProtocol p => ArbitrarySt p (TestAddBlock p) where
  arbitrarySt = do
    TestBlockChain chain <- arbitrarySt
    block <- genAddBlock chain
    return (TestAddBlock chain block)

  shrinkSt (TestAddBlock c b) =
    [ TestAddBlock c' b'
    | TestBlockChain c' <- shrinkSt (TestBlockChain c)
    , let b' = Chain.fixupBlock c' b
    ]

genAddBlock :: (KnownOuroborosProtocol p, HasHeader block)
            => Chain (block p) -> GenSt p (Block p)
genAddBlock chain = do
    slotGap <- lift $ genSlotGap
    body    <- lift $ arbitrary
    pb      <- mkPartialBlock (addSlotGap slotGap (Chain.headSlot chain)) body
    let b = Chain.fixupBlock chain pb
    return b

prop_arbitrary_TestAddBlock :: KnownOuroborosProtocol p => TestAddBlock p -> Bool
prop_arbitrary_TestAddBlock (TestAddBlock c b) = Chain.valid (c :> b)


prop_shrink_TestAddBlock :: KnownOuroborosProtocol p => TestAddBlock p -> Bool
prop_shrink_TestAddBlock t =
    and [ Chain.valid (c :> b) | TestAddBlock c b <- shrinkSt t ]

--
-- Generator for chain updates
--

-- | A test generator for a chain and a sequence of updates that can be applied
-- to it.
--
data TestBlockChainAndUpdates p =
       TestBlockChainAndUpdates (Chain (Block p)) [ChainUpdate (Block p)]
  deriving Show

instance KnownOuroborosProtocol p => ArbitrarySt p (TestBlockChainAndUpdates p) where
  arbitrarySt = do
    TestBlockChain chain <- arbitrarySt
    m <- lift genNonNegative
    updates <- genChainUpdates chain m
    return (TestBlockChainAndUpdates chain updates)

-- | Like 'frequency', but it works in the 'State' monad.
-- TODO: Is this implementation correct? Are we resetting the state of the
-- generator this way?
frequencySt :: [(Int, StateT s Gen a)] -> StateT s Gen a
frequencySt xs = StateT $ \s -> do
    lowered <- forM xs $ \(n,gen) -> return (n, evalStateT gen s)
    a <- frequency lowered
    return (a, s)

{-- \"Unrolled\" version, for reference.
frequencySt :: [(Int, StateT s Gen a)] -> StateT s Gen a
frequencySt [] = error "frequencySt used with empty list"
frequencySt xs0 = do
    n <- lift (choose (1, tot))
    pick n xs0
 where
  tot :: Int
  tot = sum (map fst xs0)

  pick :: Int -> [(Int, StateT s Gen a)] -> StateT s Gen a
  pick n ((k,x):xs)
    | n <= k    = x
    | otherwise = pick (n-k) xs
  pick _ _  = error "pick used with empty list"
--}

genChainUpdate :: KnownOuroborosProtocol p => Chain (Block p) -> GenSt p (ChainUpdate (Block p))
genChainUpdate chain =
    frequencySt $
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

genChainUpdates :: KnownOuroborosProtocol p 
                => Chain (Block p) 
                -> Int 
                -> GenSt p [ChainUpdate (Block p)]
genChainUpdates _     0 = return []
genChainUpdates chain n = do
    update  <- genChainUpdate chain
    let Just chain' = Chain.applyChainUpdate update chain
    updates <- genChainUpdates chain' (n-1)
    return (update : updates)

prop_arbitrary_TestBlockChainAndUpdates :: KnownOuroborosProtocol p => TestBlockChainAndUpdates p -> Property
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
countChainUpdateNetProgress :: (KnownOuroborosProtocol p, HasHeader block)
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
data TestChainAndPoint p = TestChainAndPoint (Chain (Block p)) Point
  deriving Show

instance KnownOuroborosProtocol p => ArbitrarySt p (TestChainAndPoint p) where
  arbitrarySt = do
    TestBlockChain chain <- arbitrarySt
    let len = Chain.length chain
    -- either choose point from the chain
    point <- frequencySt
      [ (2, return (Chain.headPoint chain))
      , (2, return (mkRollbackPoint chain len))
      , (8, mkRollbackPoint chain <$> lift (choose (1, len - 1)))
      -- or a few off the chain!
      , (1, lift genPoint)
      ]
    return (TestChainAndPoint chain point)

  shrinkSt (TestChainAndPoint c p)
    | Chain.pointOnChain p c
    = [ TestChainAndPoint c' (fixupPoint c' p)
    | TestBlockChain c' <- shrinkSt (TestBlockChain c)]
    | otherwise
    = [ TestChainAndPoint c' p
      | TestBlockChain c' <- shrinkSt (TestBlockChain c) ]

genPoint :: Gen Point
genPoint = (\s h -> Point (Slot s) (HeaderHash h)) <$> arbitrary <*> arbitrary

fixupPoint :: HasHeader block => Chain (block p) -> Point -> Point
fixupPoint c p =
  case Chain.lookupBySlot c (pointSlot p) of
    Just b  -> Chain.blockPoint b
    Nothing -> Chain.headPoint c

prop_arbitrary_TestChainAndPoint :: KnownOuroborosProtocol p => TestChainAndPoint p -> Property
prop_arbitrary_TestChainAndPoint (TestChainAndPoint c p) =
  cover (85/100) onChain       "point on chain" $
  cover ( 5/100) (not onChain) "point not on chain" $
    Chain.valid c
  where
    onChain = Chain.pointOnChain p c

prop_shrink_TestChainAndPoint :: KnownOuroborosProtocol p => TestChainAndPoint p -> Bool
prop_shrink_TestChainAndPoint cp@(TestChainAndPoint c _) =
  and [ Chain.valid c' && (not (Chain.pointOnChain p c) || Chain.pointOnChain p c')
      | TestChainAndPoint c' p <- shrinkSt cp ]

--
-- Generator for chain forks sharing a common prefix
--

-- | A test generator for two chains sharing a common prefix.
--
data TestChainFork p = TestChainFork (Chain (Block p)) -- common prefix
                                     (Chain (Block p)) -- left fork
                                     (Chain (Block p)) -- right fork

instance KnownOuroborosProtocol p => Show (TestChainFork p) where
  show (TestChainFork c f1 f2)
    = let nl  = "\n    "
          nnl = "\n" ++ nl
      in "TestChainFork" ++ nl ++
      Chain.prettyPrintChain nl show c  ++ nnl ++
      Chain.prettyPrintChain nl show f1 ++ nnl ++
      Chain.prettyPrintChain nl show f2

instance KnownOuroborosProtocol p => ArbitrarySt p (TestChainFork p) where
  arbitrarySt = do
    TestBlockChain chain <- arbitrarySt
    -- at least 5% of forks should be equal
    equalChains <- frequencySt [(1, pure True), (19, pure False)]
    if equalChains
      then return (TestChainFork chain chain chain)
      else do
        l <- lift genNonNegative
        r <- lift genNonNegative
        chainL <- genAddBlocks l chain
        chainR <- genAddBlocks r chain
        return (TestChainFork chain chainL chainR)

    where
      genAddBlocks :: Int -> Chain (Block p) -> GenSt p (Chain (Block p))
      genAddBlocks 0 c = return c
      genAddBlocks n c = do
          b <- genAddBlock c
          genAddBlocks (n-1) (Chain.addBlock b c)


  shrinkSt (TestChainFork common l r) =
        -- shrink the common prefix
      [ TestChainFork (fromListFixupBlocks common')
                      (fromListFixupBlocks (exl ++ common'))
                      (fromListFixupBlocks (exr ++ common'))
      | let exl = extensionFragment common l
            exr = extensionFragment common r
      , common' <- shrinkList (const []) (Chain.toList common)
      ]
        -- shrink the left fork
   ++ [ TestChainFork common l' r
      | let exl = extensionFragment common l
      , exl' <- shrinkList (const []) exl
      , let l' = fromListFixupBlocks (exl' ++ Chain.toList common)
      ]
        -- shrink the right fork
   ++ [ TestChainFork common l r'
      | let exr = extensionFragment common r
      , exr' <- shrinkList (const []) exr
      , let r' = fromListFixupBlocks (exr' ++ Chain.toList common)
      ]
    where
      extensionFragment :: Chain (Block p) -> Chain (Block p) -> [Block p]
      extensionFragment c = reverse . L.drop (Chain.length c) . reverse . Chain.toList

prop_arbitrary_TestChainFork :: KnownOuroborosProtocol p => TestChainFork p -> Bool
prop_arbitrary_TestChainFork (TestChainFork c l r) =
    Chain.valid c && Chain.valid l && Chain.valid r
 && c `Chain.isPrefixOf` l
 && c `Chain.isPrefixOf` r


prop_shrink_TestChainFork :: KnownOuroborosProtocol p => TestChainFork p -> Bool
prop_shrink_TestChainFork forks =
  and [    prop_arbitrary_TestChainFork forks'
        && measure forks' < mforks
      | let mforks = measure forks
      , forks' <- shrinkSt forks ]
  where
    measure (TestChainFork c l r) = Chain.length c
                                  + Chain.length l
                                  + Chain.length r
