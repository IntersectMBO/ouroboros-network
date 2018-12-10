{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE TypeOperators    #-}

module Test.ChainFragment
  ( tests
  , TestBlockChainFragmentAndUpdates(..)
  , TestBlockChainFragment(..)
  , TestChainFragmentFork(..)
  , mkRollbackPoint
  , genBlockChainFragment
  , genHeaderChainFragment
  ) where

import qualified Data.List as L
import           Data.Maybe (listToMaybe, maybeToList, maybe, fromMaybe, fromJust)

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Test.Chain (genNonNegative, genSlotGap, addSlotGap,
                             mkPartialBlock, genPoint)
import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain (ChainUpdate(..), Point(..))
import           Ouroboros.Network.ChainFragment (ChainFragment,
                                                  pattern (:>), pattern Empty)
import qualified Ouroboros.Network.ChainFragment as CF
import           Ouroboros.Network.Serialise (prop_serialise)
import           Ouroboros.Network.Testing.ConcreteBlock


--
-- The list of all tests
--

tests :: TestTree
tests = testGroup "ChainFragment"
  [ testGroup "generators"
    [ testProperty "arbitrary for TestBlockChainFragment" $
      -- It's important we don't generate too many trivial test cases here
      -- so check the coverage to enforce it.
      checkCoverage prop_arbitrary_TestBlockChainFragment
    , testProperty "shrink for TestBlockChainFragment"       prop_shrink_TestBlockChainFragment

    , testProperty "arbitrary for TestHeaderChainFragment"   prop_arbitrary_TestHeaderChainFragment
    , testProperty "shrink for TestHeaderChainFragment"      prop_shrink_TestHeaderChainFragment

    , testProperty "arbitrary for TestAddBlock"              prop_arbitrary_TestAddBlock
    , testProperty "shrink for TestAddBlock"                 prop_shrink_TestAddBlock

    , testProperty "arbitrary for TestBlockChainFragmentAndUpdates" $
      -- Same deal here applies here with generating trivial test cases.
      checkCoverage prop_arbitrary_TestBlockChainFragmentAndUpdates

    , testProperty "arbitrary for TestChainFragmentAndPoint" prop_arbitrary_TestChainFragmentAndPoint
    , testProperty "shrink for TestChainFragmentAndPoint"    prop_shrink_TestChainFragmentAndPoint

    , testProperty "arbitrary for TestChainFragmentFork" $
      checkCoverage prop_arbitrary_TestChainFragmentFork_cover
    , testProperty "shrink for TestChainFragmentFork" $
      mapSize (min 40) prop_shrink_TestChainFragmentFork
    , testProperty "arbitrary for TestChainFragmentAndIndex" prop_arbitrary_TestChainFragmentAndIndex
    ]

  , testProperty "length/Empty"                              prop_length_Empty
  , testProperty "dropNewest/Empty"                          prop_dropNewest_Empty
  , testProperty "dropOldest/Empty"                          prop_dropOldest_Empty
  , testProperty "fromNewestFirst/toNewestFirst"             prop_fromNewestFirst_toNewestFirst
  , testProperty "fromOldestFirst/toOldestFirst"             prop_fromOldestFirst_toOldestFirst
  , testProperty "toList/head"                               prop_toList_head
  , testProperty "dropNewest"                                prop_dropNewest
  , testProperty "dropOldest"                                prop_dropOldest
  , testProperty "addBlock"                                  prop_addBlock
  , testProperty "rollback"                                  prop_rollback
  , testProperty "rollback/head"                             prop_rollback_head
  , testProperty "successorBlock"                            prop_successorBlock
  , testProperty "lookupBySlot"                              prop_lookupBySlot
  , testProperty "intersectChainFragments"                   prop_intersectChainFragments
  , testProperty "serialise chain"                           prop_serialise_chain
  , testProperty "slotOnChainFragment"                       prop_slotOnChainFragment
  , testProperty "pointOnChainFragment"                      prop_pointOnChainFragment
  , testProperty "lookupByIndexFromEnd"                      prop_lookupByIndexFromEnd
  , testProperty "selectPoints"                              prop_selectPoints
  , testGroup "splitAfterSlot"
    [ testProperty "splitAfterSlot join"                     prop_splitAfterSlot_join
    , testProperty "splitAfter pointOnChainFragment"         prop_splitAfterSlot_pointOnChainFragment
    , testProperty "splitAfter slots"                        prop_splitAfterSlot_slots
    ]
  , testProperty "foldChainFragment"                         prop_foldChainFragment
  ]

--
-- Properties
--

prop_length_Empty :: Bool
prop_length_Empty = CF.length (Empty :: ChainFragment Block) == 0

prop_dropNewest_Empty :: TestBlockChainFragment -> Bool
prop_dropNewest_Empty (TestBlockChainFragment chain) =
    CF.dropNewest (CF.length chain) chain == Empty

prop_dropOldest_Empty :: TestBlockChainFragment -> Bool
prop_dropOldest_Empty (TestBlockChainFragment chain) =
    CF.dropOldest (CF.length chain) chain == Empty

prop_fromNewestFirst_toNewestFirst :: TestBlockChainFragment -> Bool
prop_fromNewestFirst_toNewestFirst (TestBlockChainFragment chain) =
    (CF.fromNewestFirst . CF.toNewestFirst) chain == chain

prop_fromOldestFirst_toOldestFirst :: TestBlockChainFragment -> Bool
prop_fromOldestFirst_toOldestFirst (TestBlockChainFragment chain) =
    (CF.fromOldestFirst . CF.toOldestFirst) chain == chain

-- The list comes out in reverse order, most recent block at the head
prop_toList_head :: TestBlockChainFragment -> Bool
prop_toList_head (TestBlockChainFragment chain) =
    (listToMaybe . CF.toNewestFirst) chain == CF.head chain

prop_dropNewest :: TestBlockChainFragment -> Bool
prop_dropNewest (TestBlockChainFragment chain) =
    let blocks = CF.toNewestFirst chain in
    and [ CF.dropNewest n chain == CF.fromNewestFirst (L.drop n blocks)
        | n <- [0..Prelude.length blocks] ]

prop_dropOldest :: TestBlockChainFragment -> Bool
prop_dropOldest (TestBlockChainFragment chain) =
    let blocks = CF.toOldestFirst chain in
    and [ CF.dropOldest n chain == CF.fromOldestFirst (L.drop n blocks)
        | n <- [0..Prelude.length blocks] ]

prop_addBlock :: TestAddBlock -> Bool
prop_addBlock (TestAddBlock c b) =
  let c' = CF.addBlock b c
  in case CF.headPoint c' of
    Nothing -> False
    Just headPnt' ->
      -- after adding a block, that block is at the head
         headPnt' == CF.blockPoint b
      -- chain is still valid
      && CF.valid c'
      -- removing the block gives the original
      && case CF.headPoint c of
           Nothing -> True
           Just headPnt -> CF.rollback headPnt c' == Just c
      && CF.dropNewest 1 c' == c
      -- chain is one longer
      && CF.length c' == CF.length c + 1

prop_rollback :: TestChainFragmentAndPoint -> Bool
prop_rollback (TestChainFragmentAndPoint c p) =
    case CF.rollback p c of
      Nothing -> not (CF.pointOnChainFragment p c)
      Just c' ->
        -- chain is a prefix of original
           CF.isPrefixOf c' c
        -- chain head point is the rollback point
        && CF.headPoint c' == Just p

prop_rollback_head :: TestBlockChainFragment -> Bool
prop_rollback_head (TestBlockChainFragment c) =
  case CF.headPoint c of
    Nothing      -> True
    Just headPnt -> CF.rollback headPnt c == Just c

prop_successorBlock :: TestChainFragmentAndPoint -> Property
prop_successorBlock (TestChainFragmentAndPoint c p) =
  CF.pointOnChainFragment p c ==>
  case CF.successorBlock p c of
    Nothing -> CF.headPoint c === Just p
    Just b  -> property $ CF.pointOnChainFragment (CF.blockPoint b) c
          .&&. blockPrevHash b === pointHash p

prop_lookupBySlot :: TestChainFragmentAndPoint -> Bool
prop_lookupBySlot (TestChainFragmentAndPoint c p) =
  let slot = pointSlot p in
  case CF.lookupBySlot c slot of
    Just b  -> CF.slotOnChainFragment (blockSlot b) c
            && CF.blockSlot b == slot
    Nothing -> not (CF.slotOnChainFragment slot c)

prop_intersectChainFragments :: TestChainFragmentFork -> Bool
prop_intersectChainFragments (TestChainFragmentFork origL1 origL2 c1 c2) =
  case CF.intersectChainFragments c1 c2 of
    Nothing -> (CF.null origL1 || CF.null origL2)
            && L.intersect (CF.toNewestFirst c1) (CF.toNewestFirst c2) == []
    Just (l1, l2, r1, r2) ->
      l1 == origL1 && l2 == origL2 &&
      CF.headPoint l1 == CF.headPoint l2 &&
      CF.toNewestFirst r1 ++ CF.toNewestFirst l1 == CF.toNewestFirst c1 &&
      CF.toNewestFirst r2 ++ CF.toNewestFirst l2 == CF.toNewestFirst c2


prop_serialise_chain :: TestBlockChainFragment -> Property
prop_serialise_chain (TestBlockChainFragment chain) =
  prop_serialise chain

prop_slotOnChainFragment :: TestChainFragmentAndPoint -> Bool
prop_slotOnChainFragment (TestChainFragmentAndPoint c p) =
  let slot = pointSlot p in
  CF.slotOnChainFragment slot c == CF.slotOnChainFragmentSpec slot c

prop_pointOnChainFragment :: TestChainFragmentAndPoint -> Bool
prop_pointOnChainFragment (TestChainFragmentAndPoint c p) =
  CF.pointOnChainFragment p c == CF.pointOnChainFragmentSpec p c

prop_splitAfterSlot_join :: TestChainFragmentAndPoint -> Property
prop_splitAfterSlot_join (TestChainFragmentAndPoint c p) =
  case CF.splitAfterSlot c (pointSlot p) of
    Just (l, r) -> CF.joinChainFragments l r === Just c
    Nothing     -> property $ not (CF.pointOnChainFragment p c)

prop_splitAfterSlot_pointOnChainFragment :: TestChainFragmentAndPoint -> Bool
prop_splitAfterSlot_pointOnChainFragment (TestChainFragmentAndPoint c p) =
  case CF.splitAfterSlot c (pointSlot p) of
    Just (l, r) -> not (CF.pointOnChainFragment p r)
                && CF.joinChainFragments l r == Just c
                && CF.headSlot l == Just (pointSlot p)
                && if CF.pointOnChainFragment p c
                   then CF.pointOnChainFragment p l
                   else True
    Nothing     -> not (CF.pointOnChainFragment p c)

prop_splitAfterSlot_slots :: TestChainFragmentAndPoint -> Bool
prop_splitAfterSlot_slots (TestChainFragmentAndPoint c p) =
  case CF.splitAfterSlot c (pointSlot p) of
    Just (l, r) -> all (<= pointSlot p) (slots l)
                && all (>  pointSlot p) (slots r)
    Nothing     -> not (CF.pointOnChainFragment p c)
  where
    slots :: ChainFragment Block -> [Slot]
    slots = map blockSlot . CF.toOldestFirst

prop_foldChainFragment :: TestBlockChainFragment -> Property
prop_foldChainFragment (TestBlockChainFragment c) =
    CF.foldChainFragment f [] c === CF.foldChainFragmentSpec f [] c
  where
    f l block = getSlot (blockSlot block) : l

prop_lookupByIndexFromEnd :: TestChainFragmentAndIndex -> Property
prop_lookupByIndexFromEnd (TestChainFragmentAndIndex c i) =
  case CF.lookupByIndexFromEnd c i of
    CF.Position _ b _  -> b === CF.toNewestFirst c !! i
    _                  -> property (i < 0 || i >= CF.length c)

prop_selectPoints :: TestBlockChainFragment -> Property
prop_selectPoints (TestBlockChainFragment c) =
    CF.selectPoints offsets c === CF.selectPointsSpec offsets c .&&.
    CF.selectPoints []      c === CF.selectPointsSpec []      c .&&.
    CF.selectPoints [1,1]   c === CF.selectPointsSpec [1,1]   c
  where
    offsets = [0,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584]


--
-- Generators for chains
--

-- | A test generator for a valid chain of blocks.
--
newtype TestBlockChainFragment = TestBlockChainFragment { getTestBlockChainFragment :: ChainFragment Block }
    deriving (Eq, Show)

-- | A test generator for a valid chain of block headers.
--
newtype TestHeaderChainFragment = TestHeaderChainFragment (ChainFragment BlockHeader)
    deriving (Eq, Show)

instance Arbitrary TestBlockChainFragment where
    arbitrary = do
        n <- genNonNegative
        TestBlockChainFragment <$> genBlockChainFragment n

    shrink (TestBlockChainFragment c) =
        [ TestBlockChainFragment (fromListFixupBlocks c')
        | c' <- shrinkList (const []) (CF.toNewestFirst c) ]

instance Arbitrary TestHeaderChainFragment where
    arbitrary = do
        n <- genNonNegative
        TestHeaderChainFragment <$> genHeaderChainFragment n

    shrink (TestHeaderChainFragment c) =
        [ TestHeaderChainFragment (fromListFixupHeaders c')
        | c' <- shrinkList (const []) (CF.toNewestFirst c) ]

prop_arbitrary_TestBlockChainFragment :: TestBlockChainFragment -> Property
prop_arbitrary_TestBlockChainFragment (TestBlockChainFragment c) =
    -- check we get some but not too many zero-length chains
    cover 95   (not (CF.null c)) "non-null" $
    cover 1.5       (CF.null c)  "null"     $
    CF.valid c

prop_arbitrary_TestHeaderChainFragment :: TestHeaderChainFragment -> Bool
prop_arbitrary_TestHeaderChainFragment (TestHeaderChainFragment c) =
    CF.valid c

prop_shrink_TestBlockChainFragment :: TestBlockChainFragment -> Bool
prop_shrink_TestBlockChainFragment c =
    and [ CF.valid c' | TestBlockChainFragment c' <- shrink c ]

prop_shrink_TestHeaderChainFragment :: TestHeaderChainFragment -> Bool
prop_shrink_TestHeaderChainFragment c =
    and [ CF.valid c' | TestHeaderChainFragment c' <- shrink c ]

genBlockChainFragment :: Int -> Gen (ChainFragment Block)
genBlockChainFragment n = do
    bodies <- vector n
    slots  <- mkSlots <$> vectorOf n genSlotGap
    return (mkChainFragment slots bodies)
  where
    mkSlots :: [Int] -> [Slot]
    mkSlots = map toEnum . tail . scanl (+) 0

    mkChainFragment :: [Slot] -> [BlockBody] -> ChainFragment Block
    mkChainFragment slots bodies =
        fromListFixupBlocks
      . reverse
      $ zipWith mkPartialBlock slots bodies

genHeaderChainFragment :: Int -> Gen (ChainFragment BlockHeader)
genHeaderChainFragment = fmap (CF.mapChainFragment blockHeader) . genBlockChainFragment

-- | To help with chain construction and shrinking it's handy to recalculate
-- all the hashes.
--
fromListFixupBlocks :: [Block] -> ChainFragment Block
fromListFixupBlocks []      = Empty
fromListFixupBlocks (b : c) = c' :> b'
  where
    c' = fromListFixupBlocks c
    b' = fixupBlockCF c' b

fromListFixupHeaders :: [BlockHeader] -> ChainFragment BlockHeader
fromListFixupHeaders []      = Empty
fromListFixupHeaders (b : c) = c' :> b'
  where
    c' = fromListFixupHeaders c
    b' = fixupBlockHeaderCF c' (headerBodyHash b) b

-- | The Ouroboros K paramater. This is also the maximum rollback length.
--
k :: Int
k = 5

--
-- Generator for chain and single block
--

-- | A test generator for a chain and a block that can be appended to it.
--
data TestAddBlock = TestAddBlock (ChainFragment Block) Block
  deriving Show

instance Arbitrary TestAddBlock where
  arbitrary = do
    TestBlockChainFragment chain <- arbitrary
    block <- genAddBlock chain
    return (TestAddBlock chain block)

  shrink (TestAddBlock c b) =
    [ TestAddBlock c' b'
    | TestBlockChainFragment c' <- shrink (TestBlockChainFragment c)
    , let b' = fixupBlockCF c' b
    ]

genAddBlock :: (HasHeader block, HeaderHash block ~ ConcreteHeaderHash)
            => ChainFragment block -> Gen Block
genAddBlock chain = do
    slotGap <- genSlotGap
    body    <- arbitrary
    let pb = mkPartialBlock (addSlotGap slotGap
                                        (fromMaybe (Slot 0) $ CF.headSlot chain))
                                        body
        b  = fixupBlockCF chain pb
    return b

prop_arbitrary_TestAddBlock :: TestAddBlock -> Bool
prop_arbitrary_TestAddBlock (TestAddBlock c b) =
    CF.valid (c :> b)

prop_shrink_TestAddBlock :: TestAddBlock -> Bool
prop_shrink_TestAddBlock t =
    and [ CF.valid (c :> b) | TestAddBlock c b <- shrink t ]

--
-- Generator for chain updates
--

-- | A test generator for a chain and a sequence of updates that can be applied
-- to it.
--
data TestBlockChainFragmentAndUpdates =
       TestBlockChainFragmentAndUpdates (ChainFragment Block) [ChainUpdate Block]
  deriving Show

instance Arbitrary TestBlockChainFragmentAndUpdates where
  arbitrary = do
    TestBlockChainFragment chain <- arbitrary
    m <- genNonNegative
    updates <- genChainFragmentUpdates chain m
    return (TestBlockChainFragmentAndUpdates chain updates)

genChainFragmentUpdate :: ChainFragment Block
               -> Gen (ChainUpdate Block)
genChainFragmentUpdate chain =
    frequency $
      -- To ensure we make progress on average w must ensure the weight of
      -- adding one block is more than the expected rollback length. If we
      -- used expectedRollbackLength then we would on average make no
      -- progress. We slightly arbitrarily weight 2:1 for forward progress.
      [ (expectedRollbackLength * 2, AddBlock <$> genAddBlock chain) ]
   ++ L.take (CF.length chain)
        [ (freq, pure (RollBack (fromJust (mkRollbackPoint chain len))))
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

mkRollbackPoint :: HasHeader block
                => ChainFragment block -> Int -> Maybe (Point block)
mkRollbackPoint chain n = CF.headPoint $ CF.dropNewest n chain

genChainFragmentUpdates :: ChainFragment Block
                -> Int
                -> Gen [ChainUpdate Block]
genChainFragmentUpdates _     0 = return []
genChainFragmentUpdates chain n = do
    update  <- genChainFragmentUpdate chain
    let chain' = fromMaybe chain $ CF.applyChainUpdate update chain
                 -- If applying the update failed, just continue with the
                 -- original chain fragment
    updates <- genChainFragmentUpdates chain' (n-1)
    return (update : updates)

prop_arbitrary_TestBlockChainFragmentAndUpdates :: TestBlockChainFragmentAndUpdates -> Property
prop_arbitrary_TestBlockChainFragmentAndUpdates (TestBlockChainFragmentAndUpdates c us) =
    cover 1.5 (     null us ) "empty updates"     $
    cover 95  (not (null us)) "non-empty updates" $
    tabulate "ChainFragmentUpdate" (map updateKind us) $
    tabulate "Growth" [hist (countChainFragmentUpdateNetProgress c us)] $

    CF.valid c
 && case CF.applyChainUpdates us c of
      Nothing -> False
      Just c' -> CF.valid c'
  where
    hist n = show lower ++ " to " ++ show upper
      where
        lower = (n `div` 10)     * 10
        upper = (n `div` 10 + 1) * 10 - 1

    updateKind AddBlock{} = "AddBlock"
    updateKind RollBack{} = "RollBack"

-- | Count the number of blocks forward - the number of blocks backward.
--
countChainFragmentUpdateNetProgress
  :: HasHeader block
  => ChainFragment block
  -> [ChainUpdate block]
  -> Int
countChainFragmentUpdateNetProgress = go 0
  where
    go n _c []     = n
    go n c  (u:us) = go n' c' us
      where
        Just c' = CF.applyChainUpdate u c
        n'      = n + (maybe 0 fromEnum $ CF.headBlockNo c')
                    - (maybe 0 fromEnum $ CF.headBlockNo c)

--
-- Generator for chain and single point on the chain
--

-- | A test generator for a chain and a points. In most cases the point is
-- on the chain, but it also covers at least 5% of cases where the point is
-- not on the chain.
--
data TestChainFragmentAndPoint = TestChainFragmentAndPoint (ChainFragment Block) (Point Block)
  deriving Show

instance Arbitrary TestChainFragmentAndPoint where
  arbitrary = do
    TestBlockChainFragment chain <- arbitrary
    let len = CF.length chain
    -- either choose point from the chain
    mbPoint <- frequency
      [ (2, return (CF.headPoint chain))
      , (8, mkRollbackPoint chain <$> choose (1, len - 1))
      -- or a few off the chain!
      , (1, Just <$> genPoint)
      ]
    -- If 'mbPoint' yielded Nothing, just generate one off the chain fragment,
    -- as the chain fragment is probably empty anyway.
    point <- case mbPoint of
               Just p  -> return p
               Nothing -> genPoint
    return (TestChainFragmentAndPoint chain point)

  shrink (TestChainFragmentAndPoint c p)
    | CF.pointOnChainFragment p c
    = [ TestChainFragmentAndPoint c' p'
    | TestBlockChainFragment c' <- shrink (TestBlockChainFragment c)
    , p' <- maybeToList $ fixupPoint c' p ]
    | otherwise
    = [ TestChainFragmentAndPoint c' p
      | TestBlockChainFragment c' <- shrink (TestBlockChainFragment c) ]

fixupPoint :: HasHeader block
           => ChainFragment block -> Point block -> Maybe (Point block)
fixupPoint c p =
  case CF.lookupBySlot c (pointSlot p) of
    Just b  -> Just (CF.blockPoint b)
    Nothing -> CF.headPoint c

prop_arbitrary_TestChainFragmentAndPoint :: TestChainFragmentAndPoint -> Property
prop_arbitrary_TestChainFragmentAndPoint (TestChainFragmentAndPoint c p) =
  let onChainFragment = CF.pointOnChainFragment p c in
  cover (85/100) onChainFragment       "point on chain" $
  cover ( 5/100) (not onChainFragment) "point not on chain" $
    CF.valid c

prop_shrink_TestChainFragmentAndPoint :: TestChainFragmentAndPoint -> Bool
prop_shrink_TestChainFragmentAndPoint cp@(TestChainFragmentAndPoint c _) =
  and [ CF.valid c' && (not (CF.pointOnChainFragment p c) || CF.pointOnChainFragment p c')
      | TestChainFragmentAndPoint c' p <- shrink cp ]


--
-- Generator for chain forks
--

-- | A test generator for two chain fragments that share some blocks (exactly
-- matching), but one of the two chain fragments can have another prefix (they
-- cannot each have a different prefix). The two chains fragmenst can have
-- different suffixes, i.e. they are forks.
--
-- For example: start with a whole chain, then take a fragment of that chain.
-- Take a fragment of that fragment to obtain a second chain fragment. The two
-- fragments will share identical blocks, but one fragment may contain more
-- than the other. Now add some unique blocks to each fragment to obtain two
-- forks. These are the four fragments generated 'TestChainFragmentFork'.
--
data TestChainFragmentFork
    = TestChainFragmentFork
      (ChainFragment Block) -- first  chain fragment
      (ChainFragment Block) -- second chain fragment
      (ChainFragment Block) -- first  fork (includes the first fragment)
      (ChainFragment Block) -- second fork (includes the second fragment)

instance Show TestChainFragmentFork where
  show (TestChainFragmentFork l1 l2 c1 c2)
    = let nl  = "\n    "
          nnl = "\n" ++ nl
      in "TestChainFragmentFork" ++ nl ++
      CF.prettyPrintChainFragment nl show l1  ++ nnl ++
      CF.prettyPrintChainFragment nl show l2  ++ nnl ++
      CF.prettyPrintChainFragment nl show c1 ++ nnl ++
      CF.prettyPrintChainFragment nl show c2

instance Arbitrary TestChainFragmentFork where
  arbitrary = do
    TestBlockChainFragment c <- arbitrary
    -- at least 50% should have the same prefix
    samePrefixes <- oneof [pure True, pure False]
    (l1, l2) <-
      if samePrefixes
      then return (c, c)
      else do
        let len = CF.length c
        drop1 <- choose (1, len - 1)
        drop2 <- choose (1, len - 1)
        return (CF.dropOldest drop1 c, CF.dropOldest drop2 c)
    -- at least 5% of forks should be equal
    sameForks <- frequency [(1, pure True), (19, pure False)]
    (c1, c2) <-
      if sameForks
      then return (l1, l2)
      else do
        n1 <- genNonNegative
        n2 <- genNonNegative
        c1 <- genAddBlocks n1 l1
        c2 <- genAddBlocks n2 l2
        return (c1, c2)
    return (TestChainFragmentFork l1 l2 c1 c2)
    where
      genAddBlocks :: Int
                   -> ChainFragment Block
                   -> Gen (ChainFragment Block)
      genAddBlocks 0 c = return c
      genAddBlocks n c = do
          b <- genAddBlock c
          genAddBlocks (n-1) (CF.addBlock b c)

  shrink (TestChainFragmentFork l1 l2 c1 c2) =
    -- shrink the prefixes
    let (longestPrefix, shortestPrefix) =
          -- shrink the longest prefix of the two, then drop some of its
          -- oldest blocks to obtain a new 'shortest prefix'.
          if CF.length l1 > CF.length l2
          then (l1, l2) else (l2, l1)
        toDrop = CF.length longestPrefix - CF.length shortestPrefix
        ex1 = extensionFragment c1 l1
        ex2 = extensionFragment c2 l2 in
    [ TestChainFragmentFork
        (fromListFixupBlocks longestPrefix')
        (fromListFixupBlocks shortestPrefix')
        (fromListFixupBlocks (ex1 ++ longestPrefix'))
        (fromListFixupBlocks (ex2 ++ shortestPrefix'))
    | longestPrefix' <- shrinkList (const []) (CF.toNewestFirst longestPrefix)
    , let shortestPrefix' = reverse $ drop toDrop $ reverse longestPrefix'
    ]
    -- shrink the first fork
   ++ [ TestChainFragmentFork l1 l2 c1' c2
      | ex1' <- shrinkList (const []) ex1
      , let c1' = fromListFixupBlocks (ex1' ++ CF.toNewestFirst l1)
      ]
    -- shrink the second fork
   ++ [ TestChainFragmentFork l1 l2 c1 c2'
      | ex2' <- shrinkList (const []) ex2
      , let c2' = fromListFixupBlocks (ex2' ++ CF.toNewestFirst l2)
      ]
    where
      extensionFragment :: ChainFragment Block -> ChainFragment Block -> [Block]
      extensionFragment c = reverse . L.drop (CF.length c) . CF.toOldestFirst

prop_arbitrary_TestChainFragmentFork_cover :: TestChainFragmentFork -> Property
prop_arbitrary_TestChainFragmentFork_cover t@(TestChainFragmentFork l1 l2 c1 c2) =
    let samePrefix = l1 == l2
        c1l = CF.toNewestFirst c1
        c2l = CF.toNewestFirst c2
        sameFork = c1l `L.isPrefixOf` c2l || c2l `L.isPrefixOf` c1l in
    cover (45/100) samePrefix "same prefix" $
    cover (4/100)  sameFork   "same fork" $
    prop_arbitrary_TestChainFragmentFork t

prop_arbitrary_TestChainFragmentFork :: TestChainFragmentFork -> Bool
prop_arbitrary_TestChainFragmentFork (TestChainFragmentFork l1 l2 c1 c2) =
    CF.valid l1 && CF.valid l2 && CF.valid c1 && CF.valid c2
    && l1 `CF.isPrefixOf` c1
    && l2 `CF.isPrefixOf` c2
    && let l1l = map (headerSlot . blockHeader) $ CF.toNewestFirst l1
           l2l = map (headerSlot . blockHeader) $ CF.toNewestFirst l2
       in  (l1l `L.isPrefixOf` l2l || l2l `L.isPrefixOf` l1l)

prop_shrink_TestChainFragmentFork :: TestChainFragmentFork -> Bool
prop_shrink_TestChainFragmentFork forks =
  and [ prop_arbitrary_TestChainFragmentFork forks'
        && measure forks' < mforks
      | let mforks = measure forks
      , forks' <- shrink forks ]
  where
    measure (TestChainFragmentFork l1 l2 c1 c2) =
      CF.length l1 + CF.length l2 + CF.length c1 + CF.length c2

--
-- Generator for chain and an index
--

-- | A test generator for a chain fragment and an index (starting from the
-- right). In most cases the index is within bounds of the chain fragment, but
-- it also covers at least 5% of cases where the index is out of bounds.
--
data TestChainFragmentAndIndex = TestChainFragmentAndIndex (ChainFragment Block) Int
  deriving Show

indexInChainFragment :: Int -> ChainFragment Block -> Bool
indexInChainFragment i c = 0 <= i && i < CF.length c

instance Arbitrary TestChainFragmentAndIndex where
  arbitrary = do
    TestBlockChainFragment chain <- arbitrary
    let len = CF.length chain
    index <- frequency
      [ (95, choose (0, len - 1))
      , (2, (negate . getNonZero) <$> arbitrary)
      , (3, (+ len) <$> arbitrary)
      ]
    return (TestChainFragmentAndIndex chain index)

  shrink (TestChainFragmentAndIndex c i)
    | indexInChainFragment i c
    = [ TestChainFragmentAndIndex c' (i - (CF.length c - CF.length c'))
      | TestBlockChainFragment c' <- shrink (TestBlockChainFragment c)]
    | otherwise
    = [ TestChainFragmentAndIndex c' i
      | TestBlockChainFragment c' <- shrink (TestBlockChainFragment c) ]


prop_arbitrary_TestChainFragmentAndIndex :: TestChainFragmentAndIndex -> Property
prop_arbitrary_TestChainFragmentAndIndex (TestChainFragmentAndIndex c i) =
  let inChainFragment = indexInChainFragment i c in
  cover (85/100) inChainFragment       "index in chain" $
  cover ( 5/100) (not inChainFragment) "index not in chain" $
  CF.valid c
