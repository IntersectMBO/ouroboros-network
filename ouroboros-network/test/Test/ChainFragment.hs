{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE TypeOperators    #-}

module Test.ChainFragment
  ( tests
  , TestBlockChainFragmentAndUpdates(..)
  , TestBlockChainFragment(..)
  , TestChainFragmentAndPoint (..)
  , TestChainFragmentFork(..)
  , TestAddBlock(..)
  ) where

import qualified Data.List as L
import           Data.Maybe (fromJust, fromMaybe, isNothing, listToMaybe, maybe,
                     maybeToList)
import qualified Data.Set as Set

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Text.Show.Functions ()

import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain (ChainUpdate (..), Point (..))
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.ChainFragment (ChainFragment(..))
import qualified Ouroboros.Network.ChainFragment as CF
import           Ouroboros.Network.Testing.ConcreteBlock
import           Ouroboros.Network.Testing.Serialise (prop_serialise)
import           Test.Chain ()
import           Test.ChainGenerators
                   ( TestBlockChain (..), TestChainAndRange (..)
                   , genNonNegative, genChainAnchor, mkPartialBlock
                   , addSlotGap, genSlotGap)


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

      -- All our shrinkers rely on fixupBlock, so lets test that first
    , testProperty "fixupBlock"                              prop_fixupBlock

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
  , testProperty "toList/last"                               prop_toList_last
  , testProperty "dropNewest"                                prop_dropNewest
  , testProperty "dropOldest"                                prop_dropOldest
  , testProperty "takeNewest"                                prop_takeNewest
  , testProperty "takeOldest"                                prop_takeOldest
  , testProperty "takeWhileNewest"                           prop_takeWhileNewest
  , testProperty "dropWhileNewest"                           prop_dropWhileNewest
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
  , testProperty "filter"                                    prop_filter
  , testProperty "selectPoints"                              prop_selectPoints
  , testProperty "splitAfterSlot"                            prop_splitAfterSlot
  , testProperty "splitAfterPoint"                           prop_splitAfterPoint
  , testProperty "splitBeforeSlot"                           prop_splitBeforeSlot
  , testProperty "splitBeforePoint"                          prop_splitBeforePoint
  , testProperty "prop_sliceRange"                           prop_sliceRange
  , testProperty "foldChainFragment"                         prop_foldChainFragment
  , testProperty "(:<)"                                      prop_prepend
  , testProperty "fromChain/toChain"                         prop_fromChain_toChain
  , testProperty "toChain/fromChain"                         prop_toChain_fromChain
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

prop_toList_last :: TestBlockChainFragment -> Bool
prop_toList_last (TestBlockChainFragment chain) =
    (listToMaybe . CF.toOldestFirst) chain == CF.last chain

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

prop_takeNewest :: TestBlockChainFragment -> Bool
prop_takeNewest (TestBlockChainFragment chain) =
    let blocks = CF.toNewestFirst chain in
    and [ CF.takeNewest n chain == CF.fromNewestFirst (L.take n blocks)
        | n <- [0..Prelude.length blocks] ]

prop_takeOldest :: TestBlockChainFragment -> Bool
prop_takeOldest (TestBlockChainFragment chain) =
    let blocks = CF.toOldestFirst chain in
    and [ CF.takeOldest n chain == CF.fromOldestFirst (L.take n blocks)
        | n <- [0..Prelude.length blocks] ]

prop_takeWhileNewest :: (Block -> Bool) -> TestBlockChainFragment -> Bool
prop_takeWhileNewest p (TestBlockChainFragment chain) =
    CF.takeWhileNewest p chain
 == (CF.fromNewestFirst . L.takeWhile p . CF.toNewestFirst) chain

prop_dropWhileNewest :: (Block -> Bool) -> TestBlockChainFragment -> Bool
prop_dropWhileNewest p (TestBlockChainFragment chain) =
    CF.dropWhileNewest p chain
 == (CF.fromNewestFirst . L.dropWhile p . CF.toNewestFirst) chain

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
           Nothing      -> True
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

prop_intersectChainFragments :: TestChainFragmentFork -> Property
prop_intersectChainFragments (TestChainFragmentFork origL1 origL2 c1 c2) =
  let intersection = CF.intersectChainFragments c1 c2 in
  counterexample ("intersection: " ++ show intersection) $
  case intersection of
    Nothing -> (CF.null origL1 || CF.null origL2)
            .&&. L.intersect (CF.toNewestFirst c1) (CF.toNewestFirst c2) === []
    Just (l1, l2, r1, r2) ->
           counterexample "headPoint" (CF.headPoint l1 === CF.headPoint l2)
      .&&. counterexample "c1" (CF.joinChainFragments l1 r1 === Just c1)
      .&&. counterexample "c2" (CF.joinChainFragments l2 r2 === Just c2)

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

prop_splitAfterSlot :: TestChainFragmentAndPoint -> Bool
prop_splitAfterSlot (TestChainFragmentAndPoint c p) =
    let (l, r) = CF.splitAfterSlot c (pointSlot p)
    in CF.joinChainFragments l r == Just c
    && all (<= pointSlot p) (slots l)
    && all (>  pointSlot p) (slots r)
  where
    slots :: ChainFragment Block -> [SlotNo]
    slots = map blockSlot . CF.toOldestFirst

prop_splitAfterPoint :: TestChainFragmentAndPoint -> Bool
prop_splitAfterPoint (TestChainFragmentAndPoint c p) =
  case CF.splitAfterPoint c p of
    Just (l, r) ->
         CF.pointOnChainFragment p c
      && not (CF.pointOnChainFragment p r)
      && CF.headPoint l == Just p
      && CF.joinChainFragments l r == Just c
      && all (<= pointSlot p) (slots l)
      && all (>  pointSlot p) (slots r)
    Nothing ->
         not (CF.pointOnChainFragment p c)
  where
    slots :: ChainFragment Block -> [SlotNo]
    slots = map blockSlot . CF.toOldestFirst

prop_splitBeforeSlot :: TestChainFragmentAndPoint -> Bool
prop_splitBeforeSlot (TestChainFragmentAndPoint c p) =
    let (l, r) = CF.splitBeforeSlot c (pointSlot p)
    in CF.joinChainFragments l r == Just c
    && all (<  pointSlot p) (slots l)
    && all (>= pointSlot p) (slots r)
  where
    slots :: ChainFragment Block -> [SlotNo]
    slots = map blockSlot . CF.toOldestFirst

prop_splitBeforePoint :: TestChainFragmentAndPoint -> Bool
prop_splitBeforePoint (TestChainFragmentAndPoint c p) =
  case CF.splitBeforePoint c p of
    Just (l, r) ->
        CF.pointOnChainFragment p c
     && not (CF.pointOnChainFragment p l)
     && CF.lastPoint r == Just p
     && CF.joinChainFragments l r == Just c
     && all (<  pointSlot p) (slots l)
     && all (>= pointSlot p) (slots r)
    Nothing ->
        not (CF.pointOnChainFragment p c)
  where
    slots :: ChainFragment Block -> [SlotNo]
    slots = map blockSlot . CF.toOldestFirst

prop_sliceRange :: TestChainAndRange -> Bool
prop_sliceRange (TestChainAndRange c p1 p2) =
    case CF.sliceRange c' p1 p2 of
      Just slice ->
          CF.valid slice
       && not (CF.null slice)
       && CF.headPoint slice == Just p2
       && CF.lastPoint slice == Just p1
      Nothing ->
          not (CF.pointOnChainFragment p1 c')
       || not (CF.pointOnChainFragment p2 c')
  where
    c' = CF.fromNewestFirst (Chain.chainToList c)

prop_foldChainFragment :: TestBlockChainFragment -> Property
prop_foldChainFragment (TestBlockChainFragment c) =
    CF.foldChainFragment f [] c === CF.foldChainFragmentSpec f [] c
  where
    f l block = unSlotNo (blockSlot block) : l

prop_lookupByIndexFromEnd :: TestChainFragmentAndIndex -> Property
prop_lookupByIndexFromEnd (TestChainFragmentAndIndex c i) =
  case CF.lookupByIndexFromEnd c i of
    CF.Position _ b _ -> b === CF.toNewestFirst c !! i
    _                 -> property (i < 0 || i >= CF.length c)

prop_filter :: (Block -> Bool) -> TestBlockChainFragment -> Property
prop_filter p (TestBlockChainFragment chain) =
  let fragments = CF.filter p chain in
      cover 70 (length fragments > 1) "multiple fragments" $
      counterexample ("fragments: " ++ show fragments) $

      -- The fragments contain exactly the blocks where p holds
      (   Set.fromList (L.map CF.blockPoint (L.filter p (CF.toNewestFirst chain)))
       ===
          Set.fromList (L.map CF.blockPoint (concatMap CF.toNewestFirst fragments))
      )
   .&&.
      -- The fragments are non-empty
      all (not . CF.null) fragments
   .&&.
      -- The fragments are in order
      (let fragmentPoints = map (\c -> (CF.blockPoint <$> CF.last c,
                                        CF.blockPoint <$> CF.head c)) fragments
        in fragmentPoints == L.sort fragmentPoints)
   .&&.
      -- The fragments are of maximum size
      and [ isNothing (CF.joinChainFragments a b)
          | (a,b) <- zip fragments (tail fragments) ]



prop_selectPoints :: TestBlockChainFragment -> Property
prop_selectPoints (TestBlockChainFragment c) =
    CF.selectPoints offsets c === CF.selectPointsSpec offsets c .&&.
    CF.selectPoints []      c === CF.selectPointsSpec []      c .&&.
    CF.selectPoints [1,1]   c === CF.selectPointsSpec [1,1]   c
  where
    offsets = [0,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584]

prop_prepend :: TestBlockChainFragment -> Bool
prop_prepend (TestBlockChainFragment c) = case c of
    Empty      -> True
    b CF.:< c' -> CF.valid (b CF.:< c')

prop_fromChain_toChain :: TestBlockChainFragment -> Property
prop_fromChain_toChain (TestBlockChainFragment c) =
    startsFromGenesis ==>
    CF.fromChain (CF.toChain c) === c
  where
    startsFromGenesis = case c of
      CF.Empty  -> True
      b CF.:< _ -> blockPrevHash b == GenesisHash

prop_toChain_fromChain :: TestBlockChain -> Property
prop_toChain_fromChain (TestBlockChain c) =
    CF.toChain (CF.fromChain c) === c

prop_fixupBlock :: TestBlockChainFragment -> Bool
prop_fixupBlock (TestBlockChainFragment chain) =
  fixupChainFragmentFromSame fixupBlock (CF.toNewestFirst chain) == chain


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
        (prevhash, prevblock, prevslot) <- genChainAnchor
        n <- genNonNegative
        bodies <- vector n
        slots  <- mkSlots prevslot <$> vectorOf n genSlotGap
        let chain = mkChainFragment prevhash prevblock (zip slots bodies)
        return (TestBlockChainFragment chain)
      where
        mkSlots :: SlotNo -> [Int] -> [SlotNo]
        mkSlots (SlotNo prevslot) =
            map SlotNo . tail
          . scanl (\slot gap -> slot + fromIntegral gap) prevslot

    shrink (TestBlockChainFragment c) =
        [ TestBlockChainFragment (fixupChainFragmentFromSame fixupBlock c')
        | c' <- shrinkList (const []) (CF.toNewestFirst c) ]

instance Arbitrary TestHeaderChainFragment where
    arbitrary = do
        TestBlockChainFragment blockchain <- arbitrary
        let headerchain = CF.mapChainFragment blockHeader blockchain
        return (TestHeaderChainFragment headerchain)

    shrink (TestHeaderChainFragment c) =
        [ TestHeaderChainFragment (fixupChainFragmentFromSame fixupBlockHeader c')
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
    , let b' = maybe b (`fixupBlockAfterBlock` b) (CF.head c')
    ]

genAddBlock :: (HasHeader block, HeaderHash block ~ ConcreteHeaderHash)
            => ChainFragment block -> Gen Block
genAddBlock chain = do
    body    <- arbitrary
    slotGap <- genSlotGap
    (prevhash, prevblockno, prevslot) <- case chain of
      _ CF.:> chead -> pure ( BlockHash (blockHash chead)
                            , blockNo chead
                            , blockSlot chead )
      CF.Empty      -> genChainAnchor
    let slot    = addSlotGap slotGap prevslot
        b       = fixupBlock prevhash prevblockno (mkPartialBlock slot body)
    return b

prop_arbitrary_TestAddBlock :: TestAddBlock -> Property
prop_arbitrary_TestAddBlock (TestAddBlock c b) =
    CF.valid c .&&. CF.validExtension' c b === Right ()

prop_shrink_TestAddBlock :: TestAddBlock -> Property
prop_shrink_TestAddBlock t =
    conjoin
        [ CF.valid c .&&. CF.validExtension' c b === Right ()
        | TestAddBlock c b <- shrink t ]

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
        n'      = n + fromIntegral
                        ((maybe 0 (toInteger . unBlockNo) (CF.headBlockNo c'))
                       - (maybe 0 (toInteger . unBlockNo) (CF.headBlockNo c)))

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
      , (1, Just <$> arbitrary)
      ]
    -- If 'mbPoint' yielded Nothing, just generate one off the chain fragment,
    -- as the chain fragment is probably empty anyway.
    point <- case mbPoint of
               Just p  -> return p
               Nothing -> arbitrary
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
  cover 85 onChainFragment       "point on chain" $
  cover  5 (not onChainFragment) "point not on chain" $
    CF.valid c

prop_shrink_TestChainFragmentAndPoint :: TestChainFragmentAndPoint -> Bool
prop_shrink_TestChainFragmentAndPoint cp@(TestChainFragmentAndPoint c _) =
  and [ CF.valid c' && (not (CF.pointOnChainFragment p c) || CF.pointOnChainFragment p c')
      | TestChainFragmentAndPoint c' p <- shrink cp ]


--
-- Generator for chain forks
--

-- | A test generator for chain fragments of two forks of a chain.
--
-- We return four fragments: two prefixes and two fragments that include the
-- respective prefix and an additional suffix. The two prefixes will share
-- identical blocks, but one prefix may contain fewer blocks than the other,
-- i.e., by dropping some of its oldest blocks. We then add some random blocks
-- to each prefix, leading to two forks.
--
-- Note that we might happen to add the same exact block(s) to both prefixes,
-- leading to two identical chain fragments.
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
        c1 <- genAddBlocks n1 l1 Nothing
        let ex1 = L.drop (CF.length l1) (CF.toOldestFirst c1)
        c2 <- genAddBlocks n2 l2 (listToMaybe ex1)
        return (c1, c2)
    return (TestChainFragmentFork l1 l2 c1 c2)
    where
      genAddBlocks :: Int
                   -> ChainFragment Block
                   -> Maybe Block
                   -> Gen (ChainFragment Block)
      genAddBlocks 0 c _       = return c
      genAddBlocks n c Nothing = do
          b <- genAddBlock c
          genAddBlocks (n-1) (CF.addBlock b c) Nothing

      -- But we want to avoid the extensions starting off equal which would
      -- mean the longest common prefix was not the declared common prefix.
      -- So we optionally take the first block to avoid and use that in the
      -- second fork we generate.
      genAddBlocks n c (Just forbiddenBlock) = do
          b <- genAddBlock c `suchThat` (/= forbiddenBlock)
          genAddBlocks (n-1) (CF.addBlock b c) Nothing

  shrink (TestChainFragmentFork l1 l2 c1 c2) =
    -- shrink the prefixes
    let (longestPrefix, shortestPrefix) =
          -- shrink the longest prefix of the two, then drop some of its
          -- oldest blocks to obtain a new 'shortest prefix'.
          if CF.length l1 > CF.length l2
          then (l1, l2) else (l2, l1)
        toDrop = CF.length longestPrefix - CF.length shortestPrefix
        ex1 = extensionFragment l1 c1
        ex2 = extensionFragment l2 c2 in
    [ TestChainFragmentFork
        (fixupChainFragmentFromSame fixupBlock longestPrefix')
        (fixupChainFragmentFromSame fixupBlock shortestPrefix')
        (fixupChainFragmentFromSame fixupBlock (ex1 ++ longestPrefix'))
        (fixupChainFragmentFromSame fixupBlock (ex2 ++ shortestPrefix'))
    | longestPrefix' <- shrinkList (const []) (CF.toNewestFirst longestPrefix)
    , let shortestPrefix' = reverse $ drop toDrop $ reverse longestPrefix'
    ]
   -- shrink the first fork
   ++ [ TestChainFragmentFork l1 l2 c1' c2
      | ex1' <- shrinkList (const []) ex1
      , let c1' = fixupChainFragmentFromSame fixupBlock (ex1' ++ CF.toNewestFirst l1)
      , isLongestCommonPrefix c1' c2
      ]
   -- shrink the second fork
   ++ [ TestChainFragmentFork l1 l2 c1 c2'
      | ex2' <- shrinkList (const []) ex2
      , let c2' = fixupChainFragmentFromSame fixupBlock (ex2' ++ CF.toNewestFirst l2)
      , isLongestCommonPrefix c1 c2'
      ]
    where
      extensionFragment :: ChainFragment Block -> ChainFragment Block -> [Block]
      extensionFragment c = reverse . L.drop (CF.length c) . CF.toOldestFirst

      -- Need to make sure that when we shrink that we don't make the longest
      -- common prefix be a strict extension of the original common prefix.
      isLongestCommonPrefix c1' c2' =
        case (CF.dropOldest (CF.length l1) c1',
              CF.dropOldest (CF.length l2) c2') of
          (c1head CF.:< _, c2head CF.:< _) -> c1head /= c2head
          _                                -> True

prop_arbitrary_TestChainFragmentFork_cover :: TestChainFragmentFork -> Property
prop_arbitrary_TestChainFragmentFork_cover t@(TestChainFragmentFork l1 l2 c1 c2) =
    let samePrefix = l1 == l2
        c1l = CF.toNewestFirst c1
        c2l = CF.toNewestFirst c2
        sameFork = c1l `L.isPrefixOf` c2l || c2l `L.isPrefixOf` c1l in
    cover 45 samePrefix "same prefix" $
    cover  4  sameFork   "same fork" $
    prop_arbitrary_TestChainFragmentFork t

prop_arbitrary_TestChainFragmentFork :: TestChainFragmentFork -> Bool
prop_arbitrary_TestChainFragmentFork (TestChainFragmentFork l1 l2 c1 c2) =
    CF.valid l1 && CF.valid l2 && CF.valid c1 && CF.valid c2
    && l1 `CF.isPrefixOf` c1
    && l2 `CF.isPrefixOf` c2
    && let l1l = map (headerSlot . blockHeader) $ CF.toNewestFirst l1
           l2l = map (headerSlot . blockHeader) $ CF.toNewestFirst l2
       in  (l1l `L.isPrefixOf` l2l || l2l `L.isPrefixOf` l1l)

       -- the common prefix should be maximal, so next blocks not equal
    && case (CF.dropOldest (CF.length l1) c1,
             CF.dropOldest (CF.length l2) c2) of
         (c1head CF.:< _, c2head CF.:< _) -> c1head /= c2head
         _                                -> True

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
  cover 85 inChainFragment       "index in chain" $
  cover  5 (not inChainFragment) "index not in chain" $
  CF.valid c
