{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

module Test.AnchoredFragment
  ( tests
  , TestBlockAnchoredFragment (..)
  ) where

import qualified Data.List as L
import           Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import           Data.Word (Word64)

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Text.Show.Functions ()

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment,
                     AnchoredSeq (Empty), anchorPoint)
import qualified Ouroboros.Network.AnchoredFragment as AF
import qualified Ouroboros.Network.AnchoredSeq as AS
import           Ouroboros.Network.Block
import qualified Ouroboros.Network.Mock.Chain as Chain
import           Ouroboros.Network.Mock.ConcreteBlock
import           Ouroboros.Network.Point (WithOrigin (..))
import           Test.ChainGenerators (TestBlockChain (..),
                     TestChainAndRange (..), addSlotGap, genChainAnchor,
                     genNonNegative, genSlotGap)


--
-- The list of all tests
--

tests :: TestTree
tests = testGroup "AnchoredFragment"
  [ testGroup "generators"
    [ testProperty "arbitrary for TestBlockAnchoredFragment" $
      -- It's important we don't generate too many trivial test cases here
      -- so check the coverage to enforce it.
      checkCoverage prop_arbitrary_TestBlockAnchoredFragment
    , testProperty "shrink for TestBlockAnchoredFragment"    prop_shrink_TestBlockAnchoredFragment

    , testProperty "arbitrary for TestAddBlock"              prop_arbitrary_TestAddBlock
    , testProperty "shrink for TestAddBlock"                 prop_shrink_TestAddBlock

    , testProperty "arbitrary for TestAnchoredFragmentAndPoint" prop_arbitrary_TestAnchoredFragmentAndPoint
    , testProperty "shrink for TestAnchoredFragmentAndPoint"    prop_shrink_TestAnchoredFragmentAndPoint

    , testProperty "arbitrary for TestJoinableAnchoredFragments" prop_arbitrary_TestJoinableAnchoredFragments
    ]

  , testProperty "length/Empty"                       prop_length_Empty
  , testProperty "dropNewest/Empty"                   prop_dropNewest_Empty
  , testProperty "fromNewestFirst/toNewestFirst"      prop_fromNewestFirst_toNewestFirst
  , testProperty "fromOldestFirst/toOldestFirst"      prop_fromOldestFirst_toOldestFirst
  , testProperty "toList/head"                        prop_toList_head
  , testProperty "toList/last"                        prop_toList_last
  , testProperty "splitAt"                            prop_splitAt
  , testProperty "dropNewest"                         prop_dropNewest
  , testProperty "takeOldest"                         prop_takeOldest
  , testProperty "dropWhileNewest"                    prop_dropWhileNewest
  , testProperty "takeWhileOldest"                    prop_takeWhileOldest
  , testProperty "addBlock"                           prop_addBlock
  , testProperty "rollback"                           prop_rollback
  , testProperty "rollback/head"                      prop_rollback_head
  , testProperty "successorBlock"                     prop_successorBlock
  , testProperty "pointOnFragment"                    prop_pointOnFragment
  , testProperty "selectPoints"                       prop_selectPoints
  , testProperty "splitAfterPoint"                    prop_splitAfterPoint
  , testProperty "splitBeforePoint"                   prop_splitBeforePoint
  , testProperty "sliceRange"                         prop_sliceRange
  , testProperty "join"                               prop_join
  , testProperty "intersect"                          prop_intersect
  , testProperty "intersect when within bounds"       prop_intersect_bounds
  , testProperty "toChain/fromChain"                  prop_toChain_fromChain
  , testProperty "anchorNewest"                       prop_anchorNewest
  , testProperty "filter"                             prop_filter
  , testProperty "filterWithStop_always_stop"         prop_filterWithStop_always_stop
  , testProperty "filterWithStop_never_stop"          prop_filterWithStop_never_stop
  , testProperty "filterWithStop"                     prop_filterWithStop
  , testProperty "filterWithStop vs spec"             prop_filterWithStop_vs_spec
  , testProperty "filterWithStop_filter"              prop_filterWithStop_filter
  ]

--
-- Properties
--

prop_length_Empty :: Bool
prop_length_Empty =
    AF.length (Empty anchor :: AnchoredFragment Block) == 0
  where
    anchor = AF.AnchorGenesis

prop_dropNewest_Empty :: TestBlockAnchoredFragment -> Bool
prop_dropNewest_Empty (TestBlockAnchoredFragment chain) =
    AF.dropNewest (AF.length chain) chain == Empty anchor
  where
    anchor = AF.anchor chain

prop_fromNewestFirst_toNewestFirst :: TestBlockAnchoredFragment -> Bool
prop_fromNewestFirst_toNewestFirst (TestBlockAnchoredFragment chain) =
    (AF.fromNewestFirst anchor . AF.toNewestFirst) chain == chain
  where
    anchor = AF.anchor chain

prop_fromOldestFirst_toOldestFirst :: TestBlockAnchoredFragment -> Bool
prop_fromOldestFirst_toOldestFirst (TestBlockAnchoredFragment chain) =
    (AF.fromOldestFirst anchor . AF.toOldestFirst) chain == chain
  where
    anchor = AF.anchor chain

headOrAnchor :: a -> [b] -> Either a b
headOrAnchor anchor = maybe (Left anchor) Right . listToMaybe

-- The list comes out in reverse order, most recent block at the head
prop_toList_head :: TestBlockAnchoredFragment -> Bool
prop_toList_head (TestBlockAnchoredFragment chain) =
    (headOrAnchor anchor . AF.toNewestFirst) chain == AF.head chain
  where
    anchor = AF.anchor chain

prop_toList_last :: TestBlockAnchoredFragment -> Bool
prop_toList_last (TestBlockAnchoredFragment chain) =
    (headOrAnchor anchor . AF.toOldestFirst) chain == AF.last chain
  where
    anchor = AF.anchor chain

prop_splitAt :: TestBlockAnchoredFragment -> Bool
prop_splitAt (TestBlockAnchoredFragment chain) =
    let blocks = AF.toOldestFirst chain in
    and [ let (before,         after)         = AF.splitAt n chain
              (beforeExpected, afterExpected) = L.splitAt  n blocks
          in AF.toOldestFirst before == beforeExpected       &&
             AF.toOldestFirst after  == afterExpected        &&
             AF.anchorPoint   before == AF.anchorPoint chain &&
             AF.headPoint     before == AF.anchorPoint after &&
             AF.headPoint     after  == AF.headPoint   chain &&
             AF.join before after    == Just chain
        | n <- [0..Prelude.length blocks] ]

prop_dropNewest :: TestBlockAnchoredFragment -> Bool
prop_dropNewest (TestBlockAnchoredFragment chain) =
    let blocks = AF.toNewestFirst chain in
    and [ AF.dropNewest n chain == AF.fromNewestFirst anchor (L.drop n blocks)
        | n <- [0..Prelude.length blocks] ]
  where
    anchor = AF.anchor chain

prop_takeOldest :: TestBlockAnchoredFragment -> Bool
prop_takeOldest (TestBlockAnchoredFragment chain) =
    let blocks = AF.toOldestFirst chain in
    and [ AF.takeOldest n chain == AF.fromOldestFirst anchor (L.take n blocks)
        | n <- [0..Prelude.length blocks] ]
  where
    anchor = AF.anchor chain

prop_dropWhileNewest :: (Block -> Bool) -> TestBlockAnchoredFragment -> Bool
prop_dropWhileNewest p (TestBlockAnchoredFragment chain) =
    AF.dropWhileNewest p chain ==
    (AF.fromNewestFirst anchor . L.dropWhile p . AF.toNewestFirst) chain
  where
    anchor = AF.anchor chain

prop_takeWhileOldest :: (Block -> Bool) -> TestBlockAnchoredFragment -> Bool
prop_takeWhileOldest p (TestBlockAnchoredFragment chain) =
    AF.takeWhileOldest p chain ==
    (AF.fromOldestFirst anchor . L.takeWhile p . AF.toOldestFirst) chain
  where
    anchor = AF.anchor chain

prop_addBlock :: TestAddBlock -> Bool
prop_addBlock (TestAddBlock c b) =
    -- after adding a block, that block is at the head
    curHead == AF.blockPoint b
    -- chain is still valid
    && AF.valid c'
    -- removing the block gives the original
    && Just c == AF.rollback prevHead c'
    && AF.dropNewest 1 c' == c
    -- chain is one longer
    && AF.length c' == AF.length c + 1
  where
    c'       = AF.addBlock b c
    prevHead = AF.headPoint c
    curHead  = AF.headPoint c'

prop_rollback :: TestAnchoredFragmentAndPoint -> Bool
prop_rollback (TestAnchoredFragmentAndPoint c p) =
    case AF.rollback p c of
      Nothing -> not (AF.withinFragmentBounds p c)
      Just c' ->
        -- chain is a prefix of original
           AF.isPrefixOf c' c
        -- chain head point is the rollback point
        && AF.headPoint c' == p

prop_rollback_head :: TestBlockAnchoredFragment -> Bool
prop_rollback_head (TestBlockAnchoredFragment c) =
    AF.rollback (AF.headPoint c) c == Just c

prop_successorBlock :: TestAnchoredFragmentAndPoint -> Property
prop_successorBlock (TestAnchoredFragmentAndPoint c p) =
  AF.withinFragmentBounds p c ==>
  case AF.successorBlock p c of
    Nothing -> AF.headPoint c === p
    Just b  -> property $ AF.withinFragmentBounds (AF.blockPoint b) c
          .&&. blockPrevHash b === pointHash p

prop_pointOnFragment :: TestAnchoredFragmentAndPoint -> Bool
prop_pointOnFragment (TestAnchoredFragmentAndPoint c p) =
    AF.pointOnFragment p c == AF.pointOnFragmentSpec p c

prop_selectPoints :: TestBlockAnchoredFragment -> Property
prop_selectPoints (TestBlockAnchoredFragment c) =
    AF.selectPoints offsets c === AF.selectPointsSpec offsets c .&&.
    AF.selectPoints []      c === AF.selectPointsSpec []      c .&&.
    AF.selectPoints [1,1]   c === AF.selectPointsSpec [1,1]   c
  where
    offsets = [0,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584]

prop_splitAfterPoint :: TestAnchoredFragmentAndPoint -> Property
prop_splitAfterPoint (TestAnchoredFragmentAndPoint c pt) =
  case AF.splitAfterPoint c pt of
    Just (p, s) ->
         AF.withinFragmentBounds pt c
      .&&. AF.anchorPoint p === AF.anchorPoint c
      .&&. AF.headPoint   p === pt
      .&&. AF.anchorPoint s === pt
      .&&. AF.headPoint   s === AF.headPoint c
      .&&. AF.join p s      === Just c
    Nothing -> property $ not $ AF.withinFragmentBounds pt c

prop_splitBeforePoint :: TestAnchoredFragmentAndPoint -> Property
prop_splitBeforePoint (TestAnchoredFragmentAndPoint c pt) =
  case AF.splitBeforePoint c pt of
    Just (p, s) ->
         AF.pointOnFragment pt c
      .&&. (blockPoint <$> AF.last s) === Right pt
      .&&. AF.join p s                === Just c
    Nothing -> property $ not $ AF.pointOnFragment pt c

prop_sliceRange :: TestChainAndRange -> Bool
prop_sliceRange (TestChainAndRange c p1 p2) =
    case AF.sliceRange c' p1 p2 of
      Just slice ->
          AF.valid slice
       && not (AF.null slice)
       && AF.headPoint slice == p2
       && AF.lastPoint slice == p1
      Nothing ->
          not (AF.pointOnFragment p1 c')
       || not (AF.pointOnFragment p2 c')
  where
    c' = Chain.toAnchoredFragment c

prop_join :: TestJoinableAnchoredFragments -> Property
prop_join t@(TestJoinableAnchoredFragments c1 c2) = case AF.join c1 c2 of
    Just joined -> joinable t .&&.
                   AF.headPoint c1 === AF.anchorPoint c2 .&&.
                   AF.toNewestFirst joined ===
                   AF.toNewestFirst c2 <> AF.toNewestFirst c1
    Nothing     -> not (joinable t) .&&.
                   AF.headPoint c1 =/= AF.anchorPoint c2

prop_intersect :: TestAnchoredFragmentFork -> Property
prop_intersect (TestAnchoredFragmentFork origP1 origP2 c1 c2) =
  case AF.intersect c1 c2 of
    Nothing ->
      L.intersect (pointsList c1) (pointsList c2) === []
    Just (p1, p2, s1, s2) ->
      p1 === origP1 .&&. p2 === origP2 .&&.
      AF.join p1 s1 === Just c1 .&&.
      AF.join p2 s2 === Just c2 .&&.
      AF.headPoint p1   === AF.headPoint   p2 .&&.
      AF.anchorPoint p1 === AF.anchorPoint c1 .&&.
      AF.anchorPoint p2 === AF.anchorPoint c2 .&&.
      AF.anchorPoint s1 === AF.headPoint   p1 .&&.
      AF.anchorPoint s2 === AF.headPoint   p2
  where
    pointsList c = AF.anchorPoint c : map blockPoint (AF.toOldestFirst c)

prop_intersect_bounds :: TestAnchoredFragmentFork -> Property
prop_intersect_bounds (TestAnchoredFragmentFork _ _ c1 c2) =
    intersects === (AF.withinFragmentBounds (AF.anchorPoint c1) c2 ||
                    AF.withinFragmentBounds (AF.anchorPoint c2) c1)
  where
    intersects = isJust (AF.intersect c1 c2) || isJust (AF.intersect c2 c1)

prop_toChain_fromChain :: TestBlockChain -> Property
prop_toChain_fromChain (TestBlockChain ch) =
    Chain.fromAnchoredFragment (Chain.toAnchoredFragment ch) === Just ch

prop_anchorNewest :: NonNegative Int -> TestBlockAnchoredFragment -> Property
prop_anchorNewest (NonNegative n') (TestBlockAnchoredFragment c) =
    AF.length c' === min (AF.length c) (fromIntegral n) .&&.
             map blockPoint (AF.toNewestFirst c') ++ [anchorPoint c'] ===
    take n1 (map blockPoint (AF.toNewestFirst c)  ++ [anchorPoint c])
  where
    c' = AF.anchorNewest n c

    -- For testing purposes, we take a @'NonNegative' 'Int'@ instead of a
    -- 'Word64' because large 'Word64's can't be converted to 'Int', which we
    -- need for 'take' and 'length'.
    n :: Word64
    n = fromIntegral n'

    -- Avoid an overflow in n' + 1 by just using n', as the fragment will
    -- never be that long in the tests.
    n1 = if n' == maxBound then n' else n' + 1


--
-- Generators for chains
--

-- | A test generator for a valid anchored chain fragment of blocks/headers.
--
newtype TestBlockAnchoredFragment = TestBlockAnchoredFragment (AnchoredFragment Block)

instance Show TestBlockAnchoredFragment where
  show (TestBlockAnchoredFragment c) =
      "TestBlockAnchoredFragment" ++ nl ++
      AF.prettyPrint nl show show c
    where
      nl = "\n"

instance Arbitrary TestBlockAnchoredFragment where
    arbitrary = do
        anchor <- genChainAnchor
        n <- genNonNegative
        bodies <- vector n
        slots  <- mkSlots (AF.anchorToSlotNo anchor) <$> vectorOf n genSlotGap
        let chain = mkAnchoredFragment anchor (zip slots bodies)
        return $ TestBlockAnchoredFragment chain
      where
        mkSlots :: WithOrigin SlotNo -> [Int] -> [SlotNo]
        mkSlots Origin = mkSlots (At (SlotNo 0))
        mkSlots (At (SlotNo prevslot)) =
            map SlotNo . tail
          . scanl (\slot gap -> slot + fromIntegral gap) prevslot

    shrink (TestBlockAnchoredFragment c) =
        [ TestBlockAnchoredFragment $
            fixupAnchoredFragmentFrom
              (AF.anchor c)
              fixupBlock
              c'
        | c' <- shrinkList (const []) (AF.toNewestFirst c) ]

prop_arbitrary_TestBlockAnchoredFragment :: TestBlockAnchoredFragment -> Property
prop_arbitrary_TestBlockAnchoredFragment (TestBlockAnchoredFragment c) =
    -- check we get some but not too many zero-length chains
    cover 95   (not (AF.null c)) "non-null" $
    cover 1.5       (AF.null c)  "null"     $
    AF.valid c

prop_shrink_TestBlockAnchoredFragment :: TestBlockAnchoredFragment -> Bool
prop_shrink_TestBlockAnchoredFragment c =
    and [ AF.valid c' | TestBlockAnchoredFragment c' <- shrink c ]

--
-- Generator for chain and single block
--

-- | A test generator for an anchored chain fragment and a block that can be
-- appended to it.
--
data TestAddBlock = TestAddBlock (AnchoredFragment Block) Block

instance Arbitrary TestAddBlock where
  arbitrary = do
    TestBlockAnchoredFragment chain <- arbitrary
    block <- genAddBlock chain
    return (TestAddBlock chain block)

  shrink (TestAddBlock c b) =
    [ TestAddBlock c' b'
    | TestBlockAnchoredFragment c' <- shrink (TestBlockAnchoredFragment c)
    , let b' = fixupBlock (AF.headAnchor c') b
    ]

genAddBlock :: (HasHeader block, HeaderHash block ~ ConcreteHeaderHash)
            => AnchoredFragment block -> Gen Block
genAddBlock chain = do
    body    <- arbitrary
    slotGap <- genSlotGap
    let slot = addSlotGap slotGap (AF.headSlot chain)
    return $ fixupBlock (AF.headAnchor chain) (mkPartialBlock slot body)

instance Show TestAddBlock where
  show (TestAddBlock c b) =
      "TestAddBlock" ++ nl ++
      AF.prettyPrint nl show show c ++ nl ++
      show b
    where
      nl = "\n"

prop_arbitrary_TestAddBlock :: TestAddBlock -> Bool
prop_arbitrary_TestAddBlock (TestAddBlock c b) =
    AF.valid c && AF.validExtension c b

prop_shrink_TestAddBlock :: TestAddBlock -> Bool
prop_shrink_TestAddBlock t =
    and [ AF.valid c && AF.validExtension c b
        | TestAddBlock c b <- shrink t ]


--
-- Generator for chain and single point on the chain
--

-- | A test generator for an anchored fragment and a point. In most cases the
-- point is on the anchored fragment (referring to a block or the anchor
-- point), but it also covers at least 5% of cases where the point is not on
-- the anchored fragment nor the anchor point.
--
data TestAnchoredFragmentAndPoint =
    TestAnchoredFragmentAndPoint (AnchoredFragment Block) (Point Block)

instance Show TestAnchoredFragmentAndPoint where
  show (TestAnchoredFragmentAndPoint c pt) =
      "TestAnchoredFragmentAndPoint" ++ nl ++
      AF.prettyPrint nl show show c ++ nl ++
      show pt
    where
      nl = "\n"

instance Arbitrary TestAnchoredFragmentAndPoint where
  arbitrary = do
    TestBlockAnchoredFragment chain <- arbitrary
    point <- frequency
      [ (2, return (AF.anchorPoint chain))
      , (if AF.null chain then 0 else 7,
         blockPoint <$> elements (AF.toNewestFirst chain))
      -- A few points off the chain!
      , (1, arbitrary)
      ]
    return (TestAnchoredFragmentAndPoint chain point)

  shrink (TestAnchoredFragmentAndPoint c pt)
    | AF.withinFragmentBounds pt c
      -- If the point is within the fragment bounds, shrink the fragment and
      -- return all the points within the bounds
    = [ TestAnchoredFragmentAndPoint c' pt'
      | TestBlockAnchoredFragment c' <- shrink (TestBlockAnchoredFragment c)
      , pt' <- AF.anchorPoint c' : map blockPoint (AF.toNewestFirst c')
      ]
    | otherwise
      -- If the point is not within the bounds, just shrink the fragment and
      -- return the same point
    = [ TestAnchoredFragmentAndPoint c' pt
      | TestBlockAnchoredFragment c' <- shrink (TestBlockAnchoredFragment c)
      ]

prop_arbitrary_TestAnchoredFragmentAndPoint :: TestAnchoredFragmentAndPoint -> Property
prop_arbitrary_TestAnchoredFragmentAndPoint (TestAnchoredFragmentAndPoint c p) =
  let onAnchoredFragment = AF.pointOnFragment p c
      isAnchor           = AF.anchorPoint c == p
      neither            = not onAnchoredFragment && not isAnchor in
  cover 65 onAnchoredFragment "point on fragment"                $
  cover 15 isAnchor           "anchor point"                     $
  cover  5 neither            "point not on fragment nor anchor" $
  AF.valid c

prop_shrink_TestAnchoredFragmentAndPoint :: TestAnchoredFragmentAndPoint -> Bool
prop_shrink_TestAnchoredFragmentAndPoint t@(TestAnchoredFragmentAndPoint c _) =
  and [ AF.valid c' && (not (AF.withinFragmentBounds p c) ||
                        AF.withinFragmentBounds p c')
      | TestAnchoredFragmentAndPoint c' p <- shrink t ]


--
-- Generator for two fragments that can or cannot be joined
--

data TestJoinableAnchoredFragments
    = TestJoinableAnchoredFragments_
        (AnchoredFragment Block)
        (Point Block)
      -- ^ We guarantee that the point is on the fragment. It can correspond to
      -- the anchor
    | TestUnjoinableAnchoredFragments_
        (AnchoredFragment Block)
        (AnchoredFragment Block)
      -- ^ The fragments are guaranteed to be unjoinable

joinable :: TestJoinableAnchoredFragments -> Bool
joinable TestJoinableAnchoredFragments_   {} = True
joinable TestUnjoinableAnchoredFragments_ {} = False

instance Show TestJoinableAnchoredFragments where
  show t@(TestJoinableAnchoredFragments c1 c2) =
      "TestJoinableAnchoredFragments (" ++ j ++ ")" ++ nl ++
      AF.prettyPrint nl show show c1 ++ nl ++ nl ++
      AF.prettyPrint nl show show c2
    where
      j  = if joinable t then "joinable" else "unjoinable"
      nl = "\n"

viewJoinableAnchoredFragments
    :: TestJoinableAnchoredFragments
    -> (AnchoredFragment Block, AnchoredFragment Block)
viewJoinableAnchoredFragments (TestUnjoinableAnchoredFragments_ c1 c2) =
    (c1, c2)
viewJoinableAnchoredFragments (TestJoinableAnchoredFragments_ c pt) =
    fromMaybe (error "TestJoinableAnchoredFragments: not splittable") $
      AF.splitAfterPoint c pt

pattern TestJoinableAnchoredFragments
  :: AnchoredFragment Block
  -> AnchoredFragment Block
  -> TestJoinableAnchoredFragments
pattern TestJoinableAnchoredFragments c1 c2 <-
    (viewJoinableAnchoredFragments -> (c1, c2))

{-# COMPLETE TestJoinableAnchoredFragments #-}

instance Arbitrary TestJoinableAnchoredFragments where
  arbitrary = frequency
      [ (1, uncurry TestJoinableAnchoredFragments_   <$> genJoinable)
      , (1, uncurry TestUnjoinableAnchoredFragments_ <$> genUnjoinable)
      ]
    where
      genJoinable :: Gen (AnchoredFragment Block, Point Block)
      genJoinable = do
        TestAnchoredFragmentAndPoint c pt <- arbitrary `suchThat` validJoinable
        return (c, pt)

      genUnjoinable :: Gen (AnchoredFragment Block, AnchoredFragment Block)
      genUnjoinable = do
        taf1@(TestBlockAnchoredFragment c1) <- arbitrary
        TestBlockAnchoredFragment c2 <- arbitrary `suchThat` validUnjoinable taf1
        return (c1, c2)

  shrink (TestJoinableAnchoredFragments_ c pt) =
    [ TestJoinableAnchoredFragments_ c' pt'
    | taf'@(TestAnchoredFragmentAndPoint c' pt') <- shrink (TestAnchoredFragmentAndPoint c pt)
    , validJoinable taf' ]
  shrink (TestUnjoinableAnchoredFragments_ c1 c2) =
    [ TestUnjoinableAnchoredFragments_ c1' c2'
    | taf1'@(TestBlockAnchoredFragment c1') <- shrink (TestBlockAnchoredFragment c1)
    , taf2'@(TestBlockAnchoredFragment c2') <- shrink (TestBlockAnchoredFragment c2)
    , validUnjoinable taf1' taf2' ]

validJoinable :: TestAnchoredFragmentAndPoint -> Bool
validJoinable (TestAnchoredFragmentAndPoint cf p) =
    AF.pointOnFragment p cf

validUnjoinable :: TestBlockAnchoredFragment
                -> TestBlockAnchoredFragment
                -> Bool
validUnjoinable (TestBlockAnchoredFragment c1') (TestBlockAnchoredFragment c2') =
    AF.headPoint c1' /= AF.anchorPoint c2'

validTestJoinableAnchoredFragments :: TestJoinableAnchoredFragments -> Property
validTestJoinableAnchoredFragments t@(TestJoinableAnchoredFragments c1 c2) =
    AF.valid c1 .&&. AF.valid c2 .&&.
    joinable t === (AF.headPoint c1 == AF.anchorPoint c2) .&&.
    case t of
      TestJoinableAnchoredFragments_ c p ->
        validJoinable (TestAnchoredFragmentAndPoint c p)
      TestUnjoinableAnchoredFragments_ c1' c2' ->
        validUnjoinable (TestBlockAnchoredFragment c1') (TestBlockAnchoredFragment c2')

prop_arbitrary_TestJoinableAnchoredFragments
    :: TestJoinableAnchoredFragments -> Property
prop_arbitrary_TestJoinableAnchoredFragments t =
    let j = joinable t in
    cover 45 j       "Joinable"   $
    cover 45 (not j) "Unjoinable" $
    validTestJoinableAnchoredFragments t

-- Note: no prop_shrink_TestJoinableAnchoredFragments because shrinking it
-- gives too many results, which slows down the testsuite significantly.
--
-- Furthermore, the validation that would happen in the test is the same
-- validation that is already happening while generating shrink candidates, so
-- the test would be pretty pointless anyway.



--
-- Generator for two forks based on TestChainFragmentFork
--

-- | A test generator for anchored fragments of two forks of a chain.
--
-- We return four fragments: two prefixes and two fragments that include the
-- respective prefix and an additional suffix. The two prefixes will share
-- identical blocks, but one prefix may contain fewer blocks than the other,
-- i.e., by dropping some of its oldest blocks. We then add some random blocks
-- to each prefix, leading to two forks.
--
-- Note that we might happen to add the same exact block(s) to both prefixes,
-- leading to two identical anchored fragments.
--
data TestAnchoredFragmentFork =
    TestAnchoredFragmentFork
      (AnchoredFragment Block) -- ^ first  fragment
      (AnchoredFragment Block) -- ^ second fragment
      (AnchoredFragment Block) -- ^ first  fork (includes the first fragment)
      (AnchoredFragment Block) -- ^ second fork (includes the second fragment)

instance Show TestAnchoredFragmentFork where
  show (TestAnchoredFragmentFork p1 p2 c1 c2) =
      "TestAnchoredFragmentFork" ++ nl ++
      AF.prettyPrint nl show show p1 ++ nl ++ nl ++
      AF.prettyPrint nl show show p2 ++ nl ++ nl ++
      AF.prettyPrint nl show show c1 ++ nl ++ nl ++
      AF.prettyPrint nl show show c2
    where
      nl = "\n"


instance Arbitrary TestAnchoredFragmentFork where
  arbitrary = do
    TestBlockAnchoredFragment c <- arbitrary
    -- at least 50% should have the same prefix
    samePrefixes <- oneof [pure True, pure False]
    (l1, l2) <-
      if samePrefixes
      then return (c, c)
      else do
        let len = fromIntegral $ AF.length c
        keepNewest1 <- choose (0, len)
        keepNewest2 <- choose (0, len)
        return (AF.anchorNewest keepNewest1 c, AF.anchorNewest keepNewest2 c)
    -- at least 5% of forks should be equal
    sameForks <- frequency [(1, pure True), (19, pure False)]
    (c1, c2) <-
      if sameForks
      then return (l1, l2)
      else do
        n1 <- genNonNegative
        n2 <- genNonNegative
        c1 <- genAddBlocks n1 l1 Nothing
        let ex1 = L.drop (AF.length l1) (AF.toOldestFirst c1)
        c2 <- genAddBlocks n2 l2 (listToMaybe ex1)
        return (c1, c2)
    return (TestAnchoredFragmentFork l1 l2 c1 c2)
    where
      genAddBlocks :: Int
                   -> AnchoredFragment Block
                   -> Maybe Block
                   -> Gen (AnchoredFragment Block)
      genAddBlocks 0 c _       = return c
      genAddBlocks n c Nothing = do
          b <- genAddBlock c
          genAddBlocks (n-1) (AF.addBlock b c) Nothing

      -- But we want to avoid the extensions starting off equal which would
      -- mean the longest common prefix was not the declared common prefix.
      -- So we optionally take the first block to avoid and use that in the
      -- second fork we generate.
      genAddBlocks n c (Just forbiddenBlock) = do
          b <- genAddBlock c `suchThat` (/= forbiddenBlock)
          genAddBlocks (n-1) (AF.addBlock b c) Nothing

  shrink (TestAnchoredFragmentFork l1 l2 c1 c2) =
   -- shrink the first prefix
      [ TestAnchoredFragmentFork l1' l2 c1' c2
      | toDrop <- [1..AF.length l1]
      , let l1' = AF.anchorNewest (fromIntegral (AF.length l1 - toDrop)) l1
            c1' = AF.anchorNewest (fromIntegral (AF.length c1 - toDrop)) c1
      ]
   -- shrink the second prefix
   ++ [ TestAnchoredFragmentFork l1 l2' c1 c2'
      | toDrop <- [1..AF.length l2]
      , let l2' = AF.anchorNewest (fromIntegral (AF.length l2 - toDrop)) l2
            c2' = AF.anchorNewest (fromIntegral (AF.length c2 - toDrop)) c2
      ]
   -- shrink the first fork
   ++ [ TestAnchoredFragmentFork l1 l2 c1' c2
      | toDrop <- [1..(AF.length c1 - AF.length l1)]
      , let c1' = AF.dropNewest toDrop c1
      ]
   -- shrink the second fork
   ++ [ TestAnchoredFragmentFork l1 l2 c1 c2'
      | toDrop <- [1..(AF.length c2 - AF.length l2)]
      , let c2' = AF.dropNewest toDrop c2
      ]

--
-- Test filtering
--

prop_filter :: (Block -> Bool) -> TestBlockAnchoredFragment -> Property
prop_filter p (TestBlockAnchoredFragment chain) =
  let fragments = AF.filter p chain in
      cover 70 (length fragments > 1) "multiple fragments" $
      counterexample ("fragments: " ++ show fragments) $

      -- The fragments contain exactly the blocks where p holds, in order
      (   L.map AF.blockPoint (L.filter p (AF.toOldestFirst chain))
       ===
          L.map AF.blockPoint (concatMap AF.toOldestFirst fragments)
      )
   .&&.
      -- The fragments are non-empty
      all (not . AF.null) fragments
   .&&.
      -- The fragments are of maximum size
      and [ isNothing (AF.join a b)
          | (a,b) <- zip fragments (tail fragments) ]

prop_filterWithStop_always_stop :: (Block -> Bool) -> TestBlockAnchoredFragment -> Property
prop_filterWithStop_always_stop p (TestBlockAnchoredFragment chain) =
    AF.filterWithStop p (const True) chain ===
    if AF.null chain then [] else [chain]

prop_filterWithStop_never_stop :: (Block -> Bool) -> TestBlockAnchoredFragment -> Property
prop_filterWithStop_never_stop p (TestBlockAnchoredFragment chain) =
    AF.filterWithStop p (const False) chain === AF.filter p chain

-- If the stop condition implies that the predicate is true for all the
-- remaining arguments, 'filterWithStop' must be equivalent to 'filter', just
-- optimised.
prop_filterWithStop :: (Block -> Bool) -> (Block -> Bool) -> TestBlockAnchoredFragment -> Property
prop_filterWithStop p stop (TestBlockAnchoredFragment chain) =
    AF.filterWithStop p stop chain ===
    if AF.null chain
    then []
    else appendStopped $ AF.filter p (AF.fromOldestFirst (AF.anchor chain) before)
  where
    before, stopped :: [Block]
    (before, stopped) = break stop $ AF.toOldestFirst chain

    anchor' :: AF.Anchor Block
    anchor' = if null before
                then AF.anchor chain
                else AF.anchorFromBlock (last before)

    stoppedFrag :: AnchoredFragment Block
    stoppedFrag = AF.fromOldestFirst anchor' stopped

    -- If the last fragment in @c@ can be joined with @stoppedFrag@, do so,
    -- otherwise append @stoppedFrag@ as a separate, final fragment. If it is
    -- empty, ignore it.
    appendStopped :: [AnchoredFragment Block] -> [AnchoredFragment Block]
    appendStopped c
      | null stopped
      = c
      | lastFrag:frags <- reverse c
      , Just lastFrag' <- AF.join lastFrag stoppedFrag
      = reverse $ lastFrag':frags
      | otherwise
      = c ++ [stoppedFrag]

prop_filterWithStop_vs_spec ::
     (Block -> Bool)
  -> (Block -> Bool)
  -> TestBlockAnchoredFragment
  -> Property
prop_filterWithStop_vs_spec p stop (TestBlockAnchoredFragment chain) =
    AF.filterWithStop p stop chain === AF.filterWithStopSpec p stop chain

prop_filterWithStop_filter :: TestBlockAnchoredFragment -> Property
prop_filterWithStop_filter (TestBlockAnchoredFragment chain) =
    AF.filterWithStop p stop chain === AF.filter p chain
  where
    p    = (> 5)  . blockSlot
    stop = (> 10) . blockSlot
