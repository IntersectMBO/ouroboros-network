{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE ViewPatterns      #-}

module Test.AnchoredFragment
  ( tests
  , TestBlockAnchoredFragment (..)
  , pattern TestBlockAnchoredFragment
  ) where

import qualified Data.List as L
import           Data.Maybe (isJust, listToMaybe, maybe, maybeToList)
import           Data.Word (Word64)

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Text.Show.Functions ()

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment (..))
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.ChainFragment (ChainFragment)
import qualified Ouroboros.Network.ChainFragment as CF
import           Ouroboros.Network.Testing.ConcreteBlock
import           Test.ChainFragment (TestBlockChainFragment (..),
                     TestChainFragmentAndPoint (..),
                     TestChainFragmentFork (..))
import qualified Test.ChainFragment as CF
import           Test.ChainGenerators (TestBlockChain (..))


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
  , testProperty "dropNewest"                         prop_dropNewest
  , testProperty "takeOldest"                         prop_takeOldest
  , testProperty "dropWhileNewest"                    prop_dropWhileNewest
  , testProperty "addBlock"                           prop_addBlock
  , testProperty "rollback"                           prop_rollback
  , testProperty "rollback/head"                      prop_rollback_head
  , testProperty "successorBlock"                     prop_successorBlock
  , testProperty "pointOnFragment"                    prop_pointOnFragment
  , testProperty "selectPoints"                       prop_selectPoints
  , testProperty "splitAfterPoint"                    prop_splitAfterPoint
  , testProperty "join"                               prop_join
  , testProperty "intersect"                          prop_intersect
  , testProperty "intersect when within bounds"       prop_intersect_bounds
  , testProperty "toChain/fromChain"                  prop_toChain_fromChain
  , testProperty  "anchorNewest"                      prop_anchorNewest
  ]

--
-- Properties
--

prop_length_Empty :: Bool
prop_length_Empty =
    AF.length (Empty anchor :: AnchoredFragment Block) == 0
  where
    anchor = Chain.genesisPoint

prop_dropNewest_Empty :: TestBlockAnchoredFragment -> Bool
prop_dropNewest_Empty (TestBlockAnchoredFragment chain) =
    AF.dropNewest (AF.length chain) chain == Empty anchor
  where
    anchor = AF.anchorPoint chain

prop_fromNewestFirst_toNewestFirst :: TestBlockAnchoredFragment -> Bool
prop_fromNewestFirst_toNewestFirst (TestBlockAnchoredFragment chain) =
    (AF.fromNewestFirst anchor . AF.toNewestFirst) chain == chain
  where
    anchor = AF.anchorPoint chain

prop_fromOldestFirst_toOldestFirst :: TestBlockAnchoredFragment -> Bool
prop_fromOldestFirst_toOldestFirst (TestBlockAnchoredFragment chain) =
    (AF.fromOldestFirst anchor . AF.toOldestFirst) chain == chain
  where
    anchor = AF.anchorPoint chain

headOrAnchor :: a -> [b] -> Either a b
headOrAnchor anchor = maybe (Left anchor) Right . listToMaybe

-- The list comes out in reverse order, most recent block at the head
prop_toList_head :: TestBlockAnchoredFragment -> Bool
prop_toList_head (TestBlockAnchoredFragment chain) =
    (headOrAnchor anchor . AF.toNewestFirst) chain == AF.head chain
  where
    anchor = AF.anchorPoint chain

prop_toList_last :: TestBlockAnchoredFragment -> Bool
prop_toList_last (TestBlockAnchoredFragment chain) =
    (headOrAnchor anchor . AF.toOldestFirst) chain == AF.last chain
  where
    anchor = AF.anchorPoint chain

prop_dropNewest :: TestBlockAnchoredFragment -> Bool
prop_dropNewest (TestBlockAnchoredFragment chain) =
    let blocks = AF.toNewestFirst chain in
    and [ AF.dropNewest n chain == AF.fromNewestFirst anchor (L.drop n blocks)
        | n <- [0..Prelude.length blocks] ]
  where
    anchor = AF.anchorPoint chain

prop_takeOldest :: TestBlockAnchoredFragment -> Bool
prop_takeOldest (TestBlockAnchoredFragment chain) =
    let blocks = AF.toOldestFirst chain in
    and [ AF.takeOldest n chain == AF.fromOldestFirst anchor (L.take n blocks)
        | n <- [0..Prelude.length blocks] ]
  where
    anchor = AF.anchorPoint chain

prop_dropWhileNewest :: (Block -> Bool) -> TestBlockAnchoredFragment -> Bool
prop_dropWhileNewest p (TestBlockAnchoredFragment chain) =
    AF.dropWhileNewest p chain ==
    (AF.fromNewestFirst anchor . L.dropWhile p . AF.toNewestFirst) chain
  where
    anchor = AF.anchorPoint chain

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
    AF.pointOnFragment p c == spec
  where
    spec = CF.pointOnChainFragmentSpec p (AF.unanchorFragment c)

prop_selectPoints :: TestBlockAnchoredFragment -> Property
prop_selectPoints taf@(TestBlockAnchoredFragment c) =
    AF.selectPoints offsets c === CF.selectPointsSpec offsets cf .&&.
    AF.selectPoints []      c === CF.selectPointsSpec []      cf .&&.
    AF.selectPoints [1,1]   c === CF.selectPointsSpec [1,1]   cf
  where
    offsets = [0,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584]
    cf      = toChainFragment taf

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
    AF.toChain (AF.fromChain ch) === Just ch

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

-- Strategy for generators: use the corresponding generator from ChainFragment
-- and use its first (oldest, leftmost) block as the anchor point.
--
-- To enable easy shrinking, we remember that first block, so we can convert
-- back to the original 'ChainFragment' that we can then shrink.
--
-- We don't want to bother the actual test cases with this extra field, so we
-- use a pattern synonym to hide it (not for encapsulation, but for
-- convenience).


-- | A test generator for a valid anchored chain fragment of blocks/headers.
--
data TestBlockAnchoredFragment = TestBlockAnchoredFragment_
    { getTestAnchorBlock      :: Block
    , getTestAnchoredFragment :: AnchoredFragment Block
    }

instance Show TestBlockAnchoredFragment where
  show (TestBlockAnchoredFragment c) =
      "TestBlockAnchoredFragment" ++ nl ++
      AF.prettyPrint nl show show c
    where
      nl = "\n"

pattern TestBlockAnchoredFragment
  :: AnchoredFragment Block -> TestBlockAnchoredFragment
pattern TestBlockAnchoredFragment c <- TestBlockAnchoredFragment_ _ c
{-# COMPLETE TestBlockAnchoredFragment #-}

toTestBlockAnchoredFragment :: ChainFragment Block
                            -> Maybe TestBlockAnchoredFragment
toTestBlockAnchoredFragment c = case c of
    CF.Empty   -> Nothing
    b CF.:< c' -> Just $
        TestBlockAnchoredFragment_ b (AF.mkAnchoredFragment (blockPoint b) c')

toChainFragment :: TestBlockAnchoredFragment
                -> ChainFragment Block
toChainFragment (TestBlockAnchoredFragment_ a af) =
    a CF.:< AF.unanchorFragment af


instance Arbitrary TestBlockAnchoredFragment where
    arbitrary = arbitrary `suchThatMap`
        (toTestBlockAnchoredFragment . getTestBlockChainFragment)
    shrink taf =
      [ taf'
      | c'   <- shrink (TestBlockChainFragment (toChainFragment taf))
      , taf' <- maybeToList $ toTestBlockAnchoredFragment $
                getTestBlockChainFragment c'
      ]

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
data TestAddBlock = TestAddBlock_ TestBlockAnchoredFragment Block

instance Show TestAddBlock where
  show (TestAddBlock c b) =
      "TestAddBlock" ++ nl ++
      AF.prettyPrint nl show show c ++ nl ++
      show b
    where
      nl = "\n"

pattern TestAddBlock
  :: AnchoredFragment Block -> Block -> TestAddBlock
pattern TestAddBlock c b <- TestAddBlock_ (TestBlockAnchoredFragment c) b

{-# COMPLETE TestAddBlock #-}

instance Arbitrary TestAddBlock where
  arbitrary = arbitrary `suchThatMap` \(CF.TestAddBlock chain block) ->
    (`TestAddBlock_` block) <$> toTestBlockAnchoredFragment chain
  shrink (TestAddBlock_ taf b) =
    [ TestAddBlock_ taf' b'
    | let c = toChainFragment taf
    , CF.TestAddBlock c' b' <- shrink (CF.TestAddBlock c b)
    , taf' <- maybeToList $ toTestBlockAnchoredFragment c'
    ]

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
    TestAnchoredFragmentAndPoint_ TestBlockAnchoredFragment (Point Block)

instance Show TestAnchoredFragmentAndPoint where
  show (TestAnchoredFragmentAndPoint c pt) =
      "TestAnchoredFragmentAndPoint" ++ nl ++
      AF.prettyPrint nl show show c ++ nl ++
      show pt
    where
      nl = "\n"

pattern TestAnchoredFragmentAndPoint :: AnchoredFragment Block
                                     -> Point Block
                                     -> TestAnchoredFragmentAndPoint
pattern TestAnchoredFragmentAndPoint c pt <-
        TestAnchoredFragmentAndPoint_ (TestBlockAnchoredFragment c) pt

{-# COMPLETE TestAnchoredFragmentAndPoint #-}

instance Arbitrary TestAnchoredFragmentAndPoint where
  arbitrary = do
    taf <- arbitrary
    let chain = getTestAnchoredFragment taf
    point <- frequency
      [ (2, return (AF.anchorPoint chain))
      , (if AF.null chain then 0 else 7,
         blockPoint <$> elements (AF.toNewestFirst chain))
      -- A few points off the chain!
      , (1, arbitrary)
      ]
    return (TestAnchoredFragmentAndPoint_ taf point)

  shrink (TestAnchoredFragmentAndPoint_ taf p)
    | AF.withinFragmentBounds p (getTestAnchoredFragment taf)
      -- If the point is within the fragment bounds, shrink the fragment and
      -- return all the points within the bounds
    = [ TestAnchoredFragmentAndPoint_ taf' p'
      | taf' <- shrink taf
      , let chain' = getTestAnchoredFragment taf'
      , p' <- AF.anchorPoint chain' : map blockPoint (AF.toNewestFirst chain')
      ]
    | otherwise
      -- If the point is not within the bounds, just shrink the fragment and
      -- return the same point
    = [ TestAnchoredFragmentAndPoint_ taf' p
      | taf' <- shrink taf ]

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
    = TestJoinableAnchoredFragments_ TestChainFragmentAndPoint
      -- ^ We guarantee that the point is on the fragment and that there is at
      -- least one block before it.
    | TestUnjoinableAnchoredFragments_
        TestBlockAnchoredFragment
        TestBlockAnchoredFragment
      -- ^ The fragments are guaranteed to be unjoinable

joinable :: TestJoinableAnchoredFragments -> Bool
joinable (TestJoinableAnchoredFragments_   {}) = True
joinable (TestUnjoinableAnchoredFragments_ {}) = False

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
viewJoinableAnchoredFragments (TestUnjoinableAnchoredFragments_ taf1 taf2) =
    (getTestAnchoredFragment taf1, getTestAnchoredFragment taf2)
viewJoinableAnchoredFragments (TestJoinableAnchoredFragments_ t) =
    case (,) <$> CF.splitAfterPoint cf p <*> CF.splitBeforePoint cf p of
      Just ((cf1, _), (_, cf2)) -> (anchor cf1, anchor cf2)
      Nothing                   -> error msg1
  where
    TestChainFragmentAndPoint cf p = t
    msg1 = "TestJoinableAnchoredFragments: not splittable"
    msg2 = "TestJoinableAnchoredFragments: chain fragment cannot be anchored "
    anchor :: ChainFragment Block -> AnchoredFragment Block
    anchor cf' = maybe (error (msg2 <> show cf')) getTestAnchoredFragment $
      toTestBlockAnchoredFragment cf'

pattern TestJoinableAnchoredFragments
  :: AnchoredFragment Block
  -> AnchoredFragment Block
  -> TestJoinableAnchoredFragments
pattern TestJoinableAnchoredFragments c1 c2 <-
    (viewJoinableAnchoredFragments -> (c1, c2))

{-# COMPLETE TestJoinableAnchoredFragments #-}

instance Arbitrary TestJoinableAnchoredFragments where
  arbitrary = frequency
      [ (1, TestJoinableAnchoredFragments_           <$> genJoinable)
      , (1, uncurry TestUnjoinableAnchoredFragments_ <$> genUnjoinable)
      ]
    where
      genJoinable :: Gen TestChainFragmentAndPoint
      genJoinable = arbitrary `suchThat` validJoinable

      genUnjoinable :: Gen (TestBlockAnchoredFragment, TestBlockAnchoredFragment)
      genUnjoinable = do
        taf1 <- arbitrary
        taf2 <- arbitrary `suchThat` (validUnjoinable taf1)
        return (taf1, taf2)

  shrink (TestJoinableAnchoredFragments_ t) =
    [ TestJoinableAnchoredFragments_ t'
    | t' <- shrink t
    , validJoinable t' ]
  shrink (TestUnjoinableAnchoredFragments_ t1 t2) =
    [ TestUnjoinableAnchoredFragments_ t1' t2'
    | t1' <- shrink t1
    , t2' <- shrink t2
    , validUnjoinable t1' t2' ]

validJoinable :: TestChainFragmentAndPoint -> Bool
validJoinable (TestChainFragmentAndPoint cf p) =
    case (,) <$> CF.splitAfterPoint cf p <*> CF.splitBeforePoint cf p of
      Just ((cf1, _), (_, cf2)) -> CF.length cf1 >= 1 && CF.length cf2 >= 1
      Nothing                   -> False

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
      TestJoinableAnchoredFragments_   j     -> validJoinable j
      TestUnjoinableAnchoredFragments_ t1 t2 -> validUnjoinable t1 t2


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

newtype TestAnchoredFragmentFork = TestAnchoredFragmentFork_ TestChainFragmentFork
    -- We guarantee that the first two 'ChainFragment's in
    -- TestChainFragmentFork (and consequently the last two arguments also)
    -- can be turned into 'AnchoredFragment's.

instance Show TestAnchoredFragmentFork where
  show (TestAnchoredFragmentFork p1 p2 c1 c2) =
      "TestAnchoredFragmentFork" ++ nl ++
      AF.prettyPrint nl show show p1 ++ nl ++ nl ++
      AF.prettyPrint nl show show p2 ++ nl ++ nl ++
      AF.prettyPrint nl show show c1 ++ nl ++ nl ++
      AF.prettyPrint nl show show c2
    where
      nl = "\n"

viewAnchoredFragmentForks
    :: TestAnchoredFragmentFork
    -> (AnchoredFragment Block, AnchoredFragment Block,
        AnchoredFragment Block, AnchoredFragment Block)
viewAnchoredFragmentForks (TestAnchoredFragmentFork_ tcff) =
    (anchor p1, anchor p2, anchor s1, anchor s2)
  where
    msg = "TestAnchoredFragmentFork: chain fragment cannot be anchored "
    anchor :: ChainFragment Block -> AnchoredFragment Block
    anchor cf = maybe (error (msg <> show cf)) getTestAnchoredFragment $
      toTestBlockAnchoredFragment cf

    TestChainFragmentFork p1 p2 s1 s2 = tcff

pattern TestAnchoredFragmentFork
    :: AnchoredFragment Block
    -> AnchoredFragment Block
    -> AnchoredFragment Block
    -> AnchoredFragment Block
    -> TestAnchoredFragmentFork
pattern TestAnchoredFragmentFork p1 p2 c1 c2 <-
    (viewAnchoredFragmentForks -> (p1, p2, c1, c2))

{-# COMPLETE TestAnchoredFragmentFork #-}

instance Arbitrary TestAnchoredFragmentFork where
  arbitrary = do
    tcff <- arbitrary `suchThat` \(TestChainFragmentFork p1 p2 _ _) ->
      not (CF.null p1) && not (CF.null p2)
    return (TestAnchoredFragmentFork_ tcff)
  shrink (TestAnchoredFragmentFork_ tcff) =
    [ TestAnchoredFragmentFork_ tcff'
    | tcff'@(TestChainFragmentFork p1 p2 _ _) <- shrink tcff
    , not (CF.null p1) && not (CF.null p2)
    ] ++
    -- Also try just shrinking the suffixes
    [ TestAnchoredFragmentFork_  (TestChainFragmentFork p1 p2 cf1' cf2')
    | let TestChainFragmentFork p1 p2 cf1 cf2 = tcff
    , n1 <- [1..CF.length cf1 - CF.length p1]
    , n2 <- [1..CF.length cf2 - CF.length p2]
    , let cf1' = CF.dropNewest n1 cf1
          cf2' = CF.dropNewest n2 cf2
    ]
