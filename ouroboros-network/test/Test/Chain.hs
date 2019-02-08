{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeOperators    #-}

module Test.Chain
  ( tests
  ) where

import qualified Data.List as L
import           Data.Maybe (listToMaybe)

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain (Chain (..), ChainUpdate (..),
                     Point (..), genesisPoint)
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.Serialise (prop_serialise)

import           Test.Ouroboros.Network.Testing.Arbitrary


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

    , testProperty "arbitrary for TestChainAndRange" prop_arbitrary_TestChainAndRange
    , testProperty "shrink for TestChainAndRange"    prop_shrink_TestChainAndRange

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
  , testProperty "selectBlockRange"prop_selectBlockRange
  , testProperty "serialise chain" prop_serialise_chain
  ]

--
-- Properties
--

prop_length_genesis :: Bool
prop_length_genesis = Chain.length Genesis == 0

prop_drop_genesis :: TestBlockChain -> Bool
prop_drop_genesis (TestBlockChain chain) =
    Chain.drop (Chain.length chain) chain == Genesis

prop_fromList_toList :: TestBlockChain -> Bool
prop_fromList_toList (TestBlockChain chain) =
    (Chain.fromNewestFirst . Chain.toNewestFirst) chain == chain

-- The list comes out in reverse order, most recent block at the head
prop_toList_head :: TestBlockChain -> Bool
prop_toList_head (TestBlockChain chain) =
    (listToMaybe . Chain.toNewestFirst) chain == Chain.head chain

prop_drop :: TestBlockChain -> Bool
prop_drop (TestBlockChain chain) =
    let blocks = Chain.toNewestFirst chain in
    and [ Chain.drop n chain == Chain.fromNewestFirst (L.drop n blocks)
        | n <- [0..Prelude.length blocks] ]

prop_addBlock :: TestAddBlock -> Bool
prop_addBlock (TestAddBlock c b) =
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

prop_rollback :: TestChainAndPoint -> Bool
prop_rollback (TestChainAndPoint c p) =
    case Chain.rollback p c of
      Nothing -> True
      Just c' ->
        -- chain is a prefix of original
             Chain.isPrefixOf c' c
        -- chain head point is the rollback point
        && Chain.headPoint c' == p

prop_rollback_head :: TestBlockChain -> Bool
prop_rollback_head (TestBlockChain c) =
    Chain.rollback (Chain.headPoint c) c == Just c

prop_successorBlock :: TestChainAndPoint -> Property
prop_successorBlock (TestChainAndPoint c p) =
  Chain.pointOnChain p c ==>
  case Chain.successorBlock p c of
    Nothing -> Chain.headPoint c === p
    Just b  -> property $ Chain.pointOnChain (Chain.blockPoint b) c

prop_lookupBySlot :: TestChainAndPoint -> Bool
prop_lookupBySlot (TestChainAndPoint c p) =
  case Chain.lookupBySlot c (pointSlot p) of
    Just b  -> Chain.pointOnChain (Chain.blockPoint b) c
    Nothing | p == genesisPoint -> True
            | otherwise         -> not (Chain.pointOnChain p c)

prop_selectBlockRange :: TestChainAndRange -> Bool
prop_selectBlockRange (TestChainAndRange c p1 p2) =
  case Chain.selectBlockRange c p1 p2 of
    Just [] -> p1 == p2 && Chain.pointOnChain p1 c

    Just bs@(b:_) -> blockPrevHash b == pointHash p1
                  && Chain.blockPoint (last bs) == p2

    Nothing -> not (Chain.pointOnChain p1 c)
            || not (Chain.pointOnChain p2 c)

prop_intersectChains :: TestChainFork -> Bool
prop_intersectChains (TestChainFork c l r) =
  case Chain.intersectChains l r of
    Nothing -> c == Genesis && L.intersect (Chain.toNewestFirst l) (Chain.toNewestFirst r) == []
    Just p  -> Chain.headPoint c == p
            && Chain.pointOnChain p l
            && Chain.pointOnChain p r

prop_serialise_chain :: TestBlockChain -> Property
prop_serialise_chain (TestBlockChain chain) =
  prop_serialise chain

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

prop_arbitrary_TestAddBlock :: TestAddBlock -> Bool
prop_arbitrary_TestAddBlock (TestAddBlock c b) =
    Chain.valid (c :> b)

prop_shrink_TestAddBlock :: TestAddBlock -> Bool
prop_shrink_TestAddBlock t =
    and [ Chain.valid (c :> b) | TestAddBlock c b <- shrink t ]

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

prop_arbitrary_TestChainAndPoint :: TestChainAndPoint -> Property
prop_arbitrary_TestChainAndPoint (TestChainAndPoint c p) =
  let onChain = Chain.pointOnChain p c in
  cover (85/100) onChain       "point on chain" $
  cover ( 5/100) (not onChain) "point not on chain" $
    Chain.valid c

prop_shrink_TestChainAndPoint :: TestChainAndPoint -> Bool
prop_shrink_TestChainAndPoint cp@(TestChainAndPoint c _) =
  and [     Chain.valid c'
        && (Chain.pointOnChain p c `implies` Chain.pointOnChain p c')
      | TestChainAndPoint c' p <- shrink cp ]

prop_arbitrary_TestChainAndRange :: TestChainAndRange -> Property
prop_arbitrary_TestChainAndRange (TestChainAndRange c p1 p2) =
  let onChain = Chain.pointOnChain p1 c && Chain.pointOnChain p2 c in
  cover (85/100) onChain               "points on chain" $
  cover ( 5/100) (onChain && p1 == p2) "empty range" $
  cover ( 5/100) (not onChain)         "points not on chain" $
    Chain.valid c
 && onChain `implies` pointSlot p2 >= pointSlot p1

prop_shrink_TestChainAndRange :: TestChainAndRange -> Bool
prop_shrink_TestChainAndRange cp@(TestChainAndRange c _ _) =
  and [    Chain.valid c'
        && (Chain.pointOnChain p1 c && Chain.pointOnChain p2 c
            `implies`
            Chain.pointOnChain p1 c' && Chain.pointOnChain p2 c')
      | TestChainAndRange c' p1 p2 <- shrink cp ]

prop_arbitrary_TestChainFork :: TestChainFork -> Bool
prop_arbitrary_TestChainFork (TestChainFork c l r) =
    Chain.valid c && Chain.valid l && Chain.valid r
 && c `Chain.isPrefixOf` l
 && c `Chain.isPrefixOf` r

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
