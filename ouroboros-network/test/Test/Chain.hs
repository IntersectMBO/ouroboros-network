{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE TypeOperators    #-}

module Test.Chain (tests) where

import qualified Data.List as L
import           Data.Maybe (listToMaybe)

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Ouroboros.Network.Block (blockPrevHash, pattern GenesisPoint,
                     pointHash)
import           Ouroboros.Network.MockChain.Chain (Chain (..))
import qualified Ouroboros.Network.MockChain.Chain as Chain
import           Ouroboros.Network.Testing.Serialise (prop_serialise)

import           Test.ChainGenerators hiding (tests)


--
-- The list of all tests
--

tests :: TestTree
tests = testGroup "Chain"
  [ testProperty "length/Genesis"  prop_length_genesis
  , testProperty "drop/Genesis"    prop_drop_genesis
  , testProperty "fromList/toList" prop_fromList_toList
  , testProperty "toList/head"     prop_toList_head
  , testProperty "drop"            prop_drop
  , testProperty "addBlock"        prop_addBlock
  , testProperty "pointOnChain"    prop_pointOnChain
  , testProperty "pointIsAfter"    prop_pointIsAfter
  , testProperty "rollback"        prop_rollback
  , testProperty "rollback/head"   prop_rollback_head
  , testProperty "successorBlock"  prop_successorBlock
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

prop_pointOnChain :: TestChainAndPoint -> Bool
prop_pointOnChain (TestChainAndPoint c p) =
    spec p c == Chain.pointOnChain p c
  where
    spec GenesisPoint = const True
    spec pt           = any ((== pt) . Chain.blockPoint) . Chain.chainToList

prop_pointIsAfter :: TestChainAndPoint -> Property
prop_pointIsAfter (TestChainAndPoint c p) =
  Chain.pointOnChain p c && p /= GenesisPoint ==>
  let (beforeP, _p:afterP) =
        break (== p) $ map Chain.blockPoint $ Chain.chainToList c
  in    all (\before -> not (Chain.pointIsAfter p before c) &&
                             Chain.pointIsAfter before p c)
            beforeP
     && not (Chain.pointIsAfter p p c)
     && all (\after ->      Chain.pointIsAfter p after c &&
                       not (Chain.pointIsAfter after p c))
            afterP

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
