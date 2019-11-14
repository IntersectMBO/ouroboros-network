{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Test.Ouroboros.Storage.ChainDB.Model.Test (
    tests
  ) where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (HasHeader (..), genesisPoint)
import qualified Ouroboros.Network.MockChain.Chain as Chain

import           Ouroboros.Consensus.Protocol.Abstract
import qualified Ouroboros.Consensus.Util.AnchoredFragment as AF

import           Ouroboros.Storage.ChainDB.API (StreamFrom (..), StreamTo (..))

import           Test.Util.TestBlock

import qualified Test.Ouroboros.Storage.ChainDB.Model as M

tests :: TestTree
tests = testGroup "Model" [
      testProperty "getBlock_addBlock"        prop_getBlock_addBlock
    , testProperty "getChain_addChain"        prop_getChain_addChain
    , testProperty "alwaysPickPreferredChain" prop_alwaysPickPreferredChain
    , testProperty "between_currentChain"     prop_between_currentChain
    ]

addBlocks :: [TestBlock] -> M.Model TestBlock
addBlocks blks = M.addBlocks cfg blks m
  where
    cfg = singleNodeTestConfig
    -- Set the current slot to 'maxBound' so that no block is in the future
    m   = M.advanceCurSlot cfg maxBound (M.empty testInitExtLedger)

prop_getBlock_addBlock :: BlockTree -> Permutation -> Property
prop_getBlock_addBlock bt p =
        M.getBlock (blockHash newBlock) (M.addBlock singleNodeTestConfig newBlock model)
    === if blockNo newBlock > M.immutableBlockNo secParam model
        then Just newBlock
        else Nothing
  where
    (newBlock:initBlocks) = permute p $ treeToBlocks bt
    model = addBlocks initBlocks
    secParam = protocolSecurityParam singleNodeTestConfig

prop_getChain_addChain :: BlockChain -> Property
prop_getChain_addChain bc =
    counterexample ("model: " ++ show model) $
    blockChain bc === M.currentChain model
  where
    blocks = chainToBlocks bc
    model  = addBlocks blocks

prop_alwaysPickPreferredChain :: BlockTree -> Permutation -> Property
prop_alwaysPickPreferredChain bt p =
    conjoin [
        not $ preferCandidate' candidate
      | candidate <- treeToChains bt
      ]
  where
    blocks  = permute p $ treeToBlocks bt
    model   = addBlocks blocks
    current = M.currentChain model

    curFragment = Chain.toAnchoredFragment current

    SecurityParam k = protocolSecurityParam singleNodeTestConfig

    preferCandidate' candidate =
        AF.preferAnchoredCandidate singleNodeTestConfig curFragment candFragment &&
        AF.forksAtMostKBlocks k curFragment candFragment
      where
        candFragment = Chain.toAnchoredFragment candidate

-- TODO add properties about forks too
prop_between_currentChain :: BlockTree -> Property
prop_between_currentChain bt =
    Right (AF.toOldestFirst $ Chain.toAnchoredFragment $ M.currentChain model) ===
    M.between secParam from to model
  where
    blocks   = treeToBlocks bt
    model    = addBlocks blocks
    from     = StreamFromExclusive genesisPoint
    to       = StreamToInclusive $ M.tipPoint model
    secParam = protocolSecurityParam singleNodeTestConfig
