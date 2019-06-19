{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Test.Ouroboros.Storage.ChainDB.Model (
    tests
  ) where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Consensus.Protocol.Abstract
import qualified Ouroboros.Consensus.Util.AnchoredFragment as AF
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (HasHeader (..))
import           Ouroboros.Network.Chain (genesisPoint)
import           Ouroboros.Storage.ChainDB.API (StreamFrom (..), StreamTo (..))
import qualified Ouroboros.Storage.ChainDB.Model as M
import           Test.Ouroboros.Storage.ChainDB.TestBlock

tests :: TestTree
tests = testGroup "Model" [
      testProperty "getBlock_addBlock"        prop_getBlock_addBlock
    , testProperty "getChain_addChain"        prop_getChain_addChain
    , testProperty "alwaysPickPreferredChain" prop_alwaysPickPreferredChain
    , testProperty "between_currentChain"     prop_between_currentChain
    ]

prop_getBlock_addBlock :: BlockTree -> Permutation -> Property
prop_getBlock_addBlock bt p =
        M.getBlock (blockHash newBlock) (M.addBlock singleNodeTestConfig newBlock model)
    === if blockNo newBlock > M.immutableBlockNo secParam model
        then Just newBlock
        else Nothing
  where
    (newBlock:initBlocks) = permute p $ treeToBlocks bt
    model = M.addBlocks singleNodeTestConfig initBlocks (M.empty testInitExtLedger)
    secParam = protocolSecurityParam singleNodeTestConfig

prop_getChain_addChain :: BlockChain -> Property
prop_getChain_addChain bc =
    blockChain bc === M.currentChain model
  where
    blocks = chainToBlocks bc
    model  = M.addBlocks singleNodeTestConfig blocks (M.empty testInitExtLedger)

prop_alwaysPickPreferredChain :: BlockTree -> Permutation -> Property
prop_alwaysPickPreferredChain bt p =
    conjoin [
        not $ preferCandidate' candidate
      | candidate <- treeToChains bt
      ]
  where
    blocks  = permute p $ treeToBlocks bt
    model   = M.addBlocks singleNodeTestConfig blocks (M.empty testInitExtLedger)
    current = M.currentChain model

    curFragment = AF.fromChain current

    SecurityParam k = protocolSecurityParam singleNodeTestConfig

    preferCandidate' candidate =
        preferCandidate singleNodeTestConfig curFragment candFragment &&
        AF.forksAtMostKBlocks k curFragment candFragment
      where
        candFragment = AF.fromChain candidate

-- TODO add properties about forks too
prop_between_currentChain :: BlockTree -> Property
prop_between_currentChain bt =
    Right (AF.toOldestFirst $ AF.fromChain $ M.currentChain model) ===
    M.between secParam from to model
  where
    blocks   = treeToBlocks bt
    model    = M.addBlocks singleNodeTestConfig blocks (M.empty testInitExtLedger)
    from     = StreamFromExclusive genesisPoint
    to       = StreamToInclusive $ M.tipPoint model
    secParam = protocolSecurityParam singleNodeTestConfig
