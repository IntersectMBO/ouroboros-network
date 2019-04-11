{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Test.Ouroboros.Storage.ChainDB.Model (
    tests
  ) where

import           Codec.Serialise (Serialise(encode))
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Network.Block (HasHeader (..))
import qualified Ouroboros.Storage.ChainDB.Model as M
import           Test.Ouroboros.Storage.ChainDB.TestBlock

tests :: TestTree
tests = testGroup "Model" [
      testProperty "getBlock_addBlock"        prop_getBlock_addBlock
    , testProperty "getChain_addChain"        prop_getChain_addChain
    , testProperty "alwaysPickPreferredChain" prop_alwaysPickPreferredChain
    ]

prop_getBlock_addBlock :: BlockTree -> Permutation -> Property
prop_getBlock_addBlock bt p =
        M.getBlock (blockHash newBlock) (M.addBlock encode testConfig newBlock model)
    === Just newBlock
  where
    (newBlock:initBlocks) = permute p $ treeToBlocks bt
    model = M.addBlocks encode testConfig initBlocks (M.empty testInitExtLedger)

prop_getChain_addChain :: BlockChain -> Property
prop_getChain_addChain bc =
    blockChain bc === M.currentChain model
  where
    blocks = chainToBlocks bc
    model  = M.addBlocks encode testConfig blocks (M.empty testInitExtLedger)

prop_alwaysPickPreferredChain :: BlockTree -> Permutation -> Property
prop_alwaysPickPreferredChain bt p =
    conjoin [
        not $ preferCandidate testConfig current candidate
      | candidate <- treeToChains bt
      ]
  where
    blocks  = permute p $ treeToBlocks bt
    model   = M.addBlocks encode testConfig blocks (M.empty testInitExtLedger)
    current = M.currentChain model
