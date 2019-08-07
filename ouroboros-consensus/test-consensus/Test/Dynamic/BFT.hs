{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS -fno-warn-orphans #-}

module Test.Dynamic.BFT (
    tests
  ) where

import qualified Data.Map.Strict as Map

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Demo
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Protocol
import           Ouroboros.Consensus.Util.Random
import           Ouroboros.Network.MockChain.Chain (Chain)

import           Test.Dynamic.General
import           Test.Dynamic.Util

import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Range

tests :: TestTree
tests = testGroup "Dynamic chain generation" [
      testProperty "simple BFT convergence" $
        prop_simple_bft_convergence params
    ]
  where
    params = defaultSecurityParam

prop_simple_bft_convergence :: SecurityParam
                            -> NumCoreNodes
                            -> NumSlots
                            -> Seed
                            -> Property
prop_simple_bft_convergence k numCoreNodes =
    prop_simple_protocol_convergence
      (\nid -> protocolInfo numCoreNodes nid (ProtocolMockBFT k))
      isValid
      numCoreNodes
  where
    isValid :: TestOutput (SimpleBftBlock SimpleMockCrypto BftMockCrypto)
            -> Property
    isValid TestOutput{testOutputNodes = final} =
        counterexample (show final') $
        tabulate "shortestLength" [show (rangeK k (shortestLength final'))] $
        allEqual (takeChainPrefix <$> Map.elems final')
      where
        -- Without the 'NodeConfig's
        final' = snd <$> final
        takeChainPrefix :: Chain (SimpleBftBlock SimpleMockCrypto BftMockCrypto)
                        -> Chain (SimpleBftBlock SimpleMockCrypto BftMockCrypto)
        takeChainPrefix = id -- in BFT, chains should indeed all be equal.
