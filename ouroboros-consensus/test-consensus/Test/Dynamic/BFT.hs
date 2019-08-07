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

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Demo
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Protocol
import           Ouroboros.Consensus.Util.Random

import           Test.Dynamic.General
import           Test.Dynamic.Util

import           Test.Util.Orphans.Arbitrary ()

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
prop_simple_bft_convergence k numCoreNodes numSlots seed =
    prop_general k
        (roundRobinLeaderSchedule numCoreNodes numSlots)
        testOutput
  where
    testOutput =
        runTestNetwork
            (\nid -> protocolInfo numCoreNodes nid (ProtocolMockBFT k))
            numCoreNodes numSlots seed
