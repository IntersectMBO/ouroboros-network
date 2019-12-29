{-# LANGUAGE NamedFieldPuns #-}

module Test.ThreadNet.BFT (
    tests
  ) where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Consensus.BlockchainTime.Mock
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Protocol

import           Test.ThreadNet.General
import           Test.ThreadNet.Util

import           Test.Consensus.BlockchainTime.SlotLengths ()
import           Test.Util.Orphans.Arbitrary ()

tests :: TestTree
tests = testGroup "BFT" [
      testProperty "simple convergence" $
        prop_simple_bft_convergence k
    ]
  where
    k = SecurityParam 5

prop_simple_bft_convergence :: SecurityParam
                            -> TestConfig
                            -> Property
prop_simple_bft_convergence k
  testConfig@TestConfig{numCoreNodes, numSlots, slotLengths} =
    tabulate "slot length changes" [show $ countSlotLengthChanges numSlots slotLengths] $
    prop_general k
        testConfig
        (Just $ roundRobinLeaderSchedule numCoreNodes numSlots)
        testOutput
  where
    testOutput =
        runTestNetwork testConfig TestConfigBlock
            { forgeEBB = Nothing
            , nodeInfo = \nid -> protocolInfo $
                ProtocolMockBFT numCoreNodes nid k slotLengths
            }
