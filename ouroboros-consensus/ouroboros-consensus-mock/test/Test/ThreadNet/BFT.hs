{-# LANGUAGE NamedFieldPuns #-}

module Test.ThreadNet.BFT (
    tests
  ) where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Consensus.BlockchainTime.Mock
import           Ouroboros.Consensus.Mock.Ledger
import           Ouroboros.Consensus.Mock.Node ()
import           Ouroboros.Consensus.Mock.Node.BFT
import           Ouroboros.Consensus.Protocol.Abstract

import           Test.ThreadNet.General
import           Test.ThreadNet.TxGen.Mock ()
import           Test.ThreadNet.Util

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
    prop_general
        countSimpleGenTxs
        k
        testConfig
        (Just $ roundRobinLeaderSchedule numCoreNodes numSlots)
        (const False)
        testOutput
  where
    testOutput =
        runTestNetwork testConfig TestConfigBlock
            { forgeEbbEnv = Nothing
            , nodeInfo    = \nid ->
                protocolInfoBft numCoreNodes nid k slotLengths
            , rekeying    = Nothing
            }
