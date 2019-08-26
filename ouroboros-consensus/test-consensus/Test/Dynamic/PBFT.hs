module Test.Dynamic.PBFT (
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
      testProperty "simple PBFT convergence" $
        prop_simple_pbft_convergence k
    ]
  where
    k = defaultSecurityParam

prop_simple_pbft_convergence :: SecurityParam
                             -> NumCoreNodes
                             -> NumSlots
                             -> Seed
                             -> Property
prop_simple_pbft_convergence
  k numCoreNodes@(NumCoreNodes nn) numSlots seed =
    prop_general k
        (roundRobinLeaderSchedule numCoreNodes numSlots)
        testOutput
  where
    sigWin = fromIntegral $ nn * 10
    sigThd = (1.0 / fromIntegral nn) + 0.1
    params = PBftParams
      { pbftSecurityParam      = k
      , pbftNumNodes           = fromIntegral nn
      , pbftSlotLength         = slotLengthFromSec 20
      , pbftSignatureWindow    = sigWin
      , pbftSignatureThreshold = sigThd
      }

    testOutput =
        runTestNetwork
            (\nid -> protocolInfo numCoreNodes nid (ProtocolMockPBFT params))
            numCoreNodes numSlots seed
