{-# LANGUAGE NamedFieldPuns #-}

module Test.ThreadNet.PBFT (
    tests
  ) where

import           Test.QuickCheck

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Network.Block (SlotNo (..))

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Ledger.Extended (ExtValidationError (..))
import           Ouroboros.Consensus.Ledger.Mock.Block
import           Ouroboros.Consensus.Ledger.Mock.Block.PBFT
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol

import           Test.ThreadNet.General
import           Test.ThreadNet.Util

import           Test.Util.Orphans.Arbitrary ()

tests :: TestTree
tests = testGroup "PBFT" [
      testProperty "simple convergence" $
        prop_simple_pbft_convergence k
    ]
  where
    k = SecurityParam 5

prop_simple_pbft_convergence :: SecurityParam
                             -> TestConfig
                             -> Property
prop_simple_pbft_convergence
  k testConfig@TestConfig{numCoreNodes, numSlots} =
    prop_general k
        testConfig
        (Just $ roundRobinLeaderSchedule numCoreNodes numSlots)
        (expectedBlockRejection numCoreNodes)
        testOutput
  where
    NumCoreNodes nn = numCoreNodes

    sigThd = (1.0 / fromIntegral nn) + 0.1
    params = PBftParams k (fromIntegral nn) sigThd (slotLengthFromSec 20)

    testOutput =
        runTestNetwork testConfig TestConfigBlock
            { forgeEBB = Nothing
            , nodeInfo = \nid -> protocolInfo $
                ProtocolMockPBFT numCoreNodes nid params
            }

type Blk = SimpleBlock SimpleMockCrypto
             (SimplePBftExt SimpleMockCrypto PBftMockCrypto)

expectedBlockRejection :: NumCoreNodes -> BlockRejection Blk -> Bool
expectedBlockRejection (NumCoreNodes nn) BlockRejection
  { brBlockSlot = SlotNo s
  , brReason    = err
  , brRejector  = CoreId i
  }
  | ownBlock               = case err of
    ExtValidationErrorOuroboros
      PBftExceededSignThreshold{} -> True
    _                             -> False
  where
    -- Because of round-robin and the fact that the id divides slot, we know
    -- the node lead but rejected its own block. This is the only case we
    -- expect. (Rejecting its own block also prevents the node from propagating
    -- that block.)
    ownBlock = fromIntegral i == mod s (fromIntegral nn)
expectedBlockRejection _ _ = False
