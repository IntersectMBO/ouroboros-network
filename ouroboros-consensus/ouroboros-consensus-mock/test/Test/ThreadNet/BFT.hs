{-# LANGUAGE NamedFieldPuns #-}

module Test.ThreadNet.BFT (
    tests
  ) where

import qualified Data.Map as Map
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Cardano.Slotting.Slot

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.BlockchainTime.Mock
import           Ouroboros.Consensus.Mock.Ledger
import           Ouroboros.Consensus.Mock.Node ()
import           Ouroboros.Consensus.Mock.Node.BFT
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.Random (Seed (..))

import           Test.ThreadNet.General
import           Test.ThreadNet.TxGen.Mock ()
import           Test.ThreadNet.Util
import           Test.ThreadNet.Util.NodeJoinPlan
import           Test.ThreadNet.Util.NodeRestarts
import           Test.ThreadNet.Util.NodeTopology

import           Test.Util.Orphans.Arbitrary ()

tests :: TestTree
tests = testGroup "BFT"
    [ testProperty "delayed message corner case" $
        once $
        let ncn = NumCoreNodes 2 in
        prop_simple_bft_convergence (SecurityParam 3) TestConfig
          { numCoreNodes = ncn
          , numSlots = NumSlots 3
          , nodeJoinPlan = NodeJoinPlan (Map.fromList [(CoreNodeId 0,SlotNo {unSlotNo = 0}),(CoreNodeId 1,SlotNo {unSlotNo = 1})])
          , nodeRestarts = noRestarts
          , nodeTopology = meshNodeTopology ncn
          , slotLengths = singletonSlotLengths (slotLengthFromSec 1)
          , initSeed = Seed {getSeed = (12659702313441544615,9326820694273232011,15820857683988100572,2201554969601311572,4716411940989238571)}
          }
    ,
      testProperty "simple convergence" $ \tc ->
        forAll (SecurityParam <$> elements [2 .. 10]) $ \k ->
        prop_simple_bft_convergence k tc
    ]

prop_simple_bft_convergence :: SecurityParam
                            -> TestConfig
                            -> Property
prop_simple_bft_convergence k
  testConfig@TestConfig{numCoreNodes, numSlots, slotLengths} =
    tabulate "slot length changes" [show $ countSlotLengthChanges numSlots slotLengths] $
    prop_general PropGeneralArgs
      { pgaCountTxs               = countSimpleGenTxs
      , pgaExpectedBlockRejection = const False
      , pgaFirstBlockNo           = 0
      , pgaFixedMaxForkLength     = Nothing
      , pgaFixedSchedule          =
          Just $ roundRobinLeaderSchedule numCoreNodes numSlots
      , pgaSecurityParam          = k
      , pgaTestConfig             = testConfig
      }
      testOutput
  where
    testOutput =
        runTestNetwork testConfig epochSize TestConfigBlock
            { forgeEbbEnv = Nothing
            , nodeInfo    = \nid ->
                plainTestNodeInitialization $
                protocolInfoBft numCoreNodes nid k slotLengths
            , rekeying    = Nothing
            }

    -- The mock ledger doesn't really care, and neither does BFT.
    -- We stick with the common @k * 10@ size for now.
    epochSize :: EpochSize
    epochSize = EpochSize (maxRollbacks k * 10)
