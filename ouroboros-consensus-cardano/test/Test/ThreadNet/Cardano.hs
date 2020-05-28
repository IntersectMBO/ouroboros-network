{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE TypeApplications         #-}
module Test.ThreadNet.Cardano (
    tests
  ) where

import           Data.List ((!!))

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Cardano.Slotting.Slot (EpochSize (..))

import           Ouroboros.Consensus.Ledger.SupportsMempool (extractTxs)
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Degenerate
import           Ouroboros.Consensus.HardFork.Combinator.Unary

import           Test.ThreadNet.General
import           Test.ThreadNet.Infra.Shelley
import           Test.Util.Orphans.Arbitrary ()

import           Ouroboros.Consensus.Shelley.Node

import           Ouroboros.Consensus.Cardano.Block

import           Test.Consensus.Shelley.MockCrypto (TPraosMockCrypto)
import           Test.ThreadNet.TxGen.Cardano ()
import           Test.ThreadNet.TxGen.Shelley

tests :: TestTree
tests = testGroup "Cardano" $
    [ testProperty "simple convergence" $ withMaxSuccess 20 $
          forAllShrink
              (genShelleyTestConfig testTuning)
              shrinkShelleyTestConfig
            $ \testConfig ->
          prop_simple_cardano_convergence testConfig
    ]

prop_simple_cardano_convergence
  :: ShelleyTestConfig
  -> Property
prop_simple_cardano_convergence
  ShelleyTestConfig{stcTestConfig, stcGenesis, stcCoreNodes} =
    prop_general PropGeneralArgs
      { pgaBlockProperty          = const $ property True
      , pgaCountTxs               = fromIntegral . length . extractTxs
      , pgaExpectedBlockRejection = const False
      , pgaFirstBlockNo           = 0
      , pgaFixedMaxForkLength     = Nothing
      , pgaFixedSchedule          = Nothing
      , pgaSecurityParam          = k
      , pgaTestConfig             = stcTestConfig
      }
      testOutput
  where
    testOutput :: TestOutput (CardanoBlock TPraosMockCrypto)
    testOutput =
        runTestNetwork stcTestConfig epochSize TestConfigBlock
            { forgeEbbEnv = Nothing
            , nodeInfo    = \(CoreNodeId nid) -> plainTestNodeInitialization $
                castProtocolInfo $ injProtocolInfo
                  (sgSystemStart stcGenesis)
                  (mkProtocolRealTPraos
                    stcGenesis
                    (stcCoreNodes !! fromIntegral nid))
            , rekeying    = Nothing
            , txGenExtra  = ShelleyTxGenExtra $ mkGenEnv stcCoreNodes
            }

    epochSize :: EpochSize
    epochSize = sgEpochLength stcGenesis

    k = sgSecurityParam stcGenesis
