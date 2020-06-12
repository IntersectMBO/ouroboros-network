{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE TypeApplications         #-}
module Test.ThreadNet.Cardano (
    tests
  ) where

import           Control.Monad (replicateM)
import           Data.List ((!!))
import           Data.Word (Word64)

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Cardano.Slotting.Slot (EpochSize (..))

import           Ouroboros.Consensus.Config.SecurityParam
import           Ouroboros.Consensus.Ledger.SupportsMempool (extractTxs)
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Util.Random

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Degenerate
import           Ouroboros.Consensus.HardFork.Combinator.Unary

import           Test.ThreadNet.General
import           Test.ThreadNet.Infra.Shelley
import           Test.Util.Orphans.Arbitrary ()

import qualified Shelley.Spec.Ledger.OCert as SL

import           Ouroboros.Consensus.Shelley.Node

import           Ouroboros.Consensus.Cardano.Block

import           Test.Consensus.Shelley.MockCrypto (TPraosMockCrypto)
import           Test.ThreadNet.TxGen.Cardano ()
import           Test.ThreadNet.TxGen.Shelley
import           Test.ThreadNet.Util.NodeJoinPlan (trivialNodeJoinPlan)
import           Test.ThreadNet.Util.NodeRestarts (noRestarts)

data TestSetup = TestSetup
  { setupD          :: Double
    -- ^ decentralization parameter
  , setupK          :: SecurityParam
  , setupTestConfig :: TestConfig
  }
  deriving (Show)

instance Arbitrary TestSetup where
  arbitrary = do
    setupD <- (/10)         <$> choose   (1, 10)
    setupK <- SecurityParam <$> elements [5, 10]

    setupTestConfig <- arbitrary

    pure TestSetup
      { setupD
      , setupK
      , setupTestConfig
      }

  -- TODO shrink

tests :: TestTree
tests = testGroup "Cardano" $
    [ testProperty "simple convergence" $ withMaxSuccess 20 $ \setup ->
          prop_simple_cardano_convergence setup
    ]

prop_simple_cardano_convergence :: TestSetup -> Property
prop_simple_cardano_convergence TestSetup
  { setupD
  , setupK
  , setupTestConfig
  } =
    prop_general PropGeneralArgs
      { pgaBlockProperty      = const $ property True
      , pgaCountTxs           = fromIntegral . length . extractTxs
      , pgaExpectedCannotLead = noExpectedCannotLeads
      , pgaFirstBlockNo       = 0
      , pgaFixedMaxForkLength = Nothing
      , pgaFixedSchedule      = Nothing
      , pgaSecurityParam      = setupK
      , pgaTestConfig         = setupTestConfig
      , pgaTestConfigB        = testConfigB
      }
      testOutput
  where
    TestConfig
      { initSeed
      , numCoreNodes
      } = setupTestConfig

    testConfigB = TestConfigB
      { epochSize
      , forgeEbbEnv  = Nothing
      , nodeJoinPlan = trivialNodeJoinPlan numCoreNodes
      , nodeRestarts = noRestarts
      , slotLength   = tpraosSlotLength
      , txGenExtra   = ShelleyTxGenExtra $ mkGenEnv coreNodes
      }

    testOutput :: TestOutput (CardanoBlock TPraosMockCrypto)
    testOutput =
        runTestNetwork setupTestConfig testConfigB TestConfigMB
            { nodeInfo = \(CoreNodeId nid) -> plainTestNodeInitialization $
                castProtocolInfo $ inject
                  (mkProtocolRealTPraos
                    genesisConfig
                    (coreNodes !! fromIntegral nid))
            , mkRekeyM = Nothing
            }

    initialKESPeriod :: SL.KESPeriod
    initialKESPeriod = SL.KESPeriod 0

    maxKESEvolution :: Word64
    maxKESEvolution = 100

    coreNodes :: [CoreNode TPraosMockCrypto]
    coreNodes =
        withSeed initSeed $
        replicateM (fromIntegral n) $
        genCoreNode initialKESPeriod
      where
        NumCoreNodes n = numCoreNodes

    genesisConfig :: ShelleyGenesis TPraosMockCrypto
    genesisConfig =
        mkGenesisConfig
          setupK
          setupD
          tpraosSlotLength
          maxKESEvolution
          coreNodes

    epochSize :: EpochSize
    epochSize = sgEpochLength genesisConfig
