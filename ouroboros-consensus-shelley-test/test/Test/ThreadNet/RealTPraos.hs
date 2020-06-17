{-# LANGUAGE NamedFieldPuns #-}

module Test.ThreadNet.RealTPraos (tests) where

import           Control.Monad (replicateM)
import           Data.List ((!!))
import           Data.Word (Word64)

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Cardano.Crypto.Hash (ShortHash)
import           Cardano.Slotting.Slot (EpochSize (..))

import           Ouroboros.Consensus.Config.SecurityParam
import           Ouroboros.Consensus.Ledger.SupportsMempool (extractTxs)
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId

import           Test.ThreadNet.General
import           Test.ThreadNet.Infra.Shelley

import           Test.Util.HardFork.Future (singleEraFuture)
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Random

import qualified Shelley.Spec.Ledger.OCert as SL
import qualified Shelley.Spec.Ledger.PParams as SL

import           Ouroboros.Consensus.Shelley.Node

import           Test.Consensus.Shelley.MockCrypto (TPraosMockCrypto)
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
tests = testGroup "RealTPraos"
    [ testProperty "simple convergence" $ withMaxSuccess 20 $ \setup ->
        prop_simple_real_tpraos_convergence setup
    ]

prop_simple_real_tpraos_convergence :: TestSetup -> Property
prop_simple_real_tpraos_convergence TestSetup
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
      { forgeEbbEnv  = Nothing
      , future       = singleEraFuture tpraosSlotLength epochSize
      , nodeJoinPlan = trivialNodeJoinPlan numCoreNodes
      , nodeRestarts = noRestarts
      , txGenExtra   = ShelleyTxGenExtra $ mkGenEnv coreNodes
      }

    testOutput =
        runTestNetwork setupTestConfig testConfigB TestConfigMB
            { nodeInfo = \(CoreNodeId nid) ->
              plainTestNodeInitialization $
                mkProtocolRealTPraos
                  genesisConfig
                  (coreNodes !! fromIntegral nid)
            , mkRekeyM = Nothing
            }

    initialKESPeriod :: SL.KESPeriod
    initialKESPeriod = SL.KESPeriod 0

    maxKESEvolution :: Word64
    maxKESEvolution = 100 -- TODO

    coreNodes :: [CoreNode (TPraosMockCrypto ShortHash)]
    coreNodes =
        withSeed initSeed $
        replicateM (fromIntegral n) $
        genCoreNode initialKESPeriod
      where
        NumCoreNodes n = numCoreNodes

    genesisConfig :: ShelleyGenesis (TPraosMockCrypto ShortHash)
    genesisConfig =
        mkGenesisConfig
          (SL.ProtVer 0 0)
          setupK
          setupD
          tpraosSlotLength
          maxKESEvolution
          coreNodes

    epochSize :: EpochSize
    epochSize = sgEpochLength genesisConfig
