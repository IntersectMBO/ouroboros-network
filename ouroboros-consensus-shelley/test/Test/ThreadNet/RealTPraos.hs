{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
module Test.ThreadNet.RealTPraos (
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
import           Ouroboros.Consensus.Mempool.API (extractTxs)
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Util.Random

import           Test.ThreadNet.General
import           Test.ThreadNet.Infra.Shelley
import           Test.ThreadNet.Util.NodeJoinPlan
import           Test.ThreadNet.Util.NodeRestarts
import           Test.ThreadNet.Util.NodeTopology


import           Test.Util.Orphans.Arbitrary ()

import qualified Shelley.Spec.Ledger.OCert as SL

import           Ouroboros.Consensus.Shelley.Node

import           Test.Consensus.Shelley.MockCrypto (TPraosMockCrypto)
import qualified Test.Shelley.Spec.Ledger.Generator.Constants as Gen
import qualified Test.Shelley.Spec.Ledger.Generator.Core as Gen
import qualified Test.Shelley.Spec.Ledger.Generator.Presets as Gen.Presets
import           Test.ThreadNet.TxGen.Shelley (ShelleyTxGenExtra (..))

tests :: TestTree
tests = testGroup "RealTPraos"
    [ testProperty "simple convergence" $ withMaxSuccess 20 $
          forAll (SecurityParam <$> elements [5, 10])
            $ \k ->
          forAll (elements [x / 10 | x <- [1..10]])
            $ \d ->
          forAllShrink
              (genRealTPraosTestConfig k)
              shrinkRealTPraosTestConfig
            $ \testConfig ->
          prop_simple_real_tpraos_convergence k d testConfig
    ]

prop_simple_real_tpraos_convergence
  :: SecurityParam
  -> Double -- Decentralisation parameter
  -> TestConfig
  -> Property
prop_simple_real_tpraos_convergence k d
  testConfig@TestConfig{numCoreNodes = NumCoreNodes n, initSeed} =
    prop_general PropGeneralArgs
      { pgaBlockProperty          = const $ property True
      , pgaCountTxs               = fromIntegral . length . extractTxs
      , pgaExpectedBlockRejection = const False
      , pgaFirstBlockNo           = 0
      , pgaFixedMaxForkLength     = Nothing
      , pgaFixedSchedule          = Nothing
      , pgaSecurityParam          = k
      , pgaTestConfig             = testConfig
      }
      testOutput
  where
    testOutput =
        runTestNetwork testConfig epochSize TestConfigBlock
            { forgeEbbEnv = Nothing
            , nodeInfo    = \(CoreNodeId nid) ->
              plainTestNodeInitialization $
                mkProtocolRealTPraos
                  genesisConfig
                  (coreNodes !! fromIntegral nid)
            , rekeying    = Nothing
            , txGenExtra  = ShelleyTxGenExtra $ Gen.GenEnv keySpace constants
            }

    constants :: Gen.Constants
    constants = Gen.defaultConstants
      { Gen.frequencyMIRCert = 0
      }

    keySpace :: Gen.KeySpace
    keySpace = Gen.KeySpace
        (cnkiCoreNode <$> cn)
        (ksKeyPairs <> (cnkiKeyPair <$> cn))
        ksMSigScripts
        ksVRFKeyPairs
      where
        cn = coreNodeKeys <$> coreNodes
        Gen.KeySpace_ { ksKeyPairs, ksMSigScripts, ksVRFKeyPairs } =
          Gen.Presets.keySpace constants

    initialKESPeriod :: SL.KESPeriod
    initialKESPeriod = SL.KESPeriod 0

    maxKESEvolution :: Word64
    maxKESEvolution = 100 -- TODO

    coreNodes :: [CoreNode TPraosMockCrypto]
    coreNodes = withSeed initSeed $
      replicateM (fromIntegral n) $
      genCoreNode initialKESPeriod

    genesisConfig :: ShelleyGenesis TPraosMockCrypto
    genesisConfig = mkGenesisConfig k d maxKESEvolution coreNodes

    epochSize :: EpochSize
    epochSize = sgEpochLength genesisConfig

-------------------------------------------------------------------------
-- Test config
-------------------------------------------------------------------------

genRealTPraosTestConfig :: SecurityParam -> Gen TestConfig
genRealTPraosTestConfig _k = do
    numCoreNodes <- arbitrary
    numSlots     <- arbitrary

    -- TODO generate more interesting scenarios
    let nodeJoinPlan = trivialNodeJoinPlan numCoreNodes
        nodeTopology = meshNodeTopology numCoreNodes

    initSeed <- arbitrary

    pure TestConfig
      { nodeJoinPlan
      , nodeRestarts = noRestarts
      , nodeTopology
      , numCoreNodes
      , numSlots
      , slotLength = tpraosSlotLength
      , initSeed
      }

shrinkRealTPraosTestConfig :: TestConfig -> [TestConfig]
shrinkRealTPraosTestConfig _ = [] -- TODO
