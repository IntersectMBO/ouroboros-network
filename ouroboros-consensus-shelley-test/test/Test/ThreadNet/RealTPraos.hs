{-# LANGUAGE NamedFieldPuns #-}
module Test.ThreadNet.RealTPraos (tests) where

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

import           Test.ThreadNet.General
import           Test.ThreadNet.Infra.Shelley

import           Test.Util.Orphans.Arbitrary ()

import qualified Shelley.Spec.Ledger.OCert as SL

import           Ouroboros.Consensus.Shelley.Node

import           Test.Consensus.Shelley.MockCrypto (TPraosMockCrypto)
import           Test.ThreadNet.TxGen.Shelley

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
            , txGenExtra  = ShelleyTxGenExtra $ mkGenEnv coreNodes
            }

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
