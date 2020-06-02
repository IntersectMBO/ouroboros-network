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

tests :: TestTree
tests = testGroup "Cardano" $
    [ testProperty "simple convergence" $ withMaxSuccess 20 $
          forAll (SecurityParam <$> elements [5, 10])
            $ \k ->
          forAll (elements [x / 10 | x <- [1..10]])
            $ \d ->
          forAllShrink
              (genRealTPraosTestConfig k)
              shrinkRealTPraosTestConfig
            $ \testConfig ->
          prop_simple_cardano_convergence k d testConfig
    ]

prop_simple_cardano_convergence
  :: SecurityParam
  -> Double -- Decentralisation parameter
  -> TestConfig
  -> Property
prop_simple_cardano_convergence k d
  testConfig@TestConfig{numCoreNodes = NumCoreNodes n, initSeed} =
    prop_general PropGeneralArgs
      { pgaBlockProperty      = const $ property True
      , pgaCountTxs           = fromIntegral . length . extractTxs
      , pgaExpectedCannotLead = noExpectedCannotLeads
      , pgaFirstBlockNo       = 0
      , pgaFixedMaxForkLength = Nothing
      , pgaFixedSchedule      = Nothing
      , pgaSecurityParam      = k
      , pgaTestConfig         = testConfig
      , pgaCustomLabelling    = const id
      }
      testOutput
  where
    testOutput :: TestOutput (CardanoBlock TPraosMockCrypto)
    testOutput =
        runTestNetwork testConfig epochSize TestConfigBlock
            { forgeEbbEnv = Nothing
            , nodeInfo    = \(CoreNodeId nid) -> plainTestNodeInitialization $
                castProtocolInfo $ inject
                  (mkProtocolRealTPraos
                    genesisConfig
                    (coreNodes !! fromIntegral nid))
            , rekeying    = Nothing
            , txGenExtra  = ShelleyTxGenExtra $ mkGenEnv coreNodes
            }

    initialKESPeriod :: SL.KESPeriod
    initialKESPeriod = SL.KESPeriod 0

    maxKESEvolution :: Word64
    maxKESEvolution = 100

    coreNodes :: [CoreNode TPraosMockCrypto]
    coreNodes = withSeed initSeed $
      replicateM (fromIntegral n) $
      genCoreNode initialKESPeriod

    genesisConfig :: ShelleyGenesis TPraosMockCrypto
    genesisConfig = mkGenesisConfig k d maxKESEvolution coreNodes

    epochSize :: EpochSize
    epochSize = sgEpochLength genesisConfig
