{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Dynamic.RealPBFT (
    tests
  ) where

import           Data.Foldable (find)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust)
import           Data.Time (Day (..), UTCTime (..))

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Network.MockChain.Chain (Chain)
import qualified Ouroboros.Network.MockChain.Chain as Chain

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Ledger.Byron (ByronBlockOrEBB)
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.ProtocolInfo.Byron (plcCoreNodeId)
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol
import           Ouroboros.Consensus.Util.Random

import qualified Cardano.Chain.Common as Common
import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Update as Update
import qualified Cardano.Crypto as Crypto
import qualified Test.Cardano.Chain.Genesis.Dummy as Dummy

import           Test.Dynamic.General
import           Test.Dynamic.Network (NodeOutput (..))
import           Test.Dynamic.Util
import           Test.Dynamic.Util.NodeJoinPlan
import           Test.Dynamic.Util.NodeTopology

import           Test.Util.Orphans.Arbitrary ()

tests :: TestTree
tests = testGroup "Dynamic chain generation"
    [ localOption (QuickCheckTests 10) $   -- each takes about 0.5 seconds!
      testProperty "check Real PBFT setup" $
        \numCoreNodes ->
          forAll (elements (enumCoreNodes numCoreNodes)) $ \coreNodeId ->
          prop_setup_coreNodeId numCoreNodes coreNodeId
    , adjustOption (\(QuickCheckTests n) -> QuickCheckTests (1 `max` (div n 10))) $
      -- as of merging PR #773, this test case fails without the commit that
      -- introduces the InvalidRollForward exception
      --
      -- See a related discussion at
      -- https://github.com/input-output-hk/ouroboros-network/pull/773#issuecomment-522192097
      testProperty "addressed by InvalidRollForward exception (PR #773)" $
          let ncn = NumCoreNodes 3
          in
          prop_simple_real_pbft_convergence TestConfig
            { numCoreNodes = ncn
            , numSlots = NumSlots 24
            , nodeJoinPlan = NodeJoinPlan $ Map.fromList
              [ (CoreNodeId 0,SlotNo 0)
              , (CoreNodeId 1,SlotNo 20)
              , (CoreNodeId 2,SlotNo 22)
              ]
            , nodeTopology = meshNodeTopology ncn
            }
    , testProperty "simple Real PBFT convergence" $
        prop_simple_real_pbft_convergence
    ]

prop_setup_coreNodeId ::
     NumCoreNodes
  -> CoreNodeId
  -> Property
prop_setup_coreNodeId numCoreNodes coreNodeId =
    case mkProtocolRealPBFT numCoreNodes coreNodeId genesisConfig genesisSecrets of
      ProtocolRealPBFT _cfg _th _pv _swv (Just plc) ->
          coreNodeId === plcCoreNodeId plc
      _ ->
          counterexample "mkProtocolRealPBFT did not use ProtocolRealPBFT" $
          property False
  where
    genesisConfig  :: Genesis.Config
    genesisSecrets :: Genesis.GeneratedSecrets
    (genesisConfig, genesisSecrets) = generateGenesisConfig numCoreNodes

prop_simple_real_pbft_convergence :: TestConfig
                                  -> Seed
                                  -> Property
prop_simple_real_pbft_convergence
  testConfig@TestConfig{numCoreNodes, numSlots} seed =
    prop_general k
        testConfig
        (Just $ roundRobinLeaderSchedule numCoreNodes numSlots)
        testOutput
    .&&. not (all Chain.null finalChains)
  where
    k =
      (SecurityParam . Common.unBlockCount) $
      (Genesis.gdK . Genesis.configGenesisData) $
      genesisConfig

    testOutput =
        runTestNetwork
            (\nid -> protocolInfo numCoreNodes
                (mkProtocolRealPBFT numCoreNodes nid
                                    genesisConfig genesisSecrets))
            testConfig seed

    finalChains :: [Chain ByronBlockOrEBB]
    finalChains = Map.elems $ nodeOutputFinalChain <$> testOutputNodes testOutput

    genesisConfig  :: Genesis.Config
    genesisSecrets :: Genesis.GeneratedSecrets
    (genesisConfig, genesisSecrets) = generateGenesisConfig numCoreNodes


mkProtocolRealPBFT :: NumCoreNodes
                   -> CoreNodeId
                   -> Genesis.Config
                   -> Genesis.GeneratedSecrets
                   -> Protocol ByronBlockOrEBB
mkProtocolRealPBFT (NumCoreNodes n) (CoreNodeId i)
                   genesisConfig genesisSecrets =
    ProtocolRealPBFT
      genesisConfig
      (Just signatureThreshold)
      (Update.ProtocolVersion 1 0 0)
      (Update.SoftwareVersion (Update.ApplicationName "Cardano Test") 2)
      (Just leaderCredentials)
  where
    leaderCredentials :: PBftLeaderCredentials
    leaderCredentials = either (error . show) id $
        mkPBftLeaderCredentials
          genesisConfig
          dlgKey
          dlgCert

    signatureThreshold = PBftSignatureThreshold $
      (1.0 / fromIntegral n) + 0.1

    dlgKey :: Crypto.SigningKey
    dlgKey = fromJust $
       find (\sec -> Delegation.delegateVK dlgCert == Crypto.toVerification sec)
            $ Genesis.gsRichSecrets genesisSecrets

    dlgCert :: Delegation.Certificate
    dlgCert = snd $ Map.toAscList dlgMap !! i

    dlgMap :: Map Common.KeyHash Delegation.Certificate
    dlgMap = Genesis.unGenesisDelegation
           $ Genesis.gdHeavyDelegation
           $ Genesis.configGenesisData genesisConfig


-- Instead of using 'Dummy.dummyConfig', which hard codes the number of rich
-- men (= CoreNodes for us) to 4, we generate a dummy config with the given
-- number of rich men.
generateGenesisConfig :: NumCoreNodes -> (Genesis.Config, Genesis.GeneratedSecrets)
generateGenesisConfig (NumCoreNodes n) =
    either (error . show) id $ Genesis.generateGenesisConfig startTime spec
  where
    startTime = UTCTime (ModifiedJulianDay 0) 0

    spec :: Genesis.GenesisSpec
    spec = Dummy.dummyGenesisSpec
      { Genesis.gsInitializer = Dummy.dummyGenesisInitializer
        { Genesis.giTestBalance =
            (Genesis.giTestBalance Dummy.dummyGenesisInitializer)
              -- The nodes are the richmen
              { Genesis.tboRichmen = fromIntegral n }
        }
      }
