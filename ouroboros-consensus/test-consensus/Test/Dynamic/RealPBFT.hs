{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
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
import           Data.Reflection (give)
import           Data.Time (Day (..), UTCTime (..))

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Network.MockChain.Chain (Chain)
import qualified Ouroboros.Network.MockChain.Chain as Chain

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Demo
import           Ouroboros.Consensus.Ledger.Byron (ByronBlockOrEBB, ByronGiven)
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
import           Test.Dynamic.Util

import           Test.Util.Orphans.Arbitrary ()

tests :: TestTree
tests = testGroup "Dynamic chain generation"
    [ testProperty "check Real PBFT setup" $
        \numCoreNodes ->
          forAll (elements (enumCoreNodes numCoreNodes)) $ \coreNodeId ->
          prop_setup_coreNodeId numCoreNodes coreNodeId
    , testProperty "simple Real PBFT convergence" $
        prop_simple_real_pbft_convergence sp
    ]
  where
    sp = defaultSecurityParam

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

prop_simple_real_pbft_convergence :: SecurityParam
                                  -> NumCoreNodes
                                  -> NumSlots
                                  -> Seed
                                  -> Property
prop_simple_real_pbft_convergence k numCoreNodes numSlots seed =
    giveByron $
    prop_general k
        (roundRobinLeaderSchedule numCoreNodes numSlots)
        testOutput
    .&&. not (all Chain.null finalChains)
  where
    testOutput = giveByron $
        runTestNetwork
            (\nid -> protocolInfo numCoreNodes nid
                (mkProtocolRealPBFT numCoreNodes nid
                                    genesisConfig genesisSecrets))
            numCoreNodes numSlots seed

    finalChains :: [Chain (ByronBlockOrEBB ByronConfig)]
    finalChains = map snd $ Map.elems $ testOutputNodes testOutput

    giveByron :: forall a. (ByronGiven => a) -> a
    giveByron a =
      give (Genesis.gdProtocolMagicId $ Genesis.configGenesisData genesisConfig) $
      give (Genesis.configEpochSlots genesisConfig) a

    genesisConfig  :: Genesis.Config
    genesisSecrets :: Genesis.GeneratedSecrets
    (genesisConfig, genesisSecrets) = generateGenesisConfig numCoreNodes


mkProtocolRealPBFT :: NumCoreNodes
                   -> CoreNodeId
                   -> Genesis.Config
                   -> Genesis.GeneratedSecrets
                   -> Protocol (ByronBlockOrEBB ByronConfig)
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
      1.0 / (fromIntegral n + 1.0)

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
