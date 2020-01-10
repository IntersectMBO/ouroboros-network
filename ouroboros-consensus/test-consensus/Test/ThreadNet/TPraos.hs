{-# LANGUAGE NamedFieldPuns #-}

module Test.ThreadNet.TPraos
  ( tests,
  )
where

import Cardano.Crypto.DSIGN
import Cardano.Crypto.ProtocolMagic
import Cardano.Slotting.Slot (EpochSize (..))
import Control.Monad (replicateM)
import Crypto.PubKey.Ed448 (generateSecretKey)
import Crypto.Random (MonadRandom, withDRG)
import qualified Data.Map.Strict as Map
import Data.Time (Day (ModifiedJulianDay))
import Data.Time.Clock (UTCTime (..))
import qualified Keys
import Ouroboros.Consensus.BlockchainTime
import Ouroboros.Consensus.BlockchainTime.Mock
import Ouroboros.Consensus.Node.ProtocolInfo
import Ouroboros.Consensus.Node.ProtocolInfo.Shelley
import Ouroboros.Consensus.Protocol
import Ouroboros.Consensus.Util.Random (seedToChaCha)
import Ouroboros.Network.Magic
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.ThreadNet.General
import Test.ThreadNet.Util
import Test.ThreadNet.Util.NodeJoinPlan
import Test.ThreadNet.Util.NodeRestarts
import Test.ThreadNet.Util.NodeTopology
import Test.Util.Orphans.Arbitrary ()

data CoreNode
  = CoreNode
      { cnGenesisKey :: SignKeyDSIGN Ed448DSIGN,
        cnDelegate :: SignKeyDSIGN Ed448DSIGN
      }

tests :: TestTree
tests =
  testGroup
    "TPraos"
    [ testProperty "simple convergence" $
        \initSeed ->
          forAllShrink
            (genNodeJoinPlan numCoreNodes numSlots)
            shrinkNodeJoinPlan
            $ \nodeJoinPlan ->
              forAllShrink
                (genNodeTopology numCoreNodes)
                shrinkNodeTopology
                $ \nodeTopology ->
                  testPraos'
                    TestConfig
                      { numCoreNodes,
                        numSlots,
                        nodeJoinPlan,
                        nodeRestarts = noRestarts,
                        nodeTopology,
                        slotLengths = singletonSlotLengths slotLength,
                        initSeed
                      }
    ]
  where
    numCoreNodes = NumCoreNodes 3
    numEpochs = 3
    slotsPerEpoch = 3
    numSlots =
      NumSlots $ fromIntegral $
        maxRollbacks k * slotsPerEpoch * numEpochs
    slotLength = slotLengthFromSec 2
    k = SecurityParam 5
    testPraos' :: TestConfig -> Property
    testPraos' tc@TestConfig {initSeed} =
      prop_simple_praos_convergence shelleyGenesis coreNodes tc
      where
        shelleyGenesis =
          ShelleyGenesis
            { sgStartTime = SystemStart $ UTCTime (ModifiedJulianDay 0) 0,
              sgNetworkMagic = NetworkMagic 24601,
              sgProtocolMagicId = ProtocolMagicId 24601,
              sgActiveSlotsCoeff = 0.1,
              sgDecentralisationParam = 1,
              sgSecurityParam = k,
              sgEpochLength = EpochSize slotsPerEpoch,
              sgKESPeriod = 1000000,
              sgSlotLength = slotLength,
              sgReserves = 100,
              sgGenDelegs = coreNodesToGenesisMapping coreNodes,
              sgInitialFunds = Map.empty
            }
        coreNodes = fst $ withDRG (seedToChaCha initSeed) (genCoreNodes numCoreNodes)
        coreNodesToGenesisMapping =
          Map.fromList
            . fmap
              ( \(CoreNode skg skd) ->
                  ( Keys.GenKeyHash . Keys.hash $ deriveVerKeyDSIGN skg,
                    Keys.KeyHash . Keys.hash $ deriveVerKeyDSIGN skd
                  )
              )

genCoreNodes :: MonadRandom m => NumCoreNodes -> m [CoreNode]
genCoreNodes (NumCoreNodes n) = replicateM n genCoreNode
  where
    genCoreNode =
      CoreNode
        <$> (SignKeyEd448DSIGN <$> generateSecretKey)
        <*> (SignKeyEd448DSIGN <$> generateSecretKey)

prop_simple_praos_convergence ::
  ShelleyGenesis ->
  [CoreNode] ->
  TestConfig ->
  Property
prop_simple_praos_convergence
  sg@ShelleyGenesis {sgSecurityParam}
  coreNodes
  testConfig =
    prop_general sgSecurityParam testConfig Nothing (const False) testOutput
    where
      protocolVersion = ProtVer 0 0 0
      testOutput =
        runTestNetwork
          testConfig
          TestConfigBlock
            { forgeEBB = Nothing,
              nodeInfo = \nid ->
                protocolInfo $
                  ProtocolRealTPraos sg protocolVersion Nothing,
              rekeying = Nothing
            }
