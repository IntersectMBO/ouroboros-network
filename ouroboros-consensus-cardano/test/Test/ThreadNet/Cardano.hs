{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.ThreadNet.Cardano (
    tests
  ) where

import           Control.Exception (assert)
import           Control.Monad (replicateM)
import           Data.List ((!!))
import           Data.Word (Word64)

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Cardano.Prelude (Natural)
import           Cardano.Slotting.Slot (EpochSize (..))

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config.SecurityParam
import           Ouroboros.Consensus.Ledger.SupportsMempool (extractTxs)
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.PBFT

import qualified Cardano.Chain.Genesis as CC.Genesis
import qualified Cardano.Chain.Update as CC.Update

import           Ouroboros.Consensus.Byron.Ledger.Conversions
import           Ouroboros.Consensus.Byron.Node

import qualified Shelley.Spec.Ledger.OCert as SL
import qualified Shelley.Spec.Ledger.PParams as SL

import           Ouroboros.Consensus.Shelley.Node
import           Ouroboros.Consensus.Shelley.Protocol (TPraosCrypto)

import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.Condense ()
import           Ouroboros.Consensus.Cardano.Node

import           Test.Consensus.Shelley.MockCrypto (TPraosMockCrypto)
import           Test.ThreadNet.General
import qualified Test.ThreadNet.Infra.Byron as Byron
import qualified Test.ThreadNet.Infra.Shelley as Shelley
import           Test.ThreadNet.TxGen.Cardano ()
import           Test.ThreadNet.Util.NodeJoinPlan (trivialNodeJoinPlan)
import           Test.ThreadNet.Util.NodeRestarts (noRestarts)
import           Test.Util.HardFork.Future (singleEraFuture)
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Random

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
   -- TODO Byron hashes are 32 bytes and Shelley mock hashes are 4 bytes, so they
   -- are incompatible, causing the tests to fail. We disable them until we have
   -- a solution for this, e.g., parameterising the Shelley mock crypto by the
   -- hash type.
    const [] $ -- REMOVE this line
    [ testProperty "simple convergence" $ \setup ->
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
      { forgeEbbEnv  = Nothing
      , future       = singleEraFuture Shelley.tpraosSlotLength epochSize
      , nodeJoinPlan = trivialNodeJoinPlan numCoreNodes
      , nodeRestarts = noRestarts
      , txGenExtra   = ()
      }

    testOutput :: TestOutput (CardanoBlock TPraosMockCrypto)
    testOutput =
        runTestNetwork setupTestConfig testConfigB TestConfigMB
            { nodeInfo = \coreNodeId@(CoreNodeId nid) ->
                plainTestNodeInitialization $
                  mkProtocolCardano
                    pbftParams
                    coreNodeId
                    genesisByron
                    generatedSecrets
                    genesisShelley
                    (coreNodes !! fromIntegral nid)
                    (HardCodedTransitionAt 1)
            , mkRekeyM = Nothing
            }

    -- Shared
    --
    -- TODO we use the same @slotLength@ and @epochSize@ for Byron and Shelley
    -- until the ThreadNet infrastructure supports varying them.
    slotLength :: SlotLength
    slotLength = slotLengthFromSec 5

    epochSize :: EpochSize
    epochSize = assert (epochSizeByron == epochSizeShelley) epochSizeByron

    -- Byron
    pbftParams :: PBftParams
    pbftParams = Byron.realPBftParams setupK numCoreNodes

    epochSizeByron :: EpochSize
    epochSizeByron =
        fromByronEpochSlots $ CC.Genesis.configEpochSlots genesisByron

    genesisByron     :: CC.Genesis.Config
    generatedSecrets :: CC.Genesis.GeneratedSecrets
    (genesisByron, generatedSecrets) =
        Byron.generateGenesisConfig slotLength pbftParams

    -- Shelley

    initialKESPeriod :: SL.KESPeriod
    initialKESPeriod = SL.KESPeriod 0

    maxKESEvolution :: Word64
    maxKESEvolution = 100

    coreNodes :: [Shelley.CoreNode TPraosMockCrypto]
    coreNodes =
        withSeed initSeed $
        replicateM (fromIntegral n) $
        Shelley.genCoreNode initialKESPeriod
      where
        NumCoreNodes n = numCoreNodes

    genesisShelley :: ShelleyGenesis TPraosMockCrypto
    genesisShelley =
        Shelley.mkGenesisConfig
          setupK
          setupD
          slotLength
          maxKESEvolution
          coreNodes

    epochSizeShelley :: EpochSize
    epochSizeShelley = sgEpochLength genesisShelley

mkProtocolCardano
  :: forall sc m. (MonadRandom m, TPraosCrypto sc)
     -- Byron
  => PBftParams
  -> CoreNodeId
  -> CC.Genesis.Config
  -> CC.Genesis.GeneratedSecrets
     -- Shelley
  -> ShelleyGenesis sc
  -> Shelley.CoreNode sc
     -- Hard fork
  -> HardCodedTransition
  -> ProtocolInfo m (CardanoBlock sc)
mkProtocolCardano pbftParams coreNodeId genesisByron generatedSecretsByron
                  genesisShelley coreNodeShelley
                  hardCodedTransition =
    protocolInfoCardano
      -- Byron
      genesisByron
      (Just $ PBftSignatureThreshold pbftSignatureThreshold)
      protVerByron
      softVerByron
      (Just leaderCredentialsByron)
     -- Shelley
     genesisShelley
     protVerShelley
     maxMajorPVShelley
     (Just leaderCredentialsShelley)
     -- Hard fork
     hardCodedTransition
  where
    -- Byron
    PBftParams { pbftSignatureThreshold } = pbftParams

    leaderCredentialsByron :: PBftLeaderCredentials
    leaderCredentialsByron =
        Byron.mkLeaderCredentials
          genesisByron
          generatedSecretsByron
          coreNodeId

    protVerByron :: CC.Update.ProtocolVersion
    protVerByron = CC.Update.ProtocolVersion 2 0 0

    softVerByron :: CC.Update.SoftwareVersion
    softVerByron = CC.Update.SoftwareVersion (CC.Update.ApplicationName "Shelley") 0

    -- Shelley
    protVerShelley :: SL.ProtVer
    protVerShelley = SL.ProtVer 2 0

    maxMajorPVShelley :: Natural
    maxMajorPVShelley = 100

    leaderCredentialsShelley :: TPraosLeaderCredentials sc
    leaderCredentialsShelley = Shelley.mkLeaderCredentials coreNodeShelley
