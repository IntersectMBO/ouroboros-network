{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.ThreadNet.Cardano (
    tests
  ) where

import           Control.Exception (assert)
import           Control.Monad (guard, replicateM)
import           Data.List ((!!))
import           Data.Word (Word64)

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Cardano.Prelude (Natural)
import           Cardano.Slotting.Slot (EpochNo, EpochSize (..))

import           Cardano.Crypto.Hash.Blake2b (Blake2b_256)

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config.SecurityParam
import           Ouroboros.Consensus.Ledger.SupportsMempool (extractTxs)
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.PBFT
import           Ouroboros.Consensus.Util.IOLike (IOLike)

import qualified Cardano.Chain.Genesis as CC.Genesis
import qualified Cardano.Chain.Update as CC.Update

import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
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
import           Test.ThreadNet.Network (TestNodeInitialization (..))
import           Test.ThreadNet.TxGen.Cardano ()
import           Test.ThreadNet.Util.NodeJoinPlan (trivialNodeJoinPlan)
import           Test.ThreadNet.Util.NodeRestarts (noRestarts)
import           Test.Util.HardFork.Future
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Random

data TestSetup = TestSetup
  { setupByronLowerBound   :: Bool
    -- ^ whether to use the @HardFork.LowerBound@ optimization
  , setupD                 :: Double
    -- ^ decentralization parameter
  , setupK                 :: SecurityParam
  , setupSlotLengthByron   :: SlotLength
  , setupSlotLengthShelley :: SlotLength
  , setupTestConfig        :: TestConfig
  }
  deriving (Show)

instance Arbitrary TestSetup where
  arbitrary = do
    setupD <- (/10)         <$> choose   (1, 10)
    setupK <- SecurityParam <$> choose   (2, 5)

    setupSlotLengthByron   <- arbitrary
    setupSlotLengthShelley <- arbitrary

    setupTestConfig <- arbitrary

    setupByronLowerBound <- arbitrary

    pure TestSetup
      { setupByronLowerBound
      , setupD
      , setupK
      , setupSlotLengthByron
      , setupSlotLengthShelley
      , setupTestConfig
      }

  -- TODO shrink

tests :: TestTree
tests = testGroup "Cardano" $
    [ testProperty "simple convergence" $ \setup ->
          prop_simple_cardano_convergence setup
    ]

prop_simple_cardano_convergence :: TestSetup -> Property
prop_simple_cardano_convergence TestSetup
  { setupByronLowerBound
  , setupD
  , setupK
  , setupSlotLengthByron
  , setupSlotLengthShelley
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
      , future       =
          EraCons  setupSlotLengthByron   epochSize (EraSize numByronEpochs) $
          EraFinal setupSlotLengthShelley epochSize
      , nodeJoinPlan = trivialNodeJoinPlan numCoreNodes
      , nodeRestarts = noRestarts
      , txGenExtra   = ()
      }

    testOutput :: TestOutput (CardanoBlock (TPraosMockCrypto Blake2b_256))
    testOutput =
        runTestNetwork setupTestConfig testConfigB TestConfigMB
            { nodeInfo = \coreNodeId@(CoreNodeId nid) ->
                mkProtocolCardanoAndHardForkTxs
                  pbftParams
                  coreNodeId
                  genesisByron
                  generatedSecrets
                  genesisShelley
                  (coreNodes !! fromIntegral nid)
                  (guard setupByronLowerBound *> Just numByronEpochs)
                  (NoHardCodedTransition shelleyInitialMajorVersion)
            , mkRekeyM = Nothing
            }

    -- The team does not currently plan for Byron or Shelley to ever use an
    -- epoch size other than 10k.
    epochSize :: EpochSize
    epochSize =
        assert (tenK == epochSizeByron) $
        assert (tenK == epochSizeShelley) $
        tenK
      where
        tenK = EpochSize (10 * maxRollbacks setupK)

    -- Byron

    pbftParams :: PBftParams
    pbftParams = Byron.realPBftParams setupK numCoreNodes

    epochSizeByron :: EpochSize
    epochSizeByron =
        fromByronEpochSlots $ CC.Genesis.configEpochSlots genesisByron

    genesisByron     :: CC.Genesis.Config
    generatedSecrets :: CC.Genesis.GeneratedSecrets
    (genesisByron, generatedSecrets) =
        Byron.generateGenesisConfig setupSlotLengthByron pbftParams

    -- Shelley

    initialKESPeriod :: SL.KESPeriod
    initialKESPeriod = SL.KESPeriod 0

    maxKESEvolution :: Word64
    maxKESEvolution = 100

    coreNodes :: [Shelley.CoreNode (TPraosMockCrypto Blake2b_256)]
    coreNodes =
        withSeed initSeed $
        replicateM (fromIntegral n) $
        Shelley.genCoreNode initialKESPeriod
      where
        NumCoreNodes n = numCoreNodes

    genesisShelley :: ShelleyGenesis (TPraosMockCrypto Blake2b_256)
    genesisShelley =
        Shelley.mkGenesisConfig
          (SL.ProtVer shelleyInitialMajorVersion 0)
          setupK
          setupD
          setupSlotLengthShelley
          maxKESEvolution
          coreNodes

    epochSizeShelley :: EpochSize
    epochSizeShelley = sgEpochLength genesisShelley

mkProtocolCardanoAndHardForkTxs
  :: forall sc m. (IOLike m, TPraosCrypto sc)
     -- Byron
  => PBftParams
  -> CoreNodeId
  -> CC.Genesis.Config
  -> CC.Genesis.GeneratedSecrets
     -- Shelley
  -> ShelleyGenesis sc
  -> Shelley.CoreNode sc
     -- Hard fork
  -> Maybe EpochNo
  -> HardCodedTransition
  -> TestNodeInitialization m (CardanoBlock sc)
mkProtocolCardanoAndHardForkTxs
    pbftParams coreNodeId genesisByron generatedSecretsByron
    genesisShelley coreNodeShelley
    mbLowerBound hardCodedTransition =
    TestNodeInitialization
      { tniCrucialTxs   = crucialTxs
      , tniProtocolInfo = pInfo
      }
  where
    crucialTxs :: [GenTx (CardanoBlock sc)]
    crucialTxs =
        assert (protVerByron == Byron.theProposedProtocolVersion) $
        GenTxByron <$> tniCrucialTxs tniByron
      where
        -- reuse the RealPBft logic for generating the crucial txs, ie the
        -- proposal and votes
        tniByron :: TestNodeInitialization m ByronBlock
        tniByron =
            Byron.mkProtocolRealPBftAndHardForkTxs
              pbftParams
              coreNodeId
              genesisByron
              generatedSecretsByron

    pInfo :: ProtocolInfo (ChaChaT m) (CardanoBlock sc)
    pInfo = protocolInfoCardano
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
        mbLowerBound
        hardCodedTransition

    -- Byron
    PBftParams { pbftSignatureThreshold } = pbftParams

    leaderCredentialsByron :: PBftLeaderCredentials
    leaderCredentialsByron =
        Byron.mkLeaderCredentials
          genesisByron
          generatedSecretsByron
          coreNodeId

    -- the protocol version that each Byron node is endorsing with each block
    -- it forges (ie which the node is ready to run)
    protVerByron :: CC.Update.ProtocolVersion
    protVerByron = CC.Update.ProtocolVersion shelleyInitialMajorVersion 0 0

    -- this sets a vestigial header field which is not actually used for anything
    softVerByron :: CC.Update.SoftwareVersion
    softVerByron = Byron.theProposedSoftwareVersion

    -- Shelley

    -- the protocol version that each Shelley node is endorsing with each block
    -- it forges (ie which the node is ready to run)
    --
    -- This is still Shelley, since that's the last era of this test.
    protVerShelley :: SL.ProtVer
    protVerShelley = SL.ProtVer shelleyInitialMajorVersion 0

    maxMajorPVShelley :: Natural
    maxMajorPVShelley = 100

    leaderCredentialsShelley :: TPraosLeaderCredentials sc
    leaderCredentialsShelley = Shelley.mkLeaderCredentials coreNodeShelley

{-------------------------------------------------------------------------------
  Constants
-------------------------------------------------------------------------------}

-- | The major protocol version of Shelley in this test
--
-- On mainnet, the Byron era spans multiple major versions: 0 for Classic and 1
-- for OBFT. So Shelley is 2. But in this test, we start with OBFT as major
-- version 0: the nodes are running OBFT from slot 0 and the Byron ledger
-- defaults to an initial version of 0. So Shelley is 1 in this test.
shelleyInitialMajorVersion :: Num a => a
shelleyInitialMajorVersion = byronLastMajorVersion + 1
  where
    byronLastMajorVersion :: Num a => a
    byronLastMajorVersion = 0

-- | The number of Byron epochs in this test
--
-- All nodes join in slot 0, we generate the proposal in slot 0, we also
-- generate the votes in slot 0, and the nodes are endorsing the proposal as of
-- slot 0. Thus we expect that Byron will end after one era. Otherwise it would
-- indicate some sort of protocol failure.
numByronEpochs :: Num a => a
numByronEpochs = 1
