{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.ThreadNet.Cardano (
    tests
  ) where

import           Control.Exception (assert)
import           Control.Monad (guard, replicateM)
import           Data.List ((!!))
import qualified Data.Map as Map
import           Data.Word (Word64)
import           GHC.Generics (Generic)

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Cardano.Prelude (Natural)
import           Cardano.Slotting.Slot (EpochNo, EpochSize (..))

import           Cardano.Crypto.Hash.Blake2b (Blake2b_256)

import           Ouroboros.Network.MockChain.Chain (chainToList)

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

import qualified Shelley.Spec.Ledger.Genesis as SL
import qualified Shelley.Spec.Ledger.OCert as SL
import qualified Shelley.Spec.Ledger.PParams as SL

import           Ouroboros.Consensus.Shelley.Node
import           Ouroboros.Consensus.Shelley.Protocol (TPraosCrypto)
import qualified Ouroboros.Consensus.Shelley.Protocol as Shelley

import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.Condense ()
import           Ouroboros.Consensus.Cardano.Node

import           Test.Consensus.Shelley.MockCrypto (TPraosMockCrypto)
import           Test.ThreadNet.General
import qualified Test.ThreadNet.Infra.Byron as Byron
import qualified Test.ThreadNet.Infra.Shelley as Shelley
import           Test.ThreadNet.Network (NodeOutput (..),
                     TestNodeInitialization (..))
import           Test.ThreadNet.TxGen.Cardano ()
import           Test.ThreadNet.Util.NodeJoinPlan (trivialNodeJoinPlan)
import           Test.ThreadNet.Util.NodeRestarts (noRestarts)
import qualified Test.Util.BoolProps as BoolProps
import           Test.Util.HardFork.Future
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Random
import           Test.Util.Slots (NumSlots (..))

-- | The varying data of this test
--
-- Note: The Byron nodes in this test all join, propose an update, vote for it,
-- and endorse it literally as soon as possible. Therefore, if the test reaches
-- the end of the first epoch, the proposal will be adopted.
data TestSetup = TestSetup
  { setupByronLowerBound   :: Bool
    -- ^ whether to use the @HardFork.LowerBound@ optimization
  , setupD                 :: Double
    -- ^ decentralization parameter
  , setupHardFork          :: Bool
    -- ^ whether the proposal should trigger a hard fork or not
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
    setupHardFork        <- frequency [(9, pure True), (1, pure False)]

    pure TestSetup
      { setupByronLowerBound
      , setupD
      , setupHardFork
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
  , setupHardFork
  , setupK
  , setupSlotLengthByron
  , setupSlotLengthShelley
  , setupTestConfig
  } =
    tabulate "ReachesShelley label" [label_ReachesShelley reachesShelley] $
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
      testOutput .&&.
    prop_ReachesShelley reachesShelley
  where
    TestConfig
      { initSeed
      , numCoreNodes
      } = setupTestConfig

    testConfigB = TestConfigB
      { forgeEbbEnv  = Nothing
      , future       =
          if setupHardFork
          then
          EraCons  setupSlotLengthByron   epochSize (EraSize numByronEpochs) $
          EraFinal setupSlotLengthShelley epochSize
          else
          EraFinal setupSlotLengthByron   epochSize
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
                  propPV
                  genesisShelley
                  (coreNodes !! fromIntegral nid)
                  (guard setupByronLowerBound *> Just numByronEpochs)
                  (NoHardCodedTransition shelleyMajorVersion)
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
          (SL.ProtVer shelleyMajorVersion 0)
          setupK
          setupD
          setupSlotLengthShelley
          maxKESEvolution
          coreNodes

    epochSizeShelley :: EpochSize
    epochSizeShelley = sgEpochLength genesisShelley

    -- the protocol version of the Byron era proposal
    propPV :: CC.Update.ProtocolVersion
    propPV =
      if setupHardFork
      then
        -- this new version must induce the hard fork if accepted
        CC.Update.ProtocolVersion shelleyMajorVersion 0 0
      else
        -- this new version must not induce the hard fork if accepted
        CC.Update.ProtocolVersion
          byronMajorVersion (byronInitialMinorVersion + 1) 0

    reachesShelley :: ReachesShelley
    reachesShelley = ReachesShelley
      { rsByronSlots    =
          BoolProps.enabledIf $ t > numByronEpochs * unEpochSize epochSizeByron
      , rsPV            = BoolProps.enabledIf setupHardFork
      , rsShelleyBlocks =
          or $
          [ isShelley blk
          | (_nid, no) <- Map.toList testOutputNodes
          , let NodeOutput{nodeOutputFinalChain} = no
          , blk <- chainToList nodeOutputFinalChain
          ]
      , rsShelleySlots  =
          assert (w >= k) $
          BoolProps.requiredIf $ t > w + 1 - k
            -- logic: if we are ensured at least @k@ blocks in @w@ slots, then
            -- we're ensured at least @1@ block in @w+1-k@ slots
      }
      where
        TestConfig{numSlots}        = setupTestConfig
        NumSlots t                  = numSlots
        TestOutput{testOutputNodes} = testOutput

        k :: Word64
        k = maxRollbacks setupK

        w :: Word64
        w = Shelley.computeStabilityWindow
            setupK
            (SL.sgActiveSlotCoeff genesisShelley)

        isShelley :: CardanoBlock c -> Bool
        isShelley = \case
            BlockByron{}   -> False
            BlockShelley{} -> True

mkProtocolCardanoAndHardForkTxs
  :: forall sc m. (IOLike m, TPraosCrypto sc)
     -- Byron
  => PBftParams
  -> CoreNodeId
  -> CC.Genesis.Config
  -> CC.Genesis.GeneratedSecrets
  -> CC.Update.ProtocolVersion
     -- Shelley
  -> ShelleyGenesis sc
  -> Shelley.CoreNode sc
     -- Hard fork
  -> Maybe EpochNo
  -> HardCodedTransition
  -> TestNodeInitialization m (CardanoBlock sc)
mkProtocolCardanoAndHardForkTxs
    pbftParams coreNodeId genesisByron generatedSecretsByron propPV
    genesisShelley coreNodeShelley
    mbLowerBound hardCodedTransition =
    TestNodeInitialization
      { tniCrucialTxs   = crucialTxs
      , tniProtocolInfo = pInfo
      }
  where
    crucialTxs :: [GenTx (CardanoBlock sc)]
    crucialTxs =
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
              propPV

    pInfo :: ProtocolInfo (ChaChaT m) (CardanoBlock sc)
    pInfo = protocolInfoCardano
        -- Byron
        genesisByron
        (Just $ PBftSignatureThreshold pbftSignatureThreshold)
        propPV
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

    -- this sets a vestigial header field which is not actually used for anything
    softVerByron :: CC.Update.SoftwareVersion
    softVerByron = Byron.theProposedSoftwareVersion

    -- Shelley

    -- the protocol version that each Shelley node is endorsing with each block
    -- it forges (ie which the node is ready to run)
    --
    -- This is still Shelley, since that's the last era of this test.
    protVerShelley :: SL.ProtVer
    protVerShelley = SL.ProtVer shelleyMajorVersion 0

    maxMajorPVShelley :: Natural
    maxMajorPVShelley = 100

    leaderCredentialsShelley :: TPraosLeaderCredentials sc
    leaderCredentialsShelley = Shelley.mkLeaderCredentials coreNodeShelley

{-------------------------------------------------------------------------------
  Constants
-------------------------------------------------------------------------------}

-- | The major protocol version of Byron in this test
--
-- On mainnet, the Byron era spans multiple major versions: 0 for Classic and 1
-- for OBFT. So Shelley is 2. But in this test, we start with OBFT as major
-- version 0: the nodes are running OBFT from slot 0 and the Byron ledger
-- defaults to an initial version of 0. So Shelley is 1 in this test.
byronMajorVersion :: Num a => a
byronMajorVersion = 0

-- | The major protocol version of Shelley in this test
--
-- See 'byronMajorVersion'
shelleyMajorVersion :: Num a => a
shelleyMajorVersion = byronMajorVersion + 1

-- | The initial minor protocol version of Byron in this test
--
-- See 'byronMajorVersion'
byronInitialMinorVersion :: Num a => a
byronInitialMinorVersion = 0

-- | The number of Byron epochs in this test
--
-- All nodes join in slot 0, we generate the proposal in slot 0, we also
-- generate the votes in slot 0, and the nodes are endorsing the proposal as of
-- slot 0. Thus we expect that Byron will end after one era. Otherwise it would
-- indicate some sort of protocol failure.
numByronEpochs :: Num a => a
numByronEpochs = 1

{-------------------------------------------------------------------------------
  ReachesShelley property
-------------------------------------------------------------------------------}

-- | Whether the test included Shelley blocks and relevent (pre)reqs
--
-- Note these fields are ordered alphabetically not semantically; see
-- 'label_ReachesShelley'.
data ReachesShelley = ReachesShelley
  { rsByronSlots    :: BoolProps.Prereq
    -- ^ enough Byron slots to enable a Shelley block
  , rsPV            :: BoolProps.Prereq
    -- ^ sufficient protocol version to enable a Shelley block
  , rsShelleyBlocks :: Bool
    -- ^ Shelley blocks included in final chains
  , rsShelleySlots  :: BoolProps.Requirement
    -- ^ enough Shelley slots to necessitate a Shelley block
  }
  deriving (Generic, Show)

instance BoolProps.CollectReqs ReachesShelley

-- | List the (pre)reqs in semantic order, followed by the observation
label_ReachesShelley :: ReachesShelley -> String
label_ReachesShelley reachesShelley =
    prepend "pv" rsPV $
    prepend "bs" rsByronSlots $
    prepend "ss" rsShelleySlots $
    show         rsShelleyBlocks
  where
    -- incur a warning if the number of fields changes
    ReachesShelley _ _ _ _dummy = reachesShelley
    -- this pattern should explicitly bind all fields
    ReachesShelley
      { rsByronSlots
      , rsPV
      , rsShelleyBlocks
      , rsShelleySlots
      } = reachesShelley

    infixr `prepend`
    prepend :: Show a => String -> a -> String -> String
    prepend s req x = s <> " " <> show req <> ", " <> x

-- | Checks if the observation satisfies the (pre)reqs
prop_ReachesShelley :: ReachesShelley -> Property
prop_ReachesShelley rs = case BoolProps.checkReqs rs of
    Nothing  -> property True
    Just req ->
        counterexample (msg req <> ": " <> show rs) $
        rsShelleyBlocks == req
  where
    ReachesShelley{rsShelleyBlocks} = rs

    msg :: Bool -> String
    msg req = if req
        then "the final chains should include at least one Shelley block"
        else "the final chains should not include any Shelley blocks"
