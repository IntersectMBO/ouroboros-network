{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Test.ThreadNet.Cardano (
    tests
  ) where

import           Control.Exception (assert)
import           Control.Monad (guard, replicateM)
import           Data.List ((!!))
import qualified Data.Map as Map
import           Data.Proxy (Proxy (..))
import           Data.Word (Word64)
import           GHC.Generics (Generic)

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Cardano.Prelude (Natural)
import           Cardano.Slotting.Slot (EpochNo, EpochSize (..), SlotNo (..))

import           Cardano.Crypto.Hash.Blake2b (Blake2b_256)
import qualified Cardano.Crypto.KES.Class as KES

import           Ouroboros.Network.MockChain.Chain (chainToList)

import           Ouroboros.Consensus.Block.Abstract (getHeader)
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config.SecurityParam
import qualified Ouroboros.Consensus.HardFork.History.Util as Util
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
import qualified Ouroboros.Consensus.Shelley.Protocol as Shelley
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (KES, TPraosCrypto)

import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.Condense ()
import           Ouroboros.Consensus.Cardano.Node

import           Test.Consensus.Shelley.MockCrypto (TPraosMockCrypto)
import           Test.ThreadNet.General
import qualified Test.ThreadNet.Infra.Byron as Byron
import qualified Test.ThreadNet.Infra.Shelley as Shelley
import           Test.ThreadNet.Network (CalcMessageDelay (..),
                     NodeOutput (..), TestNodeInitialization (..))
import           Test.ThreadNet.TxGen.Cardano ()
import           Test.ThreadNet.Util.NodeJoinPlan (trivialNodeJoinPlan)
import           Test.ThreadNet.Util.NodeRestarts (noRestarts)
import           Test.ThreadNet.Util.NodeTopology (meshNodeTopology)
import qualified Test.Util.BoolProps as BoolProps
import           Test.Util.HardFork.Future
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Random
import           Test.Util.Slots (NumSlots (..))

-- | When and for how long the nodes are partitioned
--
-- The nodes are divided via message delays into @even@ and @odd@ sub-networks.
data Partition = Partition SlotNo NumSlots
  deriving (Show)

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
  , setupPartition         :: Partition
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
        `suchThat` (\x -> getSlotLength x < getSlotLength (slotLengthFromSec 1))
    setupSlotLengthShelley <- arbitrary
        `suchThat` (\x -> getSlotLength x < getSlotLength (slotLengthFromSec 1))

    setupTestConfig <- arbitrary

    setupByronLowerBound <- arbitrary
    setupHardFork        <- frequency [(9, pure True), (1, pure False)]
    setupPartition       <- genPartition (numSlots setupTestConfig) setupK

    pure TestSetup
      { setupByronLowerBound
      , setupD
      , setupHardFork
      , setupK
      , setupPartition
      , setupSlotLengthByron
      , setupSlotLengthShelley
      , setupTestConfig = setupTestConfig
        { nodeTopology =
            -- use a mesh topology so the even-odd partitions definitely remain
            -- internally connected
            meshNodeTopology (numCoreNodes setupTestConfig)
        }
      }

  -- TODO shrink

-- | Generate 'setupPartition'
genPartition :: NumSlots -> SecurityParam -> Gen Partition
genPartition (NumSlots t) (SecurityParam k)
    | t < 2     = pure $ Partition 0 (NumSlots 0)
    | otherwise = do
    -- The test must include at least one slot after the partition.
    firstSlotIn    <- choose (0, penultimateSlot)

    -- When the partition is over
    let ub =
            -- The partition must be short enough that neither partition can
            -- generate @k@ blocks. That avoids forcing ChainSync
            -- to look @k+1@ slots ahead, which the HFC might not allow.
            --
            -- Note that this is the exclusive upper bound, and so the max
            -- possible duration is @2k-1@ slots. 
            min ultimateSlot $ firstSlotIn + 2 * k
    firstSlotAfter <- choose (firstSlotIn, ub)

    let d = Util.countSlots (SlotNo firstSlotAfter) (SlotNo firstSlotIn)
    pure $ Partition (SlotNo firstSlotIn) (NumSlots d)
  where
    penultimateSlot = ultimateSlot - 1
    ultimateSlot    = t - 1

tests :: TestTree
tests = testGroup "Cardano" $
    [ testProperty "simple convergence" $ \setup ->
          prop_simple_cardano_convergence setup
    ]

prop_simple_cardano_convergence :: TestSetup -> Property
prop_simple_cardano_convergence testSetup@TestSetup
  { setupByronLowerBound
  , setupD
  , setupHardFork
  , setupK
  , setupPartition
  , setupSlotLengthByron
  , setupSlotLengthShelley
  , setupTestConfig
  } =
    tabulate "ReachesShelley label" [label_ReachesShelley reachesShelley] $
    tabulate "partition duration (approx frac of k)" [labelPartition testSetup] $
    prop_general_semisync PropGeneralArgs
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
    ( counterexample (labelFinalChains testOutput) $
      prop_ReachesShelley reachesShelley
    )
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
          EraCons  setupSlotLengthByron   epochSizeByron   eraSizeByron $
          EraFinal setupSlotLengthShelley epochSizeShelley
          else
          EraFinal setupSlotLengthByron   epochSizeByron
      , messageDelay = mkMessageDelay setupPartition
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
                  (TriggerHardForkAtVersion shelleyMajorVersion)
            , mkRekeyM = Nothing
            }

    -- Byron

    pbftParams :: PBftParams
    pbftParams = Byron.realPBftParams setupK numCoreNodes

    -- the Byron ledger is designed to use a fixed epoch size, so this test
    -- does not randomize it
    epochSizeByron :: EpochSize
    epochSizeByron =
        fromByronEpochSlots $ CC.Genesis.configEpochSlots genesisByron

    eraSizeByron :: EraSize
    eraSizeByron = EraSize numByronEpochs

    genesisByron     :: CC.Genesis.Config
    generatedSecrets :: CC.Genesis.GeneratedSecrets
    (genesisByron, generatedSecrets) =
        Byron.generateGenesisConfig setupSlotLengthByron pbftParams

    -- Shelley

    initialKESPeriod :: SL.KESPeriod
    initialKESPeriod = SL.KESPeriod 0

    maxKESEvolutions :: Word64
    maxKESEvolutions = fromIntegral $
      KES.totalPeriodsKES (Proxy @(KES (TPraosMockCrypto Blake2b_256)))

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
          maxKESEvolutions
          coreNodes

    -- the Shelley ledger is designed to use a fixed epoch size, so this test
    -- does not randomize it
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
          BoolProps.enabledIf $ t > byronSlots
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
          BoolProps.requiredIf $ t > byronSlots + w + 1 - k
            -- logic: if we are ensured at least @k@ blocks in @w@ slots, then
            -- we're ensured at least @1@ block in @w+1-k@ slots
            --
            -- Note that the number of partitioned slots during the Shelley
            -- epoch is irrelevant as long as both make the transition: the
            -- above logic ensures (statistically) at least one partition will
            -- forge a Shelley block.
      }
      where
        TestConfig{numSlots}        = setupTestConfig
        NumSlots t                  = numSlots
        TestOutput{testOutputNodes} = testOutput

        byronSlots :: Word64
        byronSlots = numByronEpochs * unEpochSize epochSizeByron

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
  -> TriggerHardFork
  -> TestNodeInitialization m (CardanoBlock sc)
mkProtocolCardanoAndHardForkTxs
    pbftParams coreNodeId genesisByron generatedSecretsByron propPV
    genesisShelley coreNodeShelley
    mbLowerBound triggerHardFork =
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

    pInfo :: ProtocolInfo m (CardanoBlock sc)
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
        triggerHardFork

    -- Byron
    pbftSignatureThreshold = 1
--    PBftParams { pbftSignatureThreshold } = pbftParams

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
        counterexample (show rs) $
        counterexample (msg req) $
        rsShelleyBlocks == req
  where
    ReachesShelley{rsShelleyBlocks} = rs

    msg :: Bool -> String
    msg req = if req
        then "the final chains should include at least one Shelley block"
        else "the final chains should not include any Shelley blocks"

labelFinalChains :: TPraosCrypto sc => TestOutput (CardanoBlock sc) -> String
labelFinalChains testOutput =
    unlines $ 
    [ show $ (,) nid $ map getHeader $ chainToList nodeOutputFinalChain
    | (nid, no) <- Map.toList testOutputNodes
    , let NodeOutput{nodeOutputFinalChain} = no
    ]
  where
    TestOutput{testOutputNodes} = testOutput

 {-------------------------------------------------------------------------------
  A short even-odd partition
-------------------------------------------------------------------------------}

-- | Temporary partition 'CalcMessageDelay'
--
-- Calculates the delays that implement 'setupPartition'
mkMessageDelay :: Partition -> CalcMessageDelay blk
mkMessageDelay part = CalcMessageDelay $
    \(CoreNodeId i, CoreNodeId j) curSlot _hdr -> NumSlots $ if
      | curSlot <  firstSlotIn    -> 0
      | curSlot >= firstSlotAfter -> 0
      | mod i 2 == mod j 2        -> 0
      | otherwise                 -> unSlotNo $ firstSlotAfter - curSlot
  where
    Partition firstSlotIn (NumSlots d) = part

    -- exclusive upper-bound on the partition
    firstSlotAfter = firstSlotIn + SlotNo d

-- | Render the partition duration as an approximate fraction of @k@.
labelPartition :: TestSetup -> String
labelPartition testSetup =
     show (fromIntegral tenths / 10 :: Double)
  where
    TestSetup{setupK, setupPartition} = testSetup
    Partition _ (NumSlots d)          = setupPartition

    ratio  = toRational d / toRational (maxRollbacks setupK)
    tenths = round (ratio * 10) :: Int
