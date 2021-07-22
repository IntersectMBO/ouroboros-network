{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.ThreadNet.DualByron (tests) where

import           Control.Monad.Except
import           Data.ByteString (ByteString)
import qualified Data.Map as Map
import           Data.Proxy
import qualified Data.Set as Set
import           Test.QuickCheck
import           Test.QuickCheck.Hedgehog (hedgehog)
import           Test.Tasty
import           Test.Tasty.QuickCheck

import qualified Cardano.Chain.ProtocolConstants as Impl
import qualified Cardano.Chain.UTxO as Impl

import qualified Byron.Spec.Chain.STS.Rule.Chain as Spec
import qualified Byron.Spec.Ledger.Core as Spec
import qualified Byron.Spec.Ledger.UTxO as Spec
import qualified Control.State.Transition.Extended as Spec
import qualified Control.State.Transition.Generator as Spec.QC

import qualified Test.Cardano.Chain.Elaboration.UTxO as Spec.Test

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Dual
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.PBFT
import           Ouroboros.Consensus.TypeFamilyWrappers

import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Byron.Ledger.Conversions

import           Ouroboros.Consensus.ByronSpec.Ledger
import qualified Ouroboros.Consensus.ByronSpec.Ledger.Genesis as Genesis
import qualified Ouroboros.Consensus.ByronSpec.Ledger.Rules as Rules

import           Ouroboros.Consensus.ByronDual.Ledger
import           Ouroboros.Consensus.ByronDual.Node

import qualified Test.ThreadNet.Byron as Byron
import           Test.ThreadNet.General
import qualified Test.ThreadNet.Ref.PBFT as Ref
import           Test.ThreadNet.TxGen
import           Test.ThreadNet.Util
import           Test.ThreadNet.Util.NodeRestarts (noRestarts)
import           Test.ThreadNet.Util.NodeToNodeVersion (newestVersion)

import           Test.Util.HardFork.Future (singleEraFuture)
import           Test.Util.Slots (NumSlots (..))

tests :: TestTree
tests = testGroup "DualByron" [
      testProperty "convergence" $ prop_convergence
    ]

-- These tests are very expensive, due to the Byron generators
-- (100 tests take about 20 minutes)
-- We limit it to 10 tests for now.
prop_convergence :: SetupDualByron -> Property
prop_convergence setup = withMaxSuccess 10 $
    (\prop -> if mightForgeInSlot0 then discard else prop) $
    tabulate "Ref.PBFT result" [Ref.resultConstrName refResult] $
    prop_general PropGeneralArgs
      { pgaBlockProperty       = const $ property True
      , pgaCountTxs            = countByronGenTxs . dualBlockMain
      , pgaExpectedCannotForge = setupExpectedCannotForge setup
      , pgaFirstBlockNo        = 1
      , pgaFixedMaxForkLength  =
          Just $ NumBlocks $ case refResult of
            Ref.Forked{} -> 1
            _            -> 0
      , pgaFixedSchedule       =
          Just $ roundRobinLeaderSchedule numCoreNodes numSlots
      , pgaSecurityParam       = setupK
      , pgaTestConfig          = setupTestConfig
      , pgaTestConfigB         = setupTestConfigB setup
      }
      (setupTestOutput setup)
  where
    SetupDualByron{..}  = setup
    Byron.TestSetup{..} = setupByron
    TestConfig{..}      = setupTestConfig

    refResult :: Ref.Result
    refResult =
      Ref.simulate (setupParams setup) setupNodeJoinPlan numSlots

    -- The test infrastructure allows nodes to forge in slot 0; however, the
    -- cardano-ledger-specs code causes @PBFTFailure (SlotNotAfterLastBlock
    -- (Slot 0) (Slot 0))@ in that case. So we discard such tests.
    --
    -- This is ultimately due to the spec not modeling EBBs, while Byron
    -- requires that successor of the genesis block is always the epoch 0 EBB.
    -- As a result, the PBFT implementation tests the slot progression with
    -- @<=@ to accomodate EBBs whereas the executable STS spec uses @<@.
    mightForgeInSlot0 :: Bool
    mightForgeInSlot0 = case refResult of
      Ref.Forked _ m        -> any (0 `Set.member`) m
      Ref.Nondeterministic  -> True
      Ref.Outcomes outcomes -> case outcomes of
        []    -> False
        o : _ -> case o of
          Ref.Absent  -> False
          Ref.Nominal -> True
          Ref.Unable  -> True
          Ref.Wasted  -> True

{-------------------------------------------------------------------------------
  Test setup
-------------------------------------------------------------------------------}

data SetupDualByron = SetupDualByron {
      setupGenesis :: ByronSpecGenesis
    , setupByron   :: Byron.TestSetup
    }
  deriving (Show)

setupParams :: SetupDualByron -> PBftParams
setupParams = byronPBftParams . setupGenesis

setupTestConfigB :: SetupDualByron -> TestConfigB DualByronBlock
setupTestConfigB SetupDualByron{..} = TestConfigB
  { forgeEbbEnv  = Nothing -- spec does not model EBBs
  , future       = singleEraFuture setupSlotLength epochSize
  , messageDelay = noCalcMessageDelay
  , nodeJoinPlan = setupNodeJoinPlan
  , nodeRestarts = setupNodeRestarts
  , txGenExtra   = ()
  , version      = newestVersion (Proxy @DualByronBlock)
  }
  where
    Byron.TestSetup{..} = setupByron

    epochSize :: EpochSize
    epochSize =
        fromByronEpochSlots $ Impl.kEpochSlots (toByronBlockCount setupK)

setupTestOutput :: SetupDualByron -> TestOutput DualByronBlock
setupTestOutput setup@SetupDualByron{..} =
    runTestNetwork testConfig testConfigB TestConfigMB {
        nodeInfo = \coreNodeId ->
          plainTestNodeInitialization $
          protocolInfoDualByron
            setupGenesis
            (setupParams setup)
            [coreNodeId]
      , mkRekeyM = Nothing -- TODO
      }
  where
    testConfig  = Byron.setupTestConfig setupByron
    testConfigB = setupTestConfigB setup

setupExpectedCannotForge ::
     SetupDualByron
  -> SlotNo
  -> NodeId
  -> WrapCannotForge DualByronBlock
  -> Bool
setupExpectedCannotForge SetupDualByron{..} s nid (WrapCannotForge cl) =
    Byron.expectedCannotForge
      setupK
      numCoreNodes
      setupNodeRestarts
      s nid (WrapCannotForge cl)
  where
    Byron.TestSetup{..} = setupByron
    TestConfig{..}      = setupTestConfig

{-------------------------------------------------------------------------------
  Generator for 'SetupDualByron'
-------------------------------------------------------------------------------}

-- | We do an awkward dance in this generator. We want to reuse
-- 'Byron.TestSetup' as much as possible. However, 'genSpecGenesis' needs values
-- provided by 'Byron.TestSetup' (ie @numSlots@ and @slotLen@) but also sets a
-- value provided by 'Byron.TestSetup' (eg @k@).
instance Arbitrary SetupDualByron where
  arbitrary = do
      numSlots <- arbitrary
      slotLen  <- arbitrary

      genesis0                 <- genSpecGenesis slotLen numSlots
      let params@PBftParams{..} = byronPBftParams genesis0
          setupGenesis          = adjustGenesis params genesis0

      -- TODO: Once we produce all kinds of transactions, we will need to
      -- rethink rekeys/restarts (but might not be trivial, as we do not
      -- generate the blocks upfront..).
      setupByron <-
        (\x -> x{Byron.setupNodeRestarts = noRestarts})
        <$> Byron.genTestSetup
              pbftSecurityParam
              pbftNumNodes
              numSlots
              slotLen

      return SetupDualByron{..}
    where
      -- The spec tests and the Byron tests compute a different test value for
      -- the PBFT threshold. For now we ignore the value computed by the spec
      -- and override it with the value computed in the Byron tests.
      --
      -- TODO: It would be interesting to see if we can bring these two in line,
      -- but if we do, we probably need to adjust 'expectedBlockRejection'.
      adjustGenesis :: PBftParams
                    -> ByronSpecGenesis
                    -> ByronSpecGenesis
      adjustGenesis =
            Genesis.modPBftThreshold
          . const
          . getPBftSignatureThreshold
          . pbftSignatureThreshold

  -- TODO shrink

-- | Generate abstract genesis config (environment for the CHAIN rule)
--
-- The generator for the environment tries to pick a @k@ that ensures the
-- trace (independent of its length) contains multiple epochs, which is why
-- this wants to know the chain length; we don't know that a-priority, but we
-- do know the number of slots, and will use that as a stand-in.
genSpecGenesis :: SlotLength -> NumSlots -> Gen ByronSpecGenesis
genSpecGenesis slotLen (NumSlots numSlots) = fmap fromEnv . hedgehog $
    -- Convert Hedgehog generator to QuickCheck one
    -- Unfortunately, this does mean we lose any shrinking.
    Spec.QC.envGen @Spec.CHAIN numSlots
  where
    -- Start with a larger initial UTxO. This is important, because the Byron
    -- spec TX generator is wasteful, and with every transaction the UTxO
    -- shrinks. By starting with a larger initial UTxO we avoid the depleting
    -- the UTxO too early (at which point we'd not be able to generate further
    -- transactions, and produce empty blocks only).
    fromEnv :: Spec.Environment Spec.CHAIN -> ByronSpecGenesis
    fromEnv = Genesis.modUtxoValues (* 10000)
            . Genesis.fromChainEnv (toByronSlotLength slotLen)

byronPBftParams :: ByronSpecGenesis -> PBftParams
byronPBftParams ByronSpecGenesis{..} =
    Byron.byronPBftParams (SecurityParam k) numCoreNodes
  where
    Spec.BlockCount k = byronSpecGenesisSecurityParam

    numCoreNodes :: NumCoreNodes
    numCoreNodes = NumCoreNodes $
                     fromIntegral (Set.size byronSpecGenesisDelegators)

{-------------------------------------------------------------------------------
  Generate transactions
-------------------------------------------------------------------------------}

instance TxGen DualByronBlock where
  testGenTxs _coreNodeId _numCoreNodes curSlotNo cfg () = \st -> do
      n <- choose (0, 20)
      go [] n $ applyChainTick (configLedger cfg) curSlotNo st
    where
      -- Attempt to produce @n@ transactions
      -- Stops when the transaction generator cannot produce more txs
      go :: [GenTx DualByronBlock]     -- Accumulator
         -> Integer                    -- Number of txs to still produce
         -> TickedLedgerState DualByronBlock
         -> Gen [GenTx DualByronBlock]
      go acc 0 _  = return (reverse acc)
      go acc n st = do
          tx <- genTx cfg st
          case runExcept $ applyTx
                             (configLedger cfg)
                             DoNotIntervene
                             curSlotNo
                             tx
                             st of
            Right (st', _vtx) -> go (tx:acc) (n - 1) st'
            Left _            -> error "testGenTxs: unexpected invalid tx"

-- | Generate transaction
--
-- For now we only generate regular transactions. Generating delegation
-- certificates and update proposals/votes is out of the scope of this test,
-- for now. Extending the scope will require integration with the restart/rekey
-- infrastructure of the Byron tests.
genTx :: TopLevelConfig DualByronBlock
      -> Ticked (LedgerState DualByronBlock)
      -> Gen (GenTx DualByronBlock)
genTx cfg st = do
    aux <- sigGen (Rules.ctxtUTXOW cfg') st'
    let main :: Impl.ATxAux ByteString
        main = Spec.Test.elaborateTxBS
                 elaborateTxId
                 aux

    return $ DualGenTx {
        dualGenTxMain   = ByronTx (byronIdTx main) main
      , dualGenTxAux    = ByronSpecGenTx $ ByronSpecGenTxTx aux
      , dualGenTxBridge = specToImplTx aux main
      }
  where
    cfg' :: ByronSpecGenesis
    st'  :: Spec.State Spec.CHAIN

    cfg' = dualLedgerConfigAux (configLedger cfg)
    st'  = tickedByronSpecLedgerState $ tickedDualLedgerStateAux st

    bridge :: ByronSpecBridge
    bridge = tickedDualLedgerStateBridge st

    elaborateTxId :: Spec.TxId -> Impl.TxId
    elaborateTxId tid =
        case Map.lookup tid (bridgeTransactionIds bridge) of
          Nothing   -> error $ "elaborateTxId: unknown tx ID " ++ show tid
          Just tid' -> tid'

sigGen :: forall sts. (Spec.QC.HasTrace sts)
       => Rules.RuleContext sts
       -> Spec.State Spec.CHAIN
       -> Gen (Spec.Signal sts)
sigGen Rules.RuleContext{..} st = hedgehog $
    -- Convert Hedgehog generator to QuickCheck one
    -- Unfortunately, this does mean we lose any shrinking.
    Spec.QC.sigGen @sts (getRuleEnv st) (getRuleState st)
