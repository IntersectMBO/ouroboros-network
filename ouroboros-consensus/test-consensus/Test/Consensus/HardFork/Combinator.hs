{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Consensus.HardFork.Combinator (tests) where

import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Map
import           Data.SOP.BasicFunctors
import           Data.SOP.Strict hiding (shape)
import           Data.Word

import           Cardano.Slotting.Slot

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.Time (dawnOfTime)

import           Ouroboros.Network.Block
import qualified Ouroboros.Network.MockChain.Chain as Mock

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.LeaderSchedule
                     (LeaderSchedule (..), leaderScheduleFor)
import           Ouroboros.Consensus.Storage.ImmutableDB (simpleChunkInfo)
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util.Counting
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.SOP

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Condense ()
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation
import qualified Ouroboros.Consensus.HardFork.Combinator.Serialisation as HFC
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs
                     (RequiringBoth (..))
import           Ouroboros.Consensus.HardFork.History (EraParams (..),
                     defaultSafeZone)
import qualified Ouroboros.Consensus.HardFork.History as History

import           Test.ThreadNet.General
import           Test.ThreadNet.Network
import           Test.ThreadNet.TxGen
import           Test.ThreadNet.Util
import           Test.ThreadNet.Util.NodeJoinPlan
import           Test.ThreadNet.Util.NodeRestarts
import           Test.ThreadNet.Util.NodeTopology

import           Test.Util.HardFork.Future (singleEraFuture)
import           Test.Util.Random (Seed (..))
import           Test.Util.WrappedClock (NumSlots (..))

import           Test.Consensus.HardFork.Combinator.A
import           Test.Consensus.HardFork.Combinator.B

tests :: TestTree
tests = testGroup "HardForkCombinator" [
      testProperty "simple convergence" $
        prop_simple_hfc_convergence
    ]

data TestSetup = TestSetup {
      -- TODO: Vary epoch size across the fork
      testSetupEpochSize  :: EpochSize
      -- ^ INVARIANT: @> 0@
    , testSetupK          :: SecurityParam
    , testSetupSeed       :: Seed
      -- TODO: Vary slot length across the fork
    , testSetupSlotLength :: SlotLength
    , testSetupTxSlot     :: SlotNo
    }
  deriving (Show)

instance Arbitrary TestSetup where
  arbitrary = do
      testSetupEpochSize <- EpochSize     <$> choose (1, 10)
      testSetupK         <- SecurityParam <$> choose (2, 10)
      -- TODO why does k=1 cause the nodes to only forge in the first epoch?
      testSetupTxSlot    <- SlotNo        <$> choose (0, 9)

      testSetupSeed       <- arbitrary
      testSetupSlotLength <- arbitrary
      return TestSetup{..}

-- | The number of epochs in the A era
testSetupEraSizeA :: TestSetup -> Word64
testSetupEraSizeA TestSetup{..} =
    -- This function, as a specification, intentionally independently
    -- reimplements the interpretation of the 'InitiateAtoB' transaction by the
    -- A ledger.
    succ lastEpochA
  where
    lastEpochA = lastSlotA `div` unEpochSize testSetupEpochSize
    lastSlotA  =
        unSlotNo testSetupTxSlot +
        stabilityWindowA testSetupK +
        safeFromTipA testSetupK

-- | Minimum number of slots needed to include exactly one epoch of the B era
testSetupNumSlots :: TestSetup -> NumSlots
testSetupNumSlots testSetup@TestSetup{..} =
    -- this test doesn't need more than one B epoch
    NumSlots $ eraSizeA * epoSizeA + epoSizeB
  where
    eraSizeA           = testSetupEraSizeA testSetup
    EpochSize epoSizeA = testSetupEpochSize
    EpochSize epoSizeB = testSetupEpochSize

prop_simple_hfc_convergence :: TestSetup -> Property
prop_simple_hfc_convergence testSetup@TestSetup{..} =
    counterexample (show testConfig) $
    tabulate "size of era A" [show (testSetupEraSizeA testSetup)] $
    prop_general args testOutput .&&.
    prop_allExpectedBlocks
  where
    k :: SecurityParam
    k = testSetupK

    eraParamsA, eraParamsB :: EraParams
    eraParamsA = EraParams {
                     eraEpochSize  = testSetupEpochSize
                   , eraSlotLength = testSetupSlotLength
                   , eraSafeZone   = defaultSafeZone (safeFromTipA k)
                   }
    eraParamsB = EraParams {
                     eraEpochSize  = testSetupEpochSize
                   , eraSlotLength = testSetupSlotLength
                   , eraSafeZone   = safeZoneB k
                   }

    shape :: History.Shape '[BlockA, BlockB]
    shape = History.Shape $ exactlyTwo eraParamsA eraParamsB

    leaderSchedule :: LeaderSchedule
    leaderSchedule = roundRobinLeaderSchedule numCoreNodes numSlots
      where
        TestConfig{..} = testConfig

    args :: PropGeneralArgs TestBlock
    args = PropGeneralArgs {
          pgaBlockProperty      = const $ property True
        , pgaCountTxs           = fromIntegral . length . extractTxs
        , pgaExpectedCannotLead = noExpectedCannotLeads
        , pgaFirstBlockNo       = BlockNo 0
        , pgaFixedMaxForkLength = Nothing
        , pgaFixedSchedule      = Just leaderSchedule
        , pgaSecurityParam      = k
        , pgaTestConfig         = testConfig
        , pgaTestConfigB        = testConfigB
        }

    testConfig :: TestConfig
    testConfig = TestConfig {
          numCoreNodes = ncn
        , numSlots     = testSetupNumSlots testSetup
        , nodeTopology = meshNodeTopology ncn
        , initSeed     = testSetupSeed
        }
      where
        ncn :: NumCoreNodes
        ncn = NumCoreNodes 2

    testConfigB :: TestConfigB TestBlock
    testConfigB = TestConfigB {
          forgeEbbEnv  = Nothing
        , future       = singleEraFuture testSetupSlotLength testSetupEpochSize
        , nodeJoinPlan = trivialNodeJoinPlan numCoreNodes
        , nodeRestarts = noRestarts
        , txGenExtra   = ()
        }
      where
        TestConfig{..} = testConfig

    testConfigMB :: Monad m => TestConfigMB m TestBlock
    testConfigMB = TestConfigMB {
          nodeInfo = plainTestNodeInitialization . protocolInfo
        , mkRekeyM = Nothing
        }

    protocolInfo :: Monad m => CoreNodeId -> ProtocolInfo m TestBlock
    protocolInfo nid = ProtocolInfo {
          pInfoConfig =
            topLevelConfig nid
        , pInfoInitLedger = ExtLedgerState {
              ledgerState = HardForkLedgerState $
                              initHardForkState
                                initLedgerState
            , headerState = genesisHeaderState $
                              initHardForkState
                                (WrapConsensusState initConsensusState)
            }
        , pInfoLeaderCreds = Just (
                OptCons (WrapCanBeLeader ())
              $ OptCons (WrapCanBeLeader ())
              $ OptNil
            , MaintainForgeState {
                  initForgeState   = PerEraForgeState $ WrapForgeState ()
                                                     :* WrapForgeState ()
                                                     :* Nil
                , updateForgeState = \_ _ -> return ()
                }
            )
        }

    initLedgerState :: LedgerState BlockA
    initLedgerState = LgrA {
          lgrA_tip        = GenesisPoint
        , lgrA_transition = Nothing
        }

    initConsensusState :: ConsensusState ProtocolA
    initConsensusState = ()

    topLevelConfig :: CoreNodeId -> TopLevelConfig TestBlock
    topLevelConfig nid = TopLevelConfig {
          configConsensus = HardForkConsensusConfig {
              hardForkConsensusConfigK      = k
            , hardForkConsensusConfigShape  = shape
            , hardForkConsensusConfigPerEra = PerEraConsensusConfig $
                   (WrapPartialConsensusConfig $ consensusConfigA nid)
                :* (WrapPartialConsensusConfig $ consensusConfigB nid)
                :* Nil
            }
        , configLedger = HardForkLedgerConfig {
              hardForkLedgerConfigK      = k
            , hardForkLedgerConfigShape  = shape
            , hardForkLedgerConfigPerEra = PerEraLedgerConfig $
                   (WrapPartialLedgerConfig $ ledgerConfigA nid)
                :* (WrapPartialLedgerConfig $ ledgerConfigB nid)
                :* Nil
            }
        , configBlock = HardForkBlockConfig {
              hardForkBlockConfigPerEra = PerEraBlockConfig $
                   blockConfigA nid
                :* blockConfigB nid
                :* Nil
            }
        }

    consensusConfigA :: CoreNodeId -> ConsensusConfig ProtocolA
    consensusConfigA nid = CfgA {
          cfgA_k           = k
        , cfgA_leadInSlots = leaderScheduleFor nid leaderSchedule
        }

    consensusConfigB :: CoreNodeId -> ConsensusConfig ProtocolB
    consensusConfigB nid = CfgB {
          cfgB_k           = k
        , cfgB_leadInSlots = leaderScheduleFor nid leaderSchedule
        }

    ledgerConfigA :: CoreNodeId -> PartialLedgerConfig BlockA
    ledgerConfigA _nid = LCfgA {
          lcfgA_k           = k
        , lcfgA_systemStart = SystemStart dawnOfTime -- required for RunNode
        , lcfgA_forgeTxs    = Map.fromList [
              (testSetupTxSlot, [TxA (TxIdA 0) InitiateAtoB])
            ]
        }

    ledgerConfigB :: CoreNodeId -> LedgerConfig BlockB
    ledgerConfigB _nid = ()

    blockConfigA :: CoreNodeId -> BlockConfig BlockA
    blockConfigA _ = BCfgA

    blockConfigB :: CoreNodeId -> BlockConfig BlockB
    blockConfigB _ = BCfgB

    testOutput :: TestOutput TestBlock
    testOutput = runTestNetwork testConfig testConfigB testConfigMB

    prop_allExpectedBlocks :: Property
    prop_allExpectedBlocks =
        counterexample
            ( "some final chain does not have " <>
              show a <> " blocks from A and " <>
              show b <> " blocks from B"
            ) $
        counterexample (show $ Map.toList counts) $
        property $ all (== (a, b)) counts
      where
        TestConfig{..} = testConfig
        NumSlots t     = numSlots

        -- we expect one epoch from B and the rest from A
        b = unEpochSize testSetupEpochSize
        a = t - b

    -- counts of A blocks and of B blocks for each final chain
    counts :: Map.Map NodeId (Word64, Word64)
    counts =
        (\c -> (chainLen isA c, chainLen isB c)) <$> testOutputNodes
      where
        TestOutput{..} = testOutput

        isA, isB :: TestBlock -> Bool
        isA (HardForkBlock (OneEraBlock blk)) = index_NS blk == 0
        isB (HardForkBlock (OneEraBlock blk)) = index_NS blk == 1

        chainLen :: (a -> Bool) -> NodeOutput a -> Word64
        chainLen p NodeOutput{..} =
              fromIntegral
            . length
            . filter p
            $ Mock.chainToList nodeOutputFinalChain

-- We ignore the mempool for these tests
instance TxGen TestBlock where
  testGenTxs _ _ _ _ _ = return []

{-------------------------------------------------------------------------------
  Hard fork
-------------------------------------------------------------------------------}

type TestBlock = HardForkBlock '[BlockA, BlockB]

instance CanHardFork '[BlockA, BlockB] where
  hardForkEraTranslation = EraTranslation {
        translateLedgerState    = PCons ledgerState_AtoB    PNil
      , translateLedgerView     = PCons ledgerView_AtoB     PNil
      , translateConsensusState = PCons consensusState_AtoB PNil
      }

versionN2N :: BlockNodeToNodeVersion TestBlock
versionN2N = HardForkNodeToNodeEnabled $
                  WrapNodeToNodeVersion ()
               :* WrapNodeToNodeVersion ()
               :* Nil

versionN2C :: BlockNodeToClientVersion TestBlock
versionN2C = HardForkNodeToClientEnabled $
                  WrapNodeToClientVersion ()
               :* WrapNodeToClientVersion ()
               :* Nil

instance TranslateNetworkProtocolVersion TestBlock where
  supportedNodeToNodeVersions     _   = versionN2N :| []
  supportedNodeToClientVersions   _   = versionN2C :| []
  mostRecentSupportedNodeToNode   _   = versionN2N
  mostRecentSupportedNodeToClient _   = versionN2C
  nodeToNodeProtocolVersion       _ _ = NodeToNodeV_1
  nodeToClientProtocolVersion     _ _ = NodeToClientV_2

instance SerialiseHFC '[BlockA, BlockB]
  -- Use defaults

instance RunNode TestBlock where
  nodeBlockFetchSize     = const 0
  nodeCheckIntegrity     = \_ _ -> True
  nodeImmDbChunkInfo     = simpleChunkInfo
                         . eraEpochSize
                         . unK . hd
                         . History.getShape
                         . hardForkLedgerConfigShape
                         . configLedger
  nodeGetBinaryBlockInfo = HFC.binaryBlockInfo
                              $ fn (mapIK binaryBlockInfoA)
                             :* fn (mapIK binaryBlockInfoB)
                             :* Nil

{-------------------------------------------------------------------------------
  Translation
-------------------------------------------------------------------------------}

ledgerState_AtoB :: RequiringBoth WrapLedgerConfig (Translate LedgerState) BlockA BlockB
ledgerState_AtoB = RequireBoth $ \_ _ -> Translate $ \_ LgrA{..} -> LgrB {
      lgrB_tip = castPoint lgrA_tip
    }

ledgerView_AtoB :: RequiringBoth WrapLedgerConfig (Translate WrapLedgerView) BlockA BlockB
ledgerView_AtoB = RequireBoth $ \_ _ -> Translate $ \_ _ -> WrapLedgerView ()

consensusState_AtoB :: RequiringBoth WrapConsensusConfig (Translate WrapConsensusState) BlockA BlockB
consensusState_AtoB = RequireBoth $ \_ _ -> Translate $ \_ _ -> WrapConsensusState ()
