{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.ThreadNet.ShelleyAllegra (
    tests
  ) where

import           Control.Monad (replicateM)
import qualified Data.Map as Map
import           Data.Maybe (maybeToList)
import           Data.Proxy (Proxy (..))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.SOP.Strict (NP (..))
import           Data.Word (Word64)

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Cardano.Crypto.Hash (ShortHash)
import           Cardano.Slotting.Slot (EpochSize (..), SlotNo (..))

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config.SecurityParam
import           Ouroboros.Consensus.Ledger.SupportsMempool (extractTxs)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId

import           Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common
                     (isHardForkNodeToNodeEnabled)

import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.OCert as SL
import qualified Shelley.Spec.Ledger.PParams as SL

import           Ouroboros.Consensus.Shelley.Eras
import           Ouroboros.Consensus.Shelley.Node
                     (ProtocolParamsShelleyBased (..), ShelleyGenesis (..))

import           Ouroboros.Consensus.Cardano.Condense ()
import           Ouroboros.Consensus.Cardano.Node
                     (ProtocolParamsTransition (..), TriggerHardFork (..))

import           Test.ThreadNet.General
import           Test.ThreadNet.Network (NodeOutput (..),
                     TestNodeInitialization (..))
import           Test.ThreadNet.Util.Expectations (NumBlocks (..))
import           Test.ThreadNet.Util.NodeJoinPlan (trivialNodeJoinPlan)
import           Test.ThreadNet.Util.NodeRestarts (noRestarts)
import           Test.ThreadNet.Util.NodeToNodeVersion (genVersionFiltered)
import           Test.ThreadNet.Util.Seed (runGen)
import qualified Test.Util.BoolProps as BoolProps
import           Test.Util.HardFork.Future (EraSize (..), Future (..))
import           Test.Util.Nightly (askIohkNightlyEnabled)
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Slots (NumSlots (..))

import           Test.Consensus.Shelley.MockCrypto (MockCrypto)
import qualified Test.ThreadNet.Infra.Shelley as Shelley
import           Test.ThreadNet.Infra.ShelleyBasedHardFork
import           Test.ThreadNet.TxGen
import           Test.ThreadNet.TxGen.Allegra ()
import           Test.ThreadNet.TxGen.Shelley

import           Test.ThreadNet.Infra.TwoEras

-- | No Byron era, so our crypto can be trivial.
type Crypto = MockCrypto ShortHash

type ShelleyAllegraBlock =
  ShelleyBasedHardForkBlock (ShelleyEra Crypto) (AllegraEra Crypto)

-- | The varying data of this test
--
-- Note: The Shelley nodes in this test all join, propose an update, and endorse
-- it literally as soon as possible. Therefore, if the test reaches the end of
-- the first epoch, the proposal will be adopted.
data TestSetup = TestSetup
  { setupD            :: Shelley.DecentralizationParam
  , setupHardFork     :: Bool
    -- ^ whether the proposal should trigger a hard fork or not
  , setupInitialNonce :: SL.Nonce
    -- ^ the initial Shelley 'SL.ticknStateEpochNonce'
    --
    -- We vary it to ensure we explore different leader schedules.
  , setupK            :: SecurityParam
  , setupPartition    :: Partition
  , setupSlotLength   :: SlotLength
  , setupTestConfig   :: TestConfig
  , setupVersion      :: (NodeToNodeVersion, BlockNodeToNodeVersion ShelleyAllegraBlock)
  }
  deriving (Show)

instance Arbitrary TestSetup where
  arbitrary = do
    setupD <- arbitrary
                -- The decentralization parameter cannot be 0 in the first
                -- Shelley epoch, since stake pools can only be created and
                -- delegated to via Shelley transactions.
                `suchThat` ((/= 0) . Shelley.decentralizationParamToRational)
    setupK <- SecurityParam <$> choose (8, 10)
                -- If k < 8, common prefix violations become too likely in
                -- Praos mode for thin overlay schedules (ie low d), even for
                -- f=0.2.

    setupInitialNonce <- genNonce

    setupSlotLength   <- arbitrary

    let epochSize = EpochSize $ shelleyEpochSize setupK
    setupTestConfig <- genTestConfig
                         setupK
                         (epochSize, epochSize)
    let TestConfig{numCoreNodes, numSlots} = setupTestConfig

    setupHardFork  <- frequency [(49, pure True), (1, pure False)]

    -- TODO How reliable is the Byron-based partition duration logic when
    -- reused for Shelley?
    setupPartition <- genPartition numCoreNodes numSlots setupK

    setupVersion   <- genVersionFiltered
                        isHardForkNodeToNodeEnabled
                        (Proxy @ShelleyAllegraBlock)

    pure TestSetup
      { setupD
      , setupHardFork
      , setupInitialNonce
      , setupK
      , setupPartition
      , setupSlotLength
      , setupTestConfig
      , setupVersion
      }

  -- TODO shrink

-- | Run relatively fewer tests
--
-- These tests are slow, so we settle for running fewer of them in this test
-- suite since it is invoked frequently (eg CI for each push).
oneTenthTestCount :: QuickCheckTests -> QuickCheckTests
oneTenthTestCount (QuickCheckTests n) = QuickCheckTests $
    if 0 == n then 0 else
    max 1 $ n `div` 10

tests :: TestTree
tests = testGroup "ShelleyAllegra ThreadNet" $
    [ let name = "simple convergence" in
      askIohkNightlyEnabled $ \enabled ->
      (if enabled then id else adjustOption oneTenthTestCount) $
      testProperty name $ \setup ->
        prop_simple_shelleyAllegra_convergence setup
    ]

prop_simple_shelleyAllegra_convergence :: TestSetup -> Property
prop_simple_shelleyAllegra_convergence TestSetup
  { setupD
  , setupHardFork
  , setupInitialNonce
  , setupK
  , setupPartition
  , setupSlotLength
  , setupTestConfig
  , setupVersion
  } =
    prop_general_semisync pga testOutput .&&.
    prop_inSync testOutput .&&.
    prop_ReachesEra2 reachesEra2 .&&.
    prop_noCPViolation .&&.
    ( tabulate "ReachesEra2 label" [label_ReachesEra2 reachesEra2] $
      tabulate "Observed forge during a non-overlay slot in the second era"
        [ label_hadActiveNonOverlaySlots
            testOutput
            overlaySlots
        ] $
      tabulatePartitionDuration setupK setupPartition $
      tabulateFinalIntersectionDepth
        setupK
        (NumBlocks finalIntersectionDepth)
        finalBlockEra $
      tabulatePartitionPosition
        (NumSlots numFirstEraSlots)
        setupPartition
        (ledgerReachesEra2 reachesEra2) $
      property True
    )
  where
    TestConfig
      { initSeed
      , numCoreNodes
      , numSlots
      } = setupTestConfig

    pga = PropGeneralArgs
        { pgaBlockProperty       = const $ property True
        , pgaCountTxs            = fromIntegral . length . extractTxs
        , pgaExpectedCannotForge = noExpectedCannotForges
        , pgaFirstBlockNo        = 0
        , pgaFixedMaxForkLength  = Just maxForkLength
          -- the leader schedule isn't fixed because the Shelley leader
          -- schedule is (at least ideally) unpredictable
        , pgaFixedSchedule       = Nothing
        , pgaSecurityParam       = setupK
        , pgaTestConfig          = setupTestConfig
        , pgaTestConfigB         = testConfigB
        }

    txGenExtra = ShelleyTxGenExtra
      { stgeGenEnv  = mkGenEnv DoNotGeneratePPUs coreNodes
        -- We don't generate any transactions before the transaction
        -- carrying the proposal because they might consume its inputs
        -- before it does, thereby rendering it invalid.
      , stgeStartAt = SlotNo 1
      }

    testConfigB = TestConfigB
      { forgeEbbEnv  = Nothing
      , future       =
          if setupHardFork
          then
          -- In this case the PVU will trigger the transition to the second era
          --
          -- By FACT (B), the PVU is always successful if we reach the second
          -- era.
          EraCons  setupSlotLength epochSize firstEraSize $
          EraFinal setupSlotLength epochSize
          else
          EraFinal setupSlotLength epochSize
      , messageDelay = mkMessageDelay setupPartition
      , nodeJoinPlan = trivialNodeJoinPlan numCoreNodes
      , nodeRestarts = noRestarts
      , txGenExtra   = WrapTxGenExtra txGenExtra :* WrapTxGenExtra () :* Nil
      , version      = setupVersion
      }

    testOutput :: TestOutput ShelleyAllegraBlock
    testOutput = runTestNetwork setupTestConfig testConfigB TestConfigMB {
          nodeInfo = \(CoreNodeId nid) ->
            TestNodeInitialization {
                tniCrucialTxs   =
                  if not setupHardFork then [] else
                  fmap GenTxShelley1 $
                  Shelley.mkSetDecentralizationParamTxs
                    coreNodes
                    (SL.ProtVer majorVersion2 0)
                    (SlotNo $ unNumSlots numSlots)   -- never expire
                    setupD   -- unchanged
              , tniProtocolInfo =
                  protocolInfoShelleyBasedHardFork
                    ProtocolParamsShelleyBased {
                        shelleyBasedGenesis           = genesisShelley
                      , shelleyBasedInitialNonce      = setupInitialNonce
                      , shelleyBasedLeaderCredentials =
                          [Shelley.mkLeaderCredentials
                            (coreNodes !! fromIntegral nid)]
                      }
                    (SL.ProtVer majorVersion1 0)
                    (SL.ProtVer majorVersion2 0)
                    ProtocolParamsTransition {
                        transitionTrigger = TriggerHardForkAtVersion majorVersion2
                      }
              }
          , mkRekeyM = Nothing
          }

    maxForkLength :: NumBlocks
    maxForkLength = NumBlocks $ maxRollbacks setupK

    initialKESPeriod :: SL.KESPeriod
    initialKESPeriod = SL.KESPeriod 0

    coreNodes :: [Shelley.CoreNode Crypto]
    coreNodes = runGen initSeed $
        replicateM (fromIntegral n) $
          Shelley.genCoreNode initialKESPeriod
      where
        NumCoreNodes n = numCoreNodes

    maxLovelaceSupply :: Word64
    maxLovelaceSupply =
      fromIntegral (length coreNodes) * Shelley.initialLovelacePerCoreNode

    genesisShelley :: ShelleyGenesis (ShelleyEra Crypto)
    genesisShelley =
        Shelley.mkGenesisConfig
          (SL.ProtVer majorVersion1 0)
          setupK
          activeSlotCoeff
          setupD
          maxLovelaceSupply
          setupSlotLength
          (Shelley.mkKesConfig (Proxy @Crypto) numSlots)
          coreNodes

    -- the Shelley ledger is designed to use a fixed epoch size, so this test
    -- does not randomize it
    epochSize :: EpochSize
    epochSize = sgEpochLength genesisShelley

    firstEraSize :: EraSize
    firstEraSize = EraSize numFirstEraEpochs

    -- Classifying test cases

    reachesEra2 :: ReachesEra2
    reachesEra2 = ReachesEra2
      { rsEra1Slots  =
          BoolProps.enabledIf $ t > numFirstEraSlots
      , rsPV         = BoolProps.enabledIf setupHardFork
      , rsEra2Blocks =
          or $
          [ not $ isFirstEraBlock blk
          | (_nid, no) <- Map.toList testOutputNodes
          , let NodeOutput{nodeOutputForges} = no
          , (blk, _m) <- maybeToList $ Map.maxView nodeOutputForges
                -- the last block the node forged
          ]
      , rsEra2Slots  =
          --- TODO this comment and code are wrong

          BoolProps.requiredIf $
          -- The active slots in the first two Shelley epochs are all overlay
          -- slots, so the first Shelley block will arise from one of those.
          not $ Set.null overlaySlots
      }
      where
        NumSlots t                  = numSlots
        TestOutput{testOutputNodes} = testOutput

    -- All OBFT overlay slots in the second era.
    overlaySlots :: Set SlotNo
    overlaySlots =
        secondEraOverlaySlots
          numSlots
          (NumSlots numFirstEraSlots)
          (SL._d (sgProtocolParams genesisShelley))
          epochSize

    numFirstEraSlots :: Word64
    numFirstEraSlots =
        numFirstEraEpochs * unEpochSize epochSize

    finalBlockEra :: String
    finalBlockEra =
        if rsEra2Blocks reachesEra2
        then "Allegra"
        else "Shelley"

    finalIntersectionDepth :: Word64
    finalIntersectionDepth = depth
      where
        NumBlocks depth = calcFinalIntersectionDepth pga testOutput

    prop_noCPViolation :: Property
    prop_noCPViolation =
        counterexample
          ( "finalChains: " <>
            show (nodeOutputFinalChain <$> testOutputNodes testOutput)
          ) $
        counterexample "CP violation in final chains!" $
        property $ maxRollbacks setupK >= finalIntersectionDepth

{-------------------------------------------------------------------------------
  Constants
-------------------------------------------------------------------------------}

-- | The major protocol version of the first era in this test
majorVersion1 :: Num a => a
majorVersion1 = 0

-- | The major protocol version of the second era in this test
majorVersion2 :: Num a => a
majorVersion2 = majorVersion1 + 1
