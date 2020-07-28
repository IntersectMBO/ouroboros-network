{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}

module Test.ThreadNet.RealTPraos (tests) where

import           Control.Monad (replicateM)
import           Data.List ((!!))
import qualified Data.Map.Strict as Map
import           Data.Proxy (Proxy (..))
import           Data.Word (Word64)

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Cardano.Crypto.Hash (ShortHash)
import           Cardano.Slotting.EpochInfo (fixedSizeEpochInfo)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config.SecurityParam
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool (extractTxs)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId

import           Test.ThreadNet.General
import           Test.ThreadNet.Infra.Shelley
import           Test.ThreadNet.Network (TestNodeInitialization (..),
                     nodeOutputFinalLedger)

import           Test.Util.HardFork.Future (singleEraFuture)
import           Test.Util.Nightly
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Slots (NumSlots (..))

import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.LedgerState as SL
import qualified Shelley.Spec.Ledger.OCert as SL
import qualified Shelley.Spec.Ledger.PParams as SL

import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger as Shelley
import           Ouroboros.Consensus.Shelley.Node
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (KES)

import           Test.Consensus.Shelley.MockCrypto (TPraosMockCrypto)
import           Test.ThreadNet.TxGen.Shelley
import           Test.ThreadNet.Util.NodeJoinPlan (trivialNodeJoinPlan)
import           Test.ThreadNet.Util.NodeRestarts (noRestarts)
import           Test.ThreadNet.Util.NodeToNodeVersion (genVersion)
import           Test.ThreadNet.Util.Seed (runGen)

type Crypto = TPraosMockCrypto ShortHash

data TestSetup = TestSetup
  { setupD          :: DecentralizationParam
  , setupD2         :: DecentralizationParam
    -- ^ scheduled value
    --
    -- If not equal to 'setupD', every node immediately (ie slot 0) issues a
    -- protocol update transaction that will change the @d@ protocol parameter
    -- accordingly.
  , setupK          :: SecurityParam
  , setupTestConfig :: TestConfig
  , setupVersion    :: (NodeToNodeVersion, BlockNodeToNodeVersion (ShelleyBlock Crypto))
  }
  deriving (Show)

minK :: Word64
minK = 5   -- Less than this increases risk of CP violations

maxK :: Word64
maxK = 10   -- More than this wastes execution time

instance Arbitrary TestSetup where
  arbitrary = do
      setupD  <- arbitrary
      setupD2 <- arbitrary

      setupK  <- SecurityParam <$> choose (minK, maxK)

      setupTestConfig <- arbitrary

      setupVersion <- genVersion (Proxy @(ShelleyBlock Crypto))

      pure TestSetup
        { setupD
        , setupD2
        , setupK
        , setupTestConfig
        , setupVersion
        }

  -- TODO shrink

-- | We run for more slots at night.
newtype NightlyTestSetup = NightlyTestSetup TestSetup
  deriving (Show)

instance Arbitrary NightlyTestSetup where
  shrink (NightlyTestSetup setup) = NightlyTestSetup <$> shrink setup

  arbitrary = do
      setup <- arbitrary

      -- At time of writing, this causes 100 tests to have an expected run time
      -- of half an hour on the BuildKite queue=bench machine.
      --
      -- 100 extended tests has an average run time of 4643 seconds (cf
      -- https://buildkite.com/input-output-hk/ouroboros-network-nightly/builds/167
      -- ). 100 unextended tests has an average of 689 seconds (cf
      -- https://buildkite.com/input-output-hk/ouroboros-network-nightly/builds/164).
      --
      -- 3/4*689 + 1/4*4643 seconds =~= 28 minutes.
      moreEpochs <- frequency [(3, pure False), (1, pure True)]

      NightlyTestSetup <$> if not moreEpochs then pure setup else do
        let TestSetup
              { setupK
              , setupTestConfig
              } = setup
            TestConfig
              { numSlots
              } = setupTestConfig
            NumSlots t = numSlots

        -- run for multiple epochs
        factor <- choose (1, 2)
        let t' = t + factor * unEpochSize (mkEpochSize setupK)

        pure setup
          { setupTestConfig = setupTestConfig
              { numSlots = NumSlots t'
              }
          }

-- | Run relatively fewer tests
--
-- These tests are slow, so we settle for running fewer of them in this test
-- suite since it is invoked frequently (eg CI for each push).
fifthTestCount :: QuickCheckTests -> QuickCheckTests
fifthTestCount (QuickCheckTests n) = QuickCheckTests $
    if 0 == n then 0 else
    max 1 $ n `div` 5

tests :: TestTree
tests = testGroup "RealTPraos ThreadNet"
    [ let name = "simple convergence" in
      askIohkNightlyEnabled $ \enabled ->
      if enabled
      then testProperty name $ \(NightlyTestSetup setup) ->
             prop_simple_real_tpraos_convergence setup
      else adjustOption fifthTestCount $
           testProperty name $ \setup ->
             prop_simple_real_tpraos_convergence setup
    ]

prop_simple_real_tpraos_convergence :: TestSetup -> Property
prop_simple_real_tpraos_convergence TestSetup
  { setupD
  , setupD2
  , setupK
  , setupTestConfig
  , setupVersion
  } =
    countertabulate "Epoch number of last slot"
      ( show $
        if 0 >= unNumSlots numSlots then 0 else
        (unNumSlots numSlots - 1) `div` unEpochSize epochSize
      ) $
    countertabulate "Updating d"
      ( if not dShouldUpdate then "No" else
        "Yes, " <> show (compare setupD setupD2)
      ) $
    counterexample (show setupK) $
    prop_general PropGeneralArgs
      { pgaBlockProperty       = const $ property True
      , pgaCountTxs            = fromIntegral . length . extractTxs
      , pgaExpectedCannotForge = noExpectedCannotForges
      , pgaFirstBlockNo        = 0
      , pgaFixedMaxForkLength  = Nothing
      , pgaFixedSchedule       = Nothing
      , pgaSecurityParam       = setupK
      , pgaTestConfig          = setupTestConfig
      , pgaTestConfigB         = testConfigB
      }
      testOutput .&&.
    prop_checkFinalD
  where
    countertabulate :: String -> String -> Property -> Property
    countertabulate lbl s =
        tabulate lbl [s] . counterexample (lbl <> ": " <> s)

    TestConfig
      { initSeed
      , numCoreNodes
      , numSlots
      } = setupTestConfig

    testConfigB = TestConfigB
      { forgeEbbEnv  = Nothing
      , future       = singleEraFuture tpraosSlotLength epochSize
      , messageDelay = noCalcMessageDelay
      , nodeJoinPlan = trivialNodeJoinPlan numCoreNodes
      , nodeRestarts = noRestarts
      , txGenExtra   = ShelleyTxGenExtra
        { stgeGenEnv          = mkGenEnv inclPPUs coreNodes
        , stgeStartAt         =
            SlotNo $ if includingDUpdateTx then 1 else 0
            -- We don't generate any transactions before the transaction
            -- carrying the proposal because they might consume its inputs
            -- before it does, thereby rendering it invalid.
        }
      , version      = setupVersion
      }

    inclPPUs :: WhetherToGeneratePPUs
    inclPPUs =
        -- We don't generate any other updates, since doing so might
        -- accidentally supplant the bespoke update that these tests are
        -- expecting.
        --
        -- The transaction this test introduces causes all nodes to propose the
        -- same parameter update. It'd technically be OK if some nodes then
        -- changed their proposal to a different update, as long as at least
        -- @Quorum@-many nodes were still proposing this test's original update
        -- as of the epoch boundary. However, we keep the test simple and just
        -- avoid introducing any other proposals.
        if includingDUpdateTx then DoNotGeneratePPUs else DoGeneratePPUs

    -- The slot immediately after the end of this test.
    sentinel :: SlotNo
    sentinel = SlotNo $ unNumSlots numSlots

    -- We don't create the update proposal etc unless @d@ would change.
    includingDUpdateTx :: Bool
    includingDUpdateTx = setupD /= setupD2

    -- The ledger state should have an updated @d@ as of this slot.
    dUpdatedAsOf :: SlotNo
    dUpdatedAsOf = SlotNo $ unEpochSize epochSize

    -- Whether we expect @d@ to be updated during this test
    dShouldUpdate :: Bool
    dShouldUpdate = includingDUpdateTx && sentinel >= dUpdatedAsOf

    testOutput =
        runTestNetwork setupTestConfig testConfigB TestConfigMB
            { nodeInfo = \(CoreNodeId nid) ->
                TestNodeInitialization
                  { tniProtocolInfo =
                      mkProtocolRealTPraos
                        genesisConfig
                        SL.NeutralNonce
                        nextProtVer
                        (coreNodes !! fromIntegral nid)
                  , tniCrucialTxs =
                      if not includingDUpdateTx then [] else
                      mkSetDecentralizationParamTxs
                        coreNodes
                        nextProtVer
                        sentinel   -- Does not expire during test
                        setupD2
                  }
            , mkRekeyM = Nothing
            }

    initialKESPeriod :: SL.KESPeriod
    initialKESPeriod = SL.KESPeriod 0

    coreNodes :: [CoreNode Crypto]
    coreNodes = runGen initSeed $
        replicateM (fromIntegral n) $
          genCoreNode initialKESPeriod
      where
        NumCoreNodes n = numCoreNodes

    genesisConfig :: ShelleyGenesis Crypto
    genesisConfig =
        mkGenesisConfig
          genesisProtVer
          setupK
          setupD
          tpraosSlotLength
          (mkKesConfig (Proxy @(KES Crypto)) numSlots)
          coreNodes

    epochSize :: EpochSize
    epochSize = sgEpochLength genesisConfig

    genesisProtVer :: SL.ProtVer
    genesisProtVer = SL.ProtVer 0 0

    -- Which protocol version to endorse
    nextProtVer :: SL.ProtVer
    nextProtVer = incrementMinorProtVer genesisProtVer

    -- Does the final ledger state have the expected @d@ value when ticked over
    -- to the 'sentinel' slot?
    prop_checkFinalD :: Property
    prop_checkFinalD =
        conjoin $
        [ let ls =
                  -- Handle the corner case where the test has enough scheduled
                  -- slots to reach the epoch transition but the last several
                  -- slots end up empty.
                  Shelley.tickedShelleyState $
                  applyChainTick ledgerConfig sentinel lsUnticked

              msg =
                  "The ticked final ledger state of " <> show nid <>
                  " has an unexpected value for the d protocol parameter."

              -- The actual final value of @d@
              actual :: SL.UnitInterval
              actual = SL._d $ SL.esPp $ SL.nesEs ls

              -- The expected final value of @d@
              expected :: DecentralizationParam
              expected = if dShouldUpdate then setupD2 else setupD
          in
          counterexample ("unticked " <> show lsUnticked) $
          counterexample ("ticked   " <> show ls) $
          counterexample ("(d,d2) = " <> show (setupD, setupD2)) $
          counterexample
            ( "(dUpdatedAsOf, dShouldUpdate) = " <>
              show (dUpdatedAsOf, dShouldUpdate)
            ) $
          counterexample msg $
          SL.unitIntervalToRational actual ===
            decentralizationParamToRational expected
        | (nid, lsUnticked) <- finalLedgers
        ]
      where
        finalLedgers :: [(NodeId, LedgerState (ShelleyBlock Crypto))]
        finalLedgers =
            Map.toList $ nodeOutputFinalLedger <$> testOutputNodes testOutput

        ledgerConfig :: LedgerConfig (ShelleyBlock Crypto)
        ledgerConfig = Shelley.mkShelleyLedgerConfig
            genesisConfig
            (fixedSizeEpochInfo epochSize)
            maxMajorPV
          where
            maxMajorPV = 1000   -- TODO
