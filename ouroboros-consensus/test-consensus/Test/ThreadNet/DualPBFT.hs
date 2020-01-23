{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.ThreadNet.DualPBFT (
    tests
  ) where

import qualified Data.Set as Set
import           Test.QuickCheck
import           Test.QuickCheck.Hedgehog (hedgehog)
import           Test.Tasty
import           Test.Tasty.QuickCheck

import qualified Cardano.Spec.Chain.STS.Rule.Chain as Abstract
import qualified Control.State.Transition.Generator as Abstract.QC
import qualified Ledger.Core as Abstract

import           Ouroboros.Consensus.BlockchainTime.Mock (NumSlots (..))
import           Ouroboros.Consensus.BlockchainTime.SlotLengths
import           Ouroboros.Consensus.Ledger.Byron.Block (ByronBlock)
import           Ouroboros.Consensus.Ledger.ByronSpec.Genesis
                     (ByronSpecGenesis (..))
import qualified Ouroboros.Consensus.Ledger.ByronSpec.Genesis as Genesis
import           Ouroboros.Consensus.Ledger.Dual
import           Ouroboros.Consensus.Ledger.Dual.Byron
import           Ouroboros.Consensus.Node.ProtocolInfo.Abstract
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.LeaderSchedule
import           Ouroboros.Consensus.Protocol.PBFT

import           Test.ThreadNet.General
import qualified Test.ThreadNet.RealPBFT as RealPBFT
import           Test.ThreadNet.TxGen
import           Test.ThreadNet.Util
import           Test.ThreadNet.Util.NodeRestarts
import           Test.ThreadNet.Util.NodeTopology

tests :: TestTree
tests = testGroup "DualPBFT" [
      testProperty "convergence" $ prop_convergence
    ]

prop_convergence :: SetupDualPBft -> Property
prop_convergence setup =
    prop_general
      (setupSecurityParam      setup)
      (setupConfig             setup)
      (setupSchedule           setup)
      (setupExpectedRejections setup)
      (setupTestOutput         setup)

{-------------------------------------------------------------------------------
  Test setup
-------------------------------------------------------------------------------}

data SetupDualPBft = SetupDualPBft {
      setupGenesis :: ByronSpecGenesis
    , setupConfig  :: TestConfig
      -- | PBftParams is derived from the parameters above
    , setupParams  :: PBftParams
    }
  deriving (Show)

setupSecurityParam :: SetupDualPBft -> SecurityParam
setupSecurityParam = pbftSecurityParam . setupParams

setupNumSlots :: SetupDualPBft -> NumSlots
setupNumSlots = numSlots . setupConfig

-- TODO: Might we want to run /without/ a schedule? Perhaps sometimes?
setupSchedule :: SetupDualPBft -> Maybe LeaderSchedule
setupSchedule setup@SetupDualPBft{..} = Just $
    roundRobinLeaderSchedule
      (numCoreNodes setupConfig)
      (setupNumSlots setup)

setupTestOutput :: SetupDualPBft -> TestOutput DualByronBlock
setupTestOutput SetupDualPBft{..} =
    runTestNetwork setupConfig $ TestConfigBlock {
        forgeEBB = Nothing -- spec does not model EBBs
      , rekeying = Nothing -- TODO
      , nodeInfo = \coreNodeId ->
                      protocolInfoDualByron
                        setupGenesis
                        setupParams
                        (Just coreNodeId)
      }

-- | Override 'TestConfig'
--
-- This is intended to update the rest of the setup after shrinking the
-- 'TestConfig'. Right now this just updates the PBFT slot length.
setupOverrideConfig :: TestConfig -> SetupDualPBft -> SetupDualPBft
setupOverrideConfig newConfig setup = setup {
      setupConfig = newConfig
    , setupParams = (setupParams setup) {
                        pbftSlotLength = slotLength
                      }
    }
  where
    SlotLengths slotLength Nothing = slotLengths newConfig

setupExpectedRejections :: SetupDualPBft
                        -> BlockRejection DualByronBlock -> Bool
setupExpectedRejections setup@SetupDualPBft{..} rejection =
    RealPBFT.expectedBlockRejection
      (setupSecurityParam setup)
      (numCoreNodes setupConfig)
      (nodeRestarts setupConfig)
      rejection'
  where
    rejection' :: BlockRejection ByronBlock
    rejection' =  BlockRejection {
          brBlockHash = brBlockHash rejection
        , brBlockSlot = brBlockSlot rejection
        , brReason    = dualExtValidationErrorMain (brReason rejection)
        , brRejector  = brRejector rejection
        }

{-------------------------------------------------------------------------------
  Generator for 'SetupDualPBft'
-------------------------------------------------------------------------------}

instance Arbitrary SetupDualPBft where
  arbitrary = do
      numSlots <- arbitrary
      genesis  <- genChainEnv numSlots
      let params = realPBftParams genesis
      config <- genDualPBFTTestConfig numSlots params
      return SetupDualPBft {
          setupGenesis = adjustGenesis params genesis
        , setupConfig  = config
        , setupParams  = params
        }
    where
      -- The spec tests and the RealPBFT tests compute a different test value
      -- for the PBFT threshold. For now we ignore the value computed by the
      -- spec and override it with the value computed in the RealPBFT tests.
      --
      -- TODO: It would be interesting to see if we can bring these two in line,
      -- but if we do, we probably need to adjust 'expectedBlockRejection'.
      adjustGenesis :: PBftParams
                    -> ByronSpecGenesis
                    -> ByronSpecGenesis
      adjustGenesis = Genesis.setPBftThreshold . pbftSignatureThreshold

  shrink setup@SetupDualPBft{..} = concat [
        -- Shrink number of slots
        --
        -- The number of slots is a parameter to everything else we generate
        -- also, so we have to be careful:
        --
        -- * The CHAIN environment just uses the number of slots to optimize
        --   some test parameter, for instance to guarantee that the test
        --   contains epoch boundaries. Once we have a failing test case,
        --   reducing the number of slots won't affect that.
        -- * The PBFT parameters are derived from the genesis (which in turn
        --   derives from the number of slots), but really only depends on
        --   @k@ and the number of genesis keys, so we can shrink the number of
        --   slots independently from the PBFT parameters.
        -- * The TestConfig /does/ depend on the number of slots quite a lot,
        --   but we already have a shrinker 'shrinkTestConfigSlotsOnly' for
        --   the test config that does precisely this: reduce the number of
        --   slots and readjust the rest.
        --
        -- Therefore here we can just piggy-back on 'shrinkTestConfigSlotsOnly'.
        [ setupOverrideConfig config' setup
        | config' <- RealPBFT.shrinkTestConfigSlotsOnly setupConfig
        ]
      ]

-- | Generate abstract genesis config (environment for the CHAIN rule)
--
-- The generator for the environment tries to pick a @k@ that ensures the
-- trace (independent of its length) contains multiple epochs, which is why
-- this wants to know the chain length; we don't know that a-priority, but we
-- do know the number of slots, and will use that as a stand-in.
genChainEnv :: NumSlots -> Gen ByronSpecGenesis
genChainEnv (NumSlots numSlots) = fmap Genesis.fromChainEnv . hedgehog $
    -- Convert Hedgehog generator to QuickCheck one
    -- Unfortunately, this does mean we lose any shrinking.
    Abstract.QC.envGen @Abstract.CHAIN numSlots

-- | Generate test config
--
-- This is adopted from 'genRealPBFTTestConfig'.
--
-- TODO: Once we produce all kinds of transactions, we will need to rethink
-- rekeys/restarts (but might not be trivial, as we do not generate the blocks
-- upfront..).
genDualPBFTTestConfig :: NumSlots
                      -> PBftParams
                      -> Gen TestConfig
genDualPBFTTestConfig numSlots params = do
    nodeJoinPlan <- RealPBFT.genRealPBFTNodeJoinPlan params numSlots
    nodeTopology <- genNodeTopology (pbftNumNodes params)
    initSeed     <- arbitrary

    return TestConfig {
          nodeRestarts = noRestarts
        , slotLengths  = singletonSlotLengths (pbftSlotLength params)
        , numCoreNodes = pbftNumNodes params
        , ..
        }

realPBftParams :: ByronSpecGenesis -> PBftParams
realPBftParams ByronSpecGenesis{..} =
    RealPBFT.realPBftParams (SecurityParam k) numCoreNodes
  where
    Abstract.BlockCount k = byronSpecGenesisSecurityParam

    numCoreNodes :: NumCoreNodes
    numCoreNodes = NumCoreNodes $
                     fromIntegral (Set.size byronSpecGenesisDelegators)

{-------------------------------------------------------------------------------
  Generate transactions
-------------------------------------------------------------------------------}

-- TODO: Implement the transaction generator
instance TxGen DualByronBlock where
  testGenTx  _ _ _ = undefined -- not used when we redefine testGenTxs
  testGenTxs _ _ _ = return []
