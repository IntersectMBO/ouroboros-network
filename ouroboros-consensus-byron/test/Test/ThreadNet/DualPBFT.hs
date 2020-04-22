{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.ThreadNet.DualPBFT (
    tests
  ) where

import           Control.Monad.Trans.Except
import           Crypto.Number.Generate as Cryptonite
import           Crypto.Random (MonadRandom)
import           Data.ByteString (ByteString)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Word
import qualified Hedgehog
import qualified Hedgehog.Internal.Gen as HH
import qualified Hedgehog.Internal.Tree as HH
import qualified Hedgehog.Range as HH
import           Test.QuickCheck
import           Test.QuickCheck.Hedgehog (hedgehog)
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Cardano.Slotting.Slot

import qualified Cardano.Chain.ProtocolConstants as Impl
import qualified Cardano.Chain.UTxO as Impl

import qualified Byron.Spec.Chain.STS.Rule.Chain as Spec
import qualified Byron.Spec.Ledger.Core as Spec
import qualified Byron.Spec.Ledger.UTxO as Spec
import qualified Control.State.Transition.Extended as Spec
import qualified Control.State.Transition.Generator as Spec.QC

import qualified Test.Cardano.Chain.Elaboration.UTxO as Spec.Test

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Dual
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.LeaderSchedule
import           Ouroboros.Consensus.Protocol.PBFT

import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Byron.Ledger.Conversions

import           Ouroboros.Consensus.ByronSpec.Ledger
import qualified Ouroboros.Consensus.ByronSpec.Ledger.Genesis as Genesis
import qualified Ouroboros.Consensus.ByronSpec.Ledger.Rules as Rules

import           Ouroboros.Consensus.ByronDual.Ledger
import           Ouroboros.Consensus.ByronDual.Node

import           Test.ThreadNet.General
import qualified Test.ThreadNet.RealPBFT as RealPBFT
import qualified Test.ThreadNet.Ref.PBFT as Ref
import           Test.ThreadNet.TxGen
import           Test.ThreadNet.Util
import           Test.ThreadNet.Util.NodeRestarts
import           Test.ThreadNet.Util.NodeTopology

import           Test.Util.Time

tests :: TestTree
tests = testGroup "DualPBFT" [
      testProperty "convergence" $ prop_convergence
    ]

-- These tests are very expensive, due to the Byron generators
-- (100 tests take about 20 minutes)
-- We limit it to 10 tests for now.
prop_convergence :: SetupDualPBft -> Property
prop_convergence setup = withMaxSuccess 10 $
    (\prop -> if mightForgeInSlot0 then discard else prop) $
    tabulate "Ref.PBFT result" [Ref.resultConstrName refResult] $
    prop_general PropGeneralArgs
      { pgaBlockProperty          = const $ property True
      , pgaCountTxs               = countByronGenTxs . dualBlockMain
      , pgaExpectedBlockRejection = setupExpectedRejections setup
      , pgaFirstBlockNo           = 1
      , pgaFixedMaxForkLength     =
          Just $ NumBlocks $ case refResult of
            Ref.Forked{} -> 1
            _            -> 0
      , pgaFixedSchedule          = setupSchedule setup
      , pgaSecurityParam          = setupSecurityParam setup
      , pgaTestConfig             = cfg
      }
      (setupTestOutput setup)
  where
    cfg = setupConfig setup

    refResult :: Ref.Result
    refResult =
      Ref.simulate (setupParams setup) (nodeJoinPlan cfg) (numSlots cfg)

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

data SetupDualPBft = SetupDualPBft {
      setupGenesis :: ByronSpecGenesis
    , setupConfig  :: TestConfig
      -- | PBftParams is derived from the parameters above
    , setupParams  :: PBftParams
    }
  deriving (Show)

setupSecurityParam :: SetupDualPBft -> SecurityParam
setupSecurityParam = pbftSecurityParam . setupParams

setupEpochSize :: SetupDualPBft -> EpochSize
setupEpochSize setup =
    fromByronEpochSlots $ Impl.kEpochSlots (toByronBlockCount k)
  where
    k = setupSecurityParam setup

setupNumSlots :: SetupDualPBft -> NumSlots
setupNumSlots = numSlots . setupConfig

setupSchedule :: SetupDualPBft -> Maybe LeaderSchedule
setupSchedule setup@SetupDualPBft{..} = Just $
    roundRobinLeaderSchedule
      (numCoreNodes setupConfig)
      (setupNumSlots setup)

setupTestOutput :: SetupDualPBft -> TestOutput DualByronBlock
setupTestOutput setup@SetupDualPBft{..} =
    runTestNetwork setupConfig (setupEpochSize setup) $ TestConfigBlock {
        forgeEbbEnv = Nothing -- spec does not model EBBs
      , rekeying    = Nothing -- TODO
      , nodeInfo    = \coreNodeId ->
          plainTestNodeInitialization $
          protocolInfoDualByron
            setupGenesis
            setupParams
            (Just coreNodeId)
      , txGenExtra  = ()
      }

-- | Override 'TestConfig'
setupOverrideConfig :: TestConfig -> SetupDualPBft -> SetupDualPBft
setupOverrideConfig newConfig setup = setup {
      setupConfig = newConfig
    , setupParams = setupParams setup
    }

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
      genesis  <- genSpecGenesis numSlots
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
      adjustGenesis = Genesis.modPBftThreshold . const . pbftSignatureThreshold

  shrink setup@SetupDualPBft{..} = concat [
        -- Shrink number of slots
        --
        -- The number of slots is a parameter to everything else we generate
        -- also, so we have to be careful:
        --
        --   * The CHAIN environment just uses the number of slots to optimize
        --     some test parameter, for instance to guarantee that the test
        --     contains epoch boundaries. Once we have a failing test case,
        --     reducing the number of slots won't affect that.
        --   * The PBFT parameters are derived from the genesis (which in turn
        --     derives from the number of slots), but really only depends on
        --     @k@ and the number of genesis keys, so we can shrink the number of
        --     slots independently from the PBFT parameters.
        --   * The TestConfig /does/ depend on the number of slots quite a lot,
        --     but we already have a shrinker 'shrinkTestConfigSlotsOnly' for
        --     the test config that does precisely this: reduce the number of
        --     slots and readjust the rest.
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
genSpecGenesis :: NumSlots -> Gen ByronSpecGenesis
genSpecGenesis (NumSlots numSlots) = fmap fromEnv . hedgehog $
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
            . Genesis.fromChainEnv

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
        , slotLength   = slotLengthFromSec 20
        , numCoreNodes = pbftNumNodes params
        , ..
        }

realPBftParams :: ByronSpecGenesis -> PBftParams
realPBftParams ByronSpecGenesis{..} =
    RealPBFT.realPBftParams (SecurityParam k) numCoreNodes
  where
    Spec.BlockCount k = byronSpecGenesisSecurityParam

    numCoreNodes :: NumCoreNodes
    numCoreNodes = NumCoreNodes $
                     fromIntegral (Set.size byronSpecGenesisDelegators)

{-------------------------------------------------------------------------------
  Generate transactions
-------------------------------------------------------------------------------}

instance TxGen DualByronBlock where
  testGenTxs _numCoreNodes curSlotNo cfg () = \st -> do
      n <- generateBetween 0 20
      go [] n $ applyChainTick (configLedger cfg) curSlotNo st
    where
      -- Attempt to produce @n@ transactions
      -- Stops when the transaction generator cannot produce more txs
      go :: MonadRandom m
         => [GenTx DualByronBlock]     -- Accumulator
         -> Integer                    -- Number of txs to still produce
         -> TickedLedgerState DualByronBlock
         -> m [GenTx DualByronBlock]
      go acc 0 _  = return (reverse acc)
      go acc n st = do
          mTx <- hedgehogAdapter $ genTx cfg (tickedLedgerState st)
          case mTx of
            Nothing -> return (reverse acc)
            Just tx ->
              case runExcept $ applyTx
                                 (configLedger cfg)
                                 tx
                                 st of
                Right st' -> go (tx:acc) (n - 1) st'
                Left _    -> error "testGenTxs: unexpected invalid tx"

-- | Generate transaction
--
-- For now we only generate regular transactions. Generating delegation
-- certificates and update proposals/votes is out of the scope of this test,
-- for now. Extending the scope will require integration with the restart/rekey
-- infrastructure of the RealPBFT tests.
genTx :: TopLevelConfig DualByronBlock
      -> LedgerState DualByronBlock
      -> Hedgehog.Gen (GenTx DualByronBlock)
genTx cfg st = HH.choice [
      do aux <- sigGen (Rules.ctxtUTXOW cfg') st'
         let main :: Impl.ATxAux ByteString
             main = Spec.Test.elaborateTxBS
                      elaborateTxId
                      aux

         return $ DualGenTx {
             dualGenTxMain   = ByronTx (byronIdTx main) main
           , dualGenTxAux    = ByronSpecGenTx $ ByronSpecGenTxTx aux
           , dualGenTxBridge = specToImplTx aux main
           }
    ]
  where
    cfg' :: ByronSpecGenesis
    st'  :: Spec.State Spec.CHAIN

    cfg' = dualLedgerConfigAux (configLedger cfg)
    st'  = byronSpecLedgerState $ dualLedgerStateAux st

    bridge :: ByronSpecBridge
    bridge = dualLedgerStateBridge st

    elaborateTxId :: Spec.TxId -> Impl.TxId
    elaborateTxId tid =
        case Map.lookup tid (bridgeTransactionIds bridge) of
          Nothing   -> error $ "elaborateTxId: unknown tx ID " ++ show tid
          Just tid' -> tid'

sigGen :: forall sts. (Spec.QC.HasTrace sts)
       => Rules.RuleContext sts
       -> Spec.State Spec.CHAIN
       -> Hedgehog.Gen (Spec.Signal sts)
sigGen Rules.RuleContext{..} st =
    Spec.QC.sigGen @sts (getRuleEnv st) (getRuleState st)

{-------------------------------------------------------------------------------
  Hedgehog to MonadRandom adapter
-------------------------------------------------------------------------------}

-- | Run the generator by producing a random seed
--
-- If the generator fails to produce a value, try again with a different seed;
-- if this fails too often, return 'Nothing'.
hedgehogAdapter :: forall m a. MonadRandom m => Hedgehog.Gen a -> m (Maybe a)
hedgehogAdapter gen =
    go 2 -- We only try twice right now, as the tests are already very slow
  where
    go :: Int -> m (Maybe a)
    go 0 = return Nothing
    go n = do
      seed <- genSeed
      case HH.evalGen (HH.Size 30) seed gen of
        Nothing -> go (n - 1)
        Just ta -> return $ Just (HH.treeValue ta)

    genSeed :: m Hedgehog.Seed
    genSeed = do
      a <- fromInteger <$> Cryptonite.generateBetween mn mx
      b <- fromInteger <$> Cryptonite.generateBetween mn mx
      return $ Hedgehog.Seed a (if even b then succ b else b)

    mn, mx :: Integer
    mn = toInteger (minBound :: Word64)
    mx = toInteger (maxBound :: Word64)
