{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Test.ThreadNet.Cardano (
    tests
  ) where

import           Control.Exception (assert)
import           Control.Monad (guard, replicateM)
import           Control.Monad.Identity (Identity (runIdentity))
import           Data.Functor ((<&>))
import qualified Data.Map as Map
import           Data.Maybe (isJust, maybeToList)
import           Data.Proxy (Proxy (..))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word (Word64)
import           GHC.Generics (Generic)

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Cardano.Slotting.EpochInfo
import           Cardano.Slotting.Slot (EpochNo (..), EpochSize (..),
                     SlotNo (..))

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config.SecurityParam
import qualified Ouroboros.Consensus.HardFork.History.Util as Util
import           Ouroboros.Consensus.Ledger.SupportsMempool (extractTxs)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.PBFT
import           Ouroboros.Consensus.Util.IOLike (IOLike)

import           Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common
                     (isHardForkNodeToNodeEnabled)

import qualified Cardano.Chain.Common as CC.Common
import qualified Cardano.Chain.Genesis as CC.Genesis
import           Cardano.Chain.ProtocolConstants (kEpochSlots)
import           Cardano.Chain.Slotting (unEpochSlots)
import qualified Cardano.Chain.Update as CC.Update

import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import           Ouroboros.Consensus.Byron.Ledger.Conversions
import           Ouroboros.Consensus.Byron.Node

import qualified Shelley.Spec.Ledger.API as SL
import qualified Shelley.Spec.Ledger.BaseTypes as SL (ActiveSlotCoeff,
                     mkNonceFromNumber)
import qualified Shelley.Spec.Ledger.OverlaySchedule as SL (overlaySlots)

import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Node

import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.Condense ()
import           Ouroboros.Consensus.Cardano.Node

import           Test.Consensus.Cardano.MockCrypto (MockCryptoCompatByron)
import           Test.ThreadNet.General
import qualified Test.ThreadNet.Infra.Byron as Byron
import qualified Test.ThreadNet.Infra.Shelley as Shelley
import           Test.ThreadNet.Network (CalcMessageDelay (..), NodeOutput (..),
                     TestNodeInitialization (..))
import           Test.ThreadNet.TxGen.Cardano (CardanoTxGenExtra (..))
import           Test.ThreadNet.Util.Expectations (NumBlocks (..))
import           Test.ThreadNet.Util.NodeJoinPlan (trivialNodeJoinPlan)
import           Test.ThreadNet.Util.NodeRestarts (noRestarts)
import           Test.ThreadNet.Util.NodeToNodeVersion (genVersionFiltered)
import qualified Test.ThreadNet.Util.NodeTopology as Topo
import           Test.ThreadNet.Util.Seed (runGen)
import qualified Test.Util.BoolProps as BoolProps
import           Test.Util.HardFork.Future
import           Test.Util.Nightly
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Slots (NumSlots (..))

-- | Use 'MockCryptoCompatByron' so that bootstrap addresses and
-- bootstrap witnesses are supported.
type Crypto = MockCryptoCompatByron

-- | When and for how long the nodes are partitioned
--
-- The nodes are divided via message delays into two sub-networks by the parity
-- of their 'CoreNodeId'.
data Partition = Partition SlotNo NumSlots
  -- ^ the scheduled start slot and duration (which includes the start slot)
  deriving (Show)

partitionExclusiveUpperBound :: Partition -> SlotNo
partitionExclusiveUpperBound (Partition s (NumSlots dur)) =
    Util.addSlots dur s

-- | The varying data of this test
--
-- Note: The Byron nodes in this test all join, propose an update, vote for it,
-- and endorse it literally as soon as possible. Therefore, if the test reaches
-- the end of the first epoch, the proposal will be adopted.
data TestSetup = TestSetup
  { setupByronLowerBound   :: Bool
    -- ^ whether to use the @HardFork.LowerBound@ optimization
  , setupD                 :: Shelley.DecentralizationParam
  , setupHardFork          :: Bool
    -- ^ whether the proposal should trigger a hard fork or not
  , setupInitialNonce      :: SL.Nonce
    -- ^ the initial Shelley 'SL.ticknStateEpochNonce'
    --
    -- We vary it to ensure we explore different leader schedules.
  , setupK                 :: SecurityParam
  , setupPartition         :: Partition
  , setupSlotLengthByron   :: SlotLength
  , setupSlotLengthShelley :: SlotLength
  , setupTestConfig        :: TestConfig
  , setupVersion           :: (NodeToNodeVersion, BlockNodeToNodeVersion (CardanoBlock Crypto))
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

    setupInitialNonce <- frequency
      [ (1, pure SL.NeutralNonce)
      , (9, SL.mkNonceFromNumber <$> arbitrary)
      ]

    setupSlotLengthByron   <- arbitrary
    setupSlotLengthShelley <- arbitrary

    setupTestConfig                       <- genTestConfig setupK
    let TestConfig{numCoreNodes, numSlots} = setupTestConfig

    setupByronLowerBound <- arbitrary
    setupHardFork        <- frequency [(49, pure True), (1, pure False)]
    setupPartition       <- genPartition numCoreNodes numSlots setupK

    setupVersion         <- genVersionFiltered
                              isHardForkNodeToNodeEnabled
                              (Proxy @(CardanoBlock Crypto))

    pure TestSetup
      { setupByronLowerBound
      , setupD
      , setupHardFork
      , setupInitialNonce
      , setupK
      , setupPartition
      , setupSlotLengthByron
      , setupSlotLengthShelley
      , setupTestConfig
      , setupVersion
      }

  -- TODO shrink

-- | Generate 'setupTestConfig'
genTestConfig :: SecurityParam -> Gen TestConfig
genTestConfig k = do
    initSeed <- arbitrary

    numSlots <- do
      let byronE   = byronEpochSize k
          shelleyE = shelleyEpochSize k
          wiggle   = min byronE (2 * maxRollbacks k)

          approachShelleyEra     =
              choose (0, wiggle)   <&> \t -> byronE + t - wiggle
          reachShelleyEra        =
              choose (1, shelleyE) <&> \t -> byronE + t
          reachThirdShelleyEpoch =
              choose (1, shelleyE) <&> \t -> byronE + 2 * shelleyE + t

      fmap NumSlots $ frequency $
        [ (05, approachShelleyEra)
        , (64, reachShelleyEra)
        , (31, reachThirdShelleyEpoch)
        ]

    -- This test has more slots than most, so we de-emphasize the relatively
    -- expensive n=5 case. For example:
    --
    -- > 250 tests with 30% 2, 30% 3, 30% 4, and 10% 5 took 500s
    -- > 250 tests with 100% 5 took 1000s
    ncn <- frequency [(9, choose (2, 4)), (1, pure 5)]

    -- Ensure that each partition class is internally connected.
    nodeTopology <- do
      topo0   <- Topo.genNodeTopology $ NumCoreNodes ncn
      oddTopo <- do
        -- eg nodes 1 3 for ncn = 5
        topo <- Topo.genNodeTopology $ NumCoreNodes $ div ncn 2
        let rename (CoreNodeId i) = CoreNodeId (2 * i + 1)
        pure $ Topo.mapNodeTopology rename topo
      evenTopo <- do
        -- eg nodes 0 2 4 for ncn = 5
        topo <- Topo.genNodeTopology $ NumCoreNodes $ ncn - div ncn 2
        let rename (CoreNodeId i) = CoreNodeId (2 * i)
        pure $ Topo.mapNodeTopology rename topo
      pure $
        Topo.unionNodeTopology evenTopo $
        Topo.unionNodeTopology oddTopo $
        topo0

    pure TestConfig
      { initSeed
      , nodeTopology
      , numCoreNodes = NumCoreNodes ncn
      , numSlots
      }

-- | Generate 'setupPartition'
genPartition :: NumCoreNodes -> NumSlots -> SecurityParam -> Gen Partition
genPartition (NumCoreNodes n) (NumSlots t) (SecurityParam k) = do
    let ultimateSlot :: Word64
        ultimateSlot = assert (t > 0) $ t - 1

        crop :: Word64 -> Word64
        crop s = min ultimateSlot s

    -- Fundamental plan: all @MsgRollForward@ sent during the partition only
    -- arrive at the onset of slot after it. The partition begins at the onset
    -- of @firstSlotIn@ and ends at the onset of @firstSlotAfter@.

    -- FACT (A) The leader of @firstSlotAfter@ will forge before fully reacting
    -- to the @MsgRollForward@s that just arrived, due to a race in the test
    -- infrastructure that is consistently won by the forge. Thus the two
    -- class's unique extensions consist of the blocks forged in the slots from
    -- @firstSlotIn@ to @firstSlotAfter@ /inclusive/.

    -- @w@ is defined below this large comment to be how long the partition
    -- should last (this may be 'crop'ped farther below if the partition ends
    -- up near the end of the test)
    --
    -- Because of FACT (A), we limit the partition duration so that at least
    -- one of the two partition classes will forge /strictly/ /less/ /than/ @k@
    -- such blocks. While both classes would be able to rollback if we let them
    -- forge @k@ such blocks, they might (TODO "might" or "will"?) not able to
    -- /decide/ that they need to rollback, since the relevant ChainSync client
    -- might (TODO will?) not be able to forecast a ledger view far-enough into
    -- the future to validate the @k+1@st header from the other class after the
    -- partition ends. It will remain unaware that a longer chain exists and
    -- end up stuck on its own distinct chain. Hence it's a Common Prefix
    -- violation.
    --
    -- We therefore motivate an upper bound on the partition duration @w@ for a
    -- net with @n@ nodes by considering the two variables' parities.
    --
    -- o For @n = 2h@ nodes and @w = 2u@ slots, the classes respectively forge
    --   @u@ and @u+1@ blocks. (The @+1@th block is forged in
    --   @firstSlotAfter@.) The preceding paragraph requires @u < k@ requires
    --   @u <= k-1@ requires @w <= 2k-2@. So @w <= 2k-2@ when @w@ is even.
    --
    -- o For @n = 2h@ nodes and @w = 2u+1@ slots, the classes both forge @u+1@
    --   blocks. (The second @+1@th block is forged in @firstSlotAfter@.) The
    --   preceding paragraph requires @u+1 < k@ requires @u <= k-2@ requires @w
    --   <= 2k-3@. So @w <= 2k-3@ when @w@ is odd. Note that @w <= 2k-2@ is
    --   equivalent since @w@ is assumed odd.
    --
    -- o For @n = 2h+1@ nodes, the smaller class forges at most the number of
    --   blocks considered in the above cases for even @n@, so this case
    --   doesn't contribute anything novel to the upper bound. The smaller
    --   class has less than half of the nodes and so will violate Chain Growth
    --   /if/ /given/ /enough/ /time/ /alone/. But for Byron at least, the
    --   logic for avoiding a Common Prefix violation already establishes a
    --   sufficiently strict upper bound to preclude a Chain Growth violation,
    --   since Chain Growth isn't technically violated until @2k@ slots have
    --   passed (it's also relevant that the net forges at full round-robin
    --   speed until the partition starts). (TODO does that cover Shelley too?)
    --
    -- Thus @w@ can range from @0@ to @2k-2@ inclusive.
    w <- assert (k > 0) $ choose (0, 2 * k - 2)

    -- Each of the the following milestones happens at the listed slot in the
    -- absence of a partition.
    --
    -- o In slots 0 and 1 the proposal and the votes confirm it
    --
    -- o In slot 2k+1 the confirmation becomes stable
    --
    -- o In slot 2k+1+quorum the nodes endorse it
    --
    -- o In slot 4k+1+quorum the endorsement is stable
    --
    -- o In slot 10k the update is adopted.
    --
    -- We are most interested in what happens when the partition is near these
    -- milestones.
    mbFocus <- do
      let quorum = div n 2   -- in the right ballpark
      frequency $
        map (\(wgt, mbDur) -> (wgt, pure mbDur)) $
        filter (maybe True (< t) . snd)
          [ (5,  Nothing)
          , (1,  Just 0)
          , (1,  Just $ 2 * k + 1)
          , (1,  Just $ 2 * k + 1 + quorum)
          , (1,  Just $ 4 * k + 1)
          , (1,  Just $ 4 * k + 1 + quorum)
          , (20, assert (numByronEpochs == (1 :: Int)) $
                 Just $ byronEpochSize (SecurityParam k))
          ]

    -- Position the partition so that it at least abuts the focus slot.
    firstSlotIn <- choose $
      case mbFocus of
        Nothing    -> (0,                            ultimateSlot    )
        Just focus -> (crop $ focus `monus` (w + 1), crop $ focus + 1)

    let -- Because of FACT (A), we require there to be at least one slot after
        -- the partition. This doesn't ensure full consensus, because the block
        -- forged in @firstSlotAfter@ may create a chain as long as that of the
        -- other partition (Byron) or even multiple such chains (Shelley). But
        -- it does ensure that all final chains will be the same length.
        firstSlotAfter :: Word64
        firstSlotAfter = crop $ firstSlotIn + w

        dur :: Word64
        dur = Util.countSlots (SlotNo firstSlotAfter) (SlotNo firstSlotIn)

    pure $ Partition (SlotNo firstSlotIn) (NumSlots dur)

-- | Run relatively fewer tests
--
-- These tests are slow, so we settle for running fewer of them in this test
-- suite since it is invoked frequently (eg CI for each push).
twoFifthsTestCount :: QuickCheckTests -> QuickCheckTests
twoFifthsTestCount (QuickCheckTests n) = QuickCheckTests $
    if 0 == n then 0 else
    max 1 $ (2 * n) `div` 5

tests :: TestTree
tests = testGroup "Cardano ThreadNet" $
    [ let name = "simple convergence" in
      askIohkNightlyEnabled $ \enabled ->
      if enabled
      then testProperty name $ \setup ->
             prop_simple_cardano_convergence setup
      else adjustOption twoFifthsTestCount $
           testProperty name $ \setup ->
             prop_simple_cardano_convergence setup
    ]

prop_simple_cardano_convergence :: TestSetup -> Property
prop_simple_cardano_convergence TestSetup
  { setupByronLowerBound
  , setupD
  , setupHardFork
  , setupInitialNonce
  , setupK
  , setupPartition
  , setupSlotLengthByron
  , setupSlotLengthShelley
  , setupTestConfig
  , setupVersion
  } =
    prop_general_semisync pga testOutput .&&.
    prop_inSync testOutput .&&.
    prop_ReachesShelley reachesShelley .&&.
    prop_noCPViolation .&&.
    ( tabulate "ReachesShelley label" [label_ReachesShelley reachesShelley] $
      tabulate "Observed forge during a non-overlay Shelley slot"
        [label_hadActiveNonOverlaySlots] $
      tabulatePartitionDuration $
      tabulateFinalIntersectionDepth $
      tabulatePartitionPosition $
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
        , pgaFirstBlockNo        = 1
        , pgaFixedMaxForkLength  = Just maxForkLength
        , pgaFixedSchedule       =
            -- the leader schedule isn't fixed because the Shelley leader
            -- schedule is (at least ideally) unpredictable
            Nothing
        , pgaSecurityParam       = setupK
        , pgaTestConfig          = setupTestConfig
        , pgaTestConfigB         = testConfigB
        }

    testConfigB = TestConfigB
      { forgeEbbEnv  = Nothing
      , future       =
          if setupHardFork
          then
          -- In this case the PVU will trigger the transition to Shelley.
          --
          -- By FACT (B), the PVU is always successful if we reach the second
          -- era.
          EraCons  setupSlotLengthByron   epochSizeByron   eraSizeByron $
          EraFinal setupSlotLengthShelley epochSizeShelley
          else
          EraFinal setupSlotLengthByron   epochSizeByron
      , messageDelay = mkMessageDelay setupPartition
      , nodeJoinPlan = trivialNodeJoinPlan numCoreNodes
      , nodeRestarts = noRestarts
      , txGenExtra   = CardanoTxGenExtra
        { ctgeByronGenesisKeys = generatedSecrets
        , ctgeNetworkMagic     =
            CC.Common.makeNetworkMagic $
            CC.Genesis.configProtocolMagic genesisByron
        , ctgeShelleyCoreNodes = coreNodes
        }
      , version      = setupVersion
      }

    testOutput :: TestOutput (CardanoBlock Crypto)
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
                  setupInitialNonce
                  (coreNodes !! fromIntegral nid)
                  ProtocolParamsTransition {
                      transitionLowerBound =
                        guard setupByronLowerBound *> Just numByronEpochs
                    , transitionTrigger = TriggerHardForkAtVersion shelleyMajorVersion
                    }
            , mkRekeyM = Nothing
            }

    maxForkLength :: NumBlocks
    maxForkLength = NumBlocks $
        if rsShelleyBlocks reachesShelley
        then
          -- Shelley inherently creates small forks, but we haven't yet seen a
          -- Common Prefix violation in this test even though @k@ is small
          --
          -- TODO I'd value having a repro that demonstrates a violation of
          -- this typical limit, so I'm leaving it in for now. If it never
          -- fails, we should figure out why not. Even with @k=2 ncn=5 d=0.1@
          -- fixed the deepest fork I'm seeing is ~2.5% @k-1@
          -- 'finalIntersectionDepth'.
          maxRollbacks setupK
        else
          -- Recall that all nodes join ASAP, so the partition is the only
          -- potential cause for a fork during Byron. See the reasoning in
          -- 'genPartition' for the motivation of this limit.
          div partitionDuration 2 + mod partitionDuration 2

    partitionDuration :: Word64
    partitionDuration = dur
      where
        Partition _ (NumSlots dur) = setupPartition

    -- Byron

    pbftParams :: PBftParams
    pbftParams = Byron.byronPBftParams setupK numCoreNodes

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

    coreNodes :: [Shelley.CoreNode Crypto]
    coreNodes = runGen initSeed $
        replicateM (fromIntegral n) $
          Shelley.genCoreNode initialKESPeriod
      where
        NumCoreNodes n = numCoreNodes

    genesisShelley :: ShelleyGenesis (ShelleyEra Crypto)
    genesisShelley =
        Shelley.mkGenesisConfig
          (SL.ProtVer shelleyMajorVersion 0)
          setupK
          activeSlotCoeff
          setupD
          setupSlotLengthShelley
          (Shelley.mkKesConfig (Proxy @Crypto) numSlots)
          coreNodes

    -- the Shelley ledger is designed to use a fixed epoch size, so this test
    -- does not randomize it
    epochSizeShelley :: EpochSize
    epochSizeShelley = sgEpochLength genesisShelley

    -- the protocol version of the Byron era proposal
    --
    -- FACT (B) This proposal is always adopted at the first epoch boundary.
    --
    -- o 'genTestConfig' ensures the test reaches the epoch boundary unless
    --   there's a fatal error during execution. Specifically, 'rsByronSlots'
    --   will always be 'Enabled'.
    --
    -- o 'genPartition' limits the partition duration to at most @2k-2@ slots.
    --   This leaves at least @10k - (2k-2) = 8k+2@ slots in the epoch
    --   unaffected by the partition. Moreover, the blocks forged during the
    --   partition do make some progress, even though it's not full progress.
    --   So @8k+2@ is conservative.
    --
    -- o As " crucial transactions ", the proposal and vote are added to the
    --   chain eventually and ASAP, even if they are initially placed on the
    --   losing partition class's chain.
    --
    -- o Thus, within the first two of the @8k+2@ unaffected slots, the
    --   proposal has been confirmed. Similar reasoning ensures that it is then
    --   stably confirmed, endorsed, and stably endorsed, before the epoch
    --   boundary and @SafeZone@. IE @2 + 2k + q + 2k + 2k < 8k+2@, since the
    --   quorum @q@ is ~60% of 'numCoreNodes' and so @q < 2k@, since
    --   'numCoreNodes' is at most 5 and @k@ is at least @2@. (Also recall that
    --   @8k+2@ is conservative.)
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

    -- Classifying test cases

    reachesShelley :: ReachesShelley
    reachesShelley = ReachesShelley
      { rsByronSlots    =
          BoolProps.enabledIf $ t > numByronSlots
      , rsPV            = BoolProps.enabledIf setupHardFork
      , rsShelleyBlocks =
          or $
          [ isShelley blk
          | (_nid, no) <- Map.toList testOutputNodes
          , let NodeOutput{nodeOutputForges} = no
          , (blk, _m) <- maybeToList $ Map.maxView nodeOutputForges
                -- the last block the node forged
          ]
      , rsShelleySlots  =
          assert (w >= k) $
          BoolProps.requiredIf $
          -- The active slots in the first two Shelley epochs are all overlay
          -- slots, so the first Shelley block will arise from one of those.
          not $ Set.null overlaySlots
      }
      where
        NumSlots t                  = numSlots
        TestOutput{testOutputNodes} = testOutput

        k :: Word64
        k = maxRollbacks setupK

        coeff :: SL.ActiveSlotCoeff
        coeff = SL.sgActiveSlotCoeff genesisShelley

        w :: Word64
        w = SL.computeStabilityWindow k coeff

    -- Whether there was a Shelley block forged in a non-overlay slot.
    --
    -- This event evidences that the stake pools were correctly created and
    -- delegated to.
    label_hadActiveNonOverlaySlots :: String
    label_hadActiveNonOverlaySlots =
        show $ or $
        [ Set.notMember slot overlaySlots
        | (_nid, no) <- Map.toList testOutputNodes
        , let NodeOutput{nodeOutputForges} = no
        , (slot, blk) <- Map.toDescList nodeOutputForges
        , isShelley blk
        ]
      where
        TestOutput{testOutputNodes} = testOutput

    -- All OBFT overlay slots in the test.
    overlaySlots :: Set SlotNo
    overlaySlots =
        Set.filter (< SlotNo t) $
        Set.unions $
        takeWhile (isJust . Set.lookupLT (SlotNo t)) $
        map overlayOffsets [0..]
      where
        NumSlots t = numSlots

        -- The overlay slots in the ith Shelley epoch.
        --
        -- TODO: This function conceptually should be simpler if we created a
        -- sufficiently accurate 'EpochInfo' and so didn't need the shift. But
        -- doing so (eg by constructing a HardFork @Summary@) is currently
        -- significantly more involved than this workaround.
        overlayOffsets :: Word64 -> Set SlotNo
        overlayOffsets i =
            -- Shift to account for the initial Byron era.
            Set.mapMonotonic (Util.addSlots numByronSlots) .
            Set.fromList $
            -- Note: this is only correct if each epoch uses the same value for @d@
            SL.overlaySlots
              (runIdentity $ epochInfoFirst epochInfo (EpochNo i))
              -- notably contains setupD
              (SL._d (sgProtocolParams genesisShelley))
                -- NB 0 <-> the first /Shelley/ epoch
              (runIdentity $ epochInfoSize epochInfo (EpochNo i))

    -- Suitable only for this narrow context
    epochInfo :: EpochInfo Identity
    epochInfo = fixedSizeEpochInfo epochSizeShelley

    numByronSlots :: Word64
    numByronSlots = numByronEpochs * unEpochSize epochSizeByron

    finalBlockEra :: String
    finalBlockEra =
        if rsShelleyBlocks reachesShelley then "Shelley" else "Byron"

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

    tabulatePartitionDuration :: Property -> Property
    tabulatePartitionDuration =
        tabul "count"  (show               partitionDuration) .
        tabul "k frac" (approxFracK setupK partitionDuration)
      where
        tabul s x =
            tabulate ("partition duration, " <> s) [x <> " slots"]

    tabulateFinalIntersectionDepth :: Property -> Property
    tabulateFinalIntersectionDepth =
        tabul "count"  (show               finalIntersectionDepth) .
        tabul "k frac" (approxFracK setupK finalIntersectionDepth) .
        tabul "k diff" (diffK       setupK finalIntersectionDepth)
      where
        lbl = "final intersection depth"
        tabul s x = tabulate
            (lbl <> ", " <> finalBlockEra <> ", " <> s)
            [x <> " blocks"]

    tabulatePartitionPosition :: Property -> Property
    tabulatePartitionPosition =
        tabulate "partition in or abuts era (Byron, Shelley)"
          [ show (inclByron, inclShelley) ]
      where
        Partition (SlotNo firstSlotIn) (NumSlots dur) = setupPartition
        firstSlotAfter                                = firstSlotIn + dur

        trans = ledgerReachesShelley reachesShelley

        inclByron   =
            dur > 0 && (not trans || firstSlotIn    <= numByronSlots)
        inclShelley =
            dur > 0 && (trans     && firstSlotAfter >= numByronSlots)

mkProtocolCardanoAndHardForkTxs
  :: forall c m. (IOLike m, CardanoHardForkConstraints c)
     -- Byron
  => PBftParams
  -> CoreNodeId
  -> CC.Genesis.Config
  -> CC.Genesis.GeneratedSecrets
  -> CC.Update.ProtocolVersion
     -- Shelley
  -> ShelleyGenesis (ShelleyEra c)
  -> SL.Nonce
  -> Shelley.CoreNode c
     -- HardForks
  -> ProtocolParamsTransition
       ByronBlock
       (ShelleyBlock (ShelleyEra c))
  -> TestNodeInitialization m (CardanoBlock c)
mkProtocolCardanoAndHardForkTxs
    pbftParams coreNodeId genesisByron generatedSecretsByron propPV
    genesisShelley initialNonce coreNodeShelley
    protocolParamsByronShelley =
    TestNodeInitialization
      { tniCrucialTxs   = crucialTxs
      , tniProtocolInfo = pInfo
      }
  where
    crucialTxs :: [GenTx (CardanoBlock c)]
    crucialTxs =
        GenTxByron <$> tniCrucialTxs tniByron
      where
        -- reuse the Byron logic for generating the crucial txs, ie the
        -- proposal and votes
        tniByron :: TestNodeInitialization m ByronBlock
        tniByron =
            Byron.mkProtocolByronAndHardForkTxs
              pbftParams
              coreNodeId
              genesisByron
              generatedSecretsByron
              propPV

    pInfo :: ProtocolInfo m (CardanoBlock c)
    pInfo = protocolInfoCardano
        ProtocolParamsByron {
            byronGenesis                = genesisByron
            -- Trivialize the PBFT signature window so that the forks induced by
            -- the network partition are as deep as possible.
          , byronPbftSignatureThreshold = Just $ PBftSignatureThreshold 1
          , byronProtocolVersion        = propPV
          , byronSoftwareVersion        = softVerByron
          , byronLeaderCredentials      = Just leaderCredentialsByron
          }
        ProtocolParamsShelley {
            shelleyGenesis           = genesisShelley
          , shelleyInitialNonce      = initialNonce
          , shelleyProtVer           = SL.ProtVer shelleyMajorVersion 0
          , shelleyLeaderCredentials = Just leaderCredentialsShelley
          }
        ProtocolParamsAllegra {
            allegraProtVer           = SL.ProtVer allegraMajorVersion 0
          , allegraLeaderCredentials = Nothing
          }
        ProtocolParamsMary {
            maryProtVer           = SL.ProtVer maryMajorVersion 0
          , maryLeaderCredentials = Nothing
          }
        protocolParamsByronShelley
        ProtocolParamsTransition {
            transitionLowerBound = Nothing
          , transitionTrigger    = TriggerHardForkAtVersion allegraMajorVersion
          }
        ProtocolParamsTransition {
            transitionLowerBound = Nothing
          , transitionTrigger    = TriggerHardForkAtVersion maryMajorVersion
          }

    -- Byron

    leaderCredentialsByron :: ByronLeaderCredentials
    leaderCredentialsByron =
        Byron.mkLeaderCredentials
          genesisByron
          generatedSecretsByron
          coreNodeId

    -- this sets a vestigial header field which is not actually used for anything
    softVerByron :: CC.Update.SoftwareVersion
    softVerByron = Byron.theProposedSoftwareVersion

    -- Shelley

    leaderCredentialsShelley :: TPraosLeaderCredentials c
    leaderCredentialsShelley = Shelley.mkLeaderCredentials coreNodeShelley

{-------------------------------------------------------------------------------
  Constants
-------------------------------------------------------------------------------}

-- | The active slot coefficient, @f@.
--
-- Some of these tests includes Shelley epochs in which stakepools are actually
-- leading. In that case, the @k@, @d@, and @f@ parameters and the length of
-- any scheduled network partitions need to be balanced so that Common Prefix
-- violations (in particular, wedges) are extremely unlikely.
activeSlotCoeff :: Rational
activeSlotCoeff = 0.2   -- c.f. mainnet is more conservative, using 0.05

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

-- | The major protocol version of Allegra in this test
--
-- See 'byronMajorVersion'
allegraMajorVersion :: Num a => a
allegraMajorVersion = shelleyMajorVersion + 1

-- | The major protocol version of Mary in this test
--
-- See 'byronMajorVersion'
maryMajorVersion :: Num a => a
maryMajorVersion = allegraMajorVersion + 1

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

-- | Whether the test included Shelley blocks and (pre)reqs relevant to that
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
    -- incur a GHC warning if the number of fields changes
    ReachesShelley _ _ _ _dummy = reachesShelley
    -- this pattern should bind each field by name
    ReachesShelley
      { rsByronSlots
      , rsPV
      , rsShelleyBlocks
      , rsShelleySlots
      } = reachesShelley

    prepend :: Show a => String -> a -> String -> String
    prepend s req x = s <> " " <> show req <> ", " <> x

-- | Is the update proposal adopted?
ledgerReachesShelley :: ReachesShelley -> Bool
ledgerReachesShelley rs = BoolProps.checkReqs rs /= Just False

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

{-------------------------------------------------------------------------------
  A short even-odd partition
-------------------------------------------------------------------------------}

-- | The temporary partition as a 'CalcMessageDelay'
--
-- Calculates the delays that implement 'setupPartition'.
mkMessageDelay :: Partition -> CalcMessageDelay blk
mkMessageDelay part = CalcMessageDelay $
    \(CoreNodeId i, CoreNodeId j) curSlot _hdr -> NumSlots $ if
      | curSlot <  firstSlotIn    -> 0
      | curSlot >= firstSlotAfter -> 0
      | mod i 2 == mod j 2        -> 0
      | otherwise                 -> unSlotNo $ firstSlotAfter - curSlot
  where
    Partition firstSlotIn _ = part
    firstSlotAfter          = partitionExclusiveUpperBound part

{-------------------------------------------------------------------------------
  Miscellany
-------------------------------------------------------------------------------}

byronEpochSize :: SecurityParam -> Word64
byronEpochSize (SecurityParam k) =
    unEpochSlots $ kEpochSlots $ CC.Common.BlockCount k

shelleyEpochSize :: SecurityParam -> Word64
shelleyEpochSize k = unEpochSize $ Shelley.mkEpochSize k activeSlotCoeff

-- | Return 'True' when the block is a block from the Shelley era.
--
-- Note it will return 'False' for other Shelley-flavoured eras like Allegra and
-- Mary.
isShelley :: CardanoBlock c -> Bool
isShelley = \case
    BlockShelley{} -> True
    _              -> False

-- | Render a number as a positive difference from @k@
--
-- PREREQUISITE: The number must not be greater than @k@.
diffK :: SecurityParam -> Word64 -> String
diffK (SecurityParam k) v =
    assert (k >= v) $
    "k - " <> show (k - v)

-- | Render a number as the nearest tenths of @k@
approxFracK :: SecurityParam -> Word64 -> String
approxFracK (SecurityParam k) v =
    "k * " <> show (fromIntegral tenths / 10 :: Double)
  where
    ratio  = toRational v / toRational k
    tenths = round (ratio * 10) :: Int

-- | <https://en.wikipedia.org/wiki/Monus>
monus :: (Num a, Ord a) => a -> a -> a
monus a b = if a <= b then 0 else a - b
