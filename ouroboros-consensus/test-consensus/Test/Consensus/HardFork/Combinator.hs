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

import           Codec.Serialise
import qualified Data.ByteString.Lazy as Lazy
import           Data.Coerce
import qualified Data.Map as Map
import           Data.SOP.BasicFunctors
import           Data.SOP.Strict hiding (shape)
import           Data.Type.Equality
import           Data.Void
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
import           Ouroboros.Consensus.Node.Serialisation
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.LeaderSchedule
                     (LeaderSchedule (..), leaderScheduleFor)
import           Ouroboros.Consensus.Storage.ChainDB.Serialisation
import           Ouroboros.Consensus.Storage.Common
import           Ouroboros.Consensus.Storage.ImmutableDB (simpleChunkInfo)
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util ((.:))
import           Ouroboros.Consensus.Util.Counting
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.Random (Seed (..))
import           Ouroboros.Consensus.Util.SOP

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Condense ()
import qualified Ouroboros.Consensus.HardFork.Combinator.Serialisation as Default
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs
                     (RequiringBoth (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Match as Match
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
          epochSize    = testSetupEpochSize
        , forgeEbbEnv  = Nothing
        , nodeJoinPlan = trivialNodeJoinPlan numCoreNodes
        , nodeRestarts = noRestarts
        , slotLength   = testSetupSlotLength
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
            , hardForkConsensusConfigPerEra =
                toNP (Proxy @WrapPartialConsensusConfig)
                  (WrapPartialConsensusConfig $ consensusConfigA nid)
                  (WrapPartialConsensusConfig $ consensusConfigB nid)
            }
        , configLedger = HardForkLedgerConfig {
              hardForkLedgerConfigK      = k
            , hardForkLedgerConfigShape  = shape
            , hardForkLedgerConfigPerEra =
                toNP (Proxy @WrapPartialLedgerConfig)
                  (WrapPartialLedgerConfig $ ledgerConfigA nid)
                  (WrapPartialLedgerConfig $ ledgerConfigB nid)
            }
        , configBlock = HardForkBlockConfig {
              hardForkBlockConfigPerEra =
                toNP (Proxy @BlockConfig)
                  (blockConfigA nid)
                  (blockConfigB nid)
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

instance HasNetworkProtocolVersion TestBlock where

instance TranslateNetworkProtocolVersion TestBlock where
  nodeToNodeProtocolVersion   _ _ = NodeToNodeV_1
  nodeToClientProtocolVersion _ _ = NodeToClientV_2

instance RunNode TestBlock where
  nodeBlockFetchSize        = const 0
  nodeCheckIntegrity        = \_ _ -> True

  nodeImmDbChunkInfo        = simpleChunkInfo
                            . eraEpochSize
                            . unK . hd
                            . History.getShape
                            . hardForkLedgerConfigShape
                            . configLedger
  nodeGetBinaryBlockInfo    = binaryBlockInfo

binaryBlockInfo :: TestBlock -> BinaryBlockInfo
binaryBlockInfo =
    Default.binaryBlockInfo
       $ fn (mapIK binaryBlockInfoA)
      :* fn (mapIK binaryBlockInfoB)
      :* Nil

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

deriving via Default.SerialiseOne I '[BlockA, BlockB]
         instance Serialise TestBlock

deriving via Default.SerialiseOne Header '[BlockA, BlockB]
         instance Serialise (Header TestBlock)

deriving via Default.SerialiseOne WrapTipInfo '[BlockA, BlockB]
         instance Serialise (OneEraTipInfo '[BlockA, BlockB])

-- NOTE: Perhaps interestingly, the hard fork's TipInfo _can_ distinguish
-- between the eras, because it's an NS of the TipInfos of the underlying
-- eras. Not sure if that's helpful/harmful/neither
deriving instance Serialise (AnnTip TestBlock)

instance EncodeDiskDepIx (NestedCtxt Header) TestBlock where
  encodeDiskDepIx = Default.encodeNestedCtxt

instance EncodeDiskDep (NestedCtxt Header) TestBlock where
  encodeDiskDep = Default.encodeNested

instance DecodeDiskDepIx (NestedCtxt Header) TestBlock where
  decodeDiskDepIx = Default.decodeNestedCtxt

instance DecodeDiskDep (NestedCtxt Header) TestBlock where
  decodeDiskDep = Default.decodeNested

instance DecodeDisk TestBlock (Header TestBlock)

instance ImmDbSerialiseConstraints TestBlock
instance LgrDbSerialiseConstraints TestBlock
instance VolDbSerialiseConstraints TestBlock
instance SerialiseDiskConstraints  TestBlock

--  We use the default instances relying on 'Serialise' where possible.
instance EncodeDisk TestBlock TestBlock
instance DecodeDisk TestBlock (Lazy.ByteString -> TestBlock) where
  decodeDisk _ = const <$> decode

instance EncodeDisk TestBlock (Header TestBlock)
instance DecodeDisk TestBlock (Lazy.ByteString -> Header TestBlock) where
  decodeDisk _ = const <$> decode

instance EncodeDisk TestBlock (LedgerState TestBlock) where
  encodeDisk _ = encode . fromTelescope' (Proxy @LedgerState)
instance DecodeDisk TestBlock (LedgerState TestBlock) where
  decodeDisk _ = toTelescope' (Proxy @LedgerState) <$> decode

instance EncodeDisk TestBlock (HardForkConsensusState '[BlockA, BlockB]) where
  encodeDisk _ = encode . fromTelescope' (Proxy @WrapConsensusState)
instance DecodeDisk TestBlock (HardForkConsensusState '[BlockA, BlockB]) where
  decodeDisk _ = toTelescope' (Proxy @WrapConsensusState) <$> decode

instance EncodeDisk TestBlock (AnnTip TestBlock)
instance DecodeDisk TestBlock (AnnTip TestBlock)

instance SerialiseNodeToNodeConstraints TestBlock

instance SerialiseNodeToNode TestBlock TestBlock where
  encodeNodeToNode _ _ = defaultEncodeCBORinCBOR
  decodeNodeToNode _ _ = defaultDecodeCBORinCBOR

instance SerialiseNodeToNode TestBlock (Serialised TestBlock)

instance SerialiseNodeToNode TestBlock (Header TestBlock) where
  encodeNodeToNode ccfg _ = encodeDisk ccfg . unnest
  decodeNodeToNode ccfg _ = nest <$> decodeDisk ccfg

instance SerialiseNodeToNode TestBlock (SerialisedHeader TestBlock) where
  encodeNodeToNode ccfg _ = encodeDisk ccfg
  decodeNodeToNode ccfg _ = decodeDisk ccfg

instance SerialiseNodeToNode TestBlock (GenTx TestBlock) where
  encodeNodeToNode _ _ = encode . fromNS (Proxy @GenTx)
  decodeNodeToNode _ _ = toNS (Proxy @GenTx) <$> decode

instance SerialiseNodeToNode TestBlock (GenTxId TestBlock) where
  encodeNodeToNode _ _ = encode . fromNS (Proxy @WrapGenTxId)
  decodeNodeToNode _ _ = toNS (Proxy @WrapGenTxId) <$> decode

instance SerialiseNodeToClientConstraints TestBlock

instance SerialiseNodeToClient TestBlock TestBlock where
  encodeNodeToClient _ _ = defaultEncodeCBORinCBOR
  decodeNodeToClient _ _ = defaultDecodeCBORinCBOR

instance SerialiseNodeToClient TestBlock (Serialised TestBlock)

instance SerialiseNodeToClient TestBlock (GenTx TestBlock) where
  encodeNodeToClient _ _ = encode . fromNS (Proxy @GenTx)
  decodeNodeToClient _ _ = toNS (Proxy @GenTx) <$> decode

instance SerialiseNodeToClient TestBlock (HardForkApplyTxErr '[BlockA, BlockB]) where
  encodeNodeToClient _ _ = encode . Match.mismatchTwo . mustBeMismatch
  decodeNodeToClient _ _ = (fromMismatch . Match.mkMismatchTwo) <$> decode

instance SerialiseNodeToClient TestBlock (Some (Query TestBlock)) where
  encodeNodeToClient _ _ (Some q) = absurd $ thereIsNoQuery q
  decodeNodeToClient _ _ = fail "there are no queries to be decoded"

instance SerialiseResult TestBlock (Query TestBlock) where
  encodeResult _ _ = absurd . thereIsNoQuery
  decodeResult _ _ = absurd . thereIsNoQuery

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

{-------------------------------------------------------------------------------
  Auxiliary functions required for RunNode
-------------------------------------------------------------------------------}

mustBeMismatch :: HardForkApplyTxErr '[BlockA, BlockB]
               -> Mismatch SingleEraInfo LedgerEraInfo '[BlockA, BlockB]
mustBeMismatch = getMismatchEraInfo . go
  where
    go :: HardForkApplyTxErr '[BlockA, BlockB]
       -> MismatchEraInfo '[BlockA, BlockB]
    go (HardForkApplyTxErrWrongEra e) = e
    go (HardForkApplyTxErrFromEra (OneEraApplyTxErr e)) =
        case e of
          Z (WrapApplyTxErr errA)     -> absurd errA
          S (Z (WrapApplyTxErr errB)) -> absurd errB
          S (S err)                   -> case err of {}

fromMismatch :: Mismatch SingleEraInfo LedgerEraInfo '[BlockA, BlockB]
             -> HardForkApplyTxErr '[BlockA, BlockB]
fromMismatch = HardForkApplyTxErrWrongEra . MismatchEraInfo

thereIsNoQuery :: Query TestBlock result -> Void
thereIsNoQuery qry = getHardForkQuery qry $ \Refl -> go
  where
    go :: HardForkQuery '[BlockA, BlockB] result -> Void
    go (QZ q)      = goA q
    go (QS (QZ q)) = goB q
    go (QS (QS q)) = case q of {}

    goA :: Query BlockA result -> Void
    goA q = case q of {}

    goB :: Query BlockB result -> Void
    goB q = case q of {}

{-------------------------------------------------------------------------------
  Serialisation auxiliary
-------------------------------------------------------------------------------}

deriving newtype instance Serialise a => Serialise (I a)

{-------------------------------------------------------------------------------
  Serialisation auxiliary functions
-------------------------------------------------------------------------------}

fromNS :: Coercible a (NS f '[x, y])
       => proxy f -> a -> Either (f x) (f y)
fromNS _ = aux . coerce
  where
    aux :: NS f '[x, y] -> Either (f x) (f y)
    aux (Z fx)     = Left fx
    aux (S (Z fy)) = Right fy
    aux (S (S x))  = case x of {}

toNS :: Coercible a (NS f '[x, y])
     => proxy f -> Either (f x) (f y) -> a
toNS _ = coerce . aux
  where
    aux :: Either (f x) (f y) -> NS f '[x, y]
    aux (Left  fx) = Z fx
    aux (Right fy) = S (Z fy)

toNP :: Coercible a (NP f '[x, y])
     => proxy f -> f x -> f y -> a
toNP _ = coerce .: aux
  where
    aux :: f x -> f y -> NP f '[x, y]
    aux fx fy = fx :* fy :* Nil

fromTelescope :: Coercible a (HardForkState_ g f '[x, y])
              => proxy g
              -> proxy f
              -> a -> Either (Current f x) (Past g x, Current f y)
fromTelescope _ _ = aux . getHardForkState . coerce
  where
    aux :: Telescope (Past g) (Current f) '[x, y]
        -> Either (Current f x) (Past g x, Current f y)
    aux (TZ fx)         = Left fx
    aux (TS gx (TZ fy)) = Right (gx, fy)
    aux (TS _ (TS _ t)) = case t of {}

fromTelescope' :: Coercible a (HardForkState f '[x, y])
               => proxy f
               -> a -> Either (Current f x) (Past f x, Current f y)
fromTelescope' p = fromTelescope p p

toTelescope :: Coercible a (HardForkState_ g f '[x, y])
            => proxy g
            -> proxy f
            -> Either (Current f x) (Past g x, Current f y) -> a
toTelescope _ _ = coerce . HardForkState . aux
  where
    aux :: Either (Current f x) (Past g x, Current f y)
        -> Telescope (Past g) (Current f) '[x, y]
    aux (Left fx)        = TZ fx
    aux (Right (gx, fy)) = TS gx (TZ fy)

toTelescope' :: Coercible a (HardForkState f '[x, y])
             => proxy f
             -> Either (Current f x) (Past f x, Current f y) -> a
toTelescope' p = toTelescope p p
