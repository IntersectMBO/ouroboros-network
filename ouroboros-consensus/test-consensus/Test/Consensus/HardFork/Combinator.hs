{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Consensus.HardFork.Combinator (tests) where

import           Codec.Serialise
import           Data.Coerce
import qualified Data.Map as Map
import           Data.SOP.Strict hiding (shape)
import           Data.Type.Equality
import           Data.Void

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
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.HasBlockBody
import           Ouroboros.Consensus.HardFork.Combinator.State (Current (..),
                     Past (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Match as Match
import           Ouroboros.Consensus.HardFork.History (EraParams (..),
                     defaultSafeZone)
import qualified Ouroboros.Consensus.HardFork.History as History
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
import           Ouroboros.Consensus.Util ((.:))
import           Ouroboros.Consensus.Util.Counting
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.Random (Seed (..))

import           Test.ThreadNet.General
import           Test.ThreadNet.Network
import           Test.ThreadNet.TxGen
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
      testSetupSeed :: Seed
    }
  deriving (Show)

instance Arbitrary TestSetup where
  arbitrary = do
      testSetupSeed <- arbitrary
      return TestSetup{..}

prop_simple_hfc_convergence :: TestSetup -> Property
prop_simple_hfc_convergence TestSetup{..} = once $
    prop_general args $
      runTestNetwork testConfig globalEpochSize testConfigBlock
  where
    k :: SecurityParam
    k = SecurityParam 2

    -- TODO: Tests still want a single epoch size for now
    globalEpochSize :: EpochSize
    globalEpochSize = 6

    -- TODO: Tests still want a single unique slot length
    globalSlotLength :: SlotLength
    globalSlotLength = slotLengthFromSec 1

    eraParamsA, eraParamsB :: EraParams
    eraParamsA = EraParams {
                     eraEpochSize  = globalEpochSize
                   , eraSlotLength = globalSlotLength
                   , eraSafeZone   = defaultSafeZone 2
                   }
    eraParamsB = EraParams {
                     eraEpochSize  = globalEpochSize
                   , eraSlotLength = globalSlotLength
                   , eraSafeZone   = defaultSafeZone 2
                   }

    shape :: History.Shape '[BlockA, BlockB]
    shape = History.Shape $ exactlyTwo eraParamsA eraParamsB

    leaderSchedule :: LeaderSchedule
    leaderSchedule = LeaderSchedule $ Map.fromList [
          (SlotNo 0, [CoreNodeId 0])
        , (SlotNo 1, [CoreNodeId 1])
        , (SlotNo 2, [CoreNodeId 0])
        , (SlotNo 3, [CoreNodeId 1])
        , (SlotNo 4, [CoreNodeId 0])
        , (SlotNo 5, [CoreNodeId 1])
        , (SlotNo 6, [CoreNodeId 0])
        , (SlotNo 7, [CoreNodeId 1])
        , (SlotNo 8, [CoreNodeId 0])
        , (SlotNo 9, [CoreNodeId 1])
        ]

    args :: PropGeneralArgs TestBlock
    args = PropGeneralArgs {
          pgaBlockProperty          = const $ property True
        , pgaCountTxs               = fromIntegral . length . extractTxs
        , pgaExpectedBlockRejection = const False
        , pgaFirstBlockNo           = BlockNo 0
        , pgaFixedMaxForkLength     = Nothing
        , pgaFixedSchedule          = Just leaderSchedule
        , pgaSecurityParam          = k
        , pgaTestConfig             = testConfig
        , pgaCustomLabelling        = customLabelling
        }

    customLabelling :: TestOutput TestBlock -> Property -> Property
    customLabelling TestOutput{..} =
          tabulate "length A" (map (show . chainLen isA) $ Map.elems testOutputNodes)
        . tabulate "length B" (map (show . chainLen isB) $ Map.elems testOutputNodes)
      where
        isA, isB :: TestBlock -> Bool
        isA (HardForkBlock (OneEraBlock blk)) = index_NS blk == 0
        isB (HardForkBlock (OneEraBlock blk)) = index_NS blk == 1

        chainLen ::  (TestBlock -> Bool) -> NodeOutput TestBlock -> Int
        chainLen p NodeOutput{..} =
              length
            . filter p
            $ Mock.chainToList nodeOutputFinalChain

    testConfig :: TestConfig
    testConfig = TestConfig {
          numCoreNodes = ncn
        , numSlots     = NumSlots 10
        , nodeJoinPlan = trivialNodeJoinPlan ncn
        , nodeRestarts = noRestarts
        , nodeTopology = meshNodeTopology ncn
        , slotLength   = globalSlotLength
        , initSeed     = testSetupSeed
        }
      where
        ncn :: NumCoreNodes
        ncn = NumCoreNodes 2

    testConfigBlock :: Monad m => TestConfigBlock m TestBlock
    testConfigBlock = TestConfigBlock {
          forgeEbbEnv = Nothing
        , nodeInfo    = plainTestNodeInitialization . protocolInfo
        , rekeying    = Nothing
        , txGenExtra  = ()
        }

    protocolInfo :: Monad m => CoreNodeId -> ProtocolInfo m TestBlock
    protocolInfo nid = ProtocolInfo {
          pInfoConfig =
            topLevelConfig nid
        , pInfoInitLedger = ExtLedgerState {
              ledgerState = HardForkLedgerState $
                              initHardForkState
                                (SystemStart dawnOfTime)
                                initLedgerState
            , headerState = genesisHeaderState $
                              initHardForkState
                                (SystemStart dawnOfTime)
                                (WrapConsensusState initConsensusState)
            }
        , pInfoLeaderCreds = Just (
                 Comp (Just (WrapCanBeLeader ()))
              :* Comp (Just (WrapCanBeLeader ()))
              :* Nil
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

    -- TODO: the only reason for 'lcfgA_eraParams' is to support
    -- 'singleEraParams', but now that both 'HardForkLedgerConfig' and
    -- 'HardForkConsensusConfig' have the full shape, we might be able to
    -- get rid of that method.
    --
    -- TODO: We should vary at which point we submit the transition tx.
    ledgerConfigA :: CoreNodeId -> PartialLedgerConfig BlockA
    ledgerConfigA _nid = LCfgA {
          lcfgA_eraParams   = eraParamsA
        , lcfgA_k           = k
        , lcfgA_systemStart = SystemStart dawnOfTime -- required for RunNode
        , lcfgA_forgeTxs    = Map.fromList [
              (SlotNo 2, [TxA (TxIdA 0) InitiateAtoB])
            ]
        }

    ledgerConfigB :: CoreNodeId -> LedgerConfig BlockB
    ledgerConfigB _nid = eraParamsB

    blockConfigA :: CoreNodeId -> BlockConfig BlockA
    blockConfigA _ = BCfgA

    blockConfigB :: CoreNodeId -> BlockConfig BlockB
    blockConfigB _ = BCfgB

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

instance RunNode TestBlock where
  nodeBlockFetchSize        = const 0
  nodeCheckIntegrity        = \_ _ -> True

  nodeImmDbChunkInfo        = simpleChunkInfo
                            . eraEpochSize
                            . lcfgA_eraParams
                            . firstEraLedgerConfig
                            . configLedger

  nodeEncodeBlockWithInfo   = \_   -> encodeHardForkBlockWithInfo
  nodeEncodeBlock           = \_   -> encode
  nodeEncodeHeader          = \_ _ -> encode
  nodeEncodeGenTx           =         encode . fromNS (Proxy @GenTx)
  nodeEncodeGenTxId         =         encode . fromNS (Proxy @WrapGenTxId)
  nodeEncodeHeaderHash      = \_ ->   encode
  nodeEncodeLedgerState     = \_ ->   encode . fromTelescope' (Proxy @LedgerState)
  nodeEncodeConsensusState  = \_ ->   encode . fromTelescope' (Proxy @WrapConsensusState)
  nodeEncodeApplyTxError    = \_ ->   encode . Match.mismatchTwo . mustBeMismatch
  nodeEncodeAnnTip          = \_ ->   encode
  nodeEncodeWrappedHeader   = \_ _ -> encode
  nodeEncodeQuery           = absurd . thereIsNoQuery
  nodeEncodeResult          = absurd . thereIsNoQuery

  nodeDecodeHeader          = \_ _ -> const                                    <$> decode
  nodeDecodeBlock           = \_   -> const                                    <$> decode
  nodeDecodeGenTx           =         toNS (Proxy @GenTx)                      <$> decode
  nodeDecodeGenTxId         =         toNS (Proxy @WrapGenTxId)                <$> decode
  nodeDecodeLedgerState     = \_   -> toTelescope' (Proxy @LedgerState)        <$> decode
  nodeDecodeConsensusState  = \_   -> toTelescope' (Proxy @WrapConsensusState) <$> decode
  nodeDecodeApplyTxError    = \_   -> (fromMismatch . Match.mkMismatchTwo)     <$> decode
  nodeDecodeHeaderHash      = \_   -> decode
  nodeDecodeAnnTip          = \_   -> decode
  nodeDecodeWrappedHeader   = \_ _ -> decode
  nodeDecodeQuery           = fail "there are no queries to be decoded"
  nodeDecodeResult          = absurd . thereIsNoQuery

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

deriving via SerialiseHeaderBody '[BlockA, BlockB]
         instance Serialise TestBlock

deriving via SerialiseOne Header '[BlockA, BlockB]
         instance Serialise (Header TestBlock)

deriving via SerialiseOne WrapTipInfo '[BlockA, BlockB]
         instance Serialise (OneEraTipInfo '[BlockA, BlockB])

-- NOTE: Perhaps interestingly, the hard fork's TipInfo _can_ distinguish
-- between the eras, because it's an NS of the TipInfos of the underlying
-- eras. Not sure if that's helpful/harmful/neither
deriving instance Serialise (AnnTip TestBlock)

{-------------------------------------------------------------------------------
  Translation
-------------------------------------------------------------------------------}

ledgerState_AtoB :: TranslateEraLedgerState BlockA BlockB
ledgerState_AtoB = TranslateEraLedgerState $ \_ _ _ LgrA{..} -> LgrB {
      lgrB_tip = castPoint lgrA_tip
    }

ledgerView_AtoB :: TranslateEraLedgerView BlockA BlockB
ledgerView_AtoB = TranslateEraLedgerView $ \_ _ _ _ -> ()

consensusState_AtoB :: TranslateEraConsensusState BlockA BlockB
consensusState_AtoB = TranslateEraConsensusState $ \_ _ _ _ -> ()

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

firstEraLedgerConfig :: LedgerConfig TestBlock -> PartialLedgerConfig BlockA
firstEraLedgerConfig =
      unwrapPartialLedgerConfig
    . hd
    . getPerEraLedgerConfig
    . hardForkLedgerConfigPerEra

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
