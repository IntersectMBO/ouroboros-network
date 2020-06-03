{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE EmptyDataDeriving          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Consensus.HardFork.Combinator.A (
    ProtocolA
  , BlockA(..)
  , safeFromTipA
  , stabilityWindowA
    -- * Additional types
  , PartialLedgerConfigA(..)
  , TxPayloadA(..)
    -- * Type family instances
  , ConsensusConfig(..)
  , BlockConfig(..)
  , LedgerState(..)
  , GenTx(..)
  , TxId(..)
  ) where

import           Codec.Serialise
import           Control.Monad.Except
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as Lazy
import           Data.FingerTree.Strict (Measured (..))
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Void
import           Data.Word
import           GHC.Generics (Generic)

import           Cardano.Crypto.ProtocolMagic
import           Cardano.Prelude (NoUnexpectedThunks)
import           Cardano.Slotting.EpochInfo
import           Cardano.Slotting.Slot

import           Test.Util.Time (dawnOfTime)

import           Ouroboros.Network.Block
import           Ouroboros.Network.Magic

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Config.SupportsNode
import           Ouroboros.Consensus.Forecast
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.HasBlockBody
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util (repeatedlyM, (.:))
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()

import           Test.Consensus.HardFork.Combinator.Common

{-------------------------------------------------------------------------------
  BlockA
-------------------------------------------------------------------------------}

data ProtocolA

data instance ConsensusConfig ProtocolA = CfgA {
      cfgA_k           :: SecurityParam
    , cfgA_leadInSlots :: Set SlotNo
    }
  deriving (Generic, NoUnexpectedThunks)

instance ChainSelection ProtocolA where
  -- Use defaults

instance ConsensusProtocol ProtocolA where
  type ConsensusState ProtocolA = ()
  type LedgerView     ProtocolA = ()
  type IsLeader       ProtocolA = ()
  type CanBeLeader    ProtocolA = ()
  type CannotLead     ProtocolA = Void
  type ValidateView   ProtocolA = ()
  type ValidationErr  ProtocolA = Void

  checkIsLeader CfgA{..} () (Ticked slot _) _ =
      return $ if slot `Set.member` cfgA_leadInSlots
                 then IsLeader ()
                 else NotLeader

  protocolSecurityParam = cfgA_k
  updateConsensusState  = \_ _ _ _ -> return ()
  rewindConsensusState  = \_ _ _ _ -> Just ()

data BlockA = BlkA {
      blkA_header :: Header BlockA
    , blkA_body   :: Body   BlockA
    }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (NoUnexpectedThunks, Serialise)

instance HasBlockBody BlockA where
  data Body BlockA = BodyA { blkA_txs :: [GenTx BlockA] }
    deriving stock    (Show, Eq, Generic)
    deriving anyclass (NoUnexpectedThunks, Serialise)

  getBody       = blkA_body
  assembleBlock = Just .: BlkA

instance GetHeader BlockA where
  newtype Header BlockA = HdrA { getHdrA :: HeaderFields BlockA }
    deriving stock    (Show, Eq, Generic)
    deriving anyclass (NoUnexpectedThunks, Serialise)

  getHeader          = blkA_header
  blockMatchesHeader = \_ _ -> True -- We are not interested in integrity here
  headerIsEBB        = const Nothing

data instance BlockConfig BlockA = BCfgA
  deriving (Generic, NoUnexpectedThunks)

type instance BlockProtocol BlockA = ProtocolA
type instance HeaderHash    BlockA = Hash

instance HasCodecConfig BlockA where
  data CodecConfig BlockA = CCfgA

  getCodecConfig     _ = CCfgA

instance ConfigSupportsNode BlockA where
  getSystemStart     _ = SystemStart dawnOfTime
  getNetworkMagic    _ = NetworkMagic 0
  getProtocolMagicId _ = ProtocolMagicId 0

instance StandardHash BlockA

instance Measured BlockMeasure BlockA where
  measure = blockMeasure

instance HasHeader BlockA where
  blockHash      =            blockHash     . getHeader
  blockPrevHash  = castHash . blockPrevHash . getHeader
  blockSlot      =            blockSlot     . getHeader
  blockNo        =            blockNo       . getHeader
  blockInvariant = const True

instance HasHeader (Header BlockA) where
  blockHash      =            headerFieldHash     . getHdrA
  blockPrevHash  = castHash . headerFieldPrevHash . getHdrA
  blockSlot      =            headerFieldSlot     . getHdrA
  blockNo        =            headerFieldNo       . getHdrA
  blockInvariant = const True

instance HasAnnTip BlockA where

instance BasicEnvelopeValidation BlockA where
  -- Use defaults

instance ValidateEnvelope BlockA where

data instance LedgerState BlockA = LgrA {
      lgrA_tip :: Point BlockA

      -- | The confirmed transition, and when it was confirmed
    , lgrA_transition :: Maybe (SlotNo, EpochNo)
    }
  deriving (Show, Eq, Generic, NoUnexpectedThunks, Serialise)

data PartialLedgerConfigA = LCfgA {
      lcfgA_k           :: SecurityParam
    , lcfgA_systemStart :: SystemStart
    , lcfgA_forgeTxs    :: Map SlotNo [GenTx BlockA]
    }
  deriving (Generic, NoUnexpectedThunks)

type instance LedgerCfg (LedgerState BlockA) =
    (EpochInfo Identity, PartialLedgerConfigA)

instance IsLedger (LedgerState BlockA) where
  type LedgerErr (LedgerState BlockA) = Void
  applyChainTick _ = Ticked

instance ApplyBlock (LedgerState BlockA) BlockA where
  applyLedgerBlock cfg blk =
        fmap setTip
      . repeatedlyM (applyTx cfg) (blkA_txs $ blkA_body blk)
    where
      setTip :: TickedLedgerState BlockA -> LedgerState BlockA
      setTip (Ticked _ st) = st { lgrA_tip = blockPoint blk }

  reapplyLedgerBlock cfg blk st =
      case runExcept $ applyLedgerBlock cfg blk st of
        Left  _   -> error "reapplyLedgerBlock: impossible"
        Right st' -> st'

  ledgerTipPoint = lgrA_tip

instance UpdateLedger BlockA

instance CanForge BlockA where
  forgeBlock TopLevelConfig{..} _ bno (Ticked sno st) _txs _ = return $ BlkA {
        blkA_header = HdrA HeaderFields {
            headerFieldHash     = Lazy.toStrict . B.encode $ unSlotNo sno
          , headerFieldPrevHash = ledgerTipHash st
          , headerFieldSlot     = sno
          , headerFieldNo       = bno
          }
      , blkA_body = BodyA (Map.findWithDefault [] sno (lcfgA_forgeTxs ledgerConfig))
      }
    where
      ledgerConfig :: PartialLedgerConfig BlockA
      ledgerConfig = snd configLedger

instance BlockSupportsProtocol BlockA where
  validateView _ _ = ()

instance LedgerSupportsProtocol BlockA where
  protocolLedgerView   _ _ = ()
  ledgerViewForecastAt _ _ = Just . trivialForecast

instance HasPartialConsensusConfig ProtocolA

instance HasPartialLedgerConfig BlockA where
  type PartialLedgerConfig BlockA = PartialLedgerConfigA

  completeLedgerConfig _ = (,)

data TxPayloadA = InitiateAtoB
  deriving (Show, Eq, Generic, NoUnexpectedThunks, Serialise)

-- | See 'Ouroboros.Consensus.HardFork.History.EraParams.safeFromTip'
safeFromTipA :: SecurityParam -> Word64
safeFromTipA (SecurityParam k) = k

-- | This mock ledger assumes that every node is honest and online, every slot
-- has a single leader, and ever message arrives before the next slot. So a run
-- of @k@ slots is guaranteed to extend the chain by @k@ blocks.
stabilityWindowA :: SecurityParam -> Word64
stabilityWindowA (SecurityParam k) = k

instance LedgerSupportsMempool BlockA where
  data GenTx BlockA = TxA {
         txA_id      :: TxId (GenTx BlockA)
       , txA_payload :: TxPayloadA
       }
    deriving (Show, Eq, Generic, NoUnexpectedThunks, Serialise)

  type ApplyTxErr BlockA = Void

  applyTx (ei, ledgerConfig) (TxA _ tx) (Ticked sno st) =
      case tx of
        InitiateAtoB -> do
          let -- note that these @ei@ invocations are all at @sno@ or earlier,
              -- so we should see no 'PastHorizonException'
              tipEpoch          = runEI epochInfoEpoch sno
              firstSlotTipEpoch = runEI epochInfoFirst tipEpoch
              epochSizeTipEpoch = runEI epochInfoSize  tipEpoch

              -- The ledger must report the scheduled transition to the next
              -- era as soon as the block containing this transaction is
              -- immutable (that is, at least @k@ blocks have come after) --
              -- this happens elsewhere in the corresponding 'SingleEraBlock'
              -- instance. It must not report it sooner than that because the
              -- consensus layer requires that conversions about time (when
              -- successful) must not be subject to rollback.
              --
              -- Consensus /also/ insists that as long as the transition to the
              -- next era is not yet known (ie not yet determined by an
              -- immutable block), there is a safe zone that extends past the
              -- tip of the ledger in which we guarantee the next era will not
              -- begin. This means that we must have an additional
              -- @safeFromTipA k@ blocks /after/ reporting the transition and
              -- /before/ the start of the next era.
              --
              -- Thus, we schedule the next era to begin with the first
              -- upcoming epoch that starts /after/ we're guaranteed to see
              -- both the aforementioned @k@ additional blocks and also a
              -- further @safeFromTipA k@ slots after the last of those.

              -- the last slot that must be in the current era
              firstPossibleLastSlotThisEra =
                  History.addSlots (stabilityWindowA k + safeFromTipA k) sno
              lastEpochThisEra =
                  History.addEpochs
                    (History.countSlots
                       firstPossibleLastSlotThisEra firstSlotTipEpoch
                     `div` unEpochSize epochSizeTipEpoch)
                    tipEpoch
                  `asTypeOf`
                     -- an equivalent expression
                     --
                     -- The following would be equivalent if it couldn't fail
                     -- with 'PastHorizonException', which it may since we may
                     -- be inspecting a slot beyond the ledger's safe zone. In
                     -- particular, the @ei@ the HFC provided to us is overly
                     -- conservative for our specific purpose here. We're using
                     -- it to decide here when this era should end. This era
                     -- could only end sooner than that if this decision itself
                     -- gets discarded! (TODO: double-check this claim)
                     runEI epochInfoEpoch firstPossibleLastSlotThisEra
              -- the first epoch that may be in the next era (recall: eras are
              -- epoch-aligned)
              firstEpochNextEra = succ lastEpochThisEra

          return $ Ticked sno $ st {
              lgrA_transition = Just (sno, firstEpochNextEra)
            }
    where
      k = lcfgA_k ledgerConfig

      runEI :: (EpochInfo Identity -> a -> Identity b) -> a -> b
      runEI f x = runIdentity $ f ei x

  reapplyTx = applyTx

  maxTxCapacity _ = maxBound
  maxTxSize     _ = maxBound
  txInBlockSize _ = 0

instance HasTxId (GenTx BlockA) where
  newtype TxId (GenTx BlockA) = TxIdA Int
    deriving stock   (Show, Eq, Ord, Generic)
    deriving newtype (NoUnexpectedThunks, Serialise)

  txId = txA_id

instance ShowQuery (Query BlockA) where
  showResult qry = case qry of {}

instance QueryLedger BlockA where
  data Query BlockA result
    deriving (Show)

  answerQuery _ qry = case qry of {}
  eqQuery qry _qry' = case qry of {}

instance ConvertRawHash BlockA where
  toRawHash   _ = id
  fromRawHash _ = id
  hashSize    _ = 8 -- We use the SlotNo as the hash, which is Word64

instance SingleEraBlock BlockA where
  singleEraInfo _     = SingleEraInfo "A"
  singleEraTransition = \cfg st -> do
      (confirmedInSlot, transition) <- lgrA_transition st
      let confirmationDepth =
            case ledgerTipSlot st of
              Origin -> error "impossible"
              At s   -> if s < confirmedInSlot
                          then error "impossible"
                          else History.countSlots s confirmedInSlot
      guard $ confirmationDepth >= stabilityWindowA (lcfgA_k cfg)
      return transition

instance HasTxs BlockA where
  extractTxs = blkA_txs . blkA_body

instance Condense BlockA where
  condense = show
