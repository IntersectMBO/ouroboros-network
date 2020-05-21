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
import           Ouroboros.Consensus.HardFork.History (EraParams (..))
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
  type ValidateView   ProtocolA = ()
  type ValidationErr  ProtocolA = Void

  checkIsLeader CfgA{..} () (Ticked slot _) _ =
      return $ if slot `Set.member` cfgA_leadInSlots
                 then Just ()
                 else Nothing

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

instance ConfigSupportsNode BlockA where
  data CodecConfig BlockA = CCfgA

  getCodecConfig     _ = CCfgA
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
      lcfgA_eraParams   :: EraParams
    , lcfgA_k           :: SecurityParam
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
          let tipEpoch       = runIdentity $ epochInfoEpoch ei sno
              nextEpoch      = succ tipEpoch
              nextEpochStart = runIdentity $ epochInfoFirst ei nextEpoch
              distance       = History.countSlots nextEpochStart sno
          return $ Ticked sno $ st {
              lgrA_transition = Just (sno, if distance >= k
                                             then nextEpoch
                                             else succ nextEpoch)
            }
    where
      SecurityParam k = lcfgA_k ledgerConfig

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
  singleEraParams _   = lcfgA_eraParams
  singleEraTransition = \cfg st -> do
      (confirmedInSlot, transition) <- lgrA_transition st
      let confirmationDepth =
            case ledgerTipSlot st of
              Origin -> error "impossible"
              At s   -> if s < confirmedInSlot
                          then error "impossible"
                          else History.countSlots s confirmedInSlot
      guard $ confirmationDepth >= maxRollbacks (lcfgA_k cfg)
      return transition

instance HasTxs BlockA where
  extractTxs = blkA_txs . blkA_body

instance Condense BlockA where
  condense = show
