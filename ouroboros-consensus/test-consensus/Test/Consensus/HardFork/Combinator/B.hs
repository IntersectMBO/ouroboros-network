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

module Test.Consensus.HardFork.Combinator.B (
    ProtocolB
  , BlockB(..)
  , safeZoneB
    -- * Additional types
    -- * Type family instances
  , ConsensusConfig(..)
  , BlockConfig(..)
  , LedgerState(..)
  , GenTx(..)
  , TxId(..)
  ) where

import           Codec.Serialise
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as Lazy
import           Data.FingerTree.Strict (Measured (..))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Void
import           GHC.Generics (Generic)

import           Cardano.Crypto.ProtocolMagic
import           Cardano.Prelude (NoUnexpectedThunks)
import           Cardano.Slotting.Slot

import           Test.Util.Time (dawnOfTime)

import           Ouroboros.Network.Block
import           Ouroboros.Network.Magic

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
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
import           Ouroboros.Consensus.Util ((.:))
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()

import           Test.Consensus.HardFork.Combinator.Common

{-------------------------------------------------------------------------------
  BlockB
-------------------------------------------------------------------------------}

data ProtocolB

data instance ConsensusConfig ProtocolB = CfgB {
      cfgB_k           :: SecurityParam
    , cfgB_leadInSlots :: Set SlotNo
    }
  deriving (Generic, NoUnexpectedThunks)

instance ChainSelection ProtocolB where
  -- Use defaults

instance ConsensusProtocol ProtocolB where
  type ConsensusState ProtocolB = ()
  type LedgerView     ProtocolB = ()
  type IsLeader       ProtocolB = ()
  type CanBeLeader    ProtocolB = ()
  type CannotLead     ProtocolB = Void
  type ValidateView   ProtocolB = ()
  type ValidationErr  ProtocolB = Void

  checkIsLeader CfgB{..} () (Ticked slot _) _ =
      return $ if slot `Set.member` cfgB_leadInSlots
                 then IsLeader ()
                 else NotLeader

  protocolSecurityParam = cfgB_k
  updateConsensusState  = \_ _ _ _ -> return ()
  rewindConsensusState  = \_ _ _ _ -> Just ()

data BlockB = BlkB {
      blkB_header :: Header BlockB
    , blkB_body   :: Body   BlockB
    }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (NoUnexpectedThunks, Serialise)

instance HasBlockBody BlockB where
  data Body BlockB = BodyB
    deriving stock    (Show, Eq, Generic)
    deriving anyclass (NoUnexpectedThunks, Serialise)

  getBody       = blkB_body
  assembleBlock = Just .: BlkB

instance GetHeader BlockB where
  newtype Header BlockB = HdrB { getHdrB :: HeaderFields BlockB }
    deriving stock    (Show, Eq, Generic)
    deriving anyclass (NoUnexpectedThunks, Serialise)

  getHeader          = blkB_header
  blockMatchesHeader = \_ _ -> True -- We are not interested in integrity here
  headerIsEBB        = const Nothing

data instance BlockConfig BlockB = BCfgB
  deriving (Generic, NoUnexpectedThunks)

type instance BlockProtocol BlockB = ProtocolB
type instance HeaderHash    BlockB = Hash

instance HasCodecConfig BlockB where
  data CodecConfig BlockB = CCfgB

  getCodecConfig     _ = CCfgB

instance ConfigSupportsNode BlockB where
  getSystemStart     _ = SystemStart dawnOfTime
  getNetworkMagic    _ = NetworkMagic 0
  getProtocolMagicId _ = ProtocolMagicId 0

instance StandardHash BlockB

instance Measured BlockMeasure BlockB where
  measure = blockMeasure

instance HasHeader BlockB where
  blockHash      =            blockHash     . getHeader
  blockPrevHash  = castHash . blockPrevHash . getHeader
  blockSlot      =            blockSlot     . getHeader
  blockNo        =            blockNo       . getHeader
  blockInvariant = const True

instance HasHeader (Header BlockB) where
  blockHash      =            headerFieldHash     . getHdrB
  blockPrevHash  = castHash . headerFieldPrevHash . getHdrB
  blockSlot      =            headerFieldSlot     . getHdrB
  blockNo        =            headerFieldNo       . getHdrB
  blockInvariant = const True

instance HasAnnTip BlockB where

instance BasicEnvelopeValidation BlockB where
  -- Use defaults

instance ValidateEnvelope BlockB where

data instance LedgerState BlockB = LgrB {
      lgrB_tip :: Point BlockB
    }
  deriving (Show, Eq, Generic, NoUnexpectedThunks, Serialise)

type instance LedgerCfg (LedgerState BlockB) = ()

instance IsLedger (LedgerState BlockB) where
  type LedgerErr (LedgerState BlockB) = Void
  applyChainTick _ = Ticked

instance ApplyBlock (LedgerState BlockB) BlockB where
  applyLedgerBlock   = \_ b _ -> return $ LgrB (blockPoint b)
  reapplyLedgerBlock = \_ b _ -> LgrB (blockPoint b)
  ledgerTipPoint     = lgrB_tip

instance UpdateLedger BlockB

instance CanForge BlockB where
  forgeBlock _ _ bno (Ticked sno st) _txs _ = return $ BlkB {
      blkB_header = HdrB HeaderFields {
          headerFieldHash     = Lazy.toStrict . B.encode $ unSlotNo sno
        , headerFieldPrevHash = ledgerTipHash st
        , headerFieldSlot     = sno
        , headerFieldNo       = bno
        }
    , blkB_body = BodyB
    }

instance BlockSupportsProtocol BlockB where
  validateView _ _ = ()

instance LedgerSupportsProtocol BlockB where
  protocolLedgerView   _ _ = ()
  ledgerViewForecastAt _ _ = Just . trivialForecast

instance HasPartialConsensusConfig ProtocolB

instance HasPartialLedgerConfig BlockB

-- | A basic 'History.SafeZone'
--
-- The mock B ledger has no transactions and so can't end and so needs no
-- safezone. However, we give it a default one anyway, since that makes the
-- test more realistic.
safeZoneB :: SecurityParam -> History.SafeZone
safeZoneB (SecurityParam k) = History.defaultSafeZone k

instance LedgerSupportsMempool BlockB where
  data GenTx BlockB
    deriving (Show, Generic, NoUnexpectedThunks, Serialise)

  type ApplyTxErr BlockB = Void

  applyTx   = \_ tx -> case tx of {}
  reapplyTx = applyTx

  maxTxCapacity _ = maxBound
  maxTxSize     _ = maxBound
  txInBlockSize _ = 0

instance HasTxId (GenTx BlockB) where
  data TxId (GenTx BlockB)
    deriving stock    (Show, Eq, Ord, Generic)
    deriving anyclass (NoUnexpectedThunks, Serialise)

  txId tx = case tx of {}

instance ShowQuery (Query BlockB) where
  showResult qry = case qry of {}

instance QueryLedger BlockB where
  data Query BlockB result
    deriving (Show)

  answerQuery _ qry = case qry of {}
  eqQuery qry _qry' = case qry of {}

instance ConvertRawHash BlockB where
  toRawHash   _ = id
  fromRawHash _ = id
  hashSize    _ = 8 -- We use the SlotNo as the hash, which is Word64

instance SingleEraBlock BlockB where
  singleEraInfo _     = SingleEraInfo "B"
  singleEraTransition = \_ _ -> Nothing

instance HasTxs BlockB where
  extractTxs = const []

instance Condense BlockB where
  condense = show
