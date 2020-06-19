{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE EmptyDataDeriving          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Consensus.HardFork.Combinator.B (
    ProtocolB
  , BlockB(..)
  , binaryBlockInfoB
  , safeZoneB
    -- * Type family instances
  , BlockConfig(..)
  , ConsensusConfig(..)
  , GenTx(..)
  , Header(..)
  , LedgerState(..)
  , NestedCtxt_(..)
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
import           Cardano.Prelude (NoUnexpectedThunks, OnlyCheckIsWHNF (..))
import           Cardano.Slotting.Slot

import           Test.Util.Time (dawnOfTime)

import           Ouroboros.Network.Block
import           Ouroboros.Network.Magic

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config.SupportsNode
import           Ouroboros.Consensus.Forecast
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Condense
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.CommonProtocolParams
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Node.Serialisation
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Storage.ChainDB.Serialisation
import           Ouroboros.Consensus.Storage.Common
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
  deriving NoUnexpectedThunks via OnlyCheckIsWHNF "CfgB" (ConsensusConfig ProtocolB)

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
    }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (Serialise)
  deriving NoUnexpectedThunks via OnlyCheckIsWHNF "BlkB" BlockB

binaryBlockInfoB :: BlockB -> BinaryBlockInfo
binaryBlockInfoB BlkB{..} = BinaryBlockInfo {
      headerOffset = 2 -- See 'binaryBlockInfoA' for explanation
    , headerSize   = fromIntegral $ Lazy.length (serialise blkB_header)
    }

instance GetHeader BlockB where
  newtype Header BlockB = HdrB {
        hdrB_fields :: HeaderFields BlockB
      }
    deriving stock   (Show, Eq, Generic)
    deriving newtype (Serialise)
    deriving NoUnexpectedThunks via OnlyCheckIsWHNF "HdrB" (Header BlockB)

  getHeader          = blkB_header
  blockMatchesHeader = \_ _ -> True -- We are not interested in integrity here
  headerIsEBB        = const Nothing

data instance BlockConfig BlockB = BCfgB
  deriving (Generic, NoUnexpectedThunks)

type instance BlockProtocol BlockB = ProtocolB
type instance HeaderHash    BlockB = Hash

instance HasCodecConfig BlockB where
  data CodecConfig BlockB = CCfgB
    deriving (Generic, NoUnexpectedThunks)

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
  blockHash      =            headerFieldHash     . hdrB_fields
  blockPrevHash  = castHash . headerFieldPrevHash . hdrB_fields
  blockSlot      =            headerFieldSlot     . hdrB_fields
  blockNo        =            headerFieldNo       . hdrB_fields
  blockInvariant = const True

instance HasAnnTip BlockB where

instance BasicEnvelopeValidation BlockB where
  -- Use defaults

instance ValidateEnvelope BlockB where

data instance LedgerState BlockB = LgrB {
      lgrB_tip :: Point BlockB
    }
  deriving (Show, Eq, Generic, Serialise)
  deriving NoUnexpectedThunks via OnlyCheckIsWHNF "LgrB" (LedgerState BlockB)

type instance LedgerCfg (LedgerState BlockB) = ()

instance IsLedger (LedgerState BlockB) where
  type LedgerErr (LedgerState BlockB) = Void
  applyChainTick _ = Ticked

instance ApplyBlock (LedgerState BlockB) BlockB where
  applyLedgerBlock   = \_ b _ -> return $ LgrB (blockPoint b)
  reapplyLedgerBlock = \_ b _ -> LgrB (blockPoint b)
  ledgerTipPoint     = lgrB_tip

instance UpdateLedger BlockB

instance CommonProtocolParams BlockB where
  maxHeaderSize _ = maxBound
  maxTxSize     _ = maxBound

instance CanForge BlockB where
  forgeBlock _ _ bno (Ticked sno st) _txs _ = return $ BlkB {
      blkB_header = HdrB {
          hdrB_fields = HeaderFields {
              headerFieldHash     = Lazy.toStrict . B.encode $ unSlotNo sno
            , headerFieldPrevHash = ledgerTipHash st
            , headerFieldSlot     = sno
            , headerFieldNo       = bno
            }
        }
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
safeZoneB (SecurityParam k) = History.noLowerBoundSafeZone k

instance LedgerSupportsMempool BlockB where
  data GenTx BlockB
    deriving (Show, Eq, Generic, NoUnexpectedThunks, Serialise)

  type ApplyTxErr BlockB = Void

  applyTx   = \_ tx -> case tx of {}
  reapplyTx = applyTx

  maxTxCapacity _ = maxBound
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

data instance NestedCtxt_ BlockB f a where
  CtxtB :: NestedCtxt_ BlockB f (f BlockB)

deriving instance Show (NestedCtxt_ BlockB f a)
instance SameDepIndex (NestedCtxt_ BlockB f)

instance TrivialDependency (NestedCtxt_ BlockB f) where
  type TrivialIndex (NestedCtxt_ BlockB f) = f BlockB
  hasSingleIndex CtxtB CtxtB = Refl
  indexIsTrivial = CtxtB

instance EncodeDisk BlockB (Header BlockB)
instance DecodeDisk BlockB (Lazy.ByteString -> Header BlockB) where
  decodeDisk _ = const <$> decode

instance EncodeDiskDepIx (NestedCtxt Header) BlockB
instance EncodeDiskDep   (NestedCtxt Header) BlockB

instance DecodeDiskDepIx (NestedCtxt Header) BlockB
instance DecodeDiskDep   (NestedCtxt Header) BlockB

instance HasNestedContent Header BlockB where
  -- Use defaults

instance ReconstructNestedCtxt Header BlockB
  -- Use defaults

instance SingleEraBlock BlockB where
  singleEraInfo _     = SingleEraInfo "B"
  singleEraTransition = \_ _ _ _ -> Nothing

instance HasTxs BlockB where
  extractTxs = const []

{-------------------------------------------------------------------------------
  Condense
-------------------------------------------------------------------------------}

instance CondenseConstraints BlockB

instance Condense BlockB                where condense = show
instance Condense (Header BlockB)       where condense = show
instance Condense (GenTx BlockB)        where condense = show
instance Condense (TxId (GenTx BlockB)) where condense = show

{-------------------------------------------------------------------------------
  Top-level serialisation constraints
-------------------------------------------------------------------------------}

instance SerialiseConstraintsHFC          BlockB
instance ImmDbSerialiseConstraints        BlockB
instance VolDbSerialiseConstraints        BlockB
instance LgrDbSerialiseConstraints        BlockB
instance SerialiseDiskConstraints         BlockB
instance SerialiseNodeToNodeConstraints   BlockB
instance SerialiseNodeToClientConstraints BlockB

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

deriving instance Serialise (AnnTip BlockB)

instance EncodeDisk BlockB (LedgerState BlockB)
instance DecodeDisk BlockB (LedgerState BlockB)

instance EncodeDisk BlockB BlockB
instance DecodeDisk BlockB (Lazy.ByteString -> BlockB) where
  decodeDisk _ = const <$> decode

instance EncodeDisk BlockB (AnnTip BlockB)
instance DecodeDisk BlockB (AnnTip BlockB)

instance EncodeDisk BlockB ()
instance DecodeDisk BlockB ()

instance HasNetworkProtocolVersion BlockB

{-------------------------------------------------------------------------------
  SerialiseNodeToNode
-------------------------------------------------------------------------------}

instance SerialiseNodeToNode BlockB BlockB
instance SerialiseNodeToNode BlockB Hash
instance SerialiseNodeToNode BlockB (Serialised BlockB)
instance SerialiseNodeToNode BlockB (SerialisedHeader BlockB)
instance SerialiseNodeToNode BlockB (GenTx BlockB)
instance SerialiseNodeToNode BlockB (GenTxId BlockB)

-- Must be compatible with @(SerialisedHeader BlockB)@, which uses
-- the @Serialise (SerialisedHeader BlockB)@ instance below
instance SerialiseNodeToNode BlockB (Header BlockB) where
  encodeNodeToNode _ _ = wrapCBORinCBOR   encode
  decodeNodeToNode _ _ = unwrapCBORinCBOR (const <$> decode)

instance Serialise (SerialisedHeader BlockB) where
  encode = encodeTrivialSerialisedHeader
  decode = decodeTrivialSerialisedHeader

{-------------------------------------------------------------------------------
  SerialiseNodeToClient
-------------------------------------------------------------------------------}

instance SerialiseNodeToClient BlockB BlockB
instance SerialiseNodeToClient BlockB (Serialised BlockB)
instance SerialiseNodeToClient BlockB (GenTx BlockB)

instance SerialiseNodeToClient BlockB Void where
  encodeNodeToClient _ _ = absurd
  decodeNodeToClient _ _ = fail "no ApplyTxErr to be decoded"

instance SerialiseNodeToClient BlockB (SomeBlock Query BlockB) where
  encodeNodeToClient _ _ (SomeBlock q) = case q of {}
  decodeNodeToClient _ _ = fail "there are no queries to be decoded"

instance SerialiseResult BlockB (Query BlockB) where
  encodeResult _ _ = \case {}
  decodeResult _ _ = \case {}
