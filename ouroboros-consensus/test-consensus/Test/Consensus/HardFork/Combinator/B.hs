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
  , safeZoneB
    -- * Type family instances
  , BlockConfig(..)
  , CodecConfig(..)
  , ConsensusConfig(..)
  , GenTx(..)
  , Header(..)
  , LedgerState(..)
  , NestedCtxt_(..)
  , TxId(..)
  ) where

import           Codec.Serialise
import qualified Data.Binary as B
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import           Data.FingerTree.Strict (Measured (..))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Void
import           GHC.Generics (Generic)

import           Cardano.Crypto.ProtocolMagic
import           Cardano.Prelude (NoUnexpectedThunks, OnlyCheckIsWHNF (..))

import           Test.Util.Time (dawnOfTime)

import           Ouroboros.Network.Block (Serialised, unwrapCBORinCBOR,
                     wrapCBORinCBOR)
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
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Node.Serialisation
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Storage.ChainDB.Serialisation
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()

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

instance HasChainIndepState ProtocolB where
  -- Use defaults

instance ConsensusProtocol ProtocolB where
  type ChainDepState ProtocolB = ()
  type LedgerView    ProtocolB = ()
  type IsLeader      ProtocolB = ()
  type CanBeLeader   ProtocolB = ()
  type CannotLead    ProtocolB = Void
  type ValidateView  ProtocolB = ()
  type ValidationErr ProtocolB = Void

  checkIsLeader CfgB{..} () _ slot _ =
      if slot `Set.member` cfgB_leadInSlots
      then IsLeader ()
      else NotLeader

  protocolSecurityParam = cfgB_k

  tickChainDepState   _ _ _ _ = TickedTrivial
  updateChainDepState _ _ _ _ = return ()
  rewindChainDepState _ _ _ _ = Just ()

data BlockB = BlkB {
      blkB_header :: Header BlockB
    }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (Serialise)
  deriving NoUnexpectedThunks via OnlyCheckIsWHNF "BlkB" BlockB

data instance Header BlockB = HdrB {
      hdrB_fields :: HeaderFields BlockB
    , hdrB_prev   :: ChainHash BlockB
    }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (Serialise)
  deriving NoUnexpectedThunks via OnlyCheckIsWHNF "HdrB" (Header BlockB)

instance GetHeader BlockB where
  getHeader          = blkB_header
  blockMatchesHeader = \_ _ -> True -- We are not interested in integrity here
  headerIsEBB        = const Nothing

data instance BlockConfig BlockB = BCfgB
  deriving (Generic, NoUnexpectedThunks)

type instance BlockProtocol BlockB = ProtocolB
type instance HeaderHash    BlockB = Strict.ByteString

data instance CodecConfig BlockB = CCfgB
  deriving (Generic, NoUnexpectedThunks)

instance ConfigSupportsNode BlockB where
  getSystemStart     _ = SystemStart dawnOfTime
  getNetworkMagic    _ = NetworkMagic 0
  getProtocolMagicId _ = ProtocolMagicId 0

instance StandardHash BlockB

instance Measured BlockMeasure BlockB where
  measure = blockMeasure

instance HasHeader BlockB where
  getHeaderFields = getBlockHeaderFields

instance HasHeader (Header BlockB) where
  getHeaderFields = castHeaderFields . hdrB_fields

instance GetPrevHash BlockB where
  headerPrevHash _cfg = hdrB_prev

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

-- | Ticking has no state on the B ledger state
newtype instance Ticked (LedgerState BlockB) = TickedLedgerStateB {
      getTickedLedgerStateB :: LedgerState BlockB
    }
  deriving NoUnexpectedThunks via OnlyCheckIsWHNF "TickedLgrB" (Ticked (LedgerState BlockB))

instance GetTip (LedgerState BlockB) where
  getTip = castPoint . lgrB_tip

instance GetTip (Ticked (LedgerState BlockB)) where
  getTip = castPoint . getTip . getTickedLedgerStateB

instance IsLedger (LedgerState BlockB) where
  type LedgerErr (LedgerState BlockB) = Void
  applyChainTick _ _ = TickedLedgerStateB

instance ApplyBlock (LedgerState BlockB) BlockB where
  applyLedgerBlock   = \_ b _ -> return $ LgrB (blockPoint b)
  reapplyLedgerBlock = \_ b _ -> LgrB (blockPoint b)

instance UpdateLedger BlockB

instance CommonProtocolParams BlockB where
  maxHeaderSize _ = maxBound
  maxTxSize     _ = maxBound

instance CanForge BlockB where
  forgeBlock _ _ bno sno (TickedLedgerStateB st) _txs _ = BlkB {
      blkB_header = HdrB {
          hdrB_fields = HeaderFields {
              headerFieldHash    = Lazy.toStrict . B.encode $ unSlotNo sno
            , headerFieldSlot    = sno
            , headerFieldBlockNo = bno
            }
        , hdrB_prev = ledgerTipHash st
        }
    }

instance BlockSupportsProtocol BlockB where
  validateView _ _ = ()

instance LedgerSupportsProtocol BlockB where
  protocolLedgerView   _ _ = TickedTrivial
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

data instance GenTx BlockB
  deriving (Show, Eq, Generic, NoUnexpectedThunks, Serialise)

type instance ApplyTxErr BlockB = Void

instance LedgerSupportsMempool BlockB where
  applyTx   = \_ _ tx -> case tx of {}
  reapplyTx = applyTx

  maxTxCapacity _ = maxBound
  txInBlockSize _ = 0

data instance TxId (GenTx BlockB)
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (NoUnexpectedThunks, Serialise)

instance HasTxId (GenTx BlockB) where
  txId tx = case tx of {}

instance ShowQuery (Query BlockB) where
  showResult qry = case qry of {}

data instance Query BlockB result
  deriving (Show)

instance QueryLedger BlockB where
  answerQuery _ qry = case qry of {}

instance SameDepIndex (Query BlockB) where
  sameDepIndex qry _qry' = case qry of {}

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

instance InspectLedger BlockB where
  type LedgerWarning BlockB = Void
  inspectLedger _ _ = []

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

instance HasBinaryBlockInfo BlockB where
  -- Standard cborg generic serialisation is:
  --
  -- > [number of fields in the product]
  -- >   [tag of the constructor]
  -- >   field1
  -- >   ..
  -- >   fieldN
  getBinaryBlockInfo BlkB{..} = BinaryBlockInfo {
        headerOffset = 2
      , headerSize   = fromIntegral $ Lazy.length (serialise blkB_header)
      }

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
instance SerialiseNodeToNode BlockB Strict.ByteString
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
