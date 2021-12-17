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
    BlockB (..)
  , ProtocolB
  , blockForgingB
  , safeZoneB
    -- * Type family instances
  , BlockConfig (..)
  , CodecConfig (..)
  , ConsensusConfig (..)
  , GenTx (..)
  , Header (..)
  , LedgerState (..)
  , NestedCtxt_ (..)
  , StorageConfig (..)
  , TxId (..)
  ) where

import           Codec.Serialise
import qualified Data.Binary as B
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Void
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks, OnlyCheckWhnfNamed (..))

import           Test.Util.Time (dawnOfTime)

import           Ouroboros.Network.Block (Serialised, unwrapCBORinCBOR,
                     wrapCBORinCBOR)
import           Ouroboros.Network.Magic

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config
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
import           Ouroboros.Consensus.Ledger.Query
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Ledger.SupportsPeerSelection
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Node.InitStorage
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Node.Serialisation
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Storage.ImmutableDB (simpleChunkInfo)
import           Ouroboros.Consensus.Storage.Serialisation
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
  deriving NoThunks via OnlyCheckWhnfNamed "CfgB" (ConsensusConfig ProtocolB)

instance ConsensusProtocol ProtocolB where
  type ChainDepState ProtocolB = ()
  type LedgerView    ProtocolB = ()
  type IsLeader      ProtocolB = ()
  type CanBeLeader   ProtocolB = ()
  type ValidateView  ProtocolB = ()
  type ValidationErr ProtocolB = Void

  checkIsLeader CfgB{..} () slot _ =
      if slot `Set.member` cfgB_leadInSlots
      then Just ()
      else Nothing

  protocolSecurityParam = cfgB_k

  tickChainDepState     _ _ _ _ = TickedTrivial
  updateChainDepState   _ _ _ _ = return ()
  reupdateChainDepState _ _ _ _ = ()

data BlockB = BlkB {
      blkB_header :: Header BlockB
    }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (Serialise)
  deriving NoThunks via OnlyCheckWhnfNamed "BlkB" BlockB

data instance Header BlockB = HdrB {
      hdrB_fields :: HeaderFields BlockB
    , hdrB_prev   :: ChainHash BlockB
    }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (Serialise)
  deriving NoThunks via OnlyCheckWhnfNamed "HdrB" (Header BlockB)

instance GetHeader BlockB where
  getHeader          = blkB_header
  blockMatchesHeader = \_ _ -> True -- We are not interested in integrity here
  headerIsEBB        = const Nothing

data instance BlockConfig BlockB = BCfgB
  deriving (Generic, NoThunks)

type instance BlockProtocol BlockB = ProtocolB
type instance HeaderHash    BlockB = Strict.ByteString

data instance CodecConfig BlockB = CCfgB
  deriving (Generic, NoThunks)

data instance StorageConfig BlockB = SCfgB
  deriving (Generic, NoThunks)

instance ConfigSupportsNode BlockB where
  getSystemStart  _ = SystemStart dawnOfTime
  getNetworkMagic _ = NetworkMagic 0

instance StandardHash BlockB

instance HasHeader BlockB where
  getHeaderFields = getBlockHeaderFields

instance HasHeader (Header BlockB) where
  getHeaderFields = castHeaderFields . hdrB_fields

instance GetPrevHash BlockB where
  headerPrevHash = hdrB_prev

instance HasAnnTip BlockB where

instance BasicEnvelopeValidation BlockB where
  -- Use defaults

instance ValidateEnvelope BlockB where

data instance LedgerState BlockB = LgrB {
      lgrB_tip :: Point BlockB
    }
  deriving (Show, Eq, Generic, Serialise)
  deriving NoThunks via OnlyCheckWhnfNamed "LgrB" (LedgerState BlockB)

type instance LedgerCfg (LedgerState BlockB) = ()

-- | Ticking has no state on the B ledger state
newtype instance Ticked (LedgerState BlockB) = TickedLedgerStateB {
      getTickedLedgerStateB :: LedgerState BlockB
    }
  deriving NoThunks via OnlyCheckWhnfNamed "TickedLgrB" (Ticked (LedgerState BlockB))

instance GetTip (LedgerState BlockB) where
  getTip = castPoint . lgrB_tip

instance GetTip (Ticked (LedgerState BlockB)) where
  getTip = castPoint . getTip . getTickedLedgerStateB

instance IsLedger (LedgerState BlockB) where
  type LedgerErr (LedgerState BlockB) = Void

  type AuxLedgerEvent (LedgerState BlockB) =
    VoidLedgerEvent (LedgerState BlockB)

  applyChainTickLedgerResult _ _ = pureLedgerResult . TickedLedgerStateB

instance ApplyBlock (LedgerState BlockB) BlockB where
  applyBlockLedgerResult   = \_ b _ -> return $ pureLedgerResult $ LgrB (blockPoint b)
  reapplyBlockLedgerResult = \_ b _ ->          pureLedgerResult $ LgrB (blockPoint b)

instance UpdateLedger BlockB

instance CommonProtocolParams BlockB where
  maxHeaderSize _ = maxBound
  maxTxSize     _ = maxBound

instance BlockSupportsProtocol BlockB where
  validateView _ _ = ()

instance LedgerSupportsProtocol BlockB where
  protocolLedgerView   _ _ = TickedTrivial
  ledgerViewForecastAt _   = trivialForecast

instance HasPartialConsensusConfig ProtocolB

instance HasPartialLedgerConfig BlockB

type instance CannotForge           BlockB = Void
type instance ForgeStateInfo        BlockB = ()
type instance ForgeStateUpdateError BlockB = Void

forgeBlockB ::
     TopLevelConfig BlockB
  -> BlockNo
  -> SlotNo
  -> TickedLedgerState BlockB
  -> [GenTx BlockB]
  -> IsLeader (BlockProtocol BlockB)
  -> BlockB
forgeBlockB _ bno sno (TickedLedgerStateB st) _txs _ = BlkB {
      blkB_header = HdrB {
          hdrB_fields = HeaderFields {
              headerFieldHash    = Lazy.toStrict . B.encode $ unSlotNo sno
            , headerFieldSlot    = sno
            , headerFieldBlockNo = bno
            }
        , hdrB_prev = ledgerTipHash st
        }
    }

blockForgingB :: Monad m => BlockForging m BlockB
blockForgingB = BlockForging {
     forgeLabel       = "BlockB"
   , canBeLeader      = ()
   , updateForgeState = \_ _ _ -> return $ ForgeStateUpdated ()
   , checkCanForge    = \_ _ _ _ _ -> return ()
   , forgeBlock       = \cfg bno slot st txs proof -> return $
       forgeBlockB cfg bno slot st (fmap txForgetValidated txs) proof
   }

-- | A basic 'History.SafeZone'
--
-- The mock B ledger has no transactions and so can't end and so needs no
-- safezone. However, we give it a default one anyway, since that makes the
-- test more realistic.
safeZoneB :: SecurityParam -> History.SafeZone
safeZoneB (SecurityParam k) = History.StandardSafeZone k

data instance GenTx BlockB
  deriving (Show, Eq, Generic, NoThunks, Serialise)

data instance Validated (GenTx BlockB)
  deriving (Show, Eq, Generic, NoThunks)

type instance ApplyTxErr BlockB = Void

instance LedgerSupportsMempool BlockB where
  applyTx   = \_ _ _wti tx -> case tx of {}
  reapplyTx = \_ _ vtx -> case vtx of {}

  txsMaxBytes   _ = maxBound
  txInBlockSize _ = 0

  txForgetValidated = \case {}

data instance TxId (GenTx BlockB)
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (NoThunks, Serialise)

instance HasTxId (GenTx BlockB) where
  txId tx = case tx of {}

instance ShowQuery (BlockQuery BlockB) where
  showResult qry = case qry of {}

data instance BlockQuery BlockB result
  deriving (Show)

instance QueryLedger BlockB where
  answerBlockQuery _ qry = case qry of {}

instance SameDepIndex (BlockQuery BlockB) where
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
  -- Use defaults

instance LedgerSupportsPeerSelection BlockB where
  getPeers = const []

instance NodeInitStorage BlockB where
  nodeCheckIntegrity  _ _ = True

  -- Pick some chunk size
  nodeImmutableDbChunkInfo _ = simpleChunkInfo 10

instance BlockSupportsMetrics BlockB where
  isSelfIssued = isSelfIssuedConstUnknown

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
instance SerialiseDiskConstraints         BlockB
instance SerialiseNodeToClientConstraints BlockB
instance SerialiseNodeToNodeConstraints   BlockB where
    estimateBlockSize = const 0

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
instance SerialiseNodeToClient BlockB (GenTxId BlockB)
instance SerialiseNodeToClient BlockB SlotNo

instance SerialiseNodeToClient BlockB Void where
  encodeNodeToClient _ _ = absurd
  decodeNodeToClient _ _ = fail "no ApplyTxErr to be decoded"

instance SerialiseNodeToClient BlockB (SomeSecond BlockQuery BlockB) where
  encodeNodeToClient _ _ = \case {}
  decodeNodeToClient _ _ = fail "there are no queries to be decoded"

instance SerialiseResult BlockB (BlockQuery BlockB) where
  encodeResult _ _ = \case {}
  decodeResult _ _ = \case {}
