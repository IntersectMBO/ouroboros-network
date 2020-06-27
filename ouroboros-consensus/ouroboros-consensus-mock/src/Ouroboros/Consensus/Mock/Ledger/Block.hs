{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE EmptyDataDeriving          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Simple block to go with the mock ledger
--
-- None of the definitions in this module depend on, or even refer to, any
-- specific consensus protocols.
module Ouroboros.Consensus.Mock.Ledger.Block (
    SimpleBlock
  , SimpleHeader
  , SimpleBlock'(..)
  , Header(..)
  , SimpleStdHeader(..)
  , SimpleBody(..)
  , SimpleHash
  , Query(..)
    -- * Working with 'SimpleBlock'
  , mkSimpleHeader
  , matchesSimpleHeader
  , countSimpleGenTxs
    -- * Configuration
  , BlockConfig(..)
  , CodecConfig(..)
  , SimpleLedgerConfig(..)
    -- * Protocol-specific part
  , MockProtocolSpecific(..)
    -- * 'UpdateLedger'
  , LedgerState(..)
  , updateSimpleLedgerState
  , genesisSimpleLedgerState
    -- * 'ApplyTx' (mempool support)
  , GenTx(..)
  , TxId(..)
  , mkSimpleGenTx
  , txSize
    -- * Crypto
  , SimpleCrypto
  , SimpleStandardCrypto
  , SimpleMockCrypto
    -- * Serialisation
  , encodeSimpleHeader
  , decodeSimpleHeader
  , simpleBlockBinaryBlockInfo
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import           Codec.Serialise (Serialise (..), serialise)
import           Control.Monad.Except
import qualified Data.ByteString.Lazy as Lazy
import           Data.FingerTree.Strict (Measured (..))
import           Data.Proxy
import           Data.Typeable
import           Data.Word
import           GHC.Generics (Generic)

import           Cardano.Binary (ToCBOR (..))
import           Cardano.Crypto.Hash (Hash, HashAlgorithm, MD5, ShortHash)
import qualified Cardano.Crypto.Hash as Hash
import           Cardano.Prelude (NoUnexpectedThunks (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HardFork.Abstract
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.CommonProtocolParams
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Mock.Ledger.Address
import           Ouroboros.Consensus.Mock.Ledger.State
import qualified Ouroboros.Consensus.Mock.Ledger.UTxO as Mock
import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam)
import           Ouroboros.Consensus.Util ((.:))
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()

import           Ouroboros.Consensus.Storage.Common (BinaryBlockInfo (..))

{-------------------------------------------------------------------------------
  Definition of a block

  The primed versions allow to vary the @ext@ parameter independently of the
  previous block hash.
-------------------------------------------------------------------------------}

type SimpleBlock  c ext = SimpleBlock'  c ext ext
type SimpleHeader c ext = Header (SimpleBlock c ext)

data SimpleBlock' c ext ext' = SimpleBlock {
      simpleHeader :: Header (SimpleBlock' c ext ext')
    , simpleBody   :: SimpleBody
    }
  deriving stock    (Generic, Show, Eq)
  deriving anyclass (Serialise)

instance (SimpleCrypto c, Typeable ext, Typeable ext')
      => GetHeader (SimpleBlock' c ext ext') where
  data Header (SimpleBlock' c ext ext') = SimpleHeader {
        -- | The header hash
        --
        -- This is the hash of the header itself. This is a bit unpleasant,
        -- because it makes the hash look self-referential (when computing the
        -- hash we must ignore the 'simpleHeaderHash' field). However, the benefit
        -- is that we can give a 'HasHeader' instance that does not require
        -- a (static) 'Serialise' instance.
        simpleHeaderHash :: HeaderHash (SimpleBlock' c ext ext')

        -- | Fields required for the 'HasHeader' instance
      , simpleHeaderStd  :: SimpleStdHeader c ext

        -- | Header extension
        --
        -- This extension will be required when using 'SimpleBlock' for specific
        -- consensus protocols.
      , simpleHeaderExt  :: ext'
      }
    deriving (Generic, Show, Eq, NoUnexpectedThunks)

  getHeader = simpleHeader

  blockMatchesHeader = matchesSimpleHeader

  headerIsEBB = const Nothing

data SimpleStdHeader c ext = SimpleStdHeader {
      simplePrev      :: ChainHash (SimpleBlock c ext)
    , simpleSlotNo    :: SlotNo
    , simpleBlockNo   :: BlockNo
    , simpleBodyHash  :: Hash (SimpleHash c) SimpleBody
    , simpleBlockSize :: Word64
    }
  deriving stock    (Generic, Show, Eq)
  deriving anyclass (Serialise, NoUnexpectedThunks)

data SimpleBody = SimpleBody {
      simpleTxs :: [Mock.Tx]
    }
  deriving stock    (Generic, Show, Eq)
  deriving anyclass (Serialise)

{-------------------------------------------------------------------------------
  Working with 'SimpleBlock'
-------------------------------------------------------------------------------}

mkSimpleHeader :: SimpleCrypto c
               => (ext' -> CBOR.Encoding)
               -> SimpleStdHeader c ext
               -> ext'
               -> Header (SimpleBlock' c ext ext')
mkSimpleHeader encodeExt std ext =
    headerWithoutHash {
        simpleHeaderHash = Hash.hashWithSerialiser
                             (encodeSimpleHeader encodeExt)
                             headerWithoutHash
      }
  where
    headerWithoutHash = SimpleHeader {
        simpleHeaderHash = error "Serialise instances should ignore hash"
      , simpleHeaderStd  = std
      , simpleHeaderExt  = ext
      }

-- | Check whether the block matches the header
matchesSimpleHeader :: SimpleCrypto c
                    => Header (SimpleBlock' c ext ext')
                    -> SimpleBlock'  c ext ext''
                    -> Bool
matchesSimpleHeader SimpleHeader{..} SimpleBlock {..} =
    simpleBodyHash == Hash.hash simpleBody
  where
    SimpleStdHeader{..} = simpleHeaderStd

countSimpleGenTxs :: SimpleBlock c ext -> Word64
countSimpleGenTxs = fromIntegral . length . extractTxs

{-------------------------------------------------------------------------------
  HasHeader instance for SimpleHeader
-------------------------------------------------------------------------------}

instance (SimpleCrypto c, Typeable ext, Typeable ext')
      => HasHeader (Header (SimpleBlock' c ext ext')) where
  getHeaderFields hdr = HeaderFields {
        headerFieldHash    = simpleHeaderHash hdr
      , headerFieldSlot    = simpleSlotNo  . simpleHeaderStd $ hdr
      , headerFieldBlockNo = simpleBlockNo . simpleHeaderStd $ hdr
      }

{-------------------------------------------------------------------------------
  HasHeader instance for SimpleBlock
-------------------------------------------------------------------------------}

type instance HeaderHash (SimpleBlock' c ext ext') =
  Hash (SimpleHash c) (Header (SimpleBlock' c ext ext'))

instance (SimpleCrypto c, Typeable ext, Typeable ext')
      => Measured BlockMeasure (SimpleBlock' c ext ext') where
  measure = blockMeasure

instance (SimpleCrypto c, Typeable ext, Typeable ext')
      => HasHeader (SimpleBlock' c ext ext') where
  getHeaderFields = getBlockHeaderFields

instance (SimpleCrypto c, Typeable ext) => GetPrevHash (SimpleBlock c ext) where
  headerPrevHash _cfg = simplePrev . simpleHeaderStd

instance (SimpleCrypto c, Typeable ext, Typeable ext')
      => StandardHash (SimpleBlock' c ext ext')

instance SimpleCrypto c => ConvertRawHash (SimpleBlock' c ext ext') where
  toRawHash   _ = Hash.getHash
  fromRawHash _ = Hash.UnsafeHash
  hashSize    _ = fromIntegral $ Hash.sizeHash (Proxy @(SimpleHash c))

{-------------------------------------------------------------------------------
  HasMockTxs instance
-------------------------------------------------------------------------------}

instance Mock.HasMockTxs (SimpleBlock' c ext ext') where
  getMockTxs = Mock.getMockTxs . simpleBody

instance Mock.HasMockTxs SimpleBody where
  getMockTxs = simpleTxs

{-------------------------------------------------------------------------------
  Envelope validation
-------------------------------------------------------------------------------}

instance (SimpleCrypto c, Typeable ext) => HasAnnTip (SimpleBlock c ext)
  -- Use defaults

instance (SimpleCrypto c, Typeable ext) => BasicEnvelopeValidation (SimpleBlock c ext)
  -- Use defaults

instance (SimpleCrypto c, Typeable ext) => ValidateEnvelope (SimpleBlock c ext)
  -- Use defaults

{-------------------------------------------------------------------------------
  Block config
-------------------------------------------------------------------------------}

newtype instance BlockConfig (SimpleBlock c ext) =
    SimpleBlockConfig SecurityParam
  deriving stock   (Generic)
  deriving newtype (NoUnexpectedThunks)

{-------------------------------------------------------------------------------
  Codec config
-------------------------------------------------------------------------------}

-- | Only the 'SecurityParam' is required for simple blocks
newtype instance CodecConfig (SimpleBlock c ext) = SimpleCodecConfig SecurityParam
  deriving newtype (NoUnexpectedThunks)

{-------------------------------------------------------------------------------
  Hard fork history
-------------------------------------------------------------------------------}

instance HasHardForkHistory (SimpleBlock c ext) where
  type HardForkIndices (SimpleBlock c ext) = '[SimpleBlock c ext]
  hardForkSummary = neverForksHardForkSummary simpleLedgerEraParams

{-------------------------------------------------------------------------------
  Protocol specific constraints
-------------------------------------------------------------------------------}

class ( SimpleCrypto c
      , Typeable ext
      , Show               (MockLedgerConfig c ext)
      , NoUnexpectedThunks (MockLedgerConfig c ext)
      ) => MockProtocolSpecific c ext where
  type family MockLedgerConfig c ext :: *

{-------------------------------------------------------------------------------
  Update the ledger
-------------------------------------------------------------------------------}

data SimpleLedgerConfig c ext = SimpleLedgerConfig {
      -- | Config required by the various kinds of mock block (PFT, Praos, ..)
      simpleMockLedgerConfig :: !(MockLedgerConfig c ext)

      -- | Era parameters
    , simpleLedgerEraParams  :: !HardFork.EraParams

      -- | k
    , simpleLedgerK          :: !SecurityParam
    }
  deriving (Generic)

deriving instance Show (MockLedgerConfig c ext) => Show (SimpleLedgerConfig c ext)
deriving instance NoUnexpectedThunks (MockLedgerConfig c ext)
               => NoUnexpectedThunks (SimpleLedgerConfig c ext)

type instance LedgerCfg (LedgerState (SimpleBlock c ext)) = SimpleLedgerConfig c ext

instance MockProtocolSpecific c ext
      => IsLedger (LedgerState (SimpleBlock c ext)) where
  type LedgerErr (LedgerState (SimpleBlock c ext)) = MockError (SimpleBlock c ext)

  applyChainTick _ = Ticked

instance MockProtocolSpecific c ext
      => ApplyBlock (LedgerState (SimpleBlock c ext)) (SimpleBlock c ext) where
  applyLedgerBlock cfg = updateSimpleLedgerState ccfg
    where
      ccfg = SimpleCodecConfig (simpleLedgerK cfg)

  reapplyLedgerBlock cfg =
      (mustSucceed . runExcept) .: updateSimpleLedgerState ccfg
    where
      ccfg = SimpleCodecConfig (simpleLedgerK cfg)

      mustSucceed (Left  err) = error ("reapplyLedgerBlock: unexpected error: " <> show err)
      mustSucceed (Right st)  = st

  ledgerTipPoint (SimpleLedgerState st) = mockTip st

newtype instance LedgerState (SimpleBlock c ext) = SimpleLedgerState {
      simpleLedgerState :: MockState (SimpleBlock c ext)
    }
  deriving stock   (Generic, Show, Eq)
  deriving newtype (Serialise, NoUnexpectedThunks)

instance MockProtocolSpecific c ext => UpdateLedger (SimpleBlock c ext)

updateSimpleLedgerState :: (SimpleCrypto c, Typeable ext)
                        => CodecConfig (SimpleBlock c ext)
                        -> SimpleBlock c ext
                        -> TickedLedgerState (SimpleBlock c ext)
                        -> Except (MockError (SimpleBlock c ext))
                                  (LedgerState (SimpleBlock c ext))
updateSimpleLedgerState cfg b (Ticked _ (SimpleLedgerState st)) =
    SimpleLedgerState <$> updateMockState cfg b st

updateSimpleUTxO :: Mock.HasMockTxs a
                 => a
                 -> TickedLedgerState (SimpleBlock c ext)
                 -> Except (MockError (SimpleBlock c ext))
                           (TickedLedgerState (SimpleBlock c ext))
updateSimpleUTxO x (Ticked slot (SimpleLedgerState st)) =
    Ticked slot . SimpleLedgerState <$> updateMockUTxO slot x st

genesisSimpleLedgerState :: AddrDist -> LedgerState (SimpleBlock c ext)
genesisSimpleLedgerState = SimpleLedgerState . genesisMockState

-- | Dummy values
instance MockProtocolSpecific c ext => CommonProtocolParams (SimpleBlock c ext) where
  maxHeaderSize = const 2000000
  maxTxSize     = const 2000000

{-------------------------------------------------------------------------------
  Support for the mempool
-------------------------------------------------------------------------------}

instance MockProtocolSpecific c ext
      => LedgerSupportsMempool (SimpleBlock c ext) where
  data GenTx (SimpleBlock c ext) = SimpleGenTx
    { simpleGenTx   :: !Mock.Tx
    , simpleGenTxId :: !Mock.TxId
    } deriving stock    (Generic, Eq, Ord)
      deriving anyclass (Serialise)

  type ApplyTxErr (SimpleBlock c ext) = MockError (SimpleBlock c ext)

  applyTx   = const updateSimpleUTxO
  reapplyTx = const updateSimpleUTxO

  -- Large value so that the Mempool tests never run out of capacity when they
  -- don't override it.
  maxTxCapacity = const 1000000000
  txInBlockSize = txSize

instance HasTxId (GenTx (SimpleBlock c ext)) where
  newtype TxId (GenTx (SimpleBlock c ext)) = SimpleGenTxId
    { unSimpleGenTxId :: Mock.TxId
    } deriving stock   (Generic)
      deriving newtype (Show, Eq, Ord, Serialise, NoUnexpectedThunks)

  txId = SimpleGenTxId . simpleGenTxId

instance (Typeable p, Typeable c) => NoUnexpectedThunks (GenTx (SimpleBlock p c)) where
  showTypeOf _ = show $ typeRep (Proxy @(GenTx (SimpleBlock p c)))

instance HasTxs (SimpleBlock c ext) where
  extractTxs = map mkSimpleGenTx . simpleTxs . simpleBody

instance Mock.HasMockTxs (GenTx (SimpleBlock p c)) where
  getMockTxs = Mock.getMockTxs . simpleGenTx

instance Condense (GenTx (SimpleBlock p c)) where
    condense = condense . simpleGenTx

instance Show (GenTx (SimpleBlock p c)) where
    show = show . simpleGenTx

instance Condense (GenTxId (SimpleBlock p c)) where
    condense = condense . unSimpleGenTxId

mkSimpleGenTx :: Mock.Tx -> GenTx (SimpleBlock c ext)
mkSimpleGenTx tx = SimpleGenTx
    { simpleGenTx   = tx
    , simpleGenTxId = Hash.hash tx
    }

txSize :: GenTx (SimpleBlock c ext) -> Word32
txSize = fromIntegral . Lazy.length . serialise

{-------------------------------------------------------------------------------
  Support for QueryLedger
-------------------------------------------------------------------------------}

instance MockProtocolSpecific c ext => QueryLedger (SimpleBlock c ext) where
  data Query (SimpleBlock c ext) result where
      QueryLedgerTip :: Query (SimpleBlock c ext) (Point (SimpleBlock c ext))

  answerQuery _cfg QueryLedgerTip = ledgerTipPoint

  eqQuery QueryLedgerTip QueryLedgerTip = Just Refl

deriving instance Show (Query (SimpleBlock c ext) result)

instance (SimpleCrypto c, Typeable ext)
      => ShowQuery (Query (SimpleBlock c ext)) where
  showResult QueryLedgerTip = show

{-------------------------------------------------------------------------------
  Crypto needed for simple blocks
-------------------------------------------------------------------------------}

class (HashAlgorithm (SimpleHash c), Typeable c) => SimpleCrypto c where
  type family SimpleHash c :: *

data SimpleStandardCrypto
data SimpleMockCrypto

instance SimpleCrypto SimpleStandardCrypto where
  type SimpleHash SimpleStandardCrypto = MD5

instance SimpleCrypto SimpleMockCrypto where
  type SimpleHash SimpleMockCrypto = ShortHash

{-------------------------------------------------------------------------------
  Condense instances
-------------------------------------------------------------------------------}

instance Condense ext' => Condense (Header (SimpleBlock' c ext ext')) where
  condense SimpleHeader{..} = mconcat [
        "("
      , condense simplePrev
      , "->"
      , condense simpleHeaderHash
      , ","
      , condense simpleSlotNo
      , ","
      , condense simpleHeaderExt
      , ")"
      ]
    where
      SimpleStdHeader{..} = simpleHeaderStd

instance Condense ext' => Condense (SimpleBlock' c ext ext') where
  condense SimpleBlock{..} = mconcat [
        "("
      , condense simplePrev
      , "->"
      , condense simpleHeaderHash
      , ","
      , condense simpleSlotNo
      , ","
      , condense simpleHeaderExt
      , ","
      , condense simpleTxs
      , ")"
      ]
    where
      SimpleHeader{..}    = simpleHeader
      SimpleStdHeader{..} = simpleHeaderStd
      SimpleBody{..}      = simpleBody

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance ToCBOR SimpleBody where
  toCBOR = encode

encodeSimpleHeader :: (ext' -> CBOR.Encoding)
                   -> Header (SimpleBlock' c ext ext')
                   -> CBOR.Encoding
encodeSimpleHeader encodeExt SimpleHeader{..} =  mconcat [
      CBOR.encodeListLen 2
    , encode simpleHeaderStd
    , encodeExt simpleHeaderExt
    ]

decodeSimpleHeader :: SimpleCrypto c
                   => (ext' -> CBOR.Encoding)
                   -> (forall s. CBOR.Decoder s ext')
                   -> forall s. CBOR.Decoder s (Header (SimpleBlock' c ext ext'))
decodeSimpleHeader encodeExt decodeExt = do
    CBOR.decodeListLenOf 2
    mkSimpleHeader encodeExt <$> decode <*> decodeExt

-- | Custom 'Serialise' instance that doesn't serialise the hash
instance (SimpleCrypto c, Serialise ext')
      => Serialise (Header (SimpleBlock' c ext ext')) where
  encode = encodeSimpleHeader encode
  decode = decodeSimpleHeader encode decode

simpleBlockBinaryBlockInfo ::
     (SimpleCrypto c, Serialise ext', Typeable ext, Typeable ext')
  => SimpleBlock' c ext ext' -> BinaryBlockInfo
simpleBlockBinaryBlockInfo b = BinaryBlockInfo
    { headerOffset = 2 -- For the 'encodeListLen'
    , headerSize   = fromIntegral $ Lazy.length $ serialise (getHeader b)
    }
