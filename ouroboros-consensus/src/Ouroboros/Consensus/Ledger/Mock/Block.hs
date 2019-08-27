{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Simple block to go with the mock ledger
--
-- None of the definitions in this module depend on, or even refer to, any
-- specific consensus protocols.
module Ouroboros.Consensus.Ledger.Mock.Block (
    SimpleBlock
  , SimpleHeader
  , SimpleBlock'(..)
  , Header(..)
  , SimpleStdHeader(..)
  , SimpleBody(..)
    -- * Working with 'SimpleBlock'
  , mkSimpleHeader
  , matchesSimpleHeader
    -- * 'UpdateLedger'
  , LedgerState(..)
  , LedgerConfig(..)
  , updateSimpleLedgerState
  , genesisSimpleLedgerState
    -- * 'ApplyTx' (mempool support)
  , GenTx(..)
  , GenTxId(..)
  , mkSimpleGenTx
    -- * Crypto
  , SimpleCrypto
  , SimpleStandardCrypto
  , SimpleMockCrypto
    -- * Serialisation
  , encodeSimpleHeader
  , decodeSimpleHeader
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import           Codec.Serialise (Serialise (..))
import           Control.Monad.Except
import           Data.FingerTree (Measured (..))
import           Data.Typeable (Typeable)
import           Data.Word
import           GHC.Generics (Generic)

import           Cardano.Binary (ToCBOR (..))
import           Cardano.Crypto.Hash

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Mock.Address
import           Ouroboros.Consensus.Ledger.Mock.State
import           Ouroboros.Consensus.Ledger.Mock.UTxO
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Util ((.:))
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()

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

instance GetHeader (SimpleBlock' c ext ext') where
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
    deriving (Generic, Show, Eq)

  getHeader = simpleHeader

data SimpleStdHeader c ext = SimpleStdHeader {
      simplePrev      :: ChainHash (SimpleBlock c ext)
    , simpleSlotNo    :: SlotNo
    , simpleBlockNo   :: BlockNo
    , simpleBodyHash  :: Hash (SimpleHash c) SimpleBody
    , simpleBlockSize :: Word64
    }
  deriving stock    (Generic, Show, Eq)
  deriving anyclass (Serialise)

data SimpleBody = SimpleBody {
      simpleTxs :: [Tx]
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
        simpleHeaderHash = hashWithSerialiser
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
    simpleBodyHash == hash simpleBody
  where
    SimpleStdHeader{..} = simpleHeaderStd

{-------------------------------------------------------------------------------
  HasHeader instance for SimpleHeader
-------------------------------------------------------------------------------}

instance (SimpleCrypto c, Typeable ext) => HasHeader (SimpleHeader c ext) where
  blockHash      =            simpleHeaderHash
  blockPrevHash  = castHash . simplePrev    . simpleHeaderStd
  blockSlot      =            simpleSlotNo  . simpleHeaderStd
  blockNo        =            simpleBlockNo . simpleHeaderStd
  blockInvariant = const True

{-------------------------------------------------------------------------------
  HasHeader insatnce for SimpleBlock
-------------------------------------------------------------------------------}

type instance HeaderHash (SimpleBlock' c ext ext') =
  Hash (SimpleHash c) (Header (SimpleBlock' c ext ext'))

instance (SimpleCrypto c, Typeable ext)
      => Measured BlockMeasure (SimpleBlock c ext) where
  measure = blockMeasure

instance (SimpleCrypto c, Typeable ext) => HasHeader (SimpleBlock c ext) where
  blockHash      =            blockHash     . simpleHeader
  blockPrevHash  = castHash . blockPrevHash . simpleHeader
  blockSlot      =            blockSlot     . simpleHeader
  blockNo        =            blockNo       . simpleHeader
  blockInvariant = const True

instance (SimpleCrypto c, Typeable ext) => StandardHash (SimpleBlock c ext)

{-------------------------------------------------------------------------------
  HasUTxO instance
-------------------------------------------------------------------------------}

instance HasUtxo (SimpleBlock' c ext ext') where
  txIns      = txIns      . simpleBody
  txOuts     = txOuts     . simpleBody
  confirmed  = confirmed  . simpleBody
  updateUtxo = updateUtxo . simpleBody

instance HasUtxo SimpleBody where
  txIns      = txIns      . simpleTxs
  txOuts     = txOuts     . simpleTxs
  confirmed  = confirmed  . simpleTxs
  updateUtxo = updateUtxo . simpleTxs

{-------------------------------------------------------------------------------
  Update the ledger
-------------------------------------------------------------------------------}

instance (SimpleCrypto c, Typeable ext, SupportedBlock (SimpleBlock c ext))
      => UpdateLedger (SimpleBlock c ext) where
  newtype LedgerState (SimpleBlock c ext) = SimpleLedgerState {
        simpleLedgerState :: MockState (SimpleBlock c ext)
      }
    deriving stock   (Generic, Show, Eq)
    deriving newtype (Serialise)

  data LedgerConfig (SimpleBlock c ext) =
      SimpleLedgerConfig
    deriving (Show)

  type LedgerError (SimpleBlock c ext) = MockError (SimpleBlock c ext)

  applyChainTick _ _ = return
  applyLedgerBlock _cfg = updateSimpleLedgerState
  reapplyLedgerBlock _cfg = (mustSucceed . runExcept) .: updateSimpleLedgerState
    where
      mustSucceed (Left  err) = error ("reapplyLedgerBlock: unexpected error: " <> show err)
      mustSucceed (Right st)  = st
  ledgerTipPoint (SimpleLedgerState st) = mockTip st
  ledgerConfigView _ = SimpleLedgerConfig

updateSimpleLedgerState :: (Monad m, SimpleCrypto c, Typeable ext)
                        => SimpleBlock c ext
                        -> LedgerState (SimpleBlock c ext)
                        -> ExceptT (MockError (SimpleBlock c ext))
                                   m
                                   (LedgerState (SimpleBlock c ext))
updateSimpleLedgerState b (SimpleLedgerState st) =
    SimpleLedgerState <$> updateMockState b st

updateSimpleUTxO :: (Monad m, HasUtxo a)
                 => a
                 -> LedgerState (SimpleBlock c ext)
                 -> ExceptT (MockError (SimpleBlock c ext))
                            m
                            (LedgerState (SimpleBlock c ext))
updateSimpleUTxO b (SimpleLedgerState st) =
    SimpleLedgerState <$> updateMockUTxO b st

genesisSimpleLedgerState :: AddrDist -> LedgerState (SimpleBlock c ext)
genesisSimpleLedgerState = SimpleLedgerState . genesisMockState

{-------------------------------------------------------------------------------
  Support for the mempool
-------------------------------------------------------------------------------}

instance (SimpleCrypto c, Typeable ext, SupportedBlock (SimpleBlock c ext))
      => ApplyTx (SimpleBlock c ext) where
  data GenTx (SimpleBlock c ext) = SimpleGenTx
    { simpleGenTx   :: Tx
    , simpleGenTxId :: TxId
      -- ^ This field is lazy on purpose so that the TxId is computed on
      -- demand.
    } deriving stock    (Generic)
      deriving anyclass (Serialise)

  newtype GenTxId (SimpleBlock c ext) = SimpleGenTxId
    { unSimpleGenTxId :: TxId
    } deriving stock   (Generic)
      deriving newtype (Show, Eq, Ord, Serialise)

  txId = SimpleGenTxId . simpleGenTxId

  txSize _ = 2000  -- TODO #745

  type ApplyTxErr (SimpleBlock c ext) = MockError (SimpleBlock c ext)

  applyTx            = \_ -> updateSimpleUTxO
  reapplyTx          = \_ -> updateSimpleUTxO
  reapplyTxSameState = \_ -> (mustSucceed . runExcept) .: updateSimpleUTxO
    where
      mustSucceed (Left  _)  = error "reapplyTxSameState: unexpected error"
      mustSucceed (Right st) = st

instance HasUtxo (GenTx (SimpleBlock p c)) where
  txIns      = txIns      . simpleGenTx
  txOuts     = txOuts     . simpleGenTx
  confirmed  = confirmed  . simpleGenTx
  updateUtxo = updateUtxo . simpleGenTx

instance Condense (GenTx (SimpleBlock p c)) where
    condense = condense . simpleGenTx

instance Show (GenTx (SimpleBlock p c)) where
    show = show . simpleGenTx

instance Condense (GenTxId (SimpleBlock p c)) where
    condense = condense . unSimpleGenTxId

mkSimpleGenTx :: Tx -> GenTx (SimpleBlock c ext)
mkSimpleGenTx tx = SimpleGenTx
    { simpleGenTx   = tx
    , simpleGenTxId = hash tx
    }

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

instance Condense (ChainHash (SimpleBlock' c ext ext')) where
  condense GenesisHash     = "genesis"
  condense (BlockHash hdr) = show hdr

{-------------------------------------------------------------------------------
  Serialise instances
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
