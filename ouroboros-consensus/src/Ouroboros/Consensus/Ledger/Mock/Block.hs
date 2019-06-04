{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Simple block to go with the mock ledger
--
-- None of the definitions in this module depend on, or even refer to, any
-- specific consensus protocols.
module Ouroboros.Consensus.Ledger.Mock.Block (
    SimpleBlock
  , SimpleBlock'(..)
  , SimpleHeader
  , SimpleHeader'(..)
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

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Crypto.Hash
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Mock.Address
import           Ouroboros.Consensus.Ledger.Mock.State
import           Ouroboros.Consensus.Ledger.Mock.UTxO
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Util ((.:))
import           Ouroboros.Consensus.Util.Condense

{-------------------------------------------------------------------------------
  Definition of a block

  The primed versions allow to vary the @ext@ parameter independently of the
  previous block hash.
-------------------------------------------------------------------------------}

type SimpleBlock  c ext = SimpleBlock'  c ext ext
type SimpleHeader c ext = SimpleHeader' c ext ext

data SimpleBlock' c ext ext' = SimpleBlock {
      simpleHeader :: SimpleHeader' c ext ext'
    , simpleBody   :: SimpleBody
    }
  deriving (Generic, Show, Eq)

data SimpleHeader' c ext ext' = SimpleHeader {
      -- | The header hash
      --
      -- This is the hash of the header itself. This is a bit unpleasant,
      -- because it makes the hash look self-referential (when computing the
      -- hash we must ignore the 'simpleHeaderHash' field). However, the benefit
      -- is that we can give a 'HasHeader' instance that does not require
      -- a (static) 'Serialise' instance.
      simpleHeaderHash :: HeaderHash (SimpleHeader' c ext ext')

      -- | Fields required for the 'HasHeader' instance
    , simpleHeaderStd  :: SimpleStdHeader c ext

      -- | Header extension
      --
      -- This extension will be required when using 'SimpleBlock' for specific
      -- consensus protocols.
    , simpleHeaderExt  :: ext'
    }
  deriving (Generic, Show, Eq)

data SimpleStdHeader c ext = SimpleStdHeader {
      simplePrev      :: ChainHash (SimpleHeader c ext)
    , simpleSlotNo    :: SlotNo
    , simpleBlockNo   :: BlockNo
    , simpleBodyHash  :: Hash (SimpleHash c) SimpleBody
    , simpleBlockSize :: Word64
    }
  deriving (Generic, Show, Eq)

data SimpleBody = SimpleBody {
      simpleTxs :: [Tx]
    }
  deriving (Generic, Show, Eq)

{-------------------------------------------------------------------------------
  Working with 'SimpleBlock'
-------------------------------------------------------------------------------}

mkSimpleHeader :: SimpleCrypto c
               => (ext' -> CBOR.Encoding)
               -> SimpleStdHeader c ext
               -> ext'
               -> SimpleHeader' c ext ext'
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
                    => SimpleHeader' c ext ext'
                    -> SimpleBlock'  c ext ext''
                    -> Bool
matchesSimpleHeader SimpleHeader{..} SimpleBlock {..} =
    simpleBodyHash == hash simpleBody
  where
    SimpleStdHeader{..} = simpleHeaderStd

{-------------------------------------------------------------------------------
  HasHeader instance for SimpleHeader
-------------------------------------------------------------------------------}

type instance HeaderHash (SimpleHeader' c ext ext') =
  Hash (SimpleHash c) (SimpleHeader' c ext ext')

instance (SimpleCrypto c, Typeable ext)
      => Measured BlockMeasure (SimpleHeader c ext) where
  measure = blockMeasure

instance (SimpleCrypto c, Typeable ext) => HasHeader (SimpleHeader c ext) where
  blockHash      = simpleHeaderHash
  blockPrevHash  = simplePrev    . simpleHeaderStd
  blockSlot      = simpleSlotNo  . simpleHeaderStd
  blockNo        = simpleBlockNo . simpleHeaderStd
  blockInvariant = const True

instance (SimpleCrypto c, Typeable ext) => StandardHash (SimpleHeader c ext)

{-------------------------------------------------------------------------------
  HasHeader insatnce for SimpleBlock
-------------------------------------------------------------------------------}

type instance HeaderHash (SimpleBlock' c ext ext') =
  Hash (SimpleHash c) (SimpleHeader' c ext ext')

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

instance (SimpleCrypto c, Typeable ext)
      => UpdateLedger (SimpleBlock c ext) where
  newtype LedgerState (SimpleBlock c ext) = SimpleLedgerState {
        simpleLedgerState :: MockState (SimpleBlock c ext)
      }
    deriving (Show)

  data LedgerConfig (SimpleBlock c ext) =
      SimpleLedgerConfig
    deriving (Show)

  type LedgerError  (SimpleBlock c ext) = InvalidInputs

  applyLedgerHeader _cfg =
      updateSimpleLedgerState
  applyLedgerBlock  _cfg b (SimpleLedgerState st) =
      SimpleLedgerState <$> updateMockState b st
  ledgerTipPoint (SimpleLedgerState st) =
      mockTip st

instance (SimpleCrypto c, Typeable ext)
      => LedgerConfigView (SimpleBlock c ext) where
  ledgerConfigView _ = SimpleLedgerConfig

updateSimpleLedgerState :: (Monad m, HasUtxo a)
                        => a
                        -> LedgerState (SimpleBlock c ext)
                        -> ExceptT InvalidInputs
                                   m
                                   (LedgerState (SimpleBlock c ext))
updateSimpleLedgerState b (SimpleLedgerState st) =
    SimpleLedgerState <$> updateMockState b st

genesisSimpleLedgerState :: AddrDist -> LedgerState (SimpleBlock c ext)
genesisSimpleLedgerState = SimpleLedgerState . genesisMockState

{-------------------------------------------------------------------------------
  Support for the mempool
-------------------------------------------------------------------------------}

instance (SimpleCrypto c, Typeable ext)
      => ApplyTx (SimpleBlock c ext) where
  newtype GenTx   (SimpleBlock c ext) = SimpleGenTx { simpleGenTx :: Tx }
  type ApplyTxErr (SimpleBlock c ext) = InvalidInputs

  applyTx            = \_ -> updateSimpleLedgerState
  reapplyTx          = \_ -> updateSimpleLedgerState
  reapplyTxSameState = \_ -> (mustSucceed . runExcept) .: updateSimpleLedgerState
    where
      mustSucceed (Left  _)  = error "reapplyTxSameState: unexpected error"
      mustSucceed (Right st) = st

instance HasUtxo (GenTx (SimpleBlock p c)) where
  txIns      = txIns      . simpleGenTx
  txOuts     = txOuts     . simpleGenTx
  confirmed  = confirmed  . simpleGenTx
  updateUtxo = updateUtxo . simpleGenTx

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

instance Condense ext' => Condense (SimpleHeader' c ext ext') where
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

instance Condense (ChainHash (SimpleHeader' c ext ext')) where
  condense GenesisHash     = "genesis"
  condense (BlockHash hdr) = show hdr

{-------------------------------------------------------------------------------
  Serialise instances
-------------------------------------------------------------------------------}

instance (SimpleCrypto c, Serialise ext') => Serialise (SimpleBlock' c ext ext')
instance (SimpleCrypto c) => Serialise (SimpleStdHeader c ext)
instance Serialise SimpleBody

encodeSimpleHeader :: SimpleCrypto c
                   => (ext' -> CBOR.Encoding)
                   -> SimpleHeader' c ext ext' -> CBOR.Encoding
encodeSimpleHeader encodeExt SimpleHeader{..} =  mconcat [
      CBOR.encodeListLen 2
    , encode simpleHeaderStd
    , encodeExt simpleHeaderExt
    ]

decodeSimpleHeader :: SimpleCrypto c
                   => (ext' -> CBOR.Encoding)
                   -> (forall s. CBOR.Decoder s ext')
                   -> forall s. CBOR.Decoder s (SimpleHeader' c ext ext')
decodeSimpleHeader encodeExt decodeExt = do
    CBOR.decodeListLenOf 2
    mkSimpleHeader encodeExt <$> decode <*> decodeExt

-- | Custom 'Serialise' instance that doesn't serialise the hash
instance (SimpleCrypto c, Serialise ext')
      => Serialise (SimpleHeader' c ext ext') where
  encode = encodeSimpleHeader encode
  decode = decodeSimpleHeader encode decode
