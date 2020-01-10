{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTSyntax                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Ouroboros.Consensus.Ledger.Shelley.Block where

import           BlockChain (BHBody (..), BHeader (..), Block (..), HashHeader,
                     HashHeader (..), bhHash, bhbody, bheader)
import           Cardano.Binary (FromCBOR (..), ToCBOR (..), serialize)
import qualified Cardano.Crypto.Hash as Crypto
import           Cardano.Prelude (NoUnexpectedThunks (..))
import qualified Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import           Codec.Serialise (Serialise (..))
import           Data.Binary (Get, Put)
import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import qualified Data.ByteString.Lazy as BSL
import           Data.Coerce (coerce)
import           Data.FingerTree.Strict (Measured (..))
import           Data.Proxy (Proxy (..))
import           Data.Typeable (typeRep)
import           Data.Word (Word32)
import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Protocol.TPraos
import           Ouroboros.Consensus.Protocol.TPraos.Crypto (HASH)
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Network.Block
import           Ouroboros.Storage.ImmutableDB (BinaryInfo (..), HashInfo (..))

{-------------------------------------------------------------------------------
  Header hash
-------------------------------------------------------------------------------}

newtype ShelleyHash = ShelleyHash
  { unShelleyHash :: HashHeader TPraosStandardCrypto }
  deriving stock   (Eq, Ord, Show, Generic)
  deriving newtype (ToCBOR, FromCBOR)
  deriving anyclass NoUnexpectedThunks

instance Serialise ShelleyHash where
  encode = toCBOR
  decode = fromCBOR

instance Condense ShelleyHash where
  condense = show . unShelleyHash

shelleyHashInfo :: HashInfo ShelleyHash
shelleyHashInfo = HashInfo { hashSize, getHash, putHash }
  where
    hashSize :: Word32
    hashSize = fromIntegral
      $ Crypto.byteCount (Proxy :: Proxy (HASH TPraosStandardCrypto))

    getHash :: Get ShelleyHash
    getHash = do
      bytes <- Get.getByteString (fromIntegral hashSize)
      return . ShelleyHash . HashHeader $ Crypto.UnsafeHash bytes

    putHash :: ShelleyHash -> Put
    putHash (ShelleyHash (HashHeader h)) =
      Put.putByteString $ Crypto.getHash h

{-------------------------------------------------------------------------------
  Shelley blocks and headers
-------------------------------------------------------------------------------}

-- | Newtype wrapper to avoid orphan instances
--
-- The phantom type parameter is there to record the additional information
-- we need to work with this block. Most of the code here does not care,
-- but we may need different additional information when running the chain.
newtype ShelleyBlock = ShelleyBlock
  { unShelleyBlock :: Block TPraosStandardCrypto
  }
  deriving (Eq, Show)
  deriving newtype (ToCBOR, FromCBOR)

instance GetHeader ShelleyBlock where
  data Header ShelleyBlock = ShelleyHeader
    { shelleyHeader :: !(BHeader TPraosStandardCrypto)
      -- Cached hash
    , shelleyHeaderHash :: ShelleyHash
    } deriving (Eq, Show)

  getHeader (ShelleyBlock b) = ShelleyHeader
    { shelleyHeader     = bheader b
    , shelleyHeaderHash = ShelleyHash . bhHash . bheader $ b
    }

instance NoUnexpectedThunks (Header ShelleyBlock) where
  showTypeOf _ = show $ typeRep (Proxy @(Header ShelleyBlock ))

-- We explicitly allow the hash to be a thunk
  whnfNoUnexpectedThunks ctxt (ShelleyHeader hdr _hash) =
    noUnexpectedThunks ctxt hdr

type instance HeaderHash ShelleyBlock = ShelleyHash

instance HasHeader ShelleyBlock  where
  blockHash      = blockHash . getHeader
  blockPrevHash  = castHash . blockPrevHash . getHeader
  blockSlot      = blockSlot . getHeader
  blockNo        = blockNo . getHeader
  blockInvariant = const True

instance HasHeader (Header ShelleyBlock) where
  blockHash = shelleyHeaderHash

  blockPrevHash =
    BlockHash . ShelleyHash . bheaderPrev . bhbody . shelleyHeader

  blockSlot      = bheaderSlotNo . bhbody . shelleyHeader
  blockNo        = coerce . bheaderBlockNo . bhbody . shelleyHeader
  blockInvariant = const True

instance Measured BlockMeasure ShelleyBlock where
  measure = blockMeasure

instance StandardHash ShelleyBlock

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance ToCBOR (Header ShelleyBlock) where
  toCBOR ShelleyHeader{shelleyHeader, shelleyHeaderHash} =
    mconcat
      [ CBOR.encodeListLen 2
      , toCBOR shelleyHeader
      , toCBOR shelleyHeaderHash
      ]
instance FromCBOR (Header ShelleyBlock) where
  fromCBOR = do
    CBOR.decodeListLenOf 2
    ShelleyHeader
      <$> fromCBOR
      <*> fromCBOR

encodeShelleyBlockWithInfo :: ShelleyBlock -> BinaryInfo Encoding
encodeShelleyBlockWithInfo blk@(ShelleyBlock (Block h _)) = BinaryInfo
  { binaryBlob = toCBOR blk
  , headerOffset = 1 -- 'encodeListLen' of the block: header + body + wits
    -- TODO When we have annotations in the Shelley decoders, we will not
    -- need to do this
  , headerSize = fromIntegral $ BSL.length (serialize h)
  }

{-------------------------------------------------------------------------------
  Condense
-------------------------------------------------------------------------------}

instance Condense ShelleyBlock where
  condense (ShelleyBlock blk) = show blk
