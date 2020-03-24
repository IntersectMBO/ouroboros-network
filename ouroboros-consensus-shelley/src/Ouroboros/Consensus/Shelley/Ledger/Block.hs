{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
module Ouroboros.Consensus.Shelley.Ledger.Block (
    ShelleyHash (..)
  , shelleyHashInfo
  , ShelleyBlock (..)
  , mkShelleyBlock
  , GetHeader (..)
  , Header (..)
  , mkShelleyHeader
  , encodeShelleyBlockWithInfo
  , shelleyAddHeaderEnvelope
    -- * Conversion
  , fromShelleyPrevHash
  , toShelleyPrevHash
    -- * Re-exported for convenience
  , Crypto
  ) where

import           Codec.CBOR.Encoding (Encoding)
import           Codec.Serialise (Serialise (..))
import           Data.Binary (Get, Put)
import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import qualified Data.ByteString.Lazy as Lazy
import           Data.Coerce (coerce)
import           Data.FingerTree.Strict (Measured (..))
import           Data.Proxy (Proxy (..))
import           Data.Word (Word32)
import           GHC.Generics (Generic)

import           Cardano.Binary (FromCBOR (..), ToCBOR (..), serialize)
import qualified Cardano.Crypto.Hash as Crypto
import           Cardano.Prelude (NoUnexpectedThunks (..))

import           Ouroboros.Network.Block (BlockMeasure, ChainHash (..),
                     HasHeader (..), HeaderHash, StandardHash, blockMeasure,
                     castHash)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Storage.ImmutableDB (BinaryInfo (..),
                     HashInfo (..))
import           Ouroboros.Consensus.Util.Condense

import qualified Shelley.Spec.Ledger.BlockChain as SL

import           Ouroboros.Consensus.Shelley.Protocol.Crypto (Crypto, HASH)

{-------------------------------------------------------------------------------
  Header hash
-------------------------------------------------------------------------------}

newtype ShelleyHash c = ShelleyHash {
      unShelleyHash :: SL.HashHeader c
    }
  deriving stock    (Eq, Ord, Show, Generic)
  deriving newtype  (ToCBOR, FromCBOR)
  deriving anyclass NoUnexpectedThunks

instance Crypto c => Serialise (ShelleyHash c) where
  encode = toCBOR
  decode = fromCBOR

instance Condense (ShelleyHash c) where
  condense = show . unShelleyHash

shelleyHashInfo :: forall c. Crypto c => HashInfo (ShelleyHash c)
shelleyHashInfo = HashInfo { hashSize, getHash, putHash }
  where
    hashSize :: Word32
    hashSize = fromIntegral $
      Crypto.byteCount (Proxy :: Proxy (HASH c))

    getHash :: Get (ShelleyHash c)
    getHash = do
      bytes <- Get.getByteString (fromIntegral hashSize)
      return . ShelleyHash . SL.HashHeader $ Crypto.UnsafeHash bytes

    putHash :: ShelleyHash c -> Put
    putHash (ShelleyHash (SL.HashHeader h)) =
      Put.putByteString $ Crypto.getHash h

{-------------------------------------------------------------------------------
  Shelley blocks and headers
-------------------------------------------------------------------------------}

-- | Newtype wrapper to avoid orphan instances
--
-- The phantom type parameter is there to record the additional information
-- we need to work with this block. Most of the code here does not care,
-- but we may need different additional information when running the chain.
data ShelleyBlock c = ShelleyBlock {
      shelleyBlockRaw        :: !(SL.Block c)
    , shelleyBlockHeaderHash :: !(ShelleyHash c)
    }
  deriving (Eq, Show)

type instance HeaderHash (ShelleyBlock c) = ShelleyHash c

mkShelleyBlock :: Crypto c => SL.Block c -> ShelleyBlock c
mkShelleyBlock raw = ShelleyBlock {
      shelleyBlockRaw        = raw
    , shelleyBlockHeaderHash = ShelleyHash (SL.bhHash (SL.bheader raw))
    }

instance Crypto c => GetHeader (ShelleyBlock c) where
  data Header (ShelleyBlock c) = ShelleyHeader {
        shelleyHeaderRaw  :: !(SL.BHeader c)
      , shelleyHeaderHash :: !(ShelleyHash c)
      }
    deriving (Eq, Generic, Show, NoUnexpectedThunks)

  getHeader (ShelleyBlock rawBlk hdrHash) = ShelleyHeader {
      shelleyHeaderRaw  = SL.bheader rawBlk
    , shelleyHeaderHash = hdrHash
    }

mkShelleyHeader :: Crypto c => SL.BHeader c -> Header (ShelleyBlock c)
mkShelleyHeader raw = ShelleyHeader {
      shelleyHeaderRaw  = raw
    , shelleyHeaderHash = ShelleyHash (SL.bhHash raw)
    }

instance Crypto c => HasHeader (ShelleyBlock c)  where
  blockHash      =            blockHash     . getHeader
  blockPrevHash  = castHash . blockPrevHash . getHeader
  blockSlot      =            blockSlot     . getHeader
  blockNo        =            blockNo       . getHeader
  blockInvariant = const True

instance Crypto c => HasHeader (Header (ShelleyBlock c)) where
  blockHash      = shelleyHeaderHash

  blockPrevHash  =
      fromShelleyPrevHash
    . SL.bheaderPrev
    . SL.bhbody
    . shelleyHeaderRaw

  blockSlot      =            SL.bheaderSlotNo  . SL.bhbody . shelleyHeaderRaw
  blockNo        = coerce .   SL.bheaderBlockNo . SL.bhbody . shelleyHeaderRaw
  blockInvariant = const True

instance Crypto c => Measured BlockMeasure (ShelleyBlock c) where
  measure = blockMeasure

instance Crypto c => StandardHash (ShelleyBlock c)

instance Crypto c => HasAnnTip (ShelleyBlock c)

instance Crypto c => ValidateEnvelope (ShelleyBlock c)

{-------------------------------------------------------------------------------
  Conversions
-------------------------------------------------------------------------------}

-- | From @cardano-ledger-specs@ to @ouroboros-consensus@
fromShelleyPrevHash :: SL.PrevHash c -> ChainHash (Header (ShelleyBlock c))
fromShelleyPrevHash SL.GenesisHash   = GenesisHash
fromShelleyPrevHash (SL.BlockHash h) = BlockHash (ShelleyHash h)

-- | From @ouroboros-consensus@ to @cardano-ledger-specs@
toShelleyPrevHash :: ChainHash (Header (ShelleyBlock c)) -> SL.PrevHash c
toShelleyPrevHash GenesisHash                 = SL.GenesisHash
toShelleyPrevHash (BlockHash (ShelleyHash h)) = SL.BlockHash h

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance Crypto c => ToCBOR (ShelleyBlock c) where
  -- Don't encode the header hash, we recompute it during deserialisation
  toCBOR = toCBOR . shelleyBlockRaw

instance Crypto c => FromCBOR (ShelleyBlock c) where
  fromCBOR = mkShelleyBlock <$> fromCBOR

instance Crypto c => ToCBOR (Header (ShelleyBlock c)) where
  -- Don't encode the header hash, we recompute it during deserialisation
  toCBOR = toCBOR . shelleyHeaderRaw

instance Crypto c => FromCBOR (Header (ShelleyBlock c)) where
  fromCBOR = mkShelleyHeader <$> fromCBOR

encodeShelleyBlockWithInfo :: Crypto c => ShelleyBlock c -> BinaryInfo Encoding
encodeShelleyBlockWithInfo blk = BinaryInfo {
      binaryBlob   = toCBOR blk
      -- See 'shelleyAddHeaderEnvelope'
    , headerOffset = 0
      -- TODO When we have annotations in the Shelley decoders, this will
      -- become much cheaper
    , headerSize   = fromIntegral $ Lazy.length (serialize (getHeader blk))
    }

-- | When given the raw header bytes extracted from the block (see
-- 'encodeShelleyBlockWithInfo'), we still need to prepend the correct list
-- length so that we can decode the header with 'fromCBOR':
--
-- > > putStrLn $ prettyHexEnc $ toCBOR blk
-- > 93  # list(19)
-- >    44 ec 7f 7e 7b  # bytes(4)
-- >    1a 20 28 e0 2b  # int(539549739)
-- >    ...
-- >         0a  # integer(10)
-- >    80  # list(0)
-- >    80  # list(0)
-- >    a0  # map(0)
--
-- > > putStrLn $ prettyHexEnc $ toCBOR (getHeader blk)
-- > 90  # list(16)
-- >    44 ec 7f 7e 7b  # bytes(4)
-- >    1a 20 28 e0 2b  # int(539549739)
-- >    ...
-- >          0a  # integer(10)
--
-- Note that the block contains three fields more than the header. For some
-- reason, the header is not nested in the block, but its \"list\"
-- ('encodeListLen') is extended with the three extra fields.
--
-- This function will be given the bytestring corresponding to the encoding of
-- the block with but without the three extra fields that are not part of the
-- header. This function will drop the first byte (\"list length of 19\") and
-- prepend a byte representing a \"list length of 16\" and leave the remainder
-- of the bytestring untouched.
shelleyAddHeaderEnvelope :: Lazy.ByteString -> Lazy.ByteString
shelleyAddHeaderEnvelope = Lazy.cons 0x90 . Lazy.drop 1

{-------------------------------------------------------------------------------
  Condense
-------------------------------------------------------------------------------}

instance Crypto c => Condense (ShelleyBlock c) where
  condense = show . shelleyBlockRaw
