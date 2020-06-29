{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
module Ouroboros.Consensus.Shelley.Ledger.Block (
    ShelleyHash (..)
  , ShelleyBlock (..)
  , mkShelleyBlock
  , GetHeader (..)
  , Header (..)
  , mkShelleyHeader
  , NestedCtxt_(..)
    -- * Serialisation
  , encodeShelleyBlock
  , decodeShelleyBlock
  , shelleyBinaryBlockInfo
  , encodeShelleyHeader
  , decodeShelleyHeader
    -- * Conversion
  , fromShelleyPrevHash
  , toShelleyPrevHash
    -- * Re-exported for convenience
  , Crypto
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Codec.Serialise (Serialise (..))
import qualified Data.ByteString.Lazy as Lazy
import           Data.Coerce (coerce)
import           Data.FingerTree.Strict (Measured (..))
import           Data.Proxy (Proxy (..))
import           GHC.Generics (Generic)

import           Cardano.Binary (Annotator (..), FromCBOR (..),
                     FullByteString (..), ToCBOR (..), serialize)
import qualified Cardano.Crypto.Hash as Crypto
import           Cardano.Prelude (NoUnexpectedThunks (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Storage.Common (BinaryBlockInfo (..))
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

instance Crypto c => ConvertRawHash (ShelleyBlock c) where
  toRawHash   _ = Crypto.getHash . SL.unHashHeader . unShelleyHash
  fromRawHash _ = ShelleyHash . SL.HashHeader . Crypto.UnsafeHash
  hashSize    _ = fromIntegral $ Crypto.sizeHash (Proxy @(HASH c))

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

  blockMatchesHeader hdr blk =
      -- Compute the hash the body of the block (the transactions) and compare
      -- that against the hash of the body stored in the header.
      SL.bbHash txs == SL.bhash hdrBody
    where
      ShelleyHeader { shelleyHeaderRaw = SL.BHeader hdrBody _ } = hdr
      ShelleyBlock  { shelleyBlockRaw  = SL.Block _ txs }       = blk

  headerIsEBB = const Nothing

mkShelleyHeader :: Crypto c => SL.BHeader c -> Header (ShelleyBlock c)
mkShelleyHeader raw = ShelleyHeader {
      shelleyHeaderRaw  = raw
    , shelleyHeaderHash = ShelleyHash (SL.bhHash raw)
    }

instance Crypto c => HasHeader (ShelleyBlock c)  where
  getHeaderFields = getBlockHeaderFields

instance Crypto c => HasHeader (Header (ShelleyBlock c)) where
  getHeaderFields hdr = HeaderFields {
      headerFieldHash    = shelleyHeaderHash hdr
    , headerFieldSlot    =          SL.bheaderSlotNo  . SL.bhbody . shelleyHeaderRaw $ hdr
    , headerFieldBlockNo = coerce . SL.bheaderBlockNo . SL.bhbody . shelleyHeaderRaw $ hdr
    }

instance Crypto c => GetPrevHash (ShelleyBlock c)  where
  headerPrevHash _cfg =
      fromShelleyPrevHash
    . SL.bheaderPrev
    . SL.bhbody
    . shelleyHeaderRaw

instance Crypto c => Measured BlockMeasure (ShelleyBlock c) where
  measure = blockMeasure

instance Crypto c => StandardHash (ShelleyBlock c)

instance Crypto c => HasAnnTip (ShelleyBlock c)

-- The 'ValidateEnvelope' instance lives in the
-- "Ouroboros.Consensus.Shelley.Ledger.Ledger" module because of the
-- dependency on the 'LedgerConfig'.

{-------------------------------------------------------------------------------
  Conversions
-------------------------------------------------------------------------------}

-- | From @cardano-ledger-specs@ to @ouroboros-consensus@
fromShelleyPrevHash :: SL.PrevHash c -> ChainHash (ShelleyBlock c)
fromShelleyPrevHash SL.GenesisHash   = GenesisHash
fromShelleyPrevHash (SL.BlockHash h) = BlockHash (ShelleyHash h)

-- | From @ouroboros-consensus@ to @cardano-ledger-specs@
toShelleyPrevHash :: ChainHash (Header (ShelleyBlock c)) -> SL.PrevHash c
toShelleyPrevHash GenesisHash                 = SL.GenesisHash
toShelleyPrevHash (BlockHash (ShelleyHash h)) = SL.BlockHash h

{-------------------------------------------------------------------------------
  NestedCtxt
-------------------------------------------------------------------------------}

data instance NestedCtxt_ (ShelleyBlock c) f a where
  CtxtShelley :: NestedCtxt_ (ShelleyBlock c) f (f (ShelleyBlock c))

deriving instance Show (NestedCtxt_ (ShelleyBlock c) f a)

instance TrivialDependency (NestedCtxt_ (ShelleyBlock c) f) where
  type TrivialIndex (NestedCtxt_ (ShelleyBlock c) f) = f (ShelleyBlock c)
  hasSingleIndex CtxtShelley CtxtShelley = Refl
  indexIsTrivial = CtxtShelley

instance SameDepIndex (NestedCtxt_ (ShelleyBlock c) f)
instance HasNestedContent f (ShelleyBlock c)

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance Crypto c => ToCBOR (ShelleyBlock c) where
  -- Don't encode the header hash, we recompute it during deserialisation
  toCBOR = toCBOR . shelleyBlockRaw

instance Crypto c => FromCBOR (Annotator (ShelleyBlock c)) where
  fromCBOR = fmap mkShelleyBlock <$> fromCBOR

instance Crypto c => ToCBOR (Header (ShelleyBlock c)) where
  -- Don't encode the header hash, we recompute it during deserialisation
  toCBOR = toCBOR . shelleyHeaderRaw

instance Crypto c => FromCBOR (Annotator (Header (ShelleyBlock c))) where
  fromCBOR = fmap mkShelleyHeader <$> fromCBOR

encodeShelleyBlock :: Crypto c => ShelleyBlock c -> Encoding
encodeShelleyBlock = toCBOR

decodeShelleyBlock :: Crypto c => Decoder s (Lazy.ByteString -> ShelleyBlock c)
decodeShelleyBlock = (. Full) . runAnnotator <$> fromCBOR

shelleyBinaryBlockInfo :: Crypto c => ShelleyBlock c -> BinaryBlockInfo
shelleyBinaryBlockInfo blk = BinaryBlockInfo {
      -- Drop the 'encodeListLen' that precedes the header and the body (= tx
      -- seq)
      headerOffset = 1
      -- The Shelley decoders use annotations, so this is cheap
    , headerSize   = fromIntegral $ Lazy.length (serialize (getHeader blk))
    }

encodeShelleyHeader :: Crypto c => Header (ShelleyBlock c) -> Encoding
encodeShelleyHeader = toCBOR

decodeShelleyHeader :: Crypto c => Decoder s (Lazy.ByteString -> Header (ShelleyBlock c))
decodeShelleyHeader = (. Full) . runAnnotator <$> fromCBOR

{-------------------------------------------------------------------------------
  Condense
-------------------------------------------------------------------------------}

instance Crypto c => Condense (ShelleyBlock c) where
  condense = show . shelleyBlockRaw

instance Crypto c => Condense (Header (ShelleyBlock c)) where
  condense = show . shelleyHeaderRaw
