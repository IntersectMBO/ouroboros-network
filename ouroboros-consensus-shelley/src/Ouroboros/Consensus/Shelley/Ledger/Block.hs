{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
module Ouroboros.Consensus.Shelley.Ledger.Block (
    GetHeader (..)
  , Header (..)
  , NestedCtxt_ (..)
  , ShelleyBasedEra
  , ShelleyBlock (..)
  , ShelleyHash (..)
    -- * Shelley Compatibility
  , ShelleyCompatible
  , mkShelleyBlock
  , mkShelleyHeader
    -- * Serialisation
  , decodeShelleyBlock
  , decodeShelleyHeader
  , encodeShelleyBlock
  , encodeShelleyHeader
  , shelleyBinaryBlockInfo
    -- * Conversion
  , fromShelleyPrevHash
  , toShelleyPrevHash
  ) where

import qualified Data.ByteString.Lazy as Lazy
import           Data.Coerce (coerce)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks (..))

import qualified Cardano.Crypto.Hash as Crypto

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Storage.Common (BinaryBlockInfo (..))
import           Ouroboros.Consensus.Util (ShowProxy (..), hashFromBytesShortE)
import           Ouroboros.Consensus.Util.Condense

import           Cardano.Ledger.Binary (Annotator (..), DecCBOR (..),
                     EncCBOR (..), FullByteString (..), serialize,
                     toPlainDecoder)
import qualified Cardano.Ledger.Binary.Plain as Plain
import           Cardano.Ledger.Core as SL (eraProtVerLow, toEraCBOR)
import           Cardano.Ledger.Crypto (HASH)
import qualified Cardano.Ledger.Era as SL (hashTxSeq)
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Protocol.TPraos.BHeader as SL

import           Ouroboros.Consensus.HardFork.Combinator
                     (HasPartialConsensusConfig)
import           Ouroboros.Consensus.Protocol.Abstract (ChainDepState,
                     SelectView)
import           Ouroboros.Consensus.Protocol.Praos.Common
                     (PraosChainSelectView)
import           Ouroboros.Consensus.Protocol.Signed (SignedHeader)
import           Ouroboros.Consensus.Shelley.Eras
import           Ouroboros.Consensus.Shelley.Protocol.Abstract (ProtoCrypto,
                     ProtocolHeaderSupportsEnvelope (pHeaderPrevHash),
                     ProtocolHeaderSupportsProtocol (CannotForgeError),
                     ShelleyHash (ShelleyHash, unShelleyHash), ShelleyProtocol,
                     ShelleyProtocolHeader, pHeaderBlock, pHeaderBodyHash,
                     pHeaderHash, pHeaderSlot)
import           Ouroboros.Consensus.Storage.Serialisation (DecodeDisk,
                     EncodeDisk)

{-------------------------------------------------------------------------------
  ShelleyCompatible
-------------------------------------------------------------------------------}
class
  ( ShelleyBasedEra era
  , ShelleyProtocol proto
    -- Header constraints
  , Eq (ShelleyProtocolHeader proto)
  , Show (ShelleyProtocolHeader proto)
  , NoThunks (ShelleyProtocolHeader proto)
  , EncCBOR (ShelleyProtocolHeader proto)
  , DecCBOR (Annotator (ShelleyProtocolHeader proto))
  , Show (CannotForgeError proto)
    -- Currently the chain select view is identical
  , SelectView proto ~ PraosChainSelectView (EraCrypto era)
    -- Need to be able to sign the protocol header
  , SignedHeader (ShelleyProtocolHeader proto)
    -- ChainDepState needs to be serialisable
  , DecodeDisk (ShelleyBlock proto era) (ChainDepState proto)
  , EncodeDisk (ShelleyBlock proto era) (ChainDepState proto)
    -- Era and proto crypto must coincide
  , EraCrypto era ~ ProtoCrypto proto
    -- Hard-fork related constraints
  , HasPartialConsensusConfig proto
  , DecCBOR (SL.PState (ProtoCrypto proto))
  ) => ShelleyCompatible proto era

instance ShelleyCompatible proto era => ConvertRawHash (ShelleyBlock proto era) where
  toShortRawHash   _ = Crypto.hashToBytesShort . unShelleyHash
  fromShortRawHash _ = ShelleyHash . hashFromBytesShortE
  hashSize         _ = fromIntegral $ Crypto.sizeHash (Proxy @(HASH (EraCrypto era)))

{-------------------------------------------------------------------------------
  Shelley blocks and headers
-------------------------------------------------------------------------------}

-- | Shelley-based block type.
--
-- This block is parametrised over both the (ledger) era and the protocol.
data ShelleyBlock proto era = ShelleyBlock {
      shelleyBlockRaw        :: !(SL.Block (ShelleyProtocolHeader proto) era)
    , shelleyBlockHeaderHash :: !(ShelleyHash (ProtoCrypto proto))
    }

deriving instance ShelleyCompatible proto era => Show (ShelleyBlock proto era)
deriving instance ShelleyCompatible proto era => Eq   (ShelleyBlock proto era)

instance (Typeable era, Typeable proto)
  => ShowProxy (ShelleyBlock proto era) where

type instance HeaderHash (ShelleyBlock proto era) = ShelleyHash (ProtoCrypto proto)

mkShelleyBlock ::
     ShelleyCompatible proto era
  => SL.Block (ShelleyProtocolHeader proto) era
  -> ShelleyBlock proto era
mkShelleyBlock raw = ShelleyBlock {
      shelleyBlockRaw        = raw
    , shelleyBlockHeaderHash = pHeaderHash $ SL.bheader raw
    }

data instance Header (ShelleyBlock proto era) = ShelleyHeader {
      shelleyHeaderRaw  :: !(ShelleyProtocolHeader proto)
    , shelleyHeaderHash :: !(ShelleyHash (ProtoCrypto proto))
    }
  deriving (Generic)

deriving instance ShelleyCompatible proto era => Show     (Header (ShelleyBlock proto era))
deriving instance ShelleyCompatible proto era => Eq       (Header (ShelleyBlock proto era))
deriving instance ShelleyCompatible proto era => NoThunks (Header (ShelleyBlock proto era))

instance (Typeable era, Typeable proto)
  => ShowProxy (Header (ShelleyBlock proto era)) where

instance ShelleyCompatible proto era => GetHeader (ShelleyBlock proto era) where
  getHeader (ShelleyBlock rawBlk hdrHash) = ShelleyHeader {
      shelleyHeaderRaw  = SL.bheader rawBlk
    , shelleyHeaderHash = hdrHash
    }

  blockMatchesHeader hdr blk =
      -- Compute the hash the body of the block (the transactions) and compare
      -- that against the hash of the body stored in the header.
      SL.hashTxSeq @era txs == pHeaderBodyHash shelleyHdr
    where
      ShelleyHeader { shelleyHeaderRaw = shelleyHdr }     = hdr
      ShelleyBlock  { shelleyBlockRaw  = SL.Block _ txs } = blk

  headerIsEBB = const Nothing

mkShelleyHeader ::
     ShelleyCompatible proto era
  => ShelleyProtocolHeader proto
  -> Header (ShelleyBlock proto era)
mkShelleyHeader raw = ShelleyHeader {
      shelleyHeaderRaw  = raw
    , shelleyHeaderHash = pHeaderHash raw
    }

instance ShelleyCompatible proto era => HasHeader (ShelleyBlock proto era)  where
  getHeaderFields = getBlockHeaderFields

instance ShelleyCompatible proto era => HasHeader (Header (ShelleyBlock proto era)) where
  getHeaderFields hdr = HeaderFields {
      headerFieldHash    = pHeaderHash . shelleyHeaderRaw $ hdr
    , headerFieldSlot    = pHeaderSlot . shelleyHeaderRaw $ hdr
    , headerFieldBlockNo = coerce . pHeaderBlock . shelleyHeaderRaw $ hdr
    }

instance ShelleyCompatible proto era => GetPrevHash (ShelleyBlock proto era) where
  headerPrevHash =
      fromShelleyPrevHash
    . pHeaderPrevHash
    . shelleyHeaderRaw

instance ShelleyCompatible proto era => StandardHash (ShelleyBlock proto era)

instance ShelleyCompatible proto era => HasAnnTip (ShelleyBlock proto era)

-- The 'ValidateEnvelope' instance lives in the
-- "Ouroboros.Consensus.Shelley.Ledger.Ledger" module because of the
-- dependency on the 'LedgerConfig'.

{-------------------------------------------------------------------------------
  Conversions
-------------------------------------------------------------------------------}

-- | From @cardano-ledger-specs@ to @ouroboros-consensus@
fromShelleyPrevHash :: EraCrypto era ~ ProtoCrypto proto =>
  SL.PrevHash (EraCrypto era) -> ChainHash (ShelleyBlock proto era)
fromShelleyPrevHash SL.GenesisHash   = GenesisHash
fromShelleyPrevHash (SL.BlockHash h) = BlockHash (ShelleyHash $ SL.unHashHeader h)

-- | From @ouroboros-consensus@ to @cardano-ledger-specs@
toShelleyPrevHash :: EraCrypto era ~ ProtoCrypto proto =>
  ChainHash (Header (ShelleyBlock proto era)) -> SL.PrevHash (EraCrypto era)
toShelleyPrevHash GenesisHash                 = SL.GenesisHash
toShelleyPrevHash (BlockHash (ShelleyHash h)) = SL.BlockHash $ SL.HashHeader h

{-------------------------------------------------------------------------------
  NestedCtxt
-------------------------------------------------------------------------------}

data instance NestedCtxt_ (ShelleyBlock proto era) f a where
  CtxtShelley :: NestedCtxt_ (ShelleyBlock proto era) f (f (ShelleyBlock proto era))

deriving instance Show (NestedCtxt_ (ShelleyBlock proto era) f a)

instance TrivialDependency (NestedCtxt_ (ShelleyBlock proto era) f) where
  type TrivialIndex (NestedCtxt_ (ShelleyBlock proto era) f) = f (ShelleyBlock proto era)
  hasSingleIndex CtxtShelley CtxtShelley = Refl
  indexIsTrivial = CtxtShelley

instance SameDepIndex (NestedCtxt_ (ShelleyBlock proto era) f)
instance HasNestedContent f (ShelleyBlock proto era)

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance ShelleyCompatible proto era => EncCBOR (ShelleyBlock proto era) where
  -- Don't encode the header hash, we recompute it during deserialisation
  encCBOR = encCBOR . shelleyBlockRaw

instance ShelleyCompatible proto era => DecCBOR (Annotator (ShelleyBlock proto era)) where
  decCBOR = fmap mkShelleyBlock <$> decCBOR

instance ShelleyCompatible proto era => EncCBOR (Header (ShelleyBlock proto era)) where
  -- Don't encode the header hash, we recompute it during deserialisation
  encCBOR = encCBOR . shelleyHeaderRaw

instance ShelleyCompatible proto era => DecCBOR (Annotator (Header (ShelleyBlock proto era))) where
  decCBOR = fmap mkShelleyHeader <$> decCBOR

encodeShelleyBlock ::
  forall proto era. ShelleyCompatible proto era
  => ShelleyBlock proto era -> Plain.Encoding
encodeShelleyBlock = toEraCBOR @era

decodeShelleyBlock ::
  forall proto era. ShelleyCompatible proto era
  => forall s. Plain.Decoder s (Lazy.ByteString -> ShelleyBlock proto era)
decodeShelleyBlock = toPlainDecoder (eraProtVerLow @era) $ (. Full) . runAnnotator <$> decCBOR

shelleyBinaryBlockInfo :: forall proto era. ShelleyCompatible proto era => ShelleyBlock proto era -> BinaryBlockInfo
shelleyBinaryBlockInfo blk = BinaryBlockInfo {
      -- Drop the 'encodeListLen' that precedes the header and the body (= tx
      -- seq)
      headerOffset = 1
      -- The Shelley decoders use annotations, so this is cheap
    , headerSize   = fromIntegral $ Lazy.length (serialize (SL.eraProtVerLow @era) (getHeader blk))
    }

encodeShelleyHeader ::
  forall proto era. ShelleyCompatible proto era
  => Header (ShelleyBlock proto era) -> Plain.Encoding
encodeShelleyHeader = toEraCBOR @era

decodeShelleyHeader ::
  forall proto era. ShelleyCompatible proto era
  => forall s. Plain.Decoder s (Lazy.ByteString -> Header (ShelleyBlock proto era))
decodeShelleyHeader = toPlainDecoder (eraProtVerLow @era) $ (. Full) . runAnnotator <$> decCBOR

{-------------------------------------------------------------------------------
  Condense
-------------------------------------------------------------------------------}

instance ShelleyCompatible proto era => Condense (ShelleyBlock proto era) where
  condense = show . shelleyBlockRaw

instance ShelleyCompatible proto era => Condense (Header (ShelleyBlock proto era)) where
  condense = show . shelleyHeaderRaw
