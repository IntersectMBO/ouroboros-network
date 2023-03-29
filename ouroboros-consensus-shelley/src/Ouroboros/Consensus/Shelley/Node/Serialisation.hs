{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Shelley.Node.Serialisation () where

import           Cardano.Ledger.Binary (fromCBOR, toCBOR)
import           Cardano.Ledger.Core (fromEraCBOR, toEraCBOR)
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Protocol.TPraos.API as SL
import           Codec.Serialise (decode, encode)
import           Control.Exception (Exception, throw)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Typeable (Typeable)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTxId)
import           Ouroboros.Consensus.Ledger.Tables (EmptyMK)
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Node.Serialisation
import           Ouroboros.Consensus.Protocol.Praos (PraosState)
import qualified Ouroboros.Consensus.Protocol.Praos as Praos
import           Ouroboros.Consensus.Protocol.TPraos
import           Ouroboros.Consensus.Shelley.Eras
import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion ()
import           Ouroboros.Consensus.Shelley.Protocol.Abstract
                     (pHeaderBlockSize, pHeaderSize)
import           Ouroboros.Consensus.Storage.Serialisation
import           Ouroboros.Network.Block (Serialised, unwrapCBORinCBOR,
                     wrapCBORinCBOR)

{-------------------------------------------------------------------------------
  EncodeDisk & DecodeDisk
-------------------------------------------------------------------------------}

instance ShelleyCompatible proto era => HasBinaryBlockInfo (ShelleyBlock proto era) where
  getBinaryBlockInfo = shelleyBinaryBlockInfo

instance ShelleyCompatible proto era => SerialiseDiskConstraints (ShelleyBlock proto era)

instance ShelleyCompatible proto era => EncodeDisk (ShelleyBlock proto era) (ShelleyBlock proto era) where
  encodeDisk _ = encodeShelleyBlock
instance ShelleyCompatible proto era => DecodeDisk (ShelleyBlock proto era) (Lazy.ByteString -> ShelleyBlock proto era) where
  decodeDisk _ = decodeShelleyBlock

instance ShelleyCompatible proto era => EncodeDisk (ShelleyBlock proto era) (Header (ShelleyBlock proto era)) where
  encodeDisk _ = encodeShelleyHeader
instance ShelleyCompatible proto era => DecodeDisk (ShelleyBlock proto era) (Lazy.ByteString -> Header (ShelleyBlock proto era)) where
  decodeDisk _ = decodeShelleyHeader

instance ShelleyCompatible proto era => EncodeDisk (ShelleyBlock proto era) (LedgerState (ShelleyBlock proto era) EmptyMK) where
  encodeDisk _ = encodeShelleyLedgerState
instance ShelleyCompatible proto era => DecodeDisk (ShelleyBlock proto era) (LedgerState (ShelleyBlock proto era) EmptyMK) where
  decodeDisk _ = decodeShelleyLedgerState

-- | @'ChainDepState' ('BlockProtocol' ('ShelleyBlock' era))@
instance (ShelleyCompatible proto era, EraCrypto era ~ c, SL.PraosCrypto c) => EncodeDisk (ShelleyBlock proto era) (TPraosState c) where
  encodeDisk _ = encode
-- | @'ChainDepState' ('BlockProtocol' ('ShelleyBlock' era))@
instance (ShelleyCompatible proto era, EraCrypto era ~ c, SL.PraosCrypto c) => DecodeDisk (ShelleyBlock proto era) (TPraosState c) where
  decodeDisk _ = decode

instance (ShelleyCompatible proto era, EraCrypto era ~ c, Praos.PraosCrypto c) => EncodeDisk (ShelleyBlock proto era) (PraosState c) where
  encodeDisk _ = encode
-- | @'ChainDepState' ('BlockProtocol' ('ShelleyBlock' era))@
instance (ShelleyCompatible proto era, EraCrypto era ~ c, Praos.PraosCrypto c) => DecodeDisk (ShelleyBlock proto era) (PraosState c) where
  decodeDisk _ = decode
instance ShelleyCompatible proto era
  => EncodeDisk (ShelleyBlock proto era) (AnnTip (ShelleyBlock proto era)) where
  encodeDisk _ = encodeShelleyAnnTip
instance ShelleyCompatible proto era
  =>  DecodeDisk (ShelleyBlock proto era) (AnnTip (ShelleyBlock proto era)) where
  decodeDisk _ = decodeShelleyAnnTip

{-------------------------------------------------------------------------------
  SerialiseNodeToNode
-------------------------------------------------------------------------------}

instance ShelleyCompatible proto era
  => SerialiseNodeToNodeConstraints (ShelleyBlock proto era) where
  estimateBlockSize hdr = overhead + hdrSize + bodySize
    where
      -- The maximum block size is 65536, the CBOR-in-CBOR tag for this block
      -- is:
      --
      -- > D8 18          # tag(24)
      -- >    1A 00010000 # bytes(65536)
      --
      -- Which is 7 bytes, enough for up to 4294967295 bytes.
      overhead = 7 {- CBOR-in-CBOR -} + 1 {- encodeListLen -}
      bodySize = fromIntegral . pHeaderBlockSize . shelleyHeaderRaw $ hdr
      hdrSize  = fromIntegral . pHeaderSize . shelleyHeaderRaw $ hdr

-- | CBOR-in-CBOR for the annotation. This also makes it compatible with the
-- wrapped ('Serialised') variant.
instance ShelleyCompatible proto era
  => SerialiseNodeToNode (ShelleyBlock proto era) (ShelleyBlock proto era) where
  encodeNodeToNode _ _ = wrapCBORinCBOR   encodeShelleyBlock
  decodeNodeToNode _ _ = unwrapCBORinCBOR decodeShelleyBlock

-- | 'Serialised' uses CBOR-in-CBOR by default.
instance SerialiseNodeToNode (ShelleyBlock proto era) (Serialised (ShelleyBlock proto era))
  -- Default instance

-- | CBOR-in-CBOR to be compatible with the wrapped ('Serialised') variant.
instance ShelleyCompatible proto era
  => SerialiseNodeToNode (ShelleyBlock proto era) (Header (ShelleyBlock proto era)) where
  encodeNodeToNode _ _ = wrapCBORinCBOR   encodeShelleyHeader
  decodeNodeToNode _ _ = unwrapCBORinCBOR decodeShelleyHeader

-- | We use CBOR-in-CBOR
instance SerialiseNodeToNode (ShelleyBlock proto era) (SerialisedHeader (ShelleyBlock proto era)) where
  encodeNodeToNode _ _ = encodeTrivialSerialisedHeader
  decodeNodeToNode _ _ = decodeTrivialSerialisedHeader

-- | The @To/FromCBOR@ instances defined in @cardano-ledger@ use
-- CBOR-in-CBOR to get the annotation.
instance ShelleyCompatible proto era
  => SerialiseNodeToNode (ShelleyBlock proto era) (GenTx (ShelleyBlock proto era)) where
  encodeNodeToNode _ _ = toCBOR
  decodeNodeToNode _ _ = fromCBOR

instance ShelleyCompatible proto era
  => SerialiseNodeToNode (ShelleyBlock proto era) (GenTxId (ShelleyBlock proto era)) where
  encodeNodeToNode _ _ = toEraCBOR @era
  decodeNodeToNode _ _ = fromEraCBOR @era

{-------------------------------------------------------------------------------
  SerialiseNodeToClient
-------------------------------------------------------------------------------}

-- | Exception thrown in the encoders
data ShelleyEncoderException era proto =
    -- | A query was submitted that is not supported by the given
    -- 'ShelleyNodeToClientVersion'.
    ShelleyEncoderUnsupportedQuery
         (SomeSecond BlockQuery (ShelleyBlock proto era))
         ShelleyNodeToClientVersion
  deriving (Show)

instance (Typeable era, Typeable proto)
  => Exception (ShelleyEncoderException era proto)

instance ShelleyCompatible proto era
  => SerialiseNodeToClientConstraints (ShelleyBlock proto era)

-- | CBOR-in-CBOR for the annotation. This also makes it compatible with the
-- wrapped ('Serialised') variant.
instance ShelleyCompatible proto era
  => SerialiseNodeToClient (ShelleyBlock proto era) (ShelleyBlock proto era) where
  encodeNodeToClient _ _ = wrapCBORinCBOR   encodeShelleyBlock
  decodeNodeToClient _ _ = unwrapCBORinCBOR decodeShelleyBlock

-- | 'Serialised' uses CBOR-in-CBOR by default.
instance SerialiseNodeToClient (ShelleyBlock proto era) (Serialised (ShelleyBlock proto era))
  -- Default instance

-- | Uses CBOR-in-CBOR in the @To/FromCBOR@ instances to get the annotation.
instance ShelleyCompatible proto era
  => SerialiseNodeToClient (ShelleyBlock proto era) (GenTx (ShelleyBlock proto era)) where
  encodeNodeToClient _ _ = toCBOR
  decodeNodeToClient _ _ = fromCBOR

instance ShelleyCompatible proto era
  => SerialiseNodeToClient (ShelleyBlock proto era) (GenTxId (ShelleyBlock proto era)) where
  encodeNodeToClient _ _ = toEraCBOR @era
  decodeNodeToClient _ _ = fromEraCBOR @era

-- | @'ApplyTxErr' '(ShelleyBlock era)'@
instance ShelleyBasedEra era => SerialiseNodeToClient (ShelleyBlock proto era) (SL.ApplyTxError era) where
  encodeNodeToClient _ _ = toEraCBOR @era
  decodeNodeToClient _ _ = fromEraCBOR @era

instance ShelleyCompatible proto era
      => SerialiseNodeToClient (ShelleyBlock proto era) (SomeSecond BlockQuery (ShelleyBlock proto era)) where
  encodeNodeToClient _ version (SomeSecond q)
    | querySupportedVersion q version
    = encodeShelleyQuery q
    | otherwise
    = throw $ ShelleyEncoderUnsupportedQuery (SomeSecond q) version
  decodeNodeToClient _ _ = decodeShelleyQuery

instance ShelleyCompatible proto era => SerialiseResult (ShelleyBlock proto era) (BlockQuery (ShelleyBlock proto era)) where
  encodeResult _ _ = encodeShelleyResult
  decodeResult _ _ = decodeShelleyResult

instance ShelleyCompatible proto era  => SerialiseNodeToClient (ShelleyBlock proto era) SlotNo where
  encodeNodeToClient _ _ = toCBOR
  decodeNodeToClient _ _ = fromCBOR

{-------------------------------------------------------------------------------
  HFC support

  Since 'NestedCtxt' for Shelley is trivial, these instances can use defaults.
-------------------------------------------------------------------------------}

instance ShelleyBasedEra era => ReconstructNestedCtxt Header (ShelleyBlock proto era)
instance ShelleyBasedEra era => EncodeDiskDepIx (NestedCtxt Header) (ShelleyBlock proto era)
instance ShelleyCompatible proto era => EncodeDiskDep   (NestedCtxt Header) (ShelleyBlock proto era)
instance ShelleyBasedEra era => DecodeDiskDepIx (NestedCtxt Header) (ShelleyBlock proto era)
instance ShelleyCompatible proto era => DecodeDiskDep   (NestedCtxt Header) (ShelleyBlock proto era)
