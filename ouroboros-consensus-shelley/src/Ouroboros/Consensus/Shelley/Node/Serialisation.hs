{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Shelley.Node.Serialisation (

  ) where

import           Control.Exception (Exception, throw)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Typeable (Typeable)

import           Cardano.Binary (fromCBOR, toCBOR)
import           Codec.Serialise (decode, encode)

import           Ouroboros.Network.Block (Serialised, unwrapCBORinCBOR,
                     wrapCBORinCBOR)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTxId)
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Node.Serialisation
import           Ouroboros.Consensus.Storage.Serialisation

import qualified Shelley.Spec.Ledger.API as SL

import           Ouroboros.Consensus.Shelley.Eras (EraCrypto)
import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion ()
import           Ouroboros.Consensus.Shelley.Protocol

{-------------------------------------------------------------------------------
  EncodeDisk & DecodeDisk
-------------------------------------------------------------------------------}

instance ShelleyBasedEra era => HasBinaryBlockInfo (ShelleyBlock era) where
  getBinaryBlockInfo = shelleyBinaryBlockInfo

instance ShelleyBasedEra era => SerialiseDiskConstraints (ShelleyBlock era)

instance ShelleyBasedEra era => EncodeDisk (ShelleyBlock era) (ShelleyBlock era) where
  encodeDisk _ = encodeShelleyBlock
instance ShelleyBasedEra era => DecodeDisk (ShelleyBlock era) (Lazy.ByteString -> ShelleyBlock era) where
  decodeDisk _ = decodeShelleyBlock

instance ShelleyBasedEra era => EncodeDisk (ShelleyBlock era) (Header (ShelleyBlock era)) where
  encodeDisk _ = encodeShelleyHeader
instance ShelleyBasedEra era => DecodeDisk (ShelleyBlock era) (Lazy.ByteString -> Header (ShelleyBlock era)) where
  decodeDisk _ = decodeShelleyHeader

instance ShelleyBasedEra era => EncodeDisk (ShelleyBlock era) (LedgerState (ShelleyBlock era)) where
  encodeDisk _ = encodeShelleyLedgerState
instance ShelleyBasedEra era => DecodeDisk (ShelleyBlock era) (LedgerState (ShelleyBlock era)) where
  decodeDisk _ = decodeShelleyLedgerState

-- | @'ChainDepState' ('BlockProtocol' ('ShelleyBlock' era))@
instance (ShelleyBasedEra era, EraCrypto era ~ c) => EncodeDisk (ShelleyBlock era) (TPraosState c) where
  encodeDisk _ = encode
-- | @'ChainDepState' ('BlockProtocol' ('ShelleyBlock' era))@
instance (ShelleyBasedEra era, EraCrypto era ~ c) => DecodeDisk (ShelleyBlock era) (TPraosState c) where
  decodeDisk _ = decode

instance ShelleyBasedEra era => EncodeDisk (ShelleyBlock era) (AnnTip (ShelleyBlock era)) where
  encodeDisk _ = encodeShelleyAnnTip
instance ShelleyBasedEra era =>  DecodeDisk (ShelleyBlock era) (AnnTip (ShelleyBlock era)) where
  decodeDisk _ = decodeShelleyAnnTip

{-------------------------------------------------------------------------------
  SerialiseNodeToNode
-------------------------------------------------------------------------------}

instance ShelleyBasedEra era => SerialiseNodeToNodeConstraints (ShelleyBlock era) where
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
      bodySize = fromIntegral . SL.bsize . SL.bhbody . shelleyHeaderRaw $ hdr
      hdrSize  = fromIntegral . SL.bHeaderSize . shelleyHeaderRaw $ hdr

-- | CBOR-in-CBOR for the annotation. This also makes it compatible with the
-- wrapped ('Serialised') variant.
instance ShelleyBasedEra era => SerialiseNodeToNode (ShelleyBlock era) (ShelleyBlock era) where
  encodeNodeToNode _ _ = wrapCBORinCBOR   encodeShelleyBlock
  decodeNodeToNode _ _ = unwrapCBORinCBOR decodeShelleyBlock

-- | 'Serialised' uses CBOR-in-CBOR by default.
instance SerialiseNodeToNode (ShelleyBlock era) (Serialised (ShelleyBlock era))
  -- Default instance

-- | CBOR-in-CBOR to be compatible with the wrapped ('Serialised') variant.
instance ShelleyBasedEra era => SerialiseNodeToNode (ShelleyBlock era) (Header (ShelleyBlock era)) where
  encodeNodeToNode _ _ = wrapCBORinCBOR   encodeShelleyHeader
  decodeNodeToNode _ _ = unwrapCBORinCBOR decodeShelleyHeader

-- | We use CBOR-in-CBOR
instance SerialiseNodeToNode (ShelleyBlock era) (SerialisedHeader (ShelleyBlock era)) where
  encodeNodeToNode _ _ = encodeTrivialSerialisedHeader
  decodeNodeToNode _ _ = decodeTrivialSerialisedHeader

-- | The @To/FromCBOR@ instances defined in @cardano-ledger-specs@ use
-- CBOR-in-CBOR to get the annotation.
instance ShelleyBasedEra era => SerialiseNodeToNode (ShelleyBlock era) (GenTx (ShelleyBlock era)) where
  encodeNodeToNode _ _ = toCBOR
  decodeNodeToNode _ _ = fromCBOR

instance ShelleyBasedEra era => SerialiseNodeToNode (ShelleyBlock era) (GenTxId (ShelleyBlock era)) where
  encodeNodeToNode _ _ = toCBOR
  decodeNodeToNode _ _ = fromCBOR

{-------------------------------------------------------------------------------
  SerialiseNodeToClient
-------------------------------------------------------------------------------}

-- | Exception thrown in the encoders
data ShelleyEncoderException era =
    -- | A query was submitted that is not supported by the given
    -- 'ShelleyNodeToClientVersion'.
    ShelleyEncoderUnsupportedQuery
         (SomeSecond Query (ShelleyBlock era))
         ShelleyNodeToClientVersion
  deriving (Show)

instance Typeable era => Exception (ShelleyEncoderException era)

instance ShelleyBasedEra era => SerialiseNodeToClientConstraints (ShelleyBlock era)

-- | CBOR-in-CBOR for the annotation. This also makes it compatible with the
-- wrapped ('Serialised') variant.
instance ShelleyBasedEra era => SerialiseNodeToClient (ShelleyBlock era) (ShelleyBlock era) where
  encodeNodeToClient _ _ = wrapCBORinCBOR   encodeShelleyBlock
  decodeNodeToClient _ _ = unwrapCBORinCBOR decodeShelleyBlock

-- | 'Serialised' uses CBOR-in-CBOR by default.
instance SerialiseNodeToClient (ShelleyBlock era) (Serialised (ShelleyBlock era))
  -- Default instance

-- | Uses CBOR-in-CBOR in the @To/FromCBOR@ instances to get the annotation.
instance ShelleyBasedEra era => SerialiseNodeToClient (ShelleyBlock era) (GenTx (ShelleyBlock era)) where
  encodeNodeToClient _ _ = toCBOR
  decodeNodeToClient _ _ = fromCBOR

-- | @'ApplyTxErr' '(ShelleyBlock era)'@
instance ShelleyBasedEra era => SerialiseNodeToClient (ShelleyBlock era) (SL.ApplyTxError era) where
  encodeNodeToClient _ _ = toCBOR
  decodeNodeToClient _ _ = fromCBOR

instance ShelleyBasedEra era
      => SerialiseNodeToClient (ShelleyBlock era) (SomeSecond Query (ShelleyBlock era)) where
  encodeNodeToClient _ version (SomeSecond q)
    | querySupportedVersion q version
    = encodeShelleyQuery q
    | otherwise
    = throw $ ShelleyEncoderUnsupportedQuery (SomeSecond q) version
  decodeNodeToClient _ _ = decodeShelleyQuery

instance ShelleyBasedEra era => SerialiseResult (ShelleyBlock era) (Query (ShelleyBlock era)) where
  encodeResult _ _ = encodeShelleyResult
  decodeResult _ _ = decodeShelleyResult

{-------------------------------------------------------------------------------
  HFC support

  Since 'NestedCtxt' for Shelley is trivial, these instances can use defaults.
-------------------------------------------------------------------------------}

instance ShelleyBasedEra era => ReconstructNestedCtxt Header (ShelleyBlock era)
instance ShelleyBasedEra era => EncodeDiskDepIx (NestedCtxt Header) (ShelleyBlock era)
instance ShelleyBasedEra era => EncodeDiskDep   (NestedCtxt Header) (ShelleyBlock era)
instance ShelleyBasedEra era => DecodeDiskDepIx (NestedCtxt Header) (ShelleyBlock era)
instance ShelleyBasedEra era => DecodeDiskDep   (NestedCtxt Header) (ShelleyBlock era)
