{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Shelley.Node.Serialisation () where

import qualified Data.ByteString.Lazy as Lazy

import           Cardano.Binary (fromCBOR, toCBOR)

import           Ouroboros.Network.Block (Serialised, unwrapCBORinCBOR,
                     wrapCBORinCBOR)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTxId)
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Node.Serialisation
import           Ouroboros.Consensus.Storage.ChainDB.Serialisation

import qualified Shelley.Spec.Ledger.API as SL

import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion ()
import           Ouroboros.Consensus.Shelley.Protocol.State (TPraosState)

{-------------------------------------------------------------------------------
  EncodeDisk & DecodeDisk
-------------------------------------------------------------------------------}

instance Crypto c => ImmDbSerialiseConstraints (ShelleyBlock c)
instance Crypto c => LgrDbSerialiseConstraints (ShelleyBlock c)
instance Crypto c => VolDbSerialiseConstraints (ShelleyBlock c)
instance Crypto c => SerialiseDiskConstraints  (ShelleyBlock c)

instance Crypto c => EncodeDisk (ShelleyBlock c) (ShelleyBlock c) where
  encodeDisk _ = encodeShelleyBlock
instance Crypto c => DecodeDisk (ShelleyBlock c) (Lazy.ByteString -> ShelleyBlock c) where
  decodeDisk _ = decodeShelleyBlock

instance Crypto c => EncodeDisk (ShelleyBlock c) (Header (ShelleyBlock c)) where
  encodeDisk _ = encodeShelleyHeader
instance Crypto c => DecodeDisk (ShelleyBlock c) (Lazy.ByteString -> Header (ShelleyBlock c)) where
  decodeDisk _ = decodeShelleyHeader

instance Crypto c => EncodeDisk (ShelleyBlock c) (LedgerState (ShelleyBlock c)) where
  encodeDisk _ = encodeShelleyLedgerState
instance Crypto c => DecodeDisk (ShelleyBlock c) (LedgerState (ShelleyBlock c)) where
  decodeDisk _ = decodeShelleyLedgerState

-- | @'ConsensusState' ('BlockProtocol' ('ShelleyBlock' c))@
instance Crypto c => EncodeDisk (ShelleyBlock c) (TPraosState c) where
  encodeDisk _ = toCBOR
-- | @'ConsensusState' ('BlockProtocol' ('ShelleyBlock' c))@
instance Crypto c => DecodeDisk (ShelleyBlock c) (TPraosState c) where
  decodeDisk _ = fromCBOR

instance Crypto c => EncodeDisk (ShelleyBlock c) (AnnTip (ShelleyBlock c)) where
  encodeDisk _ = encodeShelleyAnnTip
instance Crypto c =>  DecodeDisk (ShelleyBlock c) (AnnTip (ShelleyBlock c)) where
  decodeDisk _ = decodeShelleyAnnTip

{-------------------------------------------------------------------------------
  SerialiseNodeToNode
-------------------------------------------------------------------------------}

instance Crypto c => SerialiseNodeToNodeConstraints (ShelleyBlock c)

-- | CBOR-in-CBOR for the annotation. This also makes it compatible with the
-- wrapped ('Serialised') variant.
instance Crypto c => SerialiseNodeToNode (ShelleyBlock c) (ShelleyBlock c) where
  encodeNodeToNode _ _ = wrapCBORinCBOR   encodeShelleyBlock
  decodeNodeToNode _ _ = unwrapCBORinCBOR decodeShelleyBlock

-- | 'Serialised' uses CBOR-in-CBOR by default.
instance SerialiseNodeToNode (ShelleyBlock c) (Serialised (ShelleyBlock c))
  -- Default instance

-- | CBOR-in-CBOR to be compatible with the wrapped ('Serialised') variant.
instance Crypto c => SerialiseNodeToNode (ShelleyBlock c) (Header (ShelleyBlock c)) where
  encodeNodeToNode _ _ = wrapCBORinCBOR   encodeShelleyHeader
  decodeNodeToNode _ _ = unwrapCBORinCBOR decodeShelleyHeader

-- | We use CBOR-in-CBOR
instance SerialiseNodeToNode (ShelleyBlock c) (SerialisedHeader (ShelleyBlock c)) where
  encodeNodeToNode _ _ = encodeTrivialSerialisedHeader
  decodeNodeToNode _ _ = decodeTrivialSerialisedHeader

-- | The @To/FromCBOR@ instances defined in @cardano-ledger-specs@ use
-- CBOR-in-CBOR to get the annotation.
instance Crypto c => SerialiseNodeToNode (ShelleyBlock c) (GenTx (ShelleyBlock c)) where
  encodeNodeToNode _ _ = toCBOR
  decodeNodeToNode _ _ = fromCBOR

instance Crypto c => SerialiseNodeToNode (ShelleyBlock c) (GenTxId (ShelleyBlock c)) where
  encodeNodeToNode _ _ = toCBOR
  decodeNodeToNode _ _ = fromCBOR

{-------------------------------------------------------------------------------
  SerialiseNodeToClient
-------------------------------------------------------------------------------}

instance Crypto c => SerialiseNodeToClientConstraints (ShelleyBlock c)

-- | CBOR-in-CBOR for the annotation. This also makes it compatible with the
-- wrapped ('Serialised') variant.
instance Crypto c => SerialiseNodeToClient (ShelleyBlock c) (ShelleyBlock c) where
  encodeNodeToClient _ _ = wrapCBORinCBOR   encodeShelleyBlock
  decodeNodeToClient _ _ = unwrapCBORinCBOR decodeShelleyBlock

-- | 'Serialised' uses CBOR-in-CBOR by default.
instance SerialiseNodeToClient (ShelleyBlock c) (Serialised (ShelleyBlock c))
  -- Default instance

-- | Uses CBOR-in-CBOR in the @To/FromCBOR@ instances to get the annotation.
instance Crypto c => SerialiseNodeToClient (ShelleyBlock c) (GenTx (ShelleyBlock c)) where
  encodeNodeToClient _ _ = toCBOR
  decodeNodeToClient _ _ = fromCBOR

-- | @'ApplyTxErr' '(ShelleyBlock c)'@
instance Crypto c => SerialiseNodeToClient (ShelleyBlock c) (SL.ApplyTxError c) where
  encodeNodeToClient _ _ = toCBOR
  decodeNodeToClient _ _ = fromCBOR

instance Crypto c => SerialiseNodeToClient (ShelleyBlock c) (SomeBlock Query (ShelleyBlock c)) where
  encodeNodeToClient _ _ (SomeBlock q) = encodeShelleyQuery q
  decodeNodeToClient _ _               = decodeShelleyQuery

instance Crypto c => SerialiseResult (ShelleyBlock c) (Query (ShelleyBlock c)) where
  encodeResult _ _ = encodeShelleyResult
  decodeResult _ _ = decodeShelleyResult

{-------------------------------------------------------------------------------
  HFC support

  Since 'NestedCtxt' for Shelley is trivial, these instances can use defaults.
-------------------------------------------------------------------------------}

instance Crypto c => ReconstructNestedCtxt Header (ShelleyBlock c)
instance Crypto c => EncodeDiskDepIx (NestedCtxt Header) (ShelleyBlock c)
instance Crypto c => EncodeDiskDep   (NestedCtxt Header) (ShelleyBlock c)
instance Crypto c => DecodeDiskDepIx (NestedCtxt Header) (ShelleyBlock c)
instance Crypto c => DecodeDiskDep   (NestedCtxt Header) (ShelleyBlock c)
