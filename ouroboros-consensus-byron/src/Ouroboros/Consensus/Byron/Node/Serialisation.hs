{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Byron.Node.Serialisation () where

import qualified Data.ByteString.Lazy as Lazy

import qualified Cardano.Chain.Byron.API as CC

import           Ouroboros.Network.Block (Serialised, unwrapCBORinCBOR,
                     wrapCBORinCBOR)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTxId)
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Node.Serialisation
import           Ouroboros.Consensus.Protocol.PBFT.State (PBftState)
import           Ouroboros.Consensus.Storage.ChainDB.Serialisation

import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Byron.Protocol

{-------------------------------------------------------------------------------
  EncodeDisk & DecodeDisk
-------------------------------------------------------------------------------}

instance ImmDbSerialiseConstraints ByronBlock
instance LgrDbSerialiseConstraints ByronBlock
instance VolDbSerialiseConstraints ByronBlock
instance SerialiseDiskConstraints  ByronBlock

instance EncodeDisk ByronBlock ByronBlock where
  encodeDisk _ = encodeByronBlock
instance DecodeDisk ByronBlock (Lazy.ByteString -> ByronBlock) where
  decodeDisk ccfg = decodeByronBlock (getByronEpochSlots ccfg)

instance EncodeDisk ByronBlock (Header ByronBlock) where
  encodeDisk _ = encodeByronHeaderWithBlockSize
instance DecodeDisk ByronBlock (Lazy.ByteString -> Header ByronBlock) where
  decodeDisk ccfg = decodeByronHeaderWithBlockSize (getByronEpochSlots ccfg)

instance EncodeDisk ByronBlock (LedgerState ByronBlock) where
  encodeDisk _ = encodeByronLedgerState
instance DecodeDisk ByronBlock (LedgerState ByronBlock) where
  decodeDisk _ = decodeByronLedgerState

-- | @'ConsensusState' ('BlockProtocol' 'ByronBlock')@
instance EncodeDisk ByronBlock (PBftState PBftByronCrypto) where
  encodeDisk _ = encodeByronConsensusState
-- | @'ConsensusState' ('BlockProtocol' 'ByronBlock')@
instance DecodeDisk ByronBlock (PBftState PBftByronCrypto) where
  decodeDisk ccfg = decodeByronConsensusState (getByronSecurityParam ccfg)

instance EncodeDisk ByronBlock (AnnTip ByronBlock) where
  encodeDisk _ = encodeByronAnnTip
instance DecodeDisk ByronBlock (AnnTip ByronBlock) where
  decodeDisk _ = decodeByronAnnTip

{-------------------------------------------------------------------------------
  SerialiseNodeToNode
-------------------------------------------------------------------------------}

instance SerialiseNodeToNodeConstraints ByronBlock

-- | @'HeaderHash' 'ByronBlock'@
instance SerialiseNodeToNode ByronBlock ByronHash where
  encodeNodeToNode _ _ = encodeByronHeaderHash
  decodeNodeToNode _ _ = decodeByronHeaderHash

-- | CBOR-in-CBOR for the annotation. This also makes it compatible with the
-- wrapped ('Serialised') variant.
instance SerialiseNodeToNode ByronBlock ByronBlock where
  encodeNodeToNode _    _ = wrapCBORinCBOR    encodeByronBlock
  decodeNodeToNode ccfg _ = unwrapCBORinCBOR (decodeByronBlock epochSlots)
    where
      epochSlots = getByronEpochSlots ccfg

-- | CBOR-in-CBOR to be compatible with the wrapped ('Serialised') variant.
-- Uses nested CBOR-in-CBOR for the annotation. We can't reuse the outer
-- CBOR-in-CBOR for the annotation because of the optional block size hint.
instance SerialiseNodeToNode ByronBlock (Header ByronBlock) where
  encodeNodeToNode _    version = wrapCBORinCBOR $ case version of
      ByronNodeToNodeVersion1 -> encodeByronHeaderWithoutBlockSize
      ByronNodeToNodeVersion2 -> encodeByronHeaderWithBlockSize
  decodeNodeToNode ccfg version = unwrapCBORinCBOR $ case version of
      ByronNodeToNodeVersion1 -> decodeByronHeaderWithoutBlockSize epochSlots
      ByronNodeToNodeVersion2 -> decodeByronHeaderWithBlockSize    epochSlots
    where
      epochSlots = getByronEpochSlots ccfg

-- | 'Serialised' uses CBOR-in-CBOR by default.
instance SerialiseNodeToNode ByronBlock (Serialised ByronBlock)
  -- Default instance

-- | 'Serialised' uses CBOR-in-CBOR by default.
instance SerialiseNodeToNode ByronBlock (Serialised (Header ByronBlock)) where
  encodeNodeToNode _ version = case version of
      ByronNodeToNodeVersion1 -> encodeWrappedByronHeaderWithoutBlockSize
      ByronNodeToNodeVersion2 -> encodeWrappedByronHeaderWithBlockSize
  decodeNodeToNode _ version = case version of
      ByronNodeToNodeVersion1 -> decodeWrappedByronHeaderWithoutBlockSize
      ByronNodeToNodeVersion2 -> decodeWrappedByronHeaderWithBlockSize

-- | No CBOR-in-CBOR, because we check for canonical encodings, which means we
-- can use the recomputed encoding for the annotation.
instance SerialiseNodeToNode ByronBlock (GenTx ByronBlock) where
  encodeNodeToNode _ _ = encodeByronGenTx
  decodeNodeToNode _ _ = decodeByronGenTx

instance SerialiseNodeToNode ByronBlock (GenTxId ByronBlock) where
  encodeNodeToNode _ _ = encodeByronGenTxId
  decodeNodeToNode _ _ = decodeByronGenTxId

{-------------------------------------------------------------------------------
  SerialiseNodeToClient
-------------------------------------------------------------------------------}

instance SerialiseNodeToClientConstraints ByronBlock

-- | @'HeaderHash' 'ByronBlock'@
instance SerialiseNodeToClient ByronBlock ByronHash where
  encodeNodeToClient _ _ = encodeByronHeaderHash
  decodeNodeToClient _ _ = decodeByronHeaderHash

-- | CBOR-in-CBOR for the annotation. This also makes it compatible with the
-- wrapped ('Serialised') variant.
instance SerialiseNodeToClient ByronBlock ByronBlock where
  encodeNodeToClient _    _ = wrapCBORinCBOR    encodeByronBlock
  decodeNodeToClient ccfg _ = unwrapCBORinCBOR (decodeByronBlock epochSlots)
    where
      epochSlots = getByronEpochSlots ccfg

-- | 'Serialised' uses CBOR-in-CBOR by default.
instance SerialiseNodeToClient ByronBlock (Serialised ByronBlock)
  -- Default instance

-- | No CBOR-in-CBOR, because we check for canonical encodings, which means we
-- can use the recomputed encoding for the annotation.
instance SerialiseNodeToClient ByronBlock (GenTx ByronBlock) where
  encodeNodeToClient _ _ = encodeByronGenTx
  decodeNodeToClient _ _ = decodeByronGenTx

-- | @'ApplyTxErr' 'ByronBlock'@
instance SerialiseNodeToClient ByronBlock CC.ApplyMempoolPayloadErr where
  encodeNodeToClient _ _ = encodeByronApplyTxError
  decodeNodeToClient _ _ = decodeByronApplyTxError

instance SerialiseNodeToClient ByronBlock (Some (Query ByronBlock)) where
  encodeNodeToClient _ _ (Some q) = encodeByronQuery q
  decodeNodeToClient _ _          = decodeByronQuery

instance SerialiseResult ByronBlock (Query ByronBlock) where
  encodeResult _ _ = encodeByronResult
  decodeResult _ _ = decodeByronResult
