{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Byron.Node.Serialisation () where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import           Codec.Serialise (decode, encode)
import           Control.Monad.Except
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Short as Short

import           Cardano.Binary
import           Cardano.Prelude (cborError)

import qualified Cardano.Chain.Block as CC
import qualified Cardano.Chain.Byron.API as CC

import           Ouroboros.Network.Block (Serialised (..), unwrapCBORinCBOR,
                     wrapCBORinCBOR)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTxId)
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Node.Serialisation
import           Ouroboros.Consensus.Protocol.PBFT.State (PBftState)
import           Ouroboros.Consensus.Storage.Serialisation

import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Byron.Ledger.Conversions
import           Ouroboros.Consensus.Byron.Protocol

{-------------------------------------------------------------------------------
  EncodeDisk & DecodeDisk
-------------------------------------------------------------------------------}

instance HasBinaryBlockInfo ByronBlock where
  getBinaryBlockInfo = byronBinaryBlockInfo

instance SerialiseDiskConstraints ByronBlock

instance EncodeDisk ByronBlock ByronBlock where
  encodeDisk _ = encodeByronBlock
instance DecodeDisk ByronBlock (Lazy.ByteString -> ByronBlock) where
  decodeDisk ccfg = decodeByronBlock (getByronEpochSlots ccfg)

instance EncodeDisk ByronBlock (LedgerState ByronBlock) where
  encodeDisk _ = encodeByronLedgerState
instance DecodeDisk ByronBlock (LedgerState ByronBlock) where
  decodeDisk _ = decodeByronLedgerState

-- | @'ChainDepState' ('BlockProtocol' 'ByronBlock')@
instance EncodeDisk ByronBlock (PBftState PBftByronCrypto) where
  encodeDisk _ = encodeByronChainDepState
-- | @'ChainDepState' ('BlockProtocol' 'ByronBlock')@
instance DecodeDisk ByronBlock (PBftState PBftByronCrypto) where
  decodeDisk _ = decodeByronChainDepState

instance EncodeDisk ByronBlock (AnnTip ByronBlock) where
  encodeDisk _ = encodeByronAnnTip
instance DecodeDisk ByronBlock (AnnTip ByronBlock) where
  decodeDisk _ = decodeByronAnnTip

{-------------------------------------------------------------------------------
  SerialiseNodeToNode
-------------------------------------------------------------------------------}

instance SerialiseNodeToNodeConstraints ByronBlock where
  estimateBlockSize = byronHeaderBlockSizeHint

-- | CBOR-in-CBOR for the annotation. This also makes it compatible with the
-- wrapped ('Serialised') variant.
instance SerialiseNodeToNode ByronBlock ByronBlock where
  encodeNodeToNode _    _ = wrapCBORinCBOR    encodeByronBlock
  decodeNodeToNode ccfg _ = unwrapCBORinCBOR (decodeByronBlock epochSlots)
    where
      epochSlots = getByronEpochSlots ccfg

instance SerialiseNodeToNode ByronBlock (Header ByronBlock) where
  encodeNodeToNode ccfg = \case
      ByronNodeToNodeVersion1 ->
        wrapCBORinCBOR $
          encodeUnsizedHeader . fst . splitSizeHint
      ByronNodeToNodeVersion2 ->
        encodeDisk ccfg . unnest

  decodeNodeToNode ccfg = \case
      ByronNodeToNodeVersion1 ->
        unwrapCBORinCBOR $
              (flip joinSizeHint fakeByronBlockSizeHint .)
          <$> decodeUnsizedHeader epochSlots
      ByronNodeToNodeVersion2 ->
        nest <$> decodeDisk ccfg
    where
      epochSlots = getByronEpochSlots ccfg

-- | 'Serialised' uses CBOR-in-CBOR by default.
instance SerialiseNodeToNode ByronBlock (Serialised ByronBlock)
  -- Default instance

instance SerialiseNodeToNode ByronBlock (SerialisedHeader ByronBlock) where
  encodeNodeToNode ccfg version = case version of
      -- Drop the context and add the tag, encode that using CBOR-in-CBOR
      ByronNodeToNodeVersion1 ->
            encode
          . Serialised
          . addV1Envelope
          . aux
          . serialisedHeaderToDepPair
        where
          aux :: GenDepPair Serialised (f blk)
              -> (SomeSecond f blk, Lazy.ByteString)
          aux (GenDepPair ix (Serialised bytes)) = (SomeSecond ix, bytes)

      ByronNodeToNodeVersion2 -> encodeDisk ccfg

  decodeNodeToNode ccfg version = case version of
      ByronNodeToNodeVersion1 -> do
          bs <- unSerialised <$> decode
          either fail (return . SerialisedHeaderFromDepPair) $
            runExcept $ aux <$> dropV1Envelope bs
        where
          aux :: (SomeSecond f blk, Lazy.ByteString)
              -> GenDepPair Serialised (f blk)
          aux (SomeSecond ix, bytes) = GenDepPair ix (Serialised bytes)

      ByronNodeToNodeVersion2 -> decodeDisk ccfg

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

instance SerialiseNodeToClient ByronBlock (SomeSecond BlockQuery ByronBlock) where
  encodeNodeToClient _ _ (SomeSecond q) = encodeByronQuery q
  decodeNodeToClient _ _               = decodeByronQuery

instance SerialiseResult ByronBlock (BlockQuery ByronBlock) where
  encodeResult _ _ = encodeByronResult
  decodeResult _ _ = decodeByronResult

{-------------------------------------------------------------------------------
  Nested contents
-------------------------------------------------------------------------------}

instance ReconstructNestedCtxt Header ByronBlock where
  reconstructPrefixLen _ = PrefixLen 2
  reconstructNestedCtxt _proxy prefix size =
      -- The first byte is @encodeListLen 2@, the second (index 1) is 0 for
      -- EBB, 1 for regular block
      case Short.index prefix 1 of
        0 -> SomeSecond $ NestedCtxt (CtxtByronBoundary size)
        1 -> SomeSecond $ NestedCtxt (CtxtByronRegular  size)
        _ -> error $ "invalid ByronBlock with prefix: " <> show prefix

instance EncodeDiskDepIx (NestedCtxt Header) ByronBlock where
  encodeDiskDepIx _ccfg (SomeSecond (NestedCtxt ctxt)) = mconcat [
        CBOR.encodeListLen 2
      , case ctxt of
          CtxtByronBoundary size -> mconcat [
              CBOR.encodeWord8 0
            , CBOR.encodeWord32 size
            ]
          CtxtByronRegular size -> mconcat [
              CBOR.encodeWord8 1
            , CBOR.encodeWord32 size
            ]
      ]

instance EncodeDiskDep (NestedCtxt Header) ByronBlock where
  encodeDiskDep _ccfg (NestedCtxt ctxt) h =
      case ctxt of
        CtxtByronRegular _size ->
          encodeByronRegularHeader h
        CtxtByronBoundary _size ->
          -- We don't encode the 'SlotNo'
          -- This is important, because this encoder/decoder must be compatible
          -- with the raw bytes as stored on disk as part of a Byron block.
          encodeByronBoundaryHeader (snd h)

instance DecodeDiskDepIx (NestedCtxt Header) ByronBlock where
  decodeDiskDepIx _ccfg = do
      enforceSize "decodeDiskDepIx ByronBlock" 2
      CBOR.decodeWord8 >>= \case
        0 -> SomeSecond . NestedCtxt . CtxtByronBoundary <$> CBOR.decodeWord32
        1 -> SomeSecond . NestedCtxt . CtxtByronRegular  <$> CBOR.decodeWord32
        t -> cborError $ DecoderErrorUnknownTag "decodeDiskDepIx ByronBlock" t

instance DecodeDiskDep (NestedCtxt Header) ByronBlock where
  decodeDiskDep ByronCodecConfig{..} (NestedCtxt ctxt) =
      case ctxt of
        CtxtByronRegular _size ->
          decodeByronRegularHeader getByronEpochSlots
        CtxtByronBoundary _size ->
          auxBoundary <$> decodeByronBoundaryHeader
    where
      auxBoundary :: (Lazy.ByteString -> RawBoundaryHeader)
                  -> (Lazy.ByteString -> (SlotNo, RawBoundaryHeader))
      auxBoundary f bs =
          (slotNo, hdr)
        where
          hdr :: RawBoundaryHeader
          hdr = f bs

          slotNo :: SlotNo
          slotNo = fromByronSlotNo $
              CC.boundaryBlockSlot getByronEpochSlots (CC.boundaryEpoch hdr)
