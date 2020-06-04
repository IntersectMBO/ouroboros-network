{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Byron.Ledger.Serialisation (
    -- * Serialisation
    byronBlockEncodingOverhead
  , encodeByronBlock
  , decodeByronBlock
  , encodeByronHeaderWithBlockSize
  , encodeByronHeaderWithoutBlockSize
  , decodeByronHeaderWithBlockSize
  , decodeByronHeaderWithoutBlockSize
  , encodeWrappedByronHeaderWithBlockSize
  , encodeWrappedByronHeaderWithoutBlockSize
  , decodeWrappedByronHeaderWithBlockSize
  , decodeWrappedByronHeaderWithoutBlockSize
  , encodeByronHeaderHash
  , decodeByronHeaderHash
    -- * Support for on-disk format
  , byronAddHeaderEnvelope
  , byronBinaryBlockInfo
    -- * Exceptions
  , DropEncodedSizeException(..)
    -- * Low-level API
  , fakeByronBlockSizeHint
  , decodeByronRegularBlock
  , decodeByronBoundaryBlock
  , ebbEnvelope
  ) where

import           Control.Exception (Exception (..), throw)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import           Data.Word (Word32)

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import           Codec.Serialise (Serialise (..))

import           Cardano.Binary

import qualified Cardano.Chain.Block as CC
import qualified Cardano.Chain.Slotting as CC

import           Ouroboros.Network.Block
import           Ouroboros.Network.DeltaQ (SizeInBytes)

import           Ouroboros.Consensus.Block

import           Ouroboros.Consensus.Storage.Common (BinaryBlockInfo (..))

import           Ouroboros.Consensus.Byron.Ledger.Block
import           Ouroboros.Consensus.Byron.Ledger.Orphans ()

{-------------------------------------------------------------------------------
  Serialise instances

  Mostly we don't depend on Serialise, but use explicit functions instead.
-------------------------------------------------------------------------------}

instance Serialise ByronHash where
  decode = decodeByronHeaderHash
  encode = encodeByronHeaderHash

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

-- | The Byron block encoding overhead size in bytes.
--
-- This encompasses the overhead in bytes for everything that is encoded
-- within a Byron block, excluding the actual generalized transactions
-- (transactions, delegation certificates, update votes, and update
-- proposals).
byronBlockEncodingOverhead :: Word32
byronBlockEncodingOverhead =
    blockHeaderOverhead + blockBodyOverhead + safetyMargin
  where
    -- The maximum block header size.
    blockHeaderOverhead = 650

    -- The block body overhead excluding the actual generalized transactions.
    blockBodyOverhead = 1 {- ABody: encodeListLen 4 -}
                      + 2 {- TxPayload: list -}
                      + 1 {- SscPayload: encodeListLen 2 -}
                      + 1 {- SscPayload: Word8 -}
                      + 1 {- SscPayload: mempty :: Set () -}
                      + 2 {- Delegation.Payload: list -}
                      + 1 {- Update.Payload: encodeListLen 2 -}
                      + 1 {- Update.Payload: Maybe AProposal -}
                      + 2 {- Update.Payload: list of AVote -}

    -- Just for safety.
    safetyMargin = 1024

encodeByronHeaderHash :: HeaderHash ByronBlock -> Encoding
encodeByronHeaderHash = toCBOR

decodeByronHeaderHash :: Decoder s (HeaderHash ByronBlock)
decodeByronHeaderHash = fromCBOR

-- | Encode a block
--
-- Should be backwards compatible with legacy (cardano-sl) nodes.
--
-- Implementation note: the decoder uses 'CC.fromCBORABlockOrBoundary', which
-- has inverse 'CC.toCBORABlockOrBoundary'. This encoder is intended to be
-- binary compatible with 'CC.toCBORABlockOrBoundary', but does not use it and
-- instead takes advantage of the annotations (using 'encodePreEncoded').
encodeByronBlock :: ByronBlock -> Encoding
encodeByronBlock blk = mconcat [
      CBOR.encodeListLen 2
    , case byronBlockRaw blk of
        CC.ABOBBoundary b -> mconcat [
            CBOR.encodeWord 0
          , CBOR.encodePreEncoded $ CC.boundaryAnnotation b
          ]
        CC.ABOBBlock b -> mconcat [
            CBOR.encodeWord 1
          , CBOR.encodePreEncoded $ CC.blockAnnotation b
          ]
    ]

-- | Inverse of 'encodeByronBlock'
decodeByronBlock :: CC.EpochSlots -> Decoder s (Lazy.ByteString -> ByronBlock)
decodeByronBlock epochSlots =
    flip (\bs -> mkByronBlock epochSlots
               . annotationBytes bs)
    <$> CC.fromCBORABlockOrBoundary epochSlots

-- | Decoder for a regular (non-EBB) Byron block.
--
-- PRECONDITION: the 'Lazy.ByteString' given as argument to the decoder is the
-- same as the one that is decoded.
--
-- This is a wrapper for 'CC.fromCBORABlock'.
--
-- Use 'decodeByronBlock' when you can, this function is provided for use by
-- the hard-fork combinator.
decodeByronRegularBlock :: CC.EpochSlots
                        -> Decoder s (Lazy.ByteString -> ByronBlock)
decodeByronRegularBlock epochSlots =
    flip (\bs -> mkByronBlock epochSlots
               . annotationBytes bs
               . CC.ABOBBlock)
    <$> CC.fromCBORABlock epochSlots

-- | Decoder for a boundary Byron block.
--
-- PRECONDITION: the 'Lazy.ByteString' given as argument to the decoder is the
-- same as the one that is decoded.
--
-- This is a wrapper for 'CC.fromCBORABoundaryBlock'.
--
-- Use 'decodeByronBlock' when you can, this function is provided for use by
-- the hard-fork combinator.
decodeByronBoundaryBlock :: CC.EpochSlots
                         -> Decoder s (Lazy.ByteString -> ByronBlock)
decodeByronBoundaryBlock epochSlots =
    flip (\bs -> mkByronBlock epochSlots
               . annotationBytes bs
               . CC.ABOBBoundary)
    <$> CC.fromCBORABoundaryBlock

-- | Encode a header with the block size.
--
-- Headers from disk, version 2 of the node-to-node protocols, and all
-- versions of the node-to-client protocols will include the size of the
-- block. Only version 1 of the node-to-node protocols doesn't include the
-- block size.
--
-- Note: after extracting the header from the binary block in the ChainDB, we
-- add the size hint using 'byronAddHeaderEnvelope'.
encodeByronHeaderWithBlockSize :: Header ByronBlock -> Encoding
encodeByronHeaderWithBlockSize =
    uncurry (flip encodeSizedHeader) . splitSizeHint

-- | Encode a header without the block size.
--
-- See 'encodeByronHeaderWithBlockSize' for more info. We drop the block size
-- from the encoding.
encodeByronHeaderWithoutBlockSize :: Header ByronBlock -> Encoding
encodeByronHeaderWithoutBlockSize =
    encodeUnsizedHeader . fst . splitSizeHint

-- | Inverse of 'encodeByronHeaderWithBlockSize'
decodeByronHeaderWithBlockSize
  :: CC.EpochSlots
  -> Decoder s (Lazy.ByteString -> Header ByronBlock)
decodeByronHeaderWithBlockSize epochSlots =
    const . uncurry (flip joinSizeHint) <$> decodeSizedHeader epochSlots

-- | Inverse of 'encodeByronHeaderWithoutBlockSize'
decodeByronHeaderWithoutBlockSize
  :: CC.EpochSlots
  -> Decoder s (Lazy.ByteString -> Header ByronBlock)
decodeByronHeaderWithoutBlockSize epochSlots =
    (flip joinSizeHint fakeByronBlockSizeHint .) <$> decodeUnsizedHeader epochSlots

-- | Encode wrapped header with the block size.
--
-- Wrapped variant of 'encodeByronHeaderWithBlockSize'.
--
-- NOTE: this uses CBOR-in-CBOR, so to be compatible with the non-wrapped
-- en/decoder, the non-wrapped ones will have to be wrapped using
-- @(un)wrapCBORinCBOR@.
encodeWrappedByronHeaderWithBlockSize
  :: Serialised (Header ByronBlock) -> Encoding
encodeWrappedByronHeaderWithBlockSize = encode

-- | Encode wrapped header without the block size.
--
-- Wrapped variant of 'encodeByronHeaderWithoutBlockSize'.
--
-- NOTE: this uses CBOR-in-CBOR, so to be compatible with the non-wrapped
-- en/decoder, the non-wrapped ones will have to be wrapped using
-- @(un)wrapCBORinCBOR@.
encodeWrappedByronHeaderWithoutBlockSize
  :: Serialised (Header ByronBlock) -> Encoding
encodeWrappedByronHeaderWithoutBlockSize = encode . dropEncodedSize

-- | Decode wrapped header with the block size
--
-- Wrapped variant of 'decodeByronHeaderWithoutBlockSize'
--
-- NOTE: this uses CBOR-in-CBOR, so to be compatible with the non-wrapped
-- en/decoder, the non-wrapped ones will have to be wrapped using
-- @(un)wrapCBORinCBOR@.
decodeWrappedByronHeaderWithBlockSize
  :: Decoder s (Serialised (Header ByronBlock))
decodeWrappedByronHeaderWithBlockSize = decode

-- | Decode wrapped header with the block size
--
-- Wrapped variant of 'decodeByronHeaderWithoutBlockSize'
--
-- NOTE: this uses CBOR-in-CBOR, so to be compatible with the non-wrapped
-- en/decoder, the non-wrapped ones will have to be wrapped using
-- @(un)wrapCBORinCBOR@.
decodeWrappedByronHeaderWithoutBlockSize
  :: Decoder s (Serialised (Header ByronBlock))
decodeWrappedByronHeaderWithoutBlockSize = fakeEncodedSize <$> decode

-- | When given the raw header bytes extracted from the block, i.e., the
-- header annotation of 'CC.AHeader' or 'CC.ABoundaryHdr', we still need to
-- prepend some bytes so that we can use 'decodeByronHeader' (when expecting a
-- size hint) to decode it:
byronAddHeaderEnvelope
  :: IsEBB
  -> SizeInBytes  -- ^ Block size
  -> Lazy.ByteString -> Lazy.ByteString
byronAddHeaderEnvelope isEBB blockSize bs =
    CBOR.toLazyByteString $
      encodeEncodedWithSize $
        EncodedWithSize blockSize $
          -- Add the envelope containing the tag distinguishing a regular
          -- block from an EBB (the envelope expected by
          -- 'CC.fromCBORABlockOrBoundaryHdr')
          Serialised (ebbEnvelope isEBB <> bs)

-- | A 'CC.ABlockOrBoundary' is a CBOR 2-tuple of a 'Word' (0 = EBB, 1 =
-- regular block) and block/ebb payload. This function returns the bytes that
-- should be prepended to the payload, i.e., the byte indicating it's a CBOR
-- 2-tuple and the 'Word' indicating whether its an EBB or regular block.
ebbEnvelope :: IsEBB -> Lazy.ByteString
ebbEnvelope = \case
  IsEBB    -> "\130\NUL"
  IsNotEBB -> "\130\SOH"

-- | The 'BinaryBlockInfo' of the given 'ByronBlock'.
--
-- NOTE: the bytestring obtained by slicing the serialised block using the
-- header offset and size will correspond to the /header annotation/, but not
-- to the serialised header, as we add an envelope ('encodeListLen' + tag)
-- around a header in 'encodeByronHeader'. This envelope must thus still be
-- added to the sliced bytestring before it can be deserialised using
-- 'decodeByronHeader'.
byronBinaryBlockInfo :: ByronBlock -> BinaryBlockInfo
byronBinaryBlockInfo blk = BinaryBlockInfo
    { headerOffset = 1 {- 'encodeListLen' of the outer 'Either' envelope -}
                   + 1 {- the tag -}
                   + 1 {- 'encodeListLen' of the block: header + body + ...  -}
      -- Compute the length of the annotated header
    , headerSize   = fromIntegral $ Strict.length $ case byronBlockRaw blk of
        CC.ABOBBoundary b -> CC.boundaryHeaderAnnotation $ CC.boundaryHeader b
        CC.ABOBBlock    b -> CC.headerAnnotation         $ CC.blockHeader    b
    }

{-------------------------------------------------------------------------------
  Sized header

  These are auxiliary functions for encoding/decoding the Byron header.
-------------------------------------------------------------------------------}

-- | Intermediate encoding of a sized header
--
-- The reason for this intermediate encoding is that we need to be able to
-- easily go from a version-2 encoding (with size) to a version-1 encoding
-- (without size) /without deserialization/ (see 'dropEncodedSize').
data EncodedWithSize = EncodedWithSize SizeInBytes (Serialised UnsizedHeader)

encodeEncodedWithSize :: EncodedWithSize -> Encoding
encodeEncodedWithSize (EncodedWithSize size encoded) = mconcat [
      encodeListLen 2
    , encode size
    , encode encoded
    ]

decodeEncodedWithSize :: Decoder s EncodedWithSize
decodeEncodedWithSize = do
    enforceSize "SizedHeader" 2
    size    <- decode
    encoded <- decode
    return $ EncodedWithSize size encoded

encodeSizedHeader :: SizeInBytes -> UnsizedHeader -> Encoding
encodeSizedHeader size =
      encodeEncodedWithSize
    . EncodedWithSize size
    . mkSerialised encodeUnsizedHeader

decodeSizedHeader :: CC.EpochSlots -> Decoder s (SizeInBytes, UnsizedHeader)
decodeSizedHeader epochSlots = do
    EncodedWithSize size encoded <- decodeEncodedWithSize
    unsized <- fromSerialised (decodeUnsizedHeader epochSlots) encoded
    return (size, unsized)

-- | Change the encoding of a sized header to the encoding of an unsized header
--
-- May throw 'DropEncodedSizeException' as a pure exception. We have no other
-- choice, because this function gets used in the codecs whilst /encoding/
-- a header, and there we have no natural hook to throw any kind of decoding
-- errors.
dropEncodedSize :: Serialised (Header ByronBlock) -> Serialised UnsizedHeader
dropEncodedSize = \(Serialised bs) -> Serialised (aux bs)
  where
    aux :: Lazy.ByteString
        -> Lazy.ByteString
    aux = noErrors
        . CBOR.deserialiseFromBytes (dropSize <$> decodeEncodedWithSize)

    dropSize :: EncodedWithSize -> Lazy.ByteString
    dropSize (EncodedWithSize _sz (Serialised unsized)) = unsized

    noErrors :: Either CBOR.DeserialiseFailure (Lazy.ByteString, a) -> a
    noErrors (Right (bs, a))
         | Lazy.null bs = a
         | otherwise    = throw $ DropEncodedSizeTrailingBytes bs
    noErrors (Left err) = throw $ DropEncodedSizeError err

-- | Failed to strip off the envelope from an encoded header
--
-- This indicates either a bug or disk corruption.
data DropEncodedSizeException =
     DropEncodedSizeError CBOR.DeserialiseFailure
   | DropEncodedSizeTrailingBytes Lazy.ByteString
   deriving (Show)

instance Exception DropEncodedSizeException

-- | Reintroduce the size wrapper
--
-- Since we don't /have/ a value for the size here, we must fake it.
fakeEncodedSize :: Serialised UnsizedHeader -> Serialised (Header ByronBlock)
fakeEncodedSize = \(Serialised bs) -> Serialised (aux bs)
  where
    aux :: Lazy.ByteString -> Lazy.ByteString
    aux bs =
      CBOR.toLazyByteString $
        encodeEncodedWithSize $
          EncodedWithSize fakeByronBlockSizeHint (Serialised bs)

-- | Fake size (used in compatibility mode)
fakeByronBlockSizeHint :: SizeInBytes
fakeByronBlockSizeHint = 2000

-- | Encode an unsized header
--
-- Does /not/ have to backwards compatible with legacy (cardano-sl) nodes
-- (which never send or store these headers), but should be inverse to
-- 'decodeSizedHeader', and moreover uses 'fromCBORABlockOrBoundaryHdr' from
-- cardano-ledger, and so we don't have too much choice in this encoder.
encodeUnsizedHeader :: UnsizedHeader -> Encoding
encodeUnsizedHeader (UnsizedHeader raw _ _) = CC.toCBORABlockOrBoundaryHdr raw

-- | Inverse of 'encodeSizedHeader'
decodeUnsizedHeader :: CC.EpochSlots
                    -> Decoder s (Lazy.ByteString -> UnsizedHeader)
decodeUnsizedHeader epochSlots =
    fillInByteString <$> CC.fromCBORABlockOrBoundaryHdr epochSlots
  where
    fillInByteString :: CC.ABlockOrBoundaryHdr ByteSpan
                     -> Lazy.ByteString
                     -> UnsizedHeader
    fillInByteString it theBytes = mkUnsizedHeader epochSlots $
      Lazy.toStrict . slice theBytes <$> it
