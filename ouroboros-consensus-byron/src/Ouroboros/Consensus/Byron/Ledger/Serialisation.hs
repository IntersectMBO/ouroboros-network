{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Byron.Ledger.Serialisation (
    -- * Serialisation
    encodeByronBlockWithInfo
  , encodeByronBlock
  , decodeByronBlock
  , encodeByronHeader
  , decodeByronHeader
  , encodeWrappedByronHeader
  , decodeWrappedByronHeader
  , encodeByronHeaderHash
  , decodeByronHeaderHash
    -- * Support for on-disk format
  , byronAddHeaderEnvelope
    -- * Exceptions
  , DropEncodedSizeException(..)
    -- * Low-level API
  , fakeByronBlockSizeHint
  ) where

import           Control.Exception (Exception (..), throw)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy

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
import           Ouroboros.Consensus.Node.NetworkProtocolVersion

import           Ouroboros.Consensus.Storage.ImmutableDB (BinaryInfo (..))

import           Ouroboros.Consensus.Byron.Ledger.Block
import           Ouroboros.Consensus.Byron.Ledger.NetworkProtocolVersion
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

encodeByronHeaderHash :: HeaderHash ByronBlock -> Encoding
encodeByronHeaderHash = toCBOR

decodeByronHeaderHash :: Decoder s (HeaderHash ByronBlock)
decodeByronHeaderHash = fromCBOR

-- | 'encodeByronBlock' including the offset and size of the header within the
-- resulting bytestring.
--
-- NOTE: the bytestring obtained by slicing the serialised block using the
-- header offset and size will correspond to the /header annotation/, but not
-- to the serialised header, as we add an envelope ('encodeListLen' + tag)
-- around a header in 'encodeByronHeader'. This envelope must thus still be
-- added to the sliced bytestring before it can be deserialised using
-- 'decodeByronHeader'.
encodeByronBlockWithInfo :: ByronBlock -> BinaryInfo Encoding
encodeByronBlockWithInfo blk = BinaryInfo
    { binaryBlob   = encodeByronBlock blk
    , headerOffset = 1 {- 'encodeListLen' of the outer 'Either' envelope -}
                   + 1 {- the tag -}
                   + 1 {- 'encodeListLen' of the block: header + body + ...  -}
      -- Compute the length of the annotated header
    , headerSize   = fromIntegral $ Strict.length $ case byronBlockRaw blk of
        CC.ABOBBoundary b -> CC.boundaryHeaderAnnotation $ CC.boundaryHeader b
        CC.ABOBBlock    b -> CC.headerAnnotation         $ CC.blockHeader    b
    }

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
    fillInByteString <$> CC.fromCBORABlockOrBoundary epochSlots
  where
    fillInByteString :: CC.ABlockOrBoundary ByteSpan
                     -> Lazy.ByteString
                     -> ByronBlock
    fillInByteString it theBytes = mkByronBlock epochSlots $
      Lazy.toStrict . slice theBytes <$> it

-- | Encode a header
encodeByronHeader :: SerialisationVersion ByronBlock
                  -> Header ByronBlock -> Encoding
encodeByronHeader = \case
    SerialisedToDisk ->
      -- Sending with size is compatible with 'byronAddHeaderEnvelope'
      -- (Moreover, version 1 is lossy)
      encWithSize
    SerialisedAcrossNetwork (SerialisedNodeToNode ByronNodeToNodeVersion1) ->
      encWithoutSize
    SerialisedAcrossNetwork (SerialisedNodeToNode ByronNodeToNodeVersion2) ->
      encWithSize
    SerialisedAcrossNetwork (SerialisedNodeToClient _) ->
      -- See 'Ouroboros.Consensus.Network.NodeToClient.defaultCodecs'
      -- for the encoders/decoders used for node-to-client communication
      error "encodeByronHeader: not used"
  where
    encWithoutSize, encWithSize :: Header ByronBlock -> Encoding
    encWithoutSize = encodeUnsizedHeader . fst . splitSizeHint
    encWithSize    = uncurry (flip encodeSizedHeader) . splitSizeHint

-- | Inverse of 'encodeByronHeader'
decodeByronHeader :: CC.EpochSlots
                  -> SerialisationVersion ByronBlock
                  -> Decoder s (Lazy.ByteString -> Header ByronBlock)
decodeByronHeader epochSlots = \case
    SerialisedToDisk ->
      -- Sending with size is compatible with 'byronAddHeaderEnvelope'
      -- (Moreover, version 1 is lossy)
      decWithSize
    SerialisedAcrossNetwork (SerialisedNodeToNode ByronNodeToNodeVersion1) ->
      decWithoutSize
    SerialisedAcrossNetwork (SerialisedNodeToNode ByronNodeToNodeVersion2) ->
      decWithSize
    SerialisedAcrossNetwork (SerialisedNodeToClient _) ->
      error "decodeByronHeader: not used"
  where
    decWithoutSize, decWithSize :: Decoder s (Lazy.ByteString -> Header ByronBlock)
    decWithoutSize = (flip joinSizeHint fakeByronBlockSizeHint .) <$>
                       decodeUnsizedHeader epochSlots
    decWithSize    = const . uncurry (flip joinSizeHint) <$>
                       decodeSizedHeader epochSlots

-- | Encode wrapped header
--
-- When we get a header from the chain DB, we add the missing envelope
-- ('byronAddHeaderEnvelope'). This envelope is valid for version 2.
-- If we are using version 1 on the wire, we must alter this envelope.
-- If we are using version 2, there is nothing to do.
--
-- For either version we add a CBOR-in-CBOR wrapper.
encodeWrappedByronHeader :: SerialisationAcrossNetwork ByronBlock
                         -> Serialised (Header ByronBlock) -> Encoding
encodeWrappedByronHeader = \case
    SerialisedNodeToNode ByronNodeToNodeVersion1 ->
      encWithoutSize
    SerialisedNodeToNode ByronNodeToNodeVersion2 ->
      encWithSize
    SerialisedNodeToClient _ ->
      error "encodeWrappedByronHeader: not used"
  where
    encWithoutSize, encWithSize :: Serialised (Header ByronBlock) -> Encoding
    encWithoutSize = encode . dropEncodedSize
    encWithSize    = encode

-- | Decode wrapped header
--
-- See 'encodeWrappedByronHeader' for details.
decodeWrappedByronHeader :: SerialisationAcrossNetwork ByronBlock
                         -> Decoder s (Serialised (Header ByronBlock))
decodeWrappedByronHeader = \case
    SerialisedNodeToNode ByronNodeToNodeVersion1 ->
      decWithoutSize
    SerialisedNodeToNode ByronNodeToNodeVersion2 ->
      decWithSize
    SerialisedNodeToClient _ ->
      error "decodeWrappedByronHeader: not used"
  where
    decWithoutSize, decWithSize :: Decoder s (Serialised (Header ByronBlock))
    decWithoutSize = fakeEncodedSize <$> decode
    decWithSize    = decode

-- | When given the raw header bytes extracted from the block, i.e., the
-- header annotation of 'CC.AHeader' or 'CC.ABoundaryHdr', we still need to
-- prepend some bytes so that we can use 'decodeByronHeader' to decode it:
byronAddHeaderEnvelope
  :: IsEBB
  -> SizeInBytes  -- ^ Block size
  -> Lazy.ByteString -> Lazy.ByteString
byronAddHeaderEnvelope isEBB blockSize bs =
    CBOR.toLazyByteString $
      encodeEncodedWithSize $
        EncodedWithSize blockSize $ Serialised (tagEBB bs)
  where
    -- Add the tag distinguishing a regular block from an EBB
    -- (the tag expected by 'CC.fromCBORABlockOrBoundaryHdr')
    tagEBB :: Lazy.ByteString -> Lazy.ByteString
    tagEBB = mappend $ case isEBB of
               IsEBB    -> "\130\NUL"
               IsNotEBB -> "\130\SOH"

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
