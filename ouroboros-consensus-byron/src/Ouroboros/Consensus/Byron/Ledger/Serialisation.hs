{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Byron.Ledger.Serialisation (
    -- * Data family instances
    NestedCtxt_(..)
  , RawHeader
  , RawBoundaryHeader
    -- * Serialisation
  , byronBlockEncodingOverhead
  , encodeByronBlock
  , decodeByronBlock
  , decodeByronRegularBlock
  , decodeByronBoundaryBlock
  , encodeByronRegularHeader
  , decodeByronRegularHeader
  , encodeByronBoundaryHeader
  , decodeByronBoundaryHeader
  , encodeByronHeaderHash
  , decodeByronHeaderHash
    -- * Support for on-disk format
  , byronBinaryBlockInfo
    -- * Unsized header
  , addV1Envelope
  , dropV1Envelope
  , fakeByronBlockSizeHint
  , encodeUnsizedHeader
  , decodeUnsizedHeader
  ) where

import           Control.Monad.Except
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import           Data.Word (Word32)

import qualified Codec.CBOR.Encoding as CBOR
import           Codec.Serialise (Serialise (..))

import           Cardano.Binary

import qualified Cardano.Chain.Block as CC
import qualified Cardano.Chain.Slotting as CC

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
  Type synonyms
-------------------------------------------------------------------------------}

type RawBoundaryHeader = CC.ABoundaryHeader Strict.ByteString
type RawHeader         = CC.AHeader         Strict.ByteString

{-------------------------------------------------------------------------------
  Nested contents
-------------------------------------------------------------------------------}

-- | Since the Byron header does not contain the size, we include it in the
-- nested type instead.
data instance NestedCtxt_ ByronBlock f a where
  CtxtByronRegular ::
       !SizeInBytes
    -> NestedCtxt_ ByronBlock Header RawHeader

  -- | In order to reconstruct 'Header ByronBlock' we need the 'SlotNo'
  --
  -- We could compute that using 'EpochSlots', but we don't have that available
  -- here.
  CtxtByronBoundary ::
       !SizeInBytes
    -> NestedCtxt_ ByronBlock Header (SlotNo, RawBoundaryHeader)

deriving instance Show (NestedCtxt_ ByronBlock f a)

instance SameDepIndex (NestedCtxt_ ByronBlock f) where
  sameDepIndex (CtxtByronRegular size) (CtxtByronRegular size') = do
      guard (size == size')
      return Refl
  sameDepIndex (CtxtByronBoundary size) (CtxtByronBoundary size') = do
      guard (size == size')
      return Refl
  sameDepIndex _ _ =
      Nothing

instance HasNestedContent Header ByronBlock where
  unnest hdr = case byronHeaderRaw hdr of
      CC.ABOBBoundaryHdr h -> DepPair (NestedCtxt (CtxtByronBoundary blockSize)) (slotNo, h)
      CC.ABOBBlockHdr    h -> DepPair (NestedCtxt (CtxtByronRegular  blockSize)) h
    where
      blockSize = byronHeaderBlockSizeHint hdr
      slotNo    = byronHeaderSlotNo        hdr

  nest = \case
      DepPair (NestedCtxt (CtxtByronBoundary blockSize)) (slotNo, h) ->
        mkBoundaryByronHeader slotNo h blockSize
      DepPair (NestedCtxt (CtxtByronRegular blockSize)) h ->
        mkRegularByronHeader h blockSize

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

-- | Encodes a raw Byron header /without/ a tag indicating whether it's a
-- regular header or an EBB header.
--
-- Uses the annotation, so cheap.
encodeByronRegularHeader :: RawHeader -> Encoding
encodeByronRegularHeader = CBOR.encodePreEncoded . CC.headerAnnotation

-- | Inverse of 'encodeByronRegularHeader'
decodeByronRegularHeader
  :: CC.EpochSlots
  -> Decoder s (Lazy.ByteString -> RawHeader)
decodeByronRegularHeader epochSlots =
    flip annotationBytes <$> CC.fromCBORAHeader epochSlots

-- | Encodes a raw Byron EBB header /without/ a tag indicating whether it's a
-- regular header or an EBB header.
--
-- Uses the annotation, so cheap.
encodeByronBoundaryHeader :: RawBoundaryHeader -> Encoding
encodeByronBoundaryHeader = CBOR.encodePreEncoded . CC.boundaryHeaderAnnotation

-- | Inverse of 'encodeByronBoundaryHeader'
decodeByronBoundaryHeader :: Decoder s (Lazy.ByteString -> RawBoundaryHeader)
decodeByronBoundaryHeader =
    flip annotationBytes <$> CC.fromCBORABoundaryHeader

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
  V1 envelope: unsized header

  These are auxiliary functions for encoding/decoding the Byron header.
-------------------------------------------------------------------------------}

-- | A 'CC.ABlockOrBoundary' is a CBOR 2-tuple of a 'Word' (0 = EBB, 1 =
-- regular block) and block/ebb payload. This function returns the bytes that
-- should be prepended to the payload, i.e., the byte indicating it's a CBOR
-- 2-tuple and the 'Word' indicating whether its an EBB or regular block.
isEbbEnvelope :: IsEBB -> Lazy.ByteString
isEbbEnvelope = \case
  IsEBB    -> "\130\NUL"
  IsNotEBB -> "\130\SOH"

addV1Envelope ::
     (SomeSecond (NestedCtxt Header) ByronBlock, Lazy.ByteString)
  -> Lazy.ByteString
addV1Envelope (SomeSecond (NestedCtxt ctxt), bs) = isEbbTag <> bs
  where
    isEbbTag = case ctxt of
      CtxtByronBoundary {} -> isEbbEnvelope IsEBB
      CtxtByronRegular  {} -> isEbbEnvelope IsNotEBB

-- | Drop the V1 EBB-or-regular-header envelope and reconstruct the context.
-- Since we don't know the block size, use 'fakeByronBlockSizeHint'.
dropV1Envelope ::
     Lazy.ByteString
  -> Except String (SomeSecond (NestedCtxt Header) ByronBlock, Lazy.ByteString)
dropV1Envelope bs = case Lazy.splitAt 2 bs of
    (prefix, suffix)
      | prefix == isEbbEnvelope IsEBB
      -> return ( SomeSecond . NestedCtxt $ CtxtByronBoundary fakeByronBlockSizeHint
                , suffix
                )
      | prefix == isEbbEnvelope IsNotEBB
      -> return ( SomeSecond . NestedCtxt $ CtxtByronRegular fakeByronBlockSizeHint
                , suffix
                )
      | otherwise
      -> throwError "decodeUnsized: invalid prefix"

-- | Fake size (used in compatibility mode)
fakeByronBlockSizeHint :: SizeInBytes
fakeByronBlockSizeHint = 2000

-- | Encode an unsized header
--
-- Does /not/ have to backwards compatible with legacy (cardano-sl) nodes
-- (which never send or store these headers), but should be inverse to
-- 'decodeSizedHeader', and moreover uses 'fromCBORABlockOrBoundaryHdr' from
-- cardano-ledger-byron, and so we don't have too much choice in this encoder.
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
