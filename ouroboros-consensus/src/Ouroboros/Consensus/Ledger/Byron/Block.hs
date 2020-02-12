{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Ouroboros.Consensus.Ledger.Byron.Block (
    -- * Hash
    ByronHash(..)
  , mkByronHash
  , byronHashInfo
    -- * Block
  , ByronBlock(..)
  , mkByronBlock
  , annotateByronBlock
    -- * Header
  , Header(..)
  , mkByronHeader
    -- * Auxiliary functions
  , countByronGenTxs
  , byronHeaderIsEBB
  , byronBlockIsEBB
    -- * Serialisation
  , encodeByronBlockWithInfo
  , encodeByronBlock
  , decodeByronBlock
  , encodeByronHeader
  , decodeByronHeader
  , encodeByronHeaderHash
  , decodeByronHeaderHash
  , byronAddHeaderEnvelope
  ) where

import           Control.Arrow ((&&&))
import           Control.Monad.Except
import           Data.Binary (Get, Put)
import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import qualified Data.ByteArray as ByteArray
import           Data.ByteString (ByteString)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import           Data.FingerTree.Strict (Measured (..))
import           Data.Maybe (maybeToList)
import           Data.Proxy
import           Data.Typeable
import           Data.Word
import           GHC.Generics (Generic)

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import           Codec.Serialise (Serialise (..))

import           Cardano.Binary
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Cardano.Slotting.Slot (WithOrigin (..), withOrigin)

import qualified Crypto.Hash as Crypto

import qualified Cardano.Chain.Block as CC
import qualified Cardano.Chain.Byron.API as CC
import qualified Cardano.Chain.Delegation as CC.Delegation
import qualified Cardano.Chain.Slotting as CC
import qualified Cardano.Chain.Update as CC.Update
import qualified Cardano.Chain.UTxO as CC
import qualified Cardano.Crypto.Hashing as CC

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Byron.Conversions
import           Ouroboros.Consensus.Ledger.Byron.Orphans ()
import           Ouroboros.Consensus.Util.Condense

import           Ouroboros.Storage.ImmutableDB (BinaryInfo (..), HashInfo (..))

{-------------------------------------------------------------------------------
  Header hash
-------------------------------------------------------------------------------}

newtype ByronHash = ByronHash { unByronHash :: CC.HeaderHash }
  deriving stock   (Eq, Ord, Show, Generic)
  deriving newtype (ToCBOR, FromCBOR, Condense)
  deriving anyclass (NoUnexpectedThunks)

instance Serialise ByronHash where
  decode = decodeByronHeaderHash
  encode = encodeByronHeaderHash

mkByronHash :: CC.ABlockOrBoundaryHdr ByteString -> ByronHash
mkByronHash = ByronHash . CC.abobHdrHash

byronHashInfo :: HashInfo ByronHash
byronHashInfo = HashInfo { hashSize, getHash, putHash }
  where
    hashSize :: Word32
    hashSize = fromIntegral $ CC.hashDigestSize' @Crypto.Blake2b_256

    getHash :: Get ByronHash
    getHash = do
      bytes <- Get.getByteString (fromIntegral hashSize)
      case Crypto.digestFromByteString bytes of
        Nothing     -> fail "digestFromByteString failed"
        Just digest -> return $ ByronHash $ CC.AbstractHash digest

    putHash :: ByronHash -> Put
    putHash (ByronHash (CC.AbstractHash digest)) =
      Put.putByteString $ ByteArray.convert digest

{-------------------------------------------------------------------------------
  Block
-------------------------------------------------------------------------------}

-- | Byron block
--
-- We cache two bits of information:
--
-- * We cache the slot number as this is not readily available for EBBs.
--   Having it cached allows us to e.g. give a 'HasHeader' instance.
-- * We cache the hash as this is expensive to compute and we need it often.
data ByronBlock = ByronBlock {
      byronBlockRaw    :: !(CC.ABlockOrBoundary ByteString)
    , byronBlockSlotNo :: !SlotNo
    , byronBlockHash   :: !ByronHash
    }
  deriving (Eq, Show)

instance Condense ByronBlock where
  condense = condense . byronBlockRaw

mkByronBlock :: CC.EpochSlots -> CC.ABlockOrBoundary ByteString -> ByronBlock
mkByronBlock epochSlots blk = ByronBlock {
      byronBlockRaw    = blk
    , byronBlockSlotNo = fromByronSlotNo $ CC.abobHdrSlotNo epochSlots hdr
    , byronBlockHash   = mkByronHash hdr
    }
  where
    hdr = CC.abobHdrFromBlock blk

-- | Construct Byron block from unannotated 'CC.Block'
--
-- This should be used only when forging blocks (not when receiving blocks
-- over the wire).
annotateByronBlock :: CC.EpochSlots -> CC.Block -> ByronBlock
annotateByronBlock es = mkByronBlock es . CC.ABOBBlock . CC.reAnnotateBlock es

{-------------------------------------------------------------------------------
  Header
-------------------------------------------------------------------------------}

instance GetHeader ByronBlock where
  -- | Byron header
  --
  -- See 'ByronBlock' for comments on why we cache certain values.
  data Header ByronBlock = ByronHeader {
        byronHeaderRaw    :: !(CC.ABlockOrBoundaryHdr ByteString)
      , byronHeaderSlotNo :: !SlotNo
      , byronHeaderHash   :: !ByronHash
      }
    deriving (Eq, Show, Generic)

  getHeader ByronBlock{..} = ByronHeader{
        byronHeaderRaw    = CC.abobHdrFromBlock byronBlockRaw
      , byronHeaderSlotNo = byronBlockSlotNo
      , byronHeaderHash   = byronBlockHash
      }

instance Condense (Header ByronBlock) where
  condense = CC.aBlockOrBoundaryHdr condense condense . byronHeaderRaw

instance NoUnexpectedThunks (Header ByronBlock) where
  showTypeOf _ = show $ typeRep (Proxy @(Header ByronBlock))

mkByronHeader :: CC.EpochSlots
              -> CC.ABlockOrBoundaryHdr ByteString
              -> Header ByronBlock
mkByronHeader epochSlots hdr = ByronHeader {
      byronHeaderRaw    = hdr
    , byronHeaderSlotNo = fromByronSlotNo $ CC.abobHdrSlotNo epochSlots hdr
    , byronHeaderHash   = mkByronHash hdr
    }

{-------------------------------------------------------------------------------
  HasHeader instances

  This doesn't do much more than pass to the instance for headers.
-------------------------------------------------------------------------------}

type instance HeaderHash ByronBlock = ByronHash
instance StandardHash ByronBlock

instance HasHeader ByronBlock where
  blockHash      =            blockHash     . getHeader
  blockPrevHash  = castHash . blockPrevHash . getHeader
  blockSlot      =            blockSlot     . getHeader
  blockNo        =            blockNo       . getHeader
  blockInvariant = const True

instance HasHeader (Header ByronBlock) where
  blockHash      = byronHeaderHash
  blockSlot      = byronHeaderSlotNo
  blockPrevHash  = fromByronPrevHash' . CC.abobHdrPrevHash        . byronHeaderRaw
  blockNo        = fromByronBlockNo   . CC.abobHdrChainDifficulty . byronHeaderRaw
  blockInvariant = const True

instance Measured BlockMeasure ByronBlock where
  measure = blockMeasure

fromByronPrevHash' :: Maybe CC.HeaderHash -> ChainHash (Header ByronBlock)
fromByronPrevHash' = fromByronPrevHash ByronHash

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

-- | Count all (generalized) transactions in the block
countByronGenTxs :: ByronBlock -> Word64
countByronGenTxs ByronBlock{..} = go byronBlockRaw
  where
    go :: CC.ABlockOrBoundary a -> Word64
    go (CC.ABOBBlock block) = goBody (CC.blockBody block)
    go (CC.ABOBBoundary _)  = 0

    goBody :: CC.ABody a -> Word64
    goBody body = sum [
          count $ CC.aUnTxPayload          $ CC.bodyTxPayload     body
        , count $ CC.Delegation.getPayload $ CC.bodyDlgPayload    body
        , count $ CC.Update.payloadVotes   $ CC.bodyUpdatePayload body
        , count $ payloadProposals         $ CC.bodyUpdatePayload body
        ]

    count :: [a] -> Word64
    count = fromIntegral . length

    payloadProposals :: CC.Update.APayload a -> [CC.Update.AProposal a]
    payloadProposals = maybeToList . CC.Update.payloadProposal

byronHeaderIsEBB :: Header ByronBlock -> IsEBB
byronHeaderIsEBB = go . byronHeaderRaw
  where
    go :: CC.ABlockOrBoundaryHdr a -> IsEBB
    go (CC.ABOBBlockHdr    _) = IsNotEBB
    go (CC.ABOBBoundaryHdr _) = IsEBB

byronBlockIsEBB :: ByronBlock -> IsEBB
byronBlockIsEBB = byronHeaderIsEBB . getHeader

{-------------------------------------------------------------------------------
  Envelope
-------------------------------------------------------------------------------}

instance HasAnnTip ByronBlock where
  type TipInfo ByronBlock = IsEBB
  getTipInfo = byronHeaderIsEBB

instance ValidateEnvelope ByronBlock where
  validateEnvelope _cfg oldTip hdr = do
      when (actualBlockNo /= expectedBlockNo) $
        throwError $ UnexpectedBlockNo expectedBlockNo actualBlockNo
      when (actualSlotNo < expectedSlotNo) $
        throwError $ UnexpectedSlotNo expectedSlotNo actualSlotNo
      when (actualPrevHash /= expectedPrevHash) $
        throwError $ UnexpectedPrevHash expectedPrevHash actualPrevHash
    where
      newIsEBB :: IsEBB
      newIsEBB = byronHeaderIsEBB hdr

      actualSlotNo   :: SlotNo
      actualBlockNo  :: BlockNo
      actualPrevHash :: ChainHash ByronBlock

      actualSlotNo   =            blockSlot     hdr
      actualBlockNo  =            blockNo       hdr
      actualPrevHash = castHash $ blockPrevHash hdr

      expectedSlotNo   :: SlotNo           -- Lower bound only
      expectedBlockNo  :: BlockNo
      expectedPrevHash :: ChainHash ByronBlock

      (expectedSlotNo, expectedBlockNo, expectedPrevHash) = (
            nextSlotNo  ((annTipInfo &&& annTipSlotNo)  <$> oldTip) newIsEBB
          , nextBlockNo ((annTipInfo &&& annTipBlockNo) <$> oldTip) newIsEBB
          , withOrigin GenesisHash (BlockHash . annTipHash) oldTip
          )

      -- EBB shares its slot number with its successor
      nextSlotNo :: WithOrigin (IsEBB, SlotNo) -> IsEBB -> SlotNo
      nextSlotNo Origin          _        = SlotNo 0
      nextSlotNo (At (IsEBB, s)) IsNotEBB = s
      nextSlotNo (At (_    , s)) _        = succ s

      -- EBB shares its block number with its predecessor
      nextBlockNo :: WithOrigin (IsEBB, BlockNo) -> IsEBB -> BlockNo
      nextBlockNo Origin             _     = BlockNo 0
      nextBlockNo (At (IsNotEBB, b)) IsEBB = b
      nextBlockNo (At (_       , b)) _     = succ b

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
--
-- Should be backwards compatible with legacy (cardano-sl) nodes.
--
-- This function should be inverse to 'decodeByronHeader'
-- (which uses 'fromCBORABlockOrBoundaryHdr').
encodeByronHeader :: Header ByronBlock -> Encoding
encodeByronHeader hdr = mconcat [
      CBOR.encodeListLen 2
    , case byronHeaderRaw hdr of
        CC.ABOBBoundaryHdr h -> mconcat [
            CBOR.encodeWord 0
          , CBOR.encodePreEncoded $ CC.boundaryHeaderAnnotation h
          ]
        CC.ABOBBlockHdr h -> mconcat [
            CBOR.encodeWord 1
          , CBOR.encodePreEncoded $ CC.headerAnnotation h
          ]
    ]

-- | Inverse of 'encodeByronHeader'
decodeByronHeader :: CC.EpochSlots
                  -> Decoder s (Lazy.ByteString -> Header ByronBlock)
decodeByronHeader epochSlots =
    fillInByteString <$> CC.fromCBORABlockOrBoundaryHdr epochSlots
  where
    fillInByteString :: CC.ABlockOrBoundaryHdr ByteSpan
                     -> Lazy.ByteString
                     -> Header ByronBlock
    fillInByteString it theBytes = mkByronHeader epochSlots $
      Lazy.toStrict . slice theBytes <$> it

-- | When given the raw header bytes extracted from the block, i.e. the header
-- annotation, we still need to prepend @CBOR.encodeListLen 2@ and
-- @CBOR.encodeWord x@ where @x@ is 0 for an EBB and 1 for a regular header.
byronAddHeaderEnvelope
  :: IsEBB -> Lazy.ByteString -> Lazy.ByteString
byronAddHeaderEnvelope = mappend . \case
    IsEBB    -> "\130\NUL"
    IsNotEBB -> "\130\SOH"
