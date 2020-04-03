{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Ouroboros.Consensus.Byron.Ledger.Block (
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
  , byronHeaderIsEBB
  , byronBlockIsEBB
    -- * Low-level API
  , UnsizedHeader(..)
  , mkUnsizedHeader
  , splitSizeHint
  , joinSizeHint
  ) where

import           Data.Binary (Get, Put)
import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import           Data.ByteString (ByteString)
import qualified Data.ByteString as Strict
import           Data.FingerTree.Strict (Measured (..))
import           Data.Proxy
import           Data.Typeable
import           Data.Word
import           GHC.Generics (Generic)

import           Cardano.Binary
import           Cardano.Prelude (NoUnexpectedThunks (..))

import qualified Crypto.Hash as Crypto

import qualified Cardano.Chain.Block as CC
import qualified Cardano.Chain.Byron.API as CC
import qualified Cardano.Chain.Slotting as CC
import qualified Cardano.Crypto.Hashing as CC

import           Ouroboros.Network.Block
import           Ouroboros.Network.DeltaQ (SizeInBytes)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Util.Condense

import           Ouroboros.Consensus.Storage.ImmutableDB (HashInfo (..))

import           Ouroboros.Consensus.Byron.Ledger.Conversions
import           Ouroboros.Consensus.Byron.Ledger.Orphans ()

{-------------------------------------------------------------------------------
  Header hash
-------------------------------------------------------------------------------}

newtype ByronHash = ByronHash { unByronHash :: CC.HeaderHash }
  deriving stock   (Eq, Ord, Show, Generic)
  deriving newtype (ToCBOR, FromCBOR, Condense)
  deriving anyclass (NoUnexpectedThunks)

mkByronHash :: CC.ABlockOrBoundaryHdr ByteString -> ByronHash
mkByronHash = ByronHash . CC.abobHdrHash

byronHashInfo :: HashInfo ByronHash
byronHashInfo = HashInfo { hashSize, getHash, putHash }
  where
    hashSize :: Word32
    hashSize = fromIntegral $ Crypto.hashDigestSize
                                (error "proxy" :: Crypto.Blake2b_256)

    getHash :: Get ByronHash
    getHash = do
      bytes <- Get.getByteString (fromIntegral hashSize)
      -- Ok to use unsafe version since we get the right size for the hash
      return $! ByronHash (CC.unsafeHashFromBytes bytes)

    putHash :: ByronHash -> Put
    putHash (ByronHash h) =
      Put.putByteString $ CC.hashToBytes h

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

        -- | Hint about the block size
        --
        -- This is used only for the block fetch client. When this value is
        -- wrong, block fetch might make suboptimal decisions, but it shouldn't
        -- /break/ anything
      , byronHeaderBlockSizeHint :: !SizeInBytes
      }
    deriving (Eq, Show, Generic)

  getHeader ByronBlock{..} = ByronHeader {
        byronHeaderRaw           = CC.abobHdrFromBlock byronBlockRaw
      , byronHeaderSlotNo        = byronBlockSlotNo
      , byronHeaderHash          = byronBlockHash
      , byronHeaderBlockSizeHint = fromIntegral . Strict.length $
          -- For some reason regular blocks lack a 'Decoded' instance
          case byronBlockRaw of
            CC.ABOBBlock    blk -> CC.blockAnnotation blk
            CC.ABOBBoundary blk -> recoverBytes       blk
      }

instance Condense (Header ByronBlock) where
  condense = CC.aBlockOrBoundaryHdr condense condense . byronHeaderRaw

instance NoUnexpectedThunks (Header ByronBlock) where
  showTypeOf _ = show $ typeRep (Proxy @(Header ByronBlock))

mkByronHeader :: CC.EpochSlots
              -> CC.ABlockOrBoundaryHdr ByteString
              -> SizeInBytes -- ^ Block size hint
              -> Header ByronBlock
mkByronHeader epochSlots = joinSizeHint . mkUnsizedHeader epochSlots

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

byronHeaderIsEBB :: Header ByronBlock -> IsEBB
byronHeaderIsEBB = go . byronHeaderRaw
  where
    go :: CC.ABlockOrBoundaryHdr a -> IsEBB
    go (CC.ABOBBlockHdr    _) = IsNotEBB
    go (CC.ABOBBoundaryHdr _) = IsEBB

byronBlockIsEBB :: ByronBlock -> IsEBB
byronBlockIsEBB = byronHeaderIsEBB . getHeader

{-------------------------------------------------------------------------------
  Unsized header
-------------------------------------------------------------------------------}

-- | Header without a size hint
--
-- Defined in order to support backwards compatible binary encodings.
data UnsizedHeader = UnsizedHeader {
      unsizedHeaderRaw    :: !(CC.ABlockOrBoundaryHdr ByteString)
    , unsizedHeaderSlotNo :: !SlotNo
    , unsizedHeaderHash   :: !ByronHash
    }

mkUnsizedHeader :: CC.EpochSlots
                -> CC.ABlockOrBoundaryHdr ByteString
                -> UnsizedHeader
mkUnsizedHeader epochSlots hdr = UnsizedHeader {
      unsizedHeaderRaw    = hdr
    , unsizedHeaderSlotNo = fromByronSlotNo $ CC.abobHdrSlotNo epochSlots hdr
    , unsizedHeaderHash   = mkByronHash hdr
    }

splitSizeHint :: Header ByronBlock -> (UnsizedHeader, SizeInBytes)
splitSizeHint ByronHeader{..} = (
      UnsizedHeader {
          unsizedHeaderRaw    = byronHeaderRaw
        , unsizedHeaderSlotNo = byronHeaderSlotNo
        , unsizedHeaderHash   = byronHeaderHash
        }
    , byronHeaderBlockSizeHint
    )

joinSizeHint :: UnsizedHeader -> SizeInBytes -> Header ByronBlock
joinSizeHint UnsizedHeader{..} size = ByronHeader {
      byronHeaderRaw           = unsizedHeaderRaw
    , byronHeaderSlotNo        = unsizedHeaderSlotNo
    , byronHeaderHash          = unsizedHeaderHash
    , byronHeaderBlockSizeHint = size
    }
