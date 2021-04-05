{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Ouroboros.Consensus.Byron.Ledger.Block (
    -- * Hash
    ByronHash (..)
  , mkByronHash
    -- * Block
  , ByronBlock (..)
  , annotateByronBlock
  , mkByronBlock
    -- * Header
  , Header (..)
  , mkBoundaryByronHeader
  , mkByronHeader
  , mkRegularByronHeader
    -- * Dealing with EBBs
  , byronBlockIsEBB
  , byronHeaderIsEBB
  , knownEBBs
    -- * Low-level API
  , UnsizedHeader (..)
  , joinSizeHint
  , mkUnsizedHeader
  , splitSizeHint
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as Strict
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Proxy
import           Data.Typeable
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks (..))

import           Cardano.Binary

import qualified Crypto.Hash as Crypto

import qualified Cardano.Chain.Block as CC
import qualified Cardano.Chain.Byron.API as CC
import qualified Cardano.Chain.Slotting as CC
import qualified Cardano.Crypto.Hashing as CC

import           Ouroboros.Network.DeltaQ (SizeInBytes)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Util (ShowProxy (..))
import           Ouroboros.Consensus.Util.Condense

import qualified Ouroboros.Consensus.Byron.EBBs as EBBs
import           Ouroboros.Consensus.Byron.Ledger.Conversions
import           Ouroboros.Consensus.Byron.Ledger.Orphans ()

{-------------------------------------------------------------------------------
  Header hash
-------------------------------------------------------------------------------}

newtype ByronHash = ByronHash { unByronHash :: CC.HeaderHash }
  deriving stock   (Eq, Ord, Show, Generic)
  deriving newtype (ToCBOR, FromCBOR, Condense)
  deriving anyclass (NoThunks)

mkByronHash :: CC.ABlockOrBoundaryHdr ByteString -> ByronHash
mkByronHash = ByronHash . CC.abobHdrHash

instance ConvertRawHash ByronBlock where
  toShortRawHash   _ = CC.abstractHashToShort . unByronHash
  fromShortRawHash _ = ByronHash . CC.unsafeAbstractHashFromShort
  hashSize         _ = fromIntegral $ Crypto.hashDigestSize
                                        (error "proxy" :: Crypto.Blake2b_256)

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

instance ShowProxy ByronBlock where

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

-- | Byron header
--
-- See 'ByronBlock' for comments on why we cache certain values.
data instance Header ByronBlock = ByronHeader {
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

instance GetHeader ByronBlock where
  getHeader ByronBlock{..} = ByronHeader {
        byronHeaderRaw           = CC.abobHdrFromBlock byronBlockRaw
      , byronHeaderSlotNo        = byronBlockSlotNo
      , byronHeaderHash          = byronBlockHash
      , byronHeaderBlockSizeHint = (+ overhead) . fromIntegral . Strict.length $
          -- For some reason regular blocks lack a 'Decoded' instance
          case byronBlockRaw of
            CC.ABOBBlock    blk -> CC.blockAnnotation blk
            CC.ABOBBoundary blk -> recoverBytes       blk
      }
    where
      -- The maximum block size is 65536, the CBOR-in-CBOR tag for this block
      -- is:
      --
      -- > D8 18          # tag(24)
      -- >    1A 00010000 # bytes(65536)
      --
      -- Which is 7 bytes, enough for up to 4294967295 bytes.
      overhead = 7 {- CBOR-in-CBOR -} + 2 {- EBB tag -}

  -- Check if a block matches its header
  --
  -- Note that we cannot check this for an EBB, as the EBB header doesn't
  -- store a hash of the EBB body.
  blockMatchesHeader hdr blk =
      CC.abobMatchesBody (byronHeaderRaw hdr) (byronBlockRaw blk)

  headerIsEBB hdr = case byronHeaderRaw hdr of
    CC.ABOBBlockHdr _       -> Nothing
    CC.ABOBBoundaryHdr bhdr -> Just
                              . EpochNo
                              . CC.boundaryEpoch
                              $ bhdr

instance Condense (Header ByronBlock) where
  condense = CC.aBlockOrBoundaryHdr condense condense . byronHeaderRaw

instance ShowProxy (Header ByronBlock) where

instance NoThunks (Header ByronBlock) where
  showTypeOf _ = show $ typeRep (Proxy @(Header ByronBlock))

mkByronHeader :: CC.EpochSlots
              -> CC.ABlockOrBoundaryHdr ByteString
              -> SizeInBytes -- ^ Block size hint
              -> Header ByronBlock
mkByronHeader epochSlots = joinSizeHint . mkUnsizedHeader epochSlots

mkRegularByronHeader :: CC.AHeader ByteString
                     -> SizeInBytes
                     -> Header ByronBlock
mkRegularByronHeader = joinSizeHint . mkRegularUnsizedHeader

mkBoundaryByronHeader :: SlotNo
                      -> CC.ABoundaryHeader ByteString
                      -> SizeInBytes
                      -> Header ByronBlock
mkBoundaryByronHeader slotNo = joinSizeHint . mkBoundaryUnsizedHeader slotNo

{-------------------------------------------------------------------------------
  HasHeader instances

  This doesn't do much more than pass to the instance for headers.
-------------------------------------------------------------------------------}

type instance HeaderHash ByronBlock = ByronHash
instance StandardHash ByronBlock

instance HasHeader ByronBlock where
  getHeaderFields = getBlockHeaderFields

instance HasHeader (Header ByronBlock) where
  getHeaderFields hdr = HeaderFields {
        headerFieldHash    = byronHeaderHash hdr
      , headerFieldSlot    = byronHeaderSlotNo hdr
      , headerFieldBlockNo = fromByronBlockNo . CC.abobHdrChainDifficulty $ byronHeaderRaw hdr
      }

instance GetPrevHash ByronBlock where
  headerPrevHash = fromByronPrevHash . CC.abobHdrPrevHash . byronHeaderRaw

fromByronPrevHash :: Maybe CC.HeaderHash -> ChainHash ByronBlock
fromByronPrevHash = \case
    Nothing -> GenesisHash
    Just h  -> BlockHash (ByronHash h)

{-------------------------------------------------------------------------------
  Dealing with EBBs
-------------------------------------------------------------------------------}

byronHeaderIsEBB :: Header ByronBlock -> IsEBB
byronHeaderIsEBB = go . byronHeaderRaw
  where
    go :: CC.ABlockOrBoundaryHdr a -> IsEBB
    go (CC.ABOBBlockHdr    _) = IsNotEBB
    go (CC.ABOBBoundaryHdr _) = IsEBB

byronBlockIsEBB :: ByronBlock -> IsEBB
byronBlockIsEBB = byronHeaderIsEBB . getHeader

knownEBBs :: Map (HeaderHash ByronBlock) (ChainHash ByronBlock)
knownEBBs = Map.fromList $ map aux EBBs.knownEBBs
  where
    aux :: (CC.HeaderHash, Maybe CC.HeaderHash)
        -> (ByronHash, ChainHash ByronBlock)
    aux (ebb, Nothing)   = (ByronHash ebb, GenesisHash)
    aux (ebb, Just prev) = (ByronHash ebb, BlockHash (ByronHash prev))

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
mkUnsizedHeader epochSlots = \case
    CC.ABOBBlockHdr    hdr -> mkRegularUnsizedHeader hdr
    CC.ABOBBoundaryHdr hdr -> mkBoundaryUnsizedHeader slotNo hdr
      where
        slotNo = fromByronSlotNo $
            CC.boundaryBlockSlot epochSlots (CC.boundaryEpoch hdr)

mkRegularUnsizedHeader :: CC.AHeader ByteString -> UnsizedHeader
mkRegularUnsizedHeader hdr = UnsizedHeader {
      unsizedHeaderRaw    = hdr'
    , unsizedHeaderSlotNo = fromByronSlotNo $ CC.headerSlot hdr
    , unsizedHeaderHash   = mkByronHash hdr'
    }
  where
    hdr' :: CC.ABlockOrBoundaryHdr ByteString
    hdr' = CC.ABOBBlockHdr hdr

-- | For a boundary header, we must be told the slot
mkBoundaryUnsizedHeader :: SlotNo
                        -> CC.ABoundaryHeader ByteString
                        -> UnsizedHeader
mkBoundaryUnsizedHeader slotNo hdr = UnsizedHeader {
      unsizedHeaderRaw    = hdr'
    , unsizedHeaderSlotNo = slotNo
    , unsizedHeaderHash   = mkByronHash hdr'
    }
  where
    hdr' :: CC.ABlockOrBoundaryHdr ByteString
    hdr' = CC.ABOBBoundaryHdr hdr

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
