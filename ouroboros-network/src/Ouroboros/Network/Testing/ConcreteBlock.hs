{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Concrete block
--
-- The network library should not export a concrete block type at all, except
-- that it might need one in its tests (but not exported). Right now this module
-- serves to isolate this in a specific module so we can identify easily where
-- it is used; eventually it should be simplified and then moved to the
-- network layer tests; the more sophiscated block abstraction (abstracted over
-- an Ouroboros protocol) will live in the consensus layer.
module Ouroboros.Network.Testing.ConcreteBlock (
    Block(..)
  , BlockHeader(..)
  , BlockBody(..)
  , BlockSigner(..)
  , hashHeader
  , BodyHash(..)
  , ConcreteHeaderHash(..)
  , hashBody
  , fixupBlock
  , fixupBlockHeader
  , fixupBlockCF
  , fixupBlockHeaderCF
  ) where

import           Data.FingerTree (Measured(measure))
import           Data.Hashable
import qualified Data.Text as Text
import           Data.Word (Word64)
import           Codec.Serialise (Serialise (..))
import           Codec.CBOR.Encoding ( encodeListLen
                                     , encodeInt
                                     , encodeWord64
                                     , encodeString
                                     )
import           Codec.CBOR.Decoding ( decodeListLenOf
                                     , decodeInt
                                     , decodeWord64
                                     , decodeString
                                     )

import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain
import qualified Ouroboros.Network.ChainFragment as CF

{-------------------------------------------------------------------------------
  Concrete block shape used currently in the network layer

  This should only exist in the network layer /tests/.
-------------------------------------------------------------------------------}

-- | Our highly-simplified version of a block. It retains the separation
-- between a block header and body, which is a detail needed for the protocols.
--
data Block = Block {
       blockHeader :: BlockHeader,
       blockBody   :: BlockBody
     }
   deriving (Show, Eq)

data BlockBody = BlockBody String
  deriving (Show, Eq, Ord)

hashBody :: BlockBody -> BodyHash
hashBody (BlockBody b) = BodyHash (hash b)

-- | A block header. It retains simplified versions of all the essential
-- elements.
--
data BlockHeader = BlockHeader {
       headerHash     :: HeaderHash BlockHeader,  -- ^ The cached 'HeaderHash' of this header.
       headerPrevHash :: Hash BlockHeader,        -- ^ The 'headerHash' of the previous block header
       headerSlot     :: SlotNo,                    -- ^ The Ouroboros time slot index of this block
       headerBlockNo  :: BlockNo,                 -- ^ The block index from the Genesis
       headerSigner   :: BlockSigner,             -- ^ Who signed this block
       headerBodyHash :: BodyHash                 -- ^ The hash of the corresponding block body
     }
   deriving (Show, Eq)

-- | An identifier for someone signing a block.
--
-- We model this as if there were an enumerated set of valid block signers
-- (which for Ouroboros BFT is actually the case), and omit the cryptography
-- and model things as if the signatures were valid.
--
-- TODO: This can probably go, the network layer does not need to worry
-- about signatures.
newtype BlockSigner = BlockSigner Word64
  deriving (Show, Eq, Ord, Hashable)

-- | Compute the 'HeaderHash' of the 'BlockHeader'.
--
hashHeader :: BlockHeader -> ConcreteHeaderHash
hashHeader (BlockHeader _ b c d e f) = HeaderHash (hash (b, c, d, e, f))

-- | The hash of all the information in a 'BlockHeader'.
--
newtype ConcreteHeaderHash = HeaderHash Int
  deriving (Show, Eq, Ord, Hashable)

-- | The hash of all the information in a 'BlockBody'.
--
newtype BodyHash = BodyHash Int
  deriving (Show, Eq, Ord, Hashable)

{-------------------------------------------------------------------------------
  HasHeader instances
-------------------------------------------------------------------------------}

instance StandardHash BlockHeader
instance StandardHash Block

instance Measured BlockMeasure BlockHeader where
  measure = blockMeasure

instance Measured BlockMeasure Block where
  measure = blockMeasure

instance HasHeader BlockHeader where
    type HeaderHash BlockHeader = ConcreteHeaderHash

    blockHash      = headerHash
    blockPrevHash  = headerPrevHash
    blockSlot      = headerSlot
    blockNo        = headerBlockNo

    -- | The header invariant is that the cached header hash is correct.
    --
    blockInvariant = \b -> hashHeader b == headerHash b

instance HasHeader Block where
    type HeaderHash Block = ConcreteHeaderHash

    blockHash      =            headerHash     . blockHeader
    blockPrevHash  = castHash . headerPrevHash . blockHeader
    blockSlot      =            headerSlot     . blockHeader
    blockNo        =            headerBlockNo  . blockHeader

    -- | The block invariant is just that the actual block body hash matches the
    -- body hash listed in the header.
    --
    blockInvariant Block { blockBody, blockHeader = BlockHeader {headerBodyHash} } =
        headerBodyHash == hashBody blockBody

{-------------------------------------------------------------------------------
  "Fixup" is used for chain construction in the network tests. These functions
  don't make much sense for real chains.
-------------------------------------------------------------------------------}

-- | Fixup block so to fit it on top of a chain.  Only block number, previous
-- hash and block hash are updated; slot number and signers are kept intact.
--
fixupBlock :: (HasHeader block, HeaderHash block ~ HeaderHash Block)
           => Chain block -> Block -> Block
fixupBlock c b@Block{blockBody, blockHeader} =
    b { blockHeader = fixupBlockHeader c (hashBody blockBody) blockHeader }

-- | Fixup block header to fit it on top of a chain.  Only block number and
-- previous hash are updated; the slot and signer are kept unchanged.
--
fixupBlockHeader :: (HasHeader block, HeaderHash block ~ HeaderHash Block)
                 => Chain block -> BodyHash -> BlockHeader -> BlockHeader
fixupBlockHeader c h b = b'
  where
    b' = BlockHeader {
      headerHash     = hashHeader b',
      headerPrevHash = castHash (headHash c),
      headerSlot     = headerSlot b,   -- keep the existing slot number
      headerSigner   = headerSigner b, -- and signer
      headerBlockNo  = succ $ headBlockNo c,
      headerBodyHash = h
    }

{-------------------------------------------------------------------------------
  Variants of "Fixup" for ChainFragment
-------------------------------------------------------------------------------}

-- | Fixup block so to fit it on top of a chain.  Only block number, previous
-- hash and block hash are updated; slot number and signers are kept intact.
--
fixupBlockCF :: (HasHeader block, HeaderHash block ~ HeaderHash Block)
             => CF.ChainFragment block -> Block -> Block
fixupBlockCF c b@Block{blockBody, blockHeader} =
    b { blockHeader = fixupBlockHeaderCF c (hashBody blockBody) blockHeader }

-- | Fixup block header to fit it on top of a chain.  Only block number and
-- previous hash are updated; the slot and signer are kept unchanged.
--
fixupBlockHeaderCF :: (HasHeader block, HeaderHash block ~ HeaderHash Block)
                   => CF.ChainFragment block -> BodyHash -> BlockHeader -> BlockHeader
fixupBlockHeaderCF c h b = b'
  where
    b' = BlockHeader {
      headerHash     = hashHeader b',
      headerPrevHash = maybe GenesisHash castHash $ CF.headHash c,
                       -- In case c is empty, default to the 'GenesisHash'
                       -- instead of @'headerPrevHash' b@, as it can end up
                       -- being a 'partialField'.
      headerSlot     = headerSlot b,   -- keep the existing slot number
      headerSigner   = headerSigner b, -- and signer
      headerBlockNo  = maybe (BlockNo 1) succ $ CF.headBlockNo c,
                       -- See the comment for 'headerPrevHash'
      headerBodyHash = h
    }

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance Serialise ConcreteHeaderHash where
  encode (HeaderHash h) = encodeInt h
  decode = HeaderHash <$> decodeInt

instance Serialise BodyHash where
  encode (BodyHash h) = encodeInt h
  decode = BodyHash <$> decodeInt

instance Serialise Block where

  encode Block {blockHeader, blockBody} =
      encodeListLen 2
   <> encode blockHeader
   <> encode   blockBody

  decode = do
      decodeListLenOf 2
      Block <$> decode <*> decode

instance Serialise BlockHeader where

  encode BlockHeader {
         headerHash     = headerHash,
         headerPrevHash = headerPrevHash,
         headerSlot     = SlotNo headerSlot,
         headerBlockNo  = BlockNo headerBlockNo,
         headerSigner   = BlockSigner headerSigner,
         headerBodyHash = BodyHash headerBodyHash
       } =
      encodeListLen 6
   <> encode     headerHash
   <> encode     headerPrevHash
   <> encodeWord64 headerSlot
   <> encodeWord64 headerBlockNo
   <> encodeWord64 headerSigner
   <> encodeInt  headerBodyHash

  decode = do
      decodeListLenOf 6
      BlockHeader <$> decode
                  <*> decode
                  <*> (SlotNo <$> decodeWord64)
                  <*> (BlockNo <$> decodeWord64)
                  <*> (BlockSigner <$> decodeWord64)
                  <*> (BodyHash <$> decodeInt)

instance Serialise BlockBody where

  encode (BlockBody b) = encodeString (Text.pack b)

  decode = BlockBody . Text.unpack <$> decodeString
