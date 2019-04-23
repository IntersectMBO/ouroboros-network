{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- This module is for examples and tests (not the library) so orphans are ok
{-# OPTIONS_GHC -Wno-orphans #-}

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

    -- * Creating sample chains
  , mkChain
  , mkChainSimple
  , mkChainFragment
  , mkChainFragmentSimple
  , mkAnchoredFragment
  , mkAnchoredFragmentSimple

    -- * Generator utilities
  , mkPartialBlock
  , mkPartialBlockHeader
  , fixupBlock
  , fixupBlockHeader
  , fixupChain
  , fixupChainFragment
  , fixupAnchoredFragment
  ) where

import           Data.FingerTree (Measured(measure))
import           Data.Hashable
import qualified Data.Text as Text
import           Data.Word (Word64)
import           Data.String (IsString)

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
import           GHC.Generics (Generic)

import           Ouroboros.Network.Block
import qualified Ouroboros.Network.Chain as C
import           Ouroboros.Network.Chain (Chain)
import qualified Ouroboros.Network.ChainFragment as CF
import           Ouroboros.Network.ChainFragment (ChainFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)

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
   deriving (Show, Eq, Generic)

newtype BlockBody = BlockBody String
  deriving (Show, Eq, Ord, IsString, Generic)

hashBody :: BlockBody -> BodyHash
hashBody (BlockBody b) = BodyHash (hash b)

-- | A block header. It retains simplified versions of all the essential
-- elements.
--
data BlockHeader = BlockHeader {
       headerHash     :: HeaderHash BlockHeader,  -- ^ The cached 'HeaderHash' of this header.
       headerPrevHash :: ChainHash BlockHeader,   -- ^ The 'headerHash' of the previous block header
       headerSlot     :: SlotNo,                  -- ^ The Ouroboros time slot index of this block
       headerBlockNo  :: BlockNo,                 -- ^ The block index from the Genesis
       headerSigner   :: BlockSigner,             -- ^ Who signed this block
       headerBodyHash :: BodyHash                 -- ^ The hash of the corresponding block body
     }
   deriving (Show, Eq, Generic)

-- | An identifier for someone signing a block.
--
-- We model this as if there were an enumerated set of valid block signers
-- (which for Ouroboros BFT is actually the case), and omit the cryptography
-- and model things as if the signatures were valid.
--
-- TODO: This can probably go, the network layer does not need to worry
-- about signatures.
newtype BlockSigner = BlockSigner Word64
  deriving (Show, Eq, Ord, Generic, Hashable)

-- | Compute the 'HeaderHash' of the 'BlockHeader'.
--
hashHeader :: BlockHeader -> ConcreteHeaderHash
hashHeader (BlockHeader _ b c d e f) = HeaderHash (hash (b, c, d, e, f))

deriving instance Hashable SlotNo
deriving instance Hashable BlockNo

-- | 'Hashable' instance for 'Hash'
--
-- We don't insist that 'Hashable' in 'StandardHash' because 'Hashable' is
-- only used in the network layer /tests/.
--
-- This requires @UndecidableInstances@ because @Hashable (HeaderHash b)@
-- is no smaller than @Hashable (ChainHash b)@.
instance Hashable (HeaderHash b) => Hashable (ChainHash b)
 -- use generic instance

-- | The hash of all the information in a 'BlockHeader'.
--
newtype ConcreteHeaderHash = HeaderHash Int
  deriving (Show, Eq, Ord, Generic, Hashable)

-- | The hash of all the information in a 'BlockBody'.
--
newtype BodyHash = BodyHash Int
  deriving (Show, Eq, Ord, Generic, Hashable)

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
  Constructing sample chains
-------------------------------------------------------------------------------}

-- | This takes the blocks in order from /oldest to newest/.
--
mkChain :: [(SlotNo, BlockBody)] -> Chain Block
mkChain = fixupChain fixupBlock
        . map (uncurry mkPartialBlock)
        . reverse

mkChainSimple :: [BlockBody] -> Chain Block
mkChainSimple = mkChain . zip [1..]

mkChainFragment :: [(SlotNo, BlockBody)] -> ChainFragment Block
mkChainFragment = fixupChainFragment fixupBlock
                . map (uncurry mkPartialBlock)
                . reverse

mkChainFragmentSimple :: [BlockBody] -> ChainFragment Block
mkChainFragmentSimple = mkChainFragment . zip [1..]

mkAnchoredFragment :: Point Block -> [(SlotNo, BlockBody)]
                   -> AnchoredFragment Block
mkAnchoredFragment anchor = fixupAnchoredFragment anchor fixupBlock
                          . map (uncurry mkPartialBlock)
                          . reverse

mkAnchoredFragmentSimple :: [BlockBody] -> AnchoredFragment Block
mkAnchoredFragmentSimple = mkAnchoredFragment (Point 0 GenesisHash) . zip [1..]


mkPartialBlock :: SlotNo -> BlockBody -> Block
mkPartialBlock sl body =
    Block {
      blockHeader = mkPartialBlockHeader sl body
    , blockBody   = body
    }

mkPartialBlockHeader :: SlotNo -> BlockBody -> BlockHeader
mkPartialBlockHeader sl body =
    BlockHeader {
      headerSlot     = sl,
      headerSigner   = expectedBFTSigner sl,
      headerHash     = partialField "headerHash",
      headerPrevHash = partialField "headerPrevHash",
      headerBlockNo  = partialField "headerBlockNo",
      headerBodyHash = hashBody body
    }
  where
    partialField n = error ("mkPartialBlock: you didn't fill in field " ++ n)

    expectedBFTSigner :: SlotNo -> BlockSigner
    expectedBFTSigner (SlotNo n) = BlockSigner (n `mod` 7)

{-------------------------------------------------------------------------------
  "Fixup" is used for chain construction in the network tests. These functions
  don't make much sense for real chains.
-------------------------------------------------------------------------------}

-- | Fixup block so to fit it on top of a chain.  Only block number, previous
-- hash and block hash are updated; slot number and signers are kept intact.
--
fixupBlock :: (HasHeader block, HeaderHash block ~ HeaderHash Block)
           => Maybe block -> Block -> Block
fixupBlock pb b@Block{blockBody, blockHeader} =
    b {
      blockHeader = (fixupBlockHeader pb blockHeader) {
                      headerBodyHash = hashBody blockBody
                    }
    }

-- | Fixup block header to fit it on top of a chain.  Only block number and
-- previous hash are updated; the slot and signer are kept unchanged.
--
fixupBlockHeader :: (HasHeader block, HeaderHash block ~ HeaderHash Block)
                 => Maybe block -> BlockHeader -> BlockHeader
fixupBlockHeader pb b = b'
  where
    b' = BlockHeader {
      headerHash     = hashHeader b',
      headerPrevHash = maybe GenesisHash (BlockHash . blockHash) pb,
      headerSlot     = headerSlot b,   -- keep the existing slot number
      headerSigner   = headerSigner b, -- and signer
      headerBlockNo  = maybe (BlockNo 1) (succ . blockNo) pb,
      headerBodyHash = headerBodyHash b
    }

fixupBlocks :: (c -> b -> c)
            -> c
            -> (Maybe b -> b -> b)
            -> [b] -> c
fixupBlocks f z fixup = fst . go
  where
    go []      = (z, Nothing)
    go (b : c) = (c' `f` b', Just b')
      where
        (c', chead) = go c
        b'          = fixup chead b

fixupChain :: (Maybe b -> b -> b)
           -> [b] -> Chain b
fixupChain = fixupBlocks (C.:>) C.Genesis

fixupChainFragment :: HasHeader b
                   => (Maybe b -> b -> b)
                   -> [b] -> ChainFragment b
fixupChainFragment = fixupBlocks (CF.:>) CF.Empty

fixupAnchoredFragment :: HasHeader b
                      => Point b
                      -> (Maybe b -> b -> b)
                      -> [b] -> AnchoredFragment b
fixupAnchoredFragment anchor = fixupBlocks (AF.:>) (AF.Empty anchor)


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
