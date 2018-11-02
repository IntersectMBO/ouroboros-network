{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
module Block.Concrete (
    Block(..)
  , BlockHeader(..)
  , BlockBody(..)
  , hashHeader
  , ConcreteBodyHash(..)
  , ConcreteHeaderHash(..)
  , hashBody
  , fixupBlock
  , fixupBlockHeader
  ) where

import           Data.Hashable
import           Data.Proxy
import qualified Data.Text as Text
import           Test.QuickCheck

import           Block
import           Chain
import           Infra.Util
import           Serialise

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

hashBody :: BlockBody -> ConcreteBodyHash
hashBody (BlockBody b) = BodyHash (hash b)

-- | A block header. It retains simplified versions of all the essential
-- elements.
--
data BlockHeader = BlockHeader {
       headerHash     :: ConcreteHeaderHash,  -- ^ The cached 'HeaderHash' of this header.
       headerPrevHash :: ConcreteHeaderHash,  -- ^ The 'headerHash' of the previous block header
       headerSlot     :: Slot,                -- ^ The Ouroboros time slot index of this block
       headerBlockNo  :: BlockNo,             -- ^ The block index from the Genesis
       headerSigner   :: BlockSigner,         -- ^ Who signed this block
       headerBodyHash :: ConcreteBodyHash     -- ^ The hash of the corresponding block body
     }
   deriving (Show, Eq)

-- | Compute the 'HeaderHash' of the 'BlockHeader'.
--
hashHeader :: BlockHeader -> ConcreteHeaderHash
hashHeader (BlockHeader _ b c d e f) = HeaderHash (hash (b, c, d, e, f))

-- | The hash of all the information in a 'BlockHeader'.
--
newtype ConcreteHeaderHash   = HeaderHash Int
  deriving (Show, Eq, Ord, Hashable, Condense)

-- | The hash of all the information in a 'BlockBody'.
--
newtype ConcreteBodyHash     = BodyHash Int
  deriving (Show, Eq, Ord, Hashable, Condense)

{-------------------------------------------------------------------------------
  HasHeader instances
-------------------------------------------------------------------------------}

instance HasHeader BlockHeader where
    type HeaderHash BlockHeader = ConcreteHeaderHash
    type BodyHash   BlockHeader = ConcreteBodyHash

    blockHash      = headerHash
    blockPrevHash  = headerPrevHash
    blockSlot      = headerSlot
    blockNo        = headerBlockNo
    blockSigner    = headerSigner
    blockBodyHash  = headerBodyHash

    -- | The header invariant is that the cached header hash is correct.
    --
    blockInvariant = \b -> hashHeader b == headerHash b

    genesisHash _ = HeaderHash 0

instance HasHeader Block where
    type HeaderHash Block = ConcreteHeaderHash
    type BodyHash   Block = ConcreteBodyHash

    blockHash      = headerHash     . blockHeader
    blockPrevHash  = headerPrevHash . blockHeader
    blockSlot      = headerSlot     . blockHeader
    blockNo        = headerBlockNo  . blockHeader
    blockSigner    = headerSigner   . blockHeader
    blockBodyHash  = headerBodyHash . blockHeader

    -- | The block invariant is just that the actual block body hash matches the
    -- body hash listed in the header.
    --
    blockInvariant Block { blockBody, blockHeader = BlockHeader {headerBodyHash} } =
        headerBodyHash == hashBody blockBody

    genesisHash _ = genesisHash (Proxy @BlockHeader)

{-------------------------------------------------------------------------------
  "Fixup" is used for chain construction in the network tests. These functions
  don't make much sense for real chains.
-------------------------------------------------------------------------------}

-- | Fixup block so to fit it on top of a chain.  Only block number, previous
-- hash and block hash are updated; slot number and signers are kept intact.
--
fixupBlock :: (HasHeader block, HeaderHash block ~ ConcreteHeaderHash)
           => Chain block -> Block -> Block
fixupBlock c b@Block{blockBody, blockHeader} =
    b { blockHeader = fixupBlockHeader c (hashBody blockBody) blockHeader }

-- | Fixup block header to fit it on top of a chain.  Only block number and
-- previous hash are updated; the slot and signer are kept unchanged.
--
fixupBlockHeader :: (HasHeader block, HeaderHash block ~ ConcreteHeaderHash)
                 => Chain block -> ConcreteBodyHash -> BlockHeader -> BlockHeader
fixupBlockHeader c h b = b'
  where
    b' = BlockHeader {
      headerHash     = hashHeader b',
      headerPrevHash = headHash c,
      headerSlot     = headerSlot b,   -- keep the existing slot number
      headerSigner   = headerSigner b, -- and signer
      headerBlockNo  = succ $ headBlockNo c,
      headerBodyHash = h
    }

{-------------------------------------------------------------------------------
  Arbitrary instances

  TODO: Should we have a policy that Arbitrary instances should not live
  in the main modules?
-------------------------------------------------------------------------------}

--
-- Generators
--

instance Arbitrary BlockBody where
    arbitrary = BlockBody <$> vectorOf 4 (choose ('A', 'Z'))
    -- probably no need for shrink, the content is arbitrary and opaque
    -- if we add one, it might be to shrink to an empty block

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance Serialise ConcreteHeaderHash where
  encode (HeaderHash h) = encodeInt h
  decode = HeaderHash <$> decodeInt

instance Serialise ConcreteBodyHash where
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
         headerHash     = HeaderHash headerHash,
         headerPrevHash = HeaderHash headerPrevHash,
         headerSlot     = Slot headerSlot,
         headerBlockNo  = BlockNo headerBlockNo,
         headerSigner   = BlockSigner headerSigner,
         headerBodyHash = BodyHash headerBodyHash
       } =
      encodeListLen 6
   <> encodeInt  headerHash
   <> encodeInt  headerPrevHash
   <> encodeWord headerSlot
   <> encodeWord headerBlockNo
   <> encodeWord headerSigner
   <> encodeInt  headerBodyHash

  decode = do
      decodeListLenOf 6
      BlockHeader <$> (HeaderHash <$> decodeInt)
                  <*> (HeaderHash <$> decodeInt)
                  <*> (Slot <$> decodeWord)
                  <*> (BlockNo <$> decodeWord)
                  <*> (BlockSigner <$> decodeWord)
                  <*> (BodyHash <$> decodeInt)

instance Serialise BlockBody where

  encode (BlockBody b) = encodeString (Text.pack b)

  decode = BlockBody . Text.unpack <$> decodeString
