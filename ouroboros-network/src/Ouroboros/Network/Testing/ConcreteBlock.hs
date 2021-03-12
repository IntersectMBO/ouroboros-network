{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE StandaloneDeriving         #-}
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
  , hashHeader
  , BodyHash(..)
  , ConcreteHeaderHash(..)
  , hashBody

    -- * Converting slots to times
  , convertSlotToTimeForTestsAssumingNoHardFork

    -- * Creating sample chains
  , mkChain
  , mkChainSimple
  , mkAnchoredFragment
  , mkAnchoredFragmentSimple

    -- * Generator utilities
  , mkPartialBlock
  , mkPartialBlockHeader
  , fixupBlock
  , fixupBlockHeader
  , fixupBlockAfterBlock
  , fixupChain
  , fixupAnchoredFragmentFrom
  ) where

import           Data.Function (fix)
import           Data.Hashable
import           Data.String (IsString)
import qualified Data.Text as Text
import           Data.Time.Calendar (fromGregorian)
import           Data.Time.Clock (UTCTime (..), addUTCTime, secondsToNominalDiffTime)
import           NoThunks.Class (NoThunks)

import           Codec.CBOR.Decoding (decodeInt, decodeListLenOf, decodeString,
                     decodeWord64)
import           Codec.CBOR.Encoding (encodeInt, encodeListLen, encodeString,
                     encodeWord64)
import           Codec.Serialise (Serialise (..))
import           GHC.Generics (Generic)

import           Ouroboros.Network.AnchoredFragment (Anchor (..),
                     AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block
import           Ouroboros.Network.MockChain.Chain (Chain)
import qualified Ouroboros.Network.MockChain.Chain as C
import           Ouroboros.Network.Point (withOrigin)
import           Ouroboros.Network.Util.ShowProxy

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

instance ShowProxy Block where

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
       headerBodyHash :: BodyHash                 -- ^ The hash of the corresponding block body
     }
   deriving (Show, Eq, Generic)

instance ShowProxy BlockHeader where

-- | Compute the 'HeaderHash' of the 'BlockHeader'.
--
hashHeader :: BlockHeader -> ConcreteHeaderHash
hashHeader (BlockHeader _ b c d e) = HeaderHash (hash (b, c, d, e))

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
  deriving (Show, Eq, Ord, Generic, Hashable, NoThunks)

-- | The hash of all the information in a 'BlockBody'.
--
newtype BodyHash = BodyHash Int
  deriving (Show, Eq, Ord, Generic, Hashable)

{-------------------------------------------------------------------------------
  HasHeader instances
-------------------------------------------------------------------------------}

instance StandardHash BlockHeader
instance StandardHash Block

type instance HeaderHash BlockHeader = ConcreteHeaderHash
type instance HeaderHash Block       = ConcreteHeaderHash

instance HasHeader BlockHeader where
  getHeaderFields hdr = HeaderFields {
      headerFieldHash    = headerHash hdr,
      headerFieldSlot    = headerSlot hdr,
      headerFieldBlockNo = headerBlockNo hdr
      }

instance HasFullHeader BlockHeader where
    blockPrevHash  = headerPrevHash

    -- | The header invariant is that the cached header hash is correct.
    --
    blockInvariant b =
        hashHeader b == headerHash b

instance HasHeader Block where
  getHeaderFields = castHeaderFields . getHeaderFields . blockHeader

instance HasFullHeader Block where
    blockPrevHash  = castHash . headerPrevHash . blockHeader

    -- | The block invariant is just that the actual block body hash matches the
    -- body hash listed in the header.
    --
    blockInvariant Block { blockBody, blockHeader } =
        blockInvariant blockHeader
     && headerBodyHash blockHeader == hashBody blockBody

{-------------------------------------------------------------------------------
  Constructing sample chains
-------------------------------------------------------------------------------}

-- | This takes the blocks in order from /oldest to newest/.
--
mkChain :: [(SlotNo, BlockBody)] -> Chain Block
mkChain =
    fixupChain fixupBlock
  . map (uncurry mkPartialBlock)
  . reverse

mkChainSimple :: [BlockBody] -> Chain Block
mkChainSimple = mkChain . zip [1..]

mkAnchoredFragment :: Anchor Block
                   -> [(SlotNo, BlockBody)]
                   -> AnchoredFragment Block
mkAnchoredFragment anchor =
    fixupAnchoredFragmentFrom anchor fixupBlock
  . map (uncurry mkPartialBlock)
  . reverse

mkAnchoredFragmentSimple :: [BlockBody] -> AnchoredFragment Block
mkAnchoredFragmentSimple =
    mkAnchoredFragment AnchorGenesis . zip [1..]


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
      headerHash     = partialField "headerHash",
      headerPrevHash = partialField "headerPrevHash",
      headerBlockNo  = partialField "headerBlockNo",
      headerBodyHash = hashBody body
    }
  where
    partialField n = error ("mkPartialBlock: you didn't fill in field " ++ n)

{-------------------------------------------------------------------------------
  "Fixup" is used for chain construction in the network tests. These functions
  don't make much sense for real chains.
-------------------------------------------------------------------------------}

-- | Fix up a block so that it fits on top of the given anchor. Only the block
-- number, the previous hash and the block hash are updated; the slot number and
-- the signers are kept intact.
--
fixupBlock :: (HeaderHash block ~ HeaderHash BlockHeader)
           => Anchor block -> Block -> Block
fixupBlock prev b@Block{blockBody, blockHeader} =
    b {
      blockHeader = (fixupBlockHeader prev blockHeader) {
                      headerBodyHash = hashBody blockBody
                    }
    }

-- | Fixup block header to fit it on top of a chain.  Only block number and
-- previous hash are updated; the slot and signer are kept unchanged.
--
fixupBlockHeader :: (HeaderHash block ~ HeaderHash BlockHeader)
                 => Anchor block -> BlockHeader -> BlockHeader
fixupBlockHeader prev b =
    fix $ \b' ->
    b {
      headerHash     = hashHeader b',
      headerPrevHash = castHash (AF.anchorToHash prev),
      headerBlockNo  = withOrigin (BlockNo 0) succ (AF.anchorToBlockNo prev)
    }


-- | Fixup a block so to fit it on top of a given previous block.

-- Like 'fixupBlock' but it takes the info from a given block.
--
fixupBlockAfterBlock :: Block -> Block -> Block
fixupBlockAfterBlock prev = fixupBlock (AF.anchorFromBlock prev)

fixupBlocks :: HasFullHeader b
            => (c -> b -> c)
            -> c
            -> Anchor b -- ^ Override prev hash and block number based on the anchor
            -> (Anchor b -> b -> b)
            -> [b] -> c
fixupBlocks _f z _     _fixup []      = z
fixupBlocks  f z anchor fixup (b0:c0) =
    fst (go b0 c0)
  where
    go b [] = (z `f` b', b')
      where
        b' = fixup anchor b

    go b (b1:c1) = (c' `f` b', b')
      where
        (c', b1') = go b1 c1
        b'        = fixup (AF.anchorFromBlock b1') b

-- | Fix up the block number and hashes of a 'Chain'. This also fixes up the
-- first block to chain-on from genesis, since by construction the 'Chain' type
-- starts from genesis.
--
fixupChain :: HasFullHeader b
           => (Anchor b -> b -> b)
           -> [b] -> Chain b
fixupChain =
    fixupBlocks
      (C.:>) C.Genesis
      AnchorGenesis

fixupAnchoredFragmentFrom :: HasFullHeader b
                          => Anchor b
                          -> (Anchor b -> b -> b)
                          -> [b] -> AnchoredFragment b
fixupAnchoredFragmentFrom anchor =
    fixupBlocks
      (AF.:>)
      (AF.Empty anchor)
      anchor

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
         headerBodyHash = BodyHash headerBodyHash
       } =
      encodeListLen 5
   <> encode     headerHash
   <> encode     headerPrevHash
   <> encodeWord64 headerSlot
   <> encodeWord64 headerBlockNo
   <> encodeInt  headerBodyHash

  decode = do
      decodeListLenOf 5
      BlockHeader <$> decode
                  <*> decode
                  <*> (SlotNo <$> decodeWord64)
                  <*> (BlockNo <$> decodeWord64)
                  <*> (BodyHash <$> decodeInt)

instance Serialise BlockBody where

  encode (BlockBody b) = encodeString (Text.pack b)

  decode = BlockBody . Text.unpack <$> decodeString

{-------------------------------------------------------------------------------
  Simple static time conversions, since no HardFork
-------------------------------------------------------------------------------}

-- | Arbitrarily but consistently converts slots UTCTimes.
--
-- It is only intended for use in tests. Notably it assumes a fixed system
-- start time, slot length, and the absence of a hard fork (ie no
-- HardForkCombinator). This is how it's available as a pure function.
convertSlotToTimeForTestsAssumingNoHardFork :: SlotNo -> UTCTime
convertSlotToTimeForTestsAssumingNoHardFork sl =
    flip addUTCTime startTime $
      --   ^^^ arbitrary start time for testing
    secondsToNominalDiffTime $
    fromIntegral $ unSlotNo sl * 10
      --   ^^^ arbitrary slot length for testing
  where
    startTime = UTCTime {
        utctDay     = fromGregorian 2000 1 1,
        utctDayTime = 0
      }
