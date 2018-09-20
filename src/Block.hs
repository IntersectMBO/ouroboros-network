{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Block
    ( Block (..)
    , BlockId
    , Slot
    , hashBlock
    , mkBlock
    , genBlock
    , genNBlocks
    , Point
    , blockPoint
    )
    where

import           Data.FingerTree (Measured (..))
import           Data.Hashable
import           Test.QuickCheck

data Block = Block {
       blockId      :: BlockId,  -- ^ hash of other fields
       prevBlockId  :: BlockId,  -- ^ 'blockId' of the previous block
       blockSlot    :: Slot,
       blockPayload :: Payload
     }
  deriving (Show, Eq)

type BlockId = Int
type Slot    = Word
type Payload = String

hashBlock :: Block -> BlockId
hashBlock Block{prevBlockId, blockSlot, blockPayload} =
    hash (prevBlockId, blockSlot, blockPayload)

--
-- Generating valid chains
--

mkBlock :: BlockId -> Slot -> Payload -> Block
mkBlock blockid' slot payload = block
  where
    block   = Block blockid blockid' slot payload
    blockid = hashBlock block

genBlock :: BlockId -> Slot -> Gen Block
genBlock blockid slot = do
    payload <- vectorOf 4 (choose ('A', 'Z'))
    return (mkBlock blockid slot payload)

genNBlocks :: Int -> BlockId -> Slot -> Gen [Block]
genNBlocks 0 _        _     = return []
genNBlocks 1 blockid0 slot0 = (:[]) <$> genBlock blockid0 slot0
genNBlocks n blockid0 slot0 = do
    c@(b':_) <- genNBlocks (n-1) blockid0 slot0
    b        <- genBlock (blockId b') (blockSlot b' + 1)
    return (b:c)


-- | A point on the chain is identified by the 'Slot' number and its 'BlockId'.
-- The 'Slot' tells us where to look and the 'BlockId' either simply serves as
-- a check, or in some contexts it disambiguates blocks from different forks
-- that were in the same slot.
--
type Point        = (Slot, BlockId)

blockPoint :: Block -> Point
blockPoint b = (blockSlot b, blockId b)
