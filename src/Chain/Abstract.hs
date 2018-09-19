-- |
-- Reference implementation of Chain representation
module Chain.Abstract
    ( Chain
    , ChainFragment
    , chainHeadBlockId
    , chainHeadSlot

    , applyChainUpdate
    , applyChainUpdates

    , validChain
    , validChainExtension
    , validChainFragment
    , validChainFragmentExtension
    , genChain
    , TestChain (..)
    ) where

import qualified Data.List as L
import           Test.QuickCheck

import           Block (Block (..), BlockId, Slot, blockPoint, genNBlocks, hashBlock)
import           Chain.Update (ChainUpdate (..))

-- |
-- Simple blockchain data type.
type Chain = [Block]  -- most recent block at the front

-- |
-- Like 'Chain but does not have to chain onto the genesis block. Its final
-- back pointer can be anything at all.
type ChainFragment = [Block]

chainHeadBlockId :: Chain -> BlockId
chainHeadBlockId []    = 0
chainHeadBlockId (b:_) = blockId b

chainHeadSlot :: Chain -> Slot
chainHeadSlot []    = 0
chainHeadSlot (b:_) = blockSlot b

validChainFragment :: ChainFragment -> Bool
validChainFragment []     = True
validChainFragment (b:bs) = validChainFragmentExtension b bs
                         && validChainFragment bs

validChainFragmentExtension :: Block -> Chain -> Bool
validChainFragmentExtension b _
  | blockId b /= hashBlock b = False

validChainFragmentExtension _ []     = True -- any prevBlockId is ok
validChainFragmentExtension b (b':_) = prevBlockId b == blockId b'
                                    && blockSlot b > blockSlot b'

validChain :: Chain -> Bool
validChain []     = True
validChain (b:bs) = validChainExtension b bs && validChain bs

validChainExtension :: Block -> Chain -> Bool
validChainExtension b _
  | blockId b /= hashBlock b = False

validChainExtension b []     = prevBlockId b == 0
validChainExtension b (b':_) = prevBlockId b == blockId b'
                            && blockSlot b > blockSlot b'

applyChainUpdate :: ChainUpdate -> Chain -> Chain
applyChainUpdate (AddBlock     b)  c = b:c
applyChainUpdate (RollBack p)      c = go c
    where
    go [] = []
    go (b : bs) | blockPoint b == p = b : bs
                | otherwise         = go bs

applyChainUpdates :: [ChainUpdate] -> Chain -> Chain
applyChainUpdates = flip (foldl (flip applyChainUpdate))

genChain :: Int -> Gen Chain
genChain n = genNBlocks n 0 1

newtype TestChain = TestChain Chain
    deriving (Show, Eq)

instance Arbitrary TestChain where
    arbitrary = do
        Positive n <- arbitrary
        TestChain <$> genChain n
    shrink (TestChain c) = TestChain <$> (L.take (length c) $ L.inits c)
