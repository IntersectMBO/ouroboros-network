-- |
-- Reference implementation of Chain representation
module Chain.Abstract
    ( Chain
    , ChainFragment
    , chainHeaderHash
    , chainHeadBlockNo
    , chainHeadSlot

    -- , applyChainUpdate
    -- , applyChainUpdates

    , validChain
    , validChainExtension
    , validChainFragment
    , validChainFragmentExtension
    , genChain
    , TestChain (..)
    ) where

import qualified Data.List as L
import           Test.QuickCheck

import           Block (Block (..), BlockNo (..), HasHeader (..), HeaderHash (..), Slot (..), genNBlocks, blockHash)

-- |
-- Simple blockchain data type.
type Chain = [Block]  -- most recent block at the front

-- |
-- Like 'Chain but does not have to chain onto the genesis block. Its final
-- back pointer can be anything at all.
type ChainFragment = [Block]

chainHeaderHash :: Chain -> HeaderHash
chainHeaderHash []    = HeaderHash 0
chainHeaderHash (b:_) = blockHash b

chainHeadBlockNo :: Chain -> BlockNo
chainHeadBlockNo []    = BlockNo 0
chainHeadBlockNo (b:_) = blockNo b

chainHeadSlot :: Chain -> Slot
chainHeadSlot []    = Slot 0
chainHeadSlot (b:_) = blockSlot b

validChainFragment :: ChainFragment -> Bool
validChainFragment []     = True
validChainFragment (b:bs) = validChainFragmentExtension b bs
                         && validChainFragment bs

validChainFragmentExtension :: Block -> Chain -> Bool
validChainFragmentExtension b _
  | False <- blockInvariant b = False

validChainFragmentExtension _ []     = True -- any prevBlockId is ok
validChainFragmentExtension b (b':_) = blockPrevHash b == blockHash b'
                                    && blockSlot b > blockSlot b'

validChain :: Chain -> Bool
validChain []     = True
validChain (b:bs) = validChainExtension b bs && validChain bs

validChainExtension :: Block -> Chain -> Bool
validChainExtension b _
  | False <- blockInvariant b = False

validChainExtension b []     = blockPrevHash b == HeaderHash 0
validChainExtension b (b':_) = blockPrevHash b == blockHash b'
                            && blockSlot b > blockSlot b'

{--
  - applyChainUpdate :: ChainUpdate -> Chain -> Chain
  - applyChainUpdate (AddBlock     b)  c = b:c
  - applyChainUpdate (RollBack p)      c = go c
  -     where
  -     go [] = []
  -     go (b : bs) | blockPoint b == p = b : bs
  -                 | otherwise         = go bs
  - 
  - applyChainUpdates :: [ChainUpdate] -> Chain -> Chain
  - applyChainUpdates = flip (foldl (flip applyChainUpdate))
  --}

genChain :: Int -> Gen Chain
genChain n = genNBlocks n (HeaderHash 0) (Slot 1) (BlockNo 0)

newtype TestChain = TestChain Chain
    deriving (Show, Eq)

instance Arbitrary TestChain where
    arbitrary = do
        Positive n <- arbitrary
        TestChain <$> genChain n
    shrink (TestChain c) = TestChain <$> (L.take (length c) $ L.inits c)
