{-# LANGUAGE DeriveFunctor #-}
-- | Reference implementation of a representation of a block chain
--
module Chain where

import Block ( Block(..), BlockHeader(..), HasHeader(..)
             , Slot(..), HeaderHash(..) {-, BlockId, invBlock
             , Point, Slot, blockPoint, pointSlot, pointHash-} )

import Control.Exception (assert)

import Test.QuickCheck

--
-- Blockchain type
--

data Chain block = Genesis | Chain block :> block
  deriving (Eq, Show, Functor)

infixl 5 :>

--
-- Points on blockchains
--

-- | A point on the chain is identified by its 'Slot' and 'HeaderHash'.
--
-- The 'Slot' tells us where to look and the 'HeaderHash' either simply serves
-- as a check, or in some contexts it disambiguates blocks from different forks
-- that were in the same slot.
--
data Point = Point {
       pointSlot :: Slot,
       pointHash :: HeaderHash
     }
  deriving (Eq, Show)

blockPoint :: HasHeader block => block -> Point
blockPoint b =
    Point {
      pointSlot = blockSlot b,
      pointHash = blockHash b
    }


genesis :: Chain b
genesis = Genesis

genesisSlot :: Slot
genesisSlot = Slot 0

genesisHash :: HeaderHash
genesisHash = HeaderHash 0

genesisPoint :: Point
genesisPoint = Point genesisSlot genesisHash


valid :: HasHeader block => Chain block -> Bool
valid Genesis  = True
valid (c :> b) = valid c && validExtension c b

validExtension ::  HasHeader block => Chain block -> block -> Bool
validExtension c b = blockInvariant b
                  && headHash c == blockPrevHash b
                  && headSlot c <  blockSlot b

head :: Chain b -> Maybe b
head Genesis  = Nothing
head (c :> b) = Just b

headPoint :: HasHeader block => Chain block -> Point
headPoint Genesis  = genesisPoint
headPoint (c :> b) = blockPoint b

headSlot :: HasHeader block => Chain block -> Slot
headSlot = pointSlot . headPoint

headHash :: HasHeader block => Chain block -> HeaderHash
headHash = pointHash . headPoint


addBlock :: HasHeader block => block -> Chain block -> Chain block
addBlock b c = assert (validExtension c b) $
               c :> b

pointOnChain :: HasHeader block => Point -> Chain block -> Bool
pointOnChain p Genesis  = p == genesisPoint
pointOnChain p (c :> b) = p == blockPoint b || pointOnChain p c

rollback :: HasHeader block => Point -> Chain block -> Chain block
rollback p (c :> b) | blockPoint b == p = c
                    | otherwise         = rollback p c
rollback p Genesis  | p == genesisPoint = Genesis
                    | otherwise         = error "rollback: point not on chain"

successorBlock :: HasHeader block => Point -> Chain block -> Maybe block
successorBlock p c0 | headPoint c0 == p = Nothing
successorBlock p c0 = go c0
  where
    go (c :> b' :> b) | blockPoint b' == p = Just b
                      | otherwise          = go (c :> b')
    go _ = error "successorBlock: point not on chain"


data ChainUpdate block = AddBlock block
                       | RollBack Point
  deriving Show

applyChainUpdate :: HasHeader block
                 => ChainUpdate block
                 -> Chain block
                 -> Chain block
applyChainUpdate (AddBlock b) c = addBlock b c
applyChainUpdate (RollBack p) c = rollback p c

applyChainUpdates :: HasHeader block
                  => [ChainUpdate block]
                  -> Chain block
                  -> Chain block
applyChainUpdates = flip (foldl (flip applyChainUpdate))


--
-- Generators
--

{-
newtype TestBlockChain = TestBlockChain Chain
    deriving (Show, Eq)

instance Arbitrary TestChain where
    arbitrary = do
        Positive n <- arbitrary
        TestChain <$> genChain n
    shrink (TestChain c) = TestChain <$> (L.take (length c) $ L.inits c)


genChain :: Int -> Gen Chain
genChain n = genNBlocks n 0 1
-}
-- make chain by making bodies and deriving the hashes
-- shrinking by remaking the hashes

--prop_addBlock:
-- after adding a block, that block is at the head
-- chain is still valid
-- removing the block gives the original


-- prop_rollback:
--   prerequisite: point is on chain
-- chain is a prefix of original
-- chain head point is the rollback point

{-
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

-}
