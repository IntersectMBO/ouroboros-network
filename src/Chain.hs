{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE NamedFieldPuns  #-}

-- | Reference implementation of a representation of a block chain
--
module Chain (
  -- * Chain type and fundamental operations
  Chain(..),
  valid,
  foldChain,

  -- * Point type
  Point(..),
  blockPoint,

  -- * Chain construction and inspection
  -- ** Genesis
  genesis,
  genesisPoint,
  genesisSlot,
  genesisHash,
  genesisBlockNo,

  -- ** Head inspection
  headPoint,
  headSlot,
  headHash,
  headBlockNo,

  -- ** Basic operations
  head,
  toList,
  fromList,
  drop,
  Chain.length,
  Chain.null,

  -- ** Update type and operations
  ChainUpdate(..),
  addBlock,
  rollback,
  applyChainUpdate,
  applyChainUpdates,

  -- * Special operations
  pointOnChain,
  successorBlock,
  lookupBySlot,
  selectChain,
  selectPoints,
  findFirstPoint,
  intersectChains,
  fixupBlock,
  fixupBlockHeader,
  isPrefixOf
  ) where

import Prelude hiding (head, drop)

import Block ( Block(..), BlockHeader(..), HasHeader(..)
             , Slot(..), BlockNo (..), HeaderHash(..)
             , BlockBody(..), BodyHash(..)
             , Slot(..), BlockNo(..), BlockSigner(..)
             , HeaderHash(..), hashHeader, hashBody )

import Control.Exception (assert)
import qualified Data.List as L
import Data.Maybe (fromMaybe, listToMaybe)


--
-- Blockchain type
--

data Chain block = Genesis | Chain block :> block
  deriving (Eq, Show, Functor)

infixl 5 :>

foldChain :: (a -> b -> a) -> a -> Chain b -> a
foldChain _blk gen Genesis  = gen
foldChain  blk gen (c :> b) = blk (foldChain blk gen c) b

--
-- Points on blockchains
--

-- | A point on the chain is identified by its 'Slot' and 'HeaderHash'.
--
-- The 'Slot' tells us where to look and the 'HeaderHash' either simply servesk
-- as a check, or in some contexts it disambiguates blocks from different forks
-- that were in the same slot.
--
data Point = Point {
       pointSlot :: Slot,
       pointHash :: HeaderHash
     }
  deriving (Eq, Ord, Show)

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

genesisBlockNo :: BlockNo
genesisBlockNo = BlockNo 0

genesisPoint :: Point
genesisPoint = Point genesisSlot genesisHash

valid :: HasHeader block => Chain block -> Bool
valid Genesis  = True
valid (c :> b) = valid c && validExtension c b

validExtension ::  HasHeader block => Chain block -> block -> Bool
validExtension c b = blockInvariant b
                  && headHash c == blockPrevHash b
                  && headSlot c <  blockSlot b
                  && headBlockNo c == pred (blockNo b)

head :: Chain b -> Maybe b
head Genesis  = Nothing
head (_ :> b) = Just b

headPoint :: HasHeader block => Chain block -> Point
headPoint Genesis  = genesisPoint
headPoint (_ :> b) = blockPoint b

headSlot :: HasHeader block => Chain block -> Slot
headSlot = pointSlot . headPoint

headHash :: HasHeader block => Chain block -> HeaderHash
headHash = pointHash . headPoint

headBlockNo :: HasHeader block => Chain block -> BlockNo
headBlockNo Genesis  = genesisBlockNo
headBlockNo (_ :> b) = blockNo b

-- | Produce the list of blocks, from most recent back to genesis
--
toList :: Chain block -> [block]
toList = foldChain (flip (:)) []

-- | Make a chain from a list of blocks. The head of the list is the head
-- of the chain.
--
fromList :: HasHeader block => [block] -> Chain block
fromList bs = assert (valid c) c
  where
    c = foldr (flip (:>)) Genesis bs

drop :: Int -> Chain block -> Chain block
drop 0 c        = c
drop _ Genesis  = Genesis
drop n (c :> _) = drop (n - 1) c

length :: Chain block -> Int
length = foldChain (\n _ -> n+1) 0

null :: Chain block -> Bool
null Genesis = True
null _       = False

addBlock :: HasHeader block => block -> Chain block -> Chain block
addBlock b c = assert (validExtension c b) $
               c :> b

pointOnChain :: HasHeader block => Point -> Chain block -> Bool
pointOnChain p Genesis        = p == genesisPoint
pointOnChain p (c :> b)
  | pointSlot p >  blockSlot b = False
  | pointSlot p == blockSlot b = pointHash p == blockHash b
  | otherwise                  = pointOnChain p c

rollback :: HasHeader block => Point -> Chain block -> Maybe (Chain block)
rollback p (c :> b) | blockPoint b == p = Just (c :> b)
                    | otherwise         = rollback p c
rollback p Genesis  | p == genesisPoint = Just Genesis
                    | otherwise         = Nothing

successorBlock :: HasHeader block => Point -> Chain block -> Maybe block
successorBlock p c0 | headPoint c0 == p = Nothing
successorBlock p c0 = go c0
  where
    go (c :> b' :> b) | blockPoint b' == p = Just b
                      | otherwise          = go (c :> b')
    go (Genesis :> b) | p == genesisPoint  = Just b
    go _ = error "successorBlock: point not on chain"

selectChain
  :: HasHeader block
  => Chain block
  -> Chain block
  -> Chain block
selectChain c1 c2 = 
  if headBlockNo c1 >= headBlockNo c2
    then c1
    else c2

lookupBySlot
  :: HasHeader block
  => Chain block
  -> Slot
  -> Maybe block
lookupBySlot Genesis _slot = Nothing
lookupBySlot (c :> b) slot | blockSlot b == slot = Just b
                           | blockSlot b < slot  = Nothing
                           | otherwise           = lookupBySlot c slot

isPrefixOf :: Eq block => Chain block -> Chain block -> Bool
a `isPrefixOf` b = reverse (toList a) `L.isPrefixOf` reverse (toList b)


data ChainUpdate block = AddBlock block
                       | RollBack Point
  deriving Show

applyChainUpdate :: HasHeader block
                 => ChainUpdate block
                 -> Chain block
                 -> Chain block
applyChainUpdate (AddBlock b) c = addBlock b c
applyChainUpdate (RollBack p) c = fromMaybe c $ rollback p c

applyChainUpdates :: HasHeader block
                  => [ChainUpdate block]
                  -> Chain block
                  -> Chain block
applyChainUpdates = flip (foldl (flip applyChainUpdate))

-- | Select a bunch of 'Point's based on offsets from the head of the chain.
-- This is used in the chain consumer protocol as part of finding the
-- intersection between a local and remote chain.
--
-- The typical pattern is to use a selection of offsets covering the last K
-- blocks, biased towards more recent blocks. For example:
--
-- > selectPoints (0 : [ fib n | n <- [1 .. 17] ])
--
selectPoints :: HasHeader block => [Int] -> Chain block -> [Point]
selectPoints offsets =
    go relativeOffsets
  where
    relativeOffsets = zipWith (-) offsets (0:offsets)
    go [] _         = []
    go _  Genesis   = []
    go (off:offs) c = headPoint c' : go offs c'
      where
        c' = drop off c

findFirstPoint
  :: HasHeader block
  => [Point]
  -> Chain block
  -> Maybe Point
findFirstPoint [] _     = Nothing
findFirstPoint (p:ps) c
  | pointOnChain p c    = Just p
  | otherwise           = findFirstPoint ps c

intersectChains
  :: HasHeader block
  => Chain block
  -> Chain block
  -> Maybe Point
intersectChains _ Genesis   = Nothing
intersectChains c (bs :> b) =
  let p = blockPoint b
  in if pointOnChain (blockPoint b) c
       then Just p
       else intersectChains c bs


fixupBlock :: Point -> BlockNo -> Block -> Block
fixupBlock p bn b@Block{blockBody, blockHeader} =
    b { blockHeader = fixupBlockHeader p bn (hashBody blockBody) blockHeader }

fixupBlockHeader :: Point -> BlockNo -> BodyHash -> BlockHeader -> BlockHeader
fixupBlockHeader p n h b = b'
  where
    b' = BlockHeader {
      headerHash     = hashHeader b',
      headerPrevHash = pointHash p,
      headerSlot     = headerSlot b,   -- keep the existing slot number
      headerSigner   = headerSigner b, -- and signer
      headerBlockNo  = succ n,
      headerBodyHash = h
    }
