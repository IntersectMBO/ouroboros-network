{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Reference implementation of a representation of a block chain
--
module Ouroboros.Network.MockChain.Chain (
  -- * Chain type and fundamental operations
  Chain(..),
  valid,
  validExtension,
  foldChain,
  chainToList,

  -- ** Block re-exports
  HasHeader(..),
  HeaderHash,

  -- * Point type
  Point(..),
  blockPoint,

  -- * Chain construction and inspection
  -- ** Genesis
  genesis,

  -- ** Head inspection
  headPoint,
  headSlot,
  headHash,
  headBlockNo,

  -- ** Basic operations
  head,
  toNewestFirst,
  toOldestFirst,
  fromNewestFirst,
  fromOldestFirst,
  drop,
  length,
  null,

  -- ** Update type and operations
  ChainUpdate(..),
  addBlock,
  rollback,
  applyChainUpdate,
  applyChainUpdates,

  -- * Special operations
  pointOnChain,
  pointIsAfter,
  successorBlock,
  selectChain,
  selectPoints,
  findBlock,
  selectBlockRange,
  findFirstPoint,
  intersectChains,
  isPrefixOf,

  -- * Conversion to/from ChainFragment
  toChainFragment,
  fromChainFragment,
  unvalidatedToChainFragment,

  -- * Conversion to/from AnchoredFragment
  fromAnchoredFragment,
  toAnchoredFragment,

  -- * Helper functions
  prettyPrintChain
  ) where

import           Prelude hiding (drop, head, length, null)

import           Codec.CBOR.Decoding (decodeListLen)
import           Codec.CBOR.Encoding (encodeListLen)
import           Codec.Serialise (Serialise (..))
import           Control.Exception (assert)
import qualified Data.FingerTree as FT
import qualified Data.List as L
import           GHC.Stack

import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block
import qualified Ouroboros.Network.ChainFragment as CF
import           Ouroboros.Network.Point (WithOrigin (..))

--
-- Blockchain type
--

data Chain block = Genesis | Chain block :> block
  deriving (Eq, Ord, Show, Functor)

infixl 5 :>

foldChain :: (a -> b -> a) -> a -> Chain b -> a
foldChain _blk gen Genesis  = gen
foldChain  blk gen (c :> b) = blk (foldChain blk gen c) b

-- | Make a list from a 'Chain', in newest-to-oldest order.
chainToList :: Chain block -> [block]
chainToList = foldChain (flip (:)) []

prettyPrintChain :: String -> (block -> String) -> Chain block -> String
prettyPrintChain nl ppBlock = foldChain (\s b -> s ++ nl ++ "    " ++ ppBlock b) "Genesis"

genesis :: Chain b
genesis = Genesis

valid :: HasHeader block => Chain block -> Bool
valid Genesis  = True
valid (c :> b) = valid c && validExtension c b

validExtension
  :: (HasCallStack, HasHeader block)
  => Chain block -> block -> Bool
validExtension c b = blockInvariant b
                  && headHash c == blockPrevHash b
                  -- The Ord instance for WithOrigin puts At _ after Origin.
                  -- An EBB has the same SlotNo as the block after it, hence
                  -- the loose inequality.
                  && headSlot c <= At (blockSlot b)
                  -- The empty chain has block number 0, but this is also the block
                  -- number of the genesis block.
                  && (headBlockNo c == 0 || succ (headBlockNo c) == blockNo b)

head :: Chain b -> Maybe b
head Genesis  = Nothing
head (_ :> b) = Just b

headPoint :: HasHeader block => Chain block -> Point block
headPoint Genesis  = genesisPoint
headPoint (_ :> b) = blockPoint b

headSlot :: HasHeader block => Chain block -> WithOrigin SlotNo
headSlot = pointSlot . headPoint

headHash :: HasHeader block => Chain block -> ChainHash block
headHash = pointHash . headPoint

headBlockNo :: HasHeader block => Chain block -> BlockNo
headBlockNo Genesis  = genesisBlockNo
headBlockNo (_ :> b) = blockNo b

-- | Produce the list of blocks, from most recent back to genesis
--
toNewestFirst :: Chain block -> [block]
toNewestFirst = foldChain (flip (:)) []

-- | Produce the list of blocks, from genesis to the most recent
toOldestFirst :: Chain block -> [block]
toOldestFirst = reverse . toNewestFirst

-- | Make a chain from a list of blocks. The head of the list is the head
-- of the chain.
--
fromNewestFirst :: HasHeader block => [block] -> Chain block
fromNewestFirst bs = assert (valid c) c
  where
    c = foldr (flip (:>)) Genesis bs

-- | Construct chain from list of blocks from oldest to newest
fromOldestFirst :: HasHeader block => [block] -> Chain block
fromOldestFirst bs = assert (valid c) c
  where
    c = L.foldl' (:>) Genesis bs

drop :: Int -> Chain block -> Chain block
drop 0 c        = c
drop _ Genesis  = Genesis
drop n (c :> _) = drop (n - 1) c

length :: Chain block -> Int
length = foldChain (\n _ -> n+1) 0

null :: Chain block -> Bool
null Genesis = True
null _       = False

addBlock :: (HasCallStack, HasHeader block) => block -> Chain block -> Chain block
addBlock b c = assert (validExtension c b) $
               c :> b

pointOnChain :: HasHeader block => Point block -> Chain block -> Bool
pointOnChain GenesisPoint               _       = True
pointOnChain (BlockPoint _ _)           Genesis = False
pointOnChain p@(BlockPoint pslot phash) (c :> b)
  | pslot >  blockSlot b = False
  | phash == blockHash b = True
  | otherwise            = pointOnChain p c

-- | Check whether the first point is after the second point on the chain.
-- Usually, this can simply be checked using the 'SlotNo's, but some blocks
-- may have the same 'SlotNo'.
--
-- When the first point equals the second point, the answer will be 'False'.
--
-- PRECONDITION: both points are on the chain.
pointIsAfter :: HasHeader block
             => Point block -> Point block -> Chain block -> Bool
pointIsAfter pt1 pt2 c =
    assert (pointOnChain pt1 c && pointOnChain pt2 c) $
    case pointSlot pt1 `compare` pointSlot pt2 of
      LT -> False
      GT -> True
      EQ | Just (_, afterPt2) <- CF.splitAfterPoint (toChainFragment c) pt2
         -> CF.pointOnChainFragment pt1 afterPt2
         | otherwise
         -> False

rollback :: HasHeader block => Point block -> Chain block -> Maybe (Chain block)
rollback p (c :> b) | blockPoint b == p = Just (c :> b)
                    | otherwise         = rollback p c
rollback p Genesis  | p == genesisPoint = Just Genesis
                    | otherwise         = Nothing

successorBlock :: HasHeader block => Point block -> Chain block -> Maybe block
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

isPrefixOf :: Eq block => Chain block -> Chain block -> Bool
a `isPrefixOf` b = reverse (toNewestFirst a) `L.isPrefixOf` reverse (toNewestFirst b)


applyChainUpdate :: HasHeader block
                 => ChainUpdate block block
                 -> Chain block
                 -> Maybe (Chain block)
applyChainUpdate (AddBlock b) c = Just (addBlock b c)
applyChainUpdate (RollBack p) c =       rollback p c

applyChainUpdates :: HasHeader block
                  => [ChainUpdate block block]
                  -> Chain block
                  -> Maybe (Chain block)
applyChainUpdates []     c = Just c
applyChainUpdates (u:us) c = applyChainUpdates us =<< applyChainUpdate u c

-- | Select a bunch of 'Point's based on offsets from the head of the chain.
-- This is used in the chain consumer protocol as part of finding the
-- intersection between a local and remote chain.
--
-- The typical pattern is to use a selection of offsets covering the last K
-- blocks, biased towards more recent blocks. For example:
--
-- > selectPoints (0 : [ fib n | n <- [1 .. 17] ])
--
selectPoints :: HasHeader block => [Int] -> Chain block -> [Point block]
selectPoints offsets =
    go relativeOffsets
  where
    relativeOffsets = zipWith (-) offsets (0:offsets)
    go [] _         = []
    go _  Genesis   = []
    go (off:offs) c = headPoint c' : go offs c'
      where
        c' = drop off c

findBlock
  :: (block -> Bool)
  -> Chain block
  -> Maybe block
findBlock _ Genesis  = Nothing
findBlock p (c :> b)
  | p b              = Just b
  | otherwise        = findBlock p c

selectBlockRange :: HasHeader block
                 => Chain block
                 -> Point block
                 -> Point block
                 -> Maybe [block]
selectBlockRange c from to
  | pointOnChain from c
  , pointOnChain to c
  =   Just
    . reverse
    . takeWhile (\b -> blockPoint b /= from)
    . dropWhile (\b -> blockPoint b /= to)
    . toNewestFirst
    $ c

  | otherwise
  = Nothing

findFirstPoint
  :: HasHeader block
  => [Point block]
  -> Chain block
  -> Maybe (Point block)
findFirstPoint [] _     = Nothing
findFirstPoint (p:ps) c
  | pointOnChain p c    = Just p
  | otherwise           = findFirstPoint ps c

intersectChains
  :: HasHeader block
  => Chain block
  -> Chain block
  -> Maybe (Point block)
intersectChains _ Genesis   = Nothing
intersectChains c (bs :> b) =
  let p = blockPoint b
  in if pointOnChain (blockPoint b) c
       then Just p
       else intersectChains c bs

-- * Conversions to/from 'ChainFragment'

-- | Convert a 'ChainFragment' to a 'Chain'.
fromChainFragment :: HasHeader block => CF.ChainFragment block -> Chain block
fromChainFragment = fromNewestFirst . CF.toNewestFirst

-- | Convert a 'Chain' to a 'ChainFragment'.
toChainFragment :: HasHeader block => Chain block -> CF.ChainFragment block
toChainFragment = CF.fromNewestFirst . toNewestFirst

-- | Variant of 'toChainFragment' that assumes a valid chain and will not validate
-- the constructed 'ChainFragment'.
unvalidatedToChainFragment :: HasHeader block => Chain block -> CF.ChainFragment block
unvalidatedToChainFragment = unvalidatedFromNewestFirst . toNewestFirst
  where
    unvalidatedFromNewestFirst = CF.ChainFragment . foldr (flip (FT.|>)) FT.empty

-- | Convert a 'Chain' to an 'AnchoredFragment'.
--
-- The anchor of the fragment will be 'Chain.genesisPoint'.
toAnchoredFragment :: HasHeader block => Chain block -> AF.AnchoredFragment block
toAnchoredFragment = AF.mkAnchoredFragment genesisPoint . unvalidatedToChainFragment

-- | Convert an 'AnchoredFragment' to a 'Chain'.
--
-- The anchor of the fragment must be 'Chain.genesisPoint', otherwise
-- 'Nothing' is returned.
fromAnchoredFragment :: HasHeader block => AF.AnchoredFragment block -> Maybe (Chain block)
fromAnchoredFragment af
    | AF.anchorPoint af == genesisPoint
    = Just $ fromNewestFirst $ AF.toNewestFirst af
    | otherwise
    = Nothing

--
-- Serialisation
--

instance Serialise block => Serialise (Chain block) where

  encode c = encodeListLen (fromIntegral $ length c)
          <> foldChain (\e b -> e <> encode b) mempty c

  decode = do
      n <- decodeListLen
      go genesis n
    where
      go c 0 = return c
      go c n = do b <- decode
                  go (c :> b) (n-1)
