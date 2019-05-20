{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Ouroboros.Network.ChainFragment (
  -- * ChainFragment type and fundamental operations
  ChainFragment(.., Empty, (:>), (:<)),
  valid,
  validExtension,
  isValidSuccessorOf,
  foldChainFragment,
  mapChainFragment,

  -- ** Block re-exports
  HasHeader(..),
  Point(..),
  castPoint,
  blockPoint,

  -- * ChainFragment construction and inspection
  -- ** Head inspection
  headPoint,
  headSlot,
  headHash,
  headBlockNo,

  -- ** Basic operations
  head,
  last,
  lastPoint,
  lastSlot,
  toNewestFirst,
  toOldestFirst,
  fromNewestFirst,
  fromOldestFirst,
  dropNewest,
  dropOldest,
  takeNewest,
  takeOldest,
  takeWhileNewest,
  dropWhileNewest,
  length,
  null,

  -- ** Update type and operations
  ChainUpdate(..),
  addBlock,
  rollback,
  applyChainUpdate,
  applyChainUpdates,

  -- * Special operations
  slotOnChainFragment,
  pointOnChainFragment,
  successorBlock,
  lookupBySlot,
  splitAfterSlot,
  splitAfterPoint,
  splitBeforeSlot,
  splitBeforePoint,
  sliceRange,
  lookupByIndexFromEnd, FT.SearchResult(..),
  filter,
  selectPoints,
  findFirstPoint,
  intersectChainFragments,
  isPrefixOf,
  joinChainFragments,

  -- * Conversion to/from Chain
  toChain,
  fromChain,
  unvalidatedFromChain,

  -- * Helper functions
  prettyPrintChainFragment,

  -- * Reference implementations for testing
  foldChainFragmentSpec,
  slotOnChainFragmentSpec,
  pointOnChainFragmentSpec,
  selectPointsSpec,
  ) where

import           Prelude hiding (drop, head, last, length, null, filter)

import           Control.Exception (assert)
import           Data.FingerTree (FingerTree)
import qualified Data.FingerTree as FT
import qualified Data.Foldable as Foldable
import qualified Data.List as L
import           Data.Maybe (isJust)
import           Codec.Serialise (Serialise (..))
import           Codec.CBOR.Encoding (encodeListLen)
import           Codec.CBOR.Decoding (decodeListLen)

import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain (Chain)
import qualified Ouroboros.Network.Chain as Chain

--
-- Blockchain fragment data type.
--

-- | A fragment of the chain.
--
-- The chain grows to the right. The oldest block is the left-most block and
-- the newest block is the right-most block.
--
-- Invariant: a chain fragment should never contain a block with slot @0@.
-- That is the slot number reserved for genesis.
--
-- It will not be possible to find it with 'lookupBySlot',
-- since @minBound \@Word == 0@.
--
-- A fragment is represented by a finger tree for efficient searching based on
-- the 'SlotNo' (or 'Point') of a block.
newtype ChainFragment block = ChainFragment (FingerTree BlockMeasure block)
  deriving (Show, Eq)

viewRight :: HasHeader block
         => ChainFragment block -> FT.ViewR ChainFragment block
viewRight (ChainFragment c) = case FT.viewr c of
  FT.EmptyR  -> FT.EmptyR
  c' FT.:> b -> ChainFragment c' FT.:> b

viewLeft :: HasHeader block
         => ChainFragment block -> FT.ViewL ChainFragment block
viewLeft (ChainFragment c) = case FT.viewl c of
  FT.EmptyL  -> FT.EmptyL
  b FT.:< c' -> b FT.:< ChainFragment c'

pattern Empty :: HasHeader block => ChainFragment block
pattern Empty <- (viewRight -> FT.EmptyR) where
  Empty = ChainFragment FT.empty

-- | \( O(1) \). Add a block to the right of the chain fragment.
pattern (:>) :: HasHeader block
             => ChainFragment block -> block -> ChainFragment block
pattern c :> b <- (viewRight -> (c FT.:> b)) where
  ChainFragment c :> b = assert (validExtension (ChainFragment c) b) $
                         ChainFragment (c FT.|> b)

-- | \( O(1) \). Add a block to the left of the chain fragment.
pattern (:<) :: HasHeader block
             => block -> ChainFragment block -> ChainFragment block
pattern b :< c <- (viewLeft -> (b FT.:< c)) where
  b :< ChainFragment c = assert (maybe True (`isValidSuccessorOf` b)
                                       (last (ChainFragment c))) $
                         ChainFragment (b FT.<| c)

infixl 5 :>, :<

{-# COMPLETE Empty, (:>) #-}
{-# COMPLETE Empty, (:<) #-}

-- | \( O(n) \). Fold a 'ChainFragment'.
--
-- Implemented as a strict left fold.
foldChainFragment :: HasHeader block
                  => (a -> block -> a) -> a -> ChainFragment block -> a
foldChainFragment blk gen (ChainFragment c) = Foldable.foldl' blk gen c

-- | \( O(n) \). Specification of 'foldChainFragment'.
--
-- Use 'foldChainFragment', as it should be faster.
--
-- This function is used to verify whether 'foldChainFragment' behaves as
-- expected.
foldChainFragmentSpec :: HasHeader block
                      => (a -> block -> a) -> a -> ChainFragment block -> a
foldChainFragmentSpec _blk gen Empty    = gen
foldChainFragmentSpec  blk gen (c :> b) = blk (foldChainFragmentSpec blk gen c) b

prettyPrintChainFragment :: HasHeader block
                         => String -> (block -> String)
                         -> ChainFragment block
                         -> String
prettyPrintChainFragment nl ppBlock =
    foldChainFragment (\s b -> s ++ nl ++ "    " ++ ppBlock b) "ChainFragment:"

-- | \( O(n) \). Maps over the chain blocks. This is not allowed to change the
-- block `Point`s, or it would create an invalid chain.
--
mapChainFragment :: (HasHeader block1, HasHeader block2)
                 => (block1 -> block2) -> ChainFragment block1 -> ChainFragment block2
mapChainFragment f (ChainFragment c) = ChainFragment (FT.fmap' f c)

-- | \( O(n) \).
valid :: HasHeader block => ChainFragment block -> Bool
valid Empty = True
valid (c :> b) = valid c && validExtension c b

-- | Checks whether the first block @bSucc@ is a valid successor of the second
-- block @b@.
--
-- * The 'blockPrevHash' of the @bSucc@ must match that of @b@.
-- * The 'blockSlot' of @bSucc@ must be strictly larger than that of @b@.
-- * The 'blockNo' of @bSucc@ must be 1 greater than that of @b@.
--
-- This function does not check whether any of the two blocks satisfy
-- 'blockInvariant'.
isValidSuccessorOf :: HasHeader block
                   => block  -- ^ @bSucc@
                   -> block  -- ^ @b@
                   -> Bool
isValidSuccessorOf bSucc b =
    pointHash p == blockPrevHash bSucc
 && pointSlot p <  blockSlot bSucc
 && blockNo   b == pred (blockNo bSucc)
  where
    p = blockPoint b

-- | \( O(1) \).
validExtension ::  HasHeader block => ChainFragment block -> block -> Bool
validExtension c bSucc =
    blockInvariant bSucc
 && blockSlot bSucc /= SlotNo 0
 && case head c of
      Nothing -> True
      Just b  -> bSucc `isValidSuccessorOf` b

-- | \( O(1) \).
head :: HasHeader block => ChainFragment block -> Maybe block
head (_ :> b) = Just b
head Empty    = Nothing

-- | \( O(1) \).
headPoint :: HasHeader block => ChainFragment block -> Maybe (Point block)
headPoint = fmap blockPoint . head

-- | \( O(1) \).
headSlot :: HasHeader block => ChainFragment block -> Maybe SlotNo
headSlot = fmap blockSlot . head

-- | \( O(1) \).
headHash :: HasHeader block => ChainFragment block -> Maybe (ChainHash block)
headHash = fmap (BlockHash . blockHash) . head

-- | \( O(1) \).
headBlockNo :: HasHeader block => ChainFragment block -> Maybe BlockNo
headBlockNo = fmap blockNo . head

-- | \( O(1) \).
last :: HasHeader block => ChainFragment block -> Maybe block
last (b :< _) = Just b
last Empty    = Nothing

-- | \( O(1) \).
lastPoint :: HasHeader block => ChainFragment block -> Maybe (Point block)
lastPoint = fmap blockPoint . last

-- | \( O(1) \).
lastSlot :: HasHeader block => ChainFragment block -> Maybe SlotNo
lastSlot = fmap blockSlot . last

-- | TODO. Make a list of blocks from a 'ChainFragment', in newest-to-oldest
-- order.
toNewestFirst :: HasHeader block => ChainFragment block -> [block]
toNewestFirst = foldChainFragment (flip (:)) []

-- | \( O(n) \). Make a list of blocks from a 'ChainFragment', in
-- oldest-to-newest order.
toOldestFirst :: HasHeader block => ChainFragment block -> [block]
toOldestFirst (ChainFragment ft) = Foldable.toList ft

-- | \( O(n) \). Make a 'ChainFragment' from a list of blocks in
-- newest-to-oldest order.
fromNewestFirst :: HasHeader block => [block] -> ChainFragment block
fromNewestFirst = foldr (flip (:>)) Empty

-- | \( O(n) \). Make a 'ChainFragment' from a list of blocks in
-- oldest-to-newest order.
fromOldestFirst :: HasHeader block => [block] -> ChainFragment block
fromOldestFirst bs = assert (valid c) c
  where
    c = ChainFragment $ FT.fromList bs

-- | \( O(\log(\min(i,n-i)) \). Drop the newest @n@ blocks from the
-- 'ChainFragment'.
dropNewest :: HasHeader block
           => Int  -- ^ @n@
           -> ChainFragment block -> ChainFragment block
dropNewest n cf@(ChainFragment c) =
    ChainFragment $ FT.takeUntil (\v -> bmSize v > remainingLength) c
  where
    remainingLength = length cf - n

-- | \( O(\log(\min(i,n-i)) \). Drop the oldest @n@ blocks from the
-- 'ChainFragment'.
dropOldest :: HasHeader block
           => Int  -- ^ @n@
           -> ChainFragment block -> ChainFragment block
dropOldest n (ChainFragment c) =
    ChainFragment $ FT.dropUntil (\v -> bmSize v > n) c

-- | \( O(\log(\min(i,n-i)) \). Take the newest @n@ blocks from the
-- 'ChainFragment'.
takeNewest :: HasHeader block
           => Int  -- ^ @n@
           -> ChainFragment block -> ChainFragment block
takeNewest n cf@(ChainFragment c) =
    ChainFragment $ FT.dropUntil (\v -> bmSize v > remainingLength) c
  where
    remainingLength = length cf - n

-- | \( O(\log(\min(i,n-i)) \). Take the oldest @n@ blocks from the
-- 'ChainFragment'.
takeOldest :: HasHeader block
           => Int  -- ^ @n@
           -> ChainFragment block -> ChainFragment block
takeOldest n (ChainFragment c) =
    ChainFragment $ FT.takeUntil (\v -> bmSize v > n) c

-- | \( O(n) \). Select the newest blocks that satisfy the predicate.
--
takeWhileNewest :: HasHeader block
                => (block -> Bool)
                -> ChainFragment block
                -> ChainFragment block
takeWhileNewest _ Empty    = Empty
takeWhileNewest p (c :> b)
               | p b       = takeWhileNewest p c :> b
               | otherwise = Empty

-- | \( O(n) \). Drop the newest blocks that satisfy the predicate, keeping
-- the remainder.
--
dropWhileNewest :: HasHeader block
                => (block -> Bool)
                -> ChainFragment block
                -> ChainFragment block
dropWhileNewest _ Empty       = Empty
dropWhileNewest p c@(c' :> b)
                  | p b       = dropWhileNewest p c'
                  | otherwise = c

-- | \( O(1) \).
length :: HasHeader block => ChainFragment block -> Int
length (ChainFragment c) = bmSize $ FT.measure c

-- | \( O(1) \).
null :: ChainFragment block -> Bool
null (ChainFragment c) = FT.null c

-- | \( O(1) \). Add a block to the right of the chain fragment.
--
-- Synonym for ':>'.
addBlock :: HasHeader block => block -> ChainFragment block -> ChainFragment block
addBlock b c = c :> b

-- | \( O(\log(\min(i,n-i)) \). If the 'Point' is in the 'ChainFragment', roll
-- back to a 'ChainFragment' such that its last 'Point' is the given 'Point'.
--
-- In other words, remove blocks from the end of the 'ChainFragment' until the
-- given 'Point' is the last block. If the given 'Point' is not part of the
-- 'ChainFragment', return 'Nothing'.
rollback :: HasHeader block
         => Point block -> ChainFragment block -> Maybe (ChainFragment block)
rollback p c = fst <$> splitAfterPoint c p

-- | \( O(\log(\min(i,n-i)) \). Internal variant of 'lookupBySlot' that
-- returns a 'FT.SearchResult'.
lookupBySlotFT :: HasHeader block
               => ChainFragment block
               -> SlotNo
               -> FT.SearchResult BlockMeasure block
lookupBySlotFT (ChainFragment t) s =
    FT.search (\vl vr -> bmMaxSlot vl >= s && bmMinSlot vr >= s) t

-- | \( O(\log(\min(i,n-i)) \). Find the oldest block in the chain fragment
-- with a slot equal to the given slot.
lookupBySlot :: HasHeader block
             => ChainFragment block
             -> SlotNo
             -> Maybe block
lookupBySlot c s = case lookupBySlotFT c s of
  FT.Position _ b _ | blockSlot b == s -> Just b
  _                                    -> Nothing

-- | \( O(\log(\min(i,n-i)) \). Look up a block in the 'ChainFragment' based
-- on the given index, i.e. the offset starting from the newest/rightmost
-- block.
--
-- Note that 'FT.search' used to contain a bug, but this has been fixed in
-- version 0.1.4.2 of the @fingertree@ library. See
-- <https://hub.darcs.net/ross/fingertree/issue/8>.
lookupByIndexFromEnd :: HasHeader block
                     => ChainFragment block
                     -> Int
                     -> FT.SearchResult BlockMeasure block
lookupByIndexFromEnd (ChainFragment t) n =
    FT.search (\vl vr -> bmSize vl >= len - n && bmSize vr <= n) t
  where
    len = bmSize (FT.measure t)

-- | \( O\(n\) \). Filter the chain based on a predicate. As filtering
-- removes blocks the result is a sequence of disconnected fragments.
-- The fragments are in the original order and are of maximum size.
--
filter :: HasHeader block
       => (block -> Bool)
       -> ChainFragment block
       -> [ChainFragment block]
filter p = go [] Empty
  where
    go cs c'    (b :< c) | p b = go     cs (c' :> b) c
    go cs Empty (_ :< c)       = go     cs  Empty    c
    go cs c'    (_ :< c)       = go (c':cs) Empty    c

    go cs Empty  Empty         = reverse     cs
    go cs c'     Empty         = reverse (c':cs)

-- | \( O(o \log(\min(i,n-i))) \). Select a bunch of 'Point's based on offsets
-- from the head of the chain fragment. This is used in the chain consumer
-- protocol as part of finding the intersection between a local and remote
-- chain.
--
-- The list of offsets must be increasing monotonically.
--
-- The typical pattern is to use a selection of offsets covering the last K
-- blocks, biased towards more recent blocks. For example:
--
-- > selectPoints (0 : [ fib n | n <- [1 .. 17] ])
--
--
-- Only for offsets within the bounds of the chain fragment, will there be
-- points in the returned list.
selectPoints :: HasHeader block
             => [Int] -> ChainFragment block -> [Point block]
selectPoints offsets = go relativeOffsets
  where
    relativeOffsets = zipWith (-) offsets (0:offsets)
    go []         _     = []
    go _          Empty = []
    go (off:offs) c     = case lookupByIndexFromEnd c off of
      FT.Position t b _ -> blockPoint b : go offs (ChainFragment (t FT.|> b))
      _                 -> []

-- | \( O(o * n) \). Specification of 'selectPoints'.
--
-- Use 'selectPoints', as it should be faster.
--
-- This function is used to verify whether 'selectPoints' behaves as expected.
selectPointsSpec :: HasHeader block
                => [Int] -> ChainFragment block -> [Point block]
selectPointsSpec offsets c =
    [ blockPoint (bs !! offset)
    | let bs = toNewestFirst c
          len = L.length bs
    , offset <- offsets
    , offset < len ]

-- | \( O(\log(\min(i,n-i)) \). Find the block after the given point.
successorBlock :: HasHeader block
               => Point block -> ChainFragment block -> Maybe block
successorBlock p c = case lookupBySlotFT c (pointSlot p) of
  FT.Position _ b ft'
    | blockPoint b == p
    , n FT.:< _ <- FT.viewl ft' -- O(1)
    -> Just n
  _ -> Nothing

-- | \( O(\log(\min(i,n-i)) \). Split the 'ChainFragment' after the block with
-- given slot. Or, if there is no block with the given slot in the chain
-- fragment, split at the location where it would have been.
--
-- If the chain fragment contained such a block, it will be the head
-- (newest/rightmost) block on the first returned chain.
splitAfterSlot :: HasHeader block
               => ChainFragment block
               -> SlotNo
               -> (ChainFragment block, ChainFragment block)
splitAfterSlot (ChainFragment t) s = (ChainFragment l, ChainFragment r)
  where
   (l, r) = FT.split (\v -> bmMaxSlot v > s) t

-- | \( O(\log(\min(i,n-i)) \). Split the 'ChainFragment' after the block at
--  the given 'Point'. Return Nothing if the 'ChainFragment' does not contain
--  a block at the given 'Point'.
--
-- If the chain fragment contained a block at the given 'Point', it will be
-- the (newest\/rightmost) block of the first returned chain.
splitAfterPoint :: (HasHeader block1, HasHeader block2,
                    HeaderHash block1 ~ HeaderHash block2)
                => ChainFragment block1
                -> Point block2
                -> Maybe (ChainFragment block1, ChainFragment block1)
splitAfterPoint c p
  | (l@(ChainFragment lt), r) <- splitAfterSlot c (pointSlot p)
  , _ FT.:> b <- FT.viewr lt  -- O(1)
  , blockPoint b == castPoint p
  = Just (l, r)
  | otherwise
  = Nothing

-- | \( O(\log(\min(i,n-i)) \). Split the 'ChainFragment' before the block with
-- the given slot. Or, if there is no block with the given slot in the chain
-- fragment, split at the location where it would have been.
--
-- If the chain fragment contained such a block, it will be the last
-- (oldest\/leftmost) block on the second returned chain.
splitBeforeSlot :: HasHeader block
               => ChainFragment block
               -> SlotNo
               -> (ChainFragment block, ChainFragment block)
splitBeforeSlot (ChainFragment t) s = (ChainFragment l, ChainFragment r)
  where
   (l, r) = FT.split (\v -> bmMaxSlot v >= s) t

splitBeforePoint :: (HasHeader block1, HasHeader block2,
                    HeaderHash block1 ~ HeaderHash block2)
                 => ChainFragment block1
                 -> Point block2
                 -> Maybe (ChainFragment block1, ChainFragment block1)
splitBeforePoint c p
  | (l, r@(ChainFragment rt)) <- splitBeforeSlot c (pointSlot p)
  , b FT.:< _ <- FT.viewl rt  -- O(1)
  , blockPoint b == castPoint p
  = Just (l, r)
  | otherwise
  = Nothing


-- | Select a slice of a chain fragment between two points, inclusive.
--
-- Both points must exist on the chain, in order, or the result is @Nothing@.
--
sliceRange :: HasHeader block
           => ChainFragment block
           -> Point block
           -> Point block
           -> Maybe (ChainFragment block)
sliceRange c from to
  | Just (_, c') <- splitBeforePoint c  from
  , Just (c'',_) <- splitAfterPoint  c' to
  = Just c''

  | otherwise
  = Nothing


-- | \( O(p \log(\min(i,n-i)) \). Find the first 'Point' in the list of points
-- that is on the given 'ChainFragment'. Return 'Nothing' if none of them are
-- on the 'ChainFragment'. TODO test?
findFirstPoint
  :: HasHeader block
  => [Point block]
  -> ChainFragment block
  -> Maybe (Point block)
findFirstPoint ps c = L.find (`pointOnChainFragment` c) ps

-- | \( O(\log(\min(i,n-i)) \).
slotOnChainFragment :: HasHeader block => SlotNo -> ChainFragment block -> Bool
slotOnChainFragment slot c = isJust (lookupBySlot c slot)

-- | \( O(n) \). Specification of 'slotOnChainFragment'.
--
-- Use 'slotOnChainFragment', as it should be faster.
--
-- This function is used to verify whether 'slotOnChainFragment' behaves as
-- expected.
slotOnChainFragmentSpec :: HasHeader block => SlotNo -> ChainFragment block -> Bool
slotOnChainFragmentSpec slot = go
  where
    -- Recursively search the fingertree from the right
    go Empty = False
    go (c' :> b) | blockSlot b == slot = True
                 | otherwise           = go c'

-- | \( O(\log(\min(i,n-i)) \).
pointOnChainFragment :: HasHeader block => Point block -> ChainFragment block -> Bool
pointOnChainFragment p c = case lookupBySlot c (pointSlot p) of
  Just b | blockPoint b == p -> True
  _                          -> False

-- | \( O(n) \). Specification of 'pointOnChainFragment'.
--
-- Use 'pointOnChainFragment', as it should be faster.
--
-- This function is used to verify whether 'pointOnChainFragment' behaves as
-- expected.
pointOnChainFragmentSpec :: HasHeader block
                         => Point block -> ChainFragment block -> Bool
pointOnChainFragmentSpec p = go
    where
    -- Recursively search the fingertree from the right
    go Empty = False
    go (c' :> b) | blockPoint b == p = True
                 | otherwise         = go c'

-- | \( O(n_2 \log(n_1)) \). Look for the intersection of the two
-- 'ChainFragment's @c1@ and @c2@.
--
-- If they intersect, i.e., share a common 'Point', then return a tuple of:
--
-- * @l1@: the prefix of the first  chain fragment
-- * @l2@: The prefix of the second chain fragment
-- * @r1@: The suffix of the first  chain fragment
-- * @r2@: The suffix of the second chain fragment
--
-- @l1@ and @l2@ will have same last block, i.e. /head/. The original chain
-- @c1@ can be obtained by putting @r1@ after @l1@, similarly for @c2@: by
-- putting @r2@ after @l2@:
--
-- @
-- Just c1 = 'joinChainFragments' l1 r1
-- Just c1 = 'joinChainFragments' l1 r1
-- @
--
-- Chains that intersect will always have the exact same common prefix, but
-- chain fragments might have prefixes that differ in length. The blocks they
-- have in common will be the same, but not each fragment might contain all
-- the blocks the other fragment contains.
--
-- Take for example the following two chain fragments that share blocks 4 and
-- 5. The two chain fragments are fragments of the same chain, but don't
-- contain all blocks of the original chain. The missing history of the
-- fragments is indicated with an asterisk (*). The @-A@ and @-B@ suffixes
-- denote that blocks are part of a fork of the chain.
--
-- >
-- >
-- >     ┆ * ┆
-- >     ├───┤
-- >     │ 2 │     ┆ * ┆
-- >     ├───┤     ├───┤
-- >     │ 4 │     │ 4 │
-- >     ├───┤     ├───┤
-- >     │ 5 │     │ 5 │
-- > ────┼───┼─────┼───┼───
-- >     │ 6A│     │ 6B│
-- >     └───┘     ├───┤
-- >               │ 8B│
-- >               └───┘
-- >       c1        c2
--
-- The intersection of @c1@ and @c2@ is block 5 (the last 'Point' the two
-- fragments have in common) and we return the following fragments:
--
-- >
-- >
-- >     ┆ * ┆
-- >     ├───┤
-- >     │ 2 │     ┆ * ┆
-- >     ├───┤     ├───┤
-- >     │ 4 │     │ 4 │
-- >     ├───┤     ├───┤
-- >     │ 5 │     │ 5 │
-- > ────┴───┴─────┴───┴──────┬───┬─────┬───┬───
-- >                          │ 6A│     │ 6B│
-- >                          └───┘     ├───┤
-- >                                    │ 8B│
-- >                                    └───┘
-- > Just (l1,       l2,        r1,       r2)
intersectChainFragments
  :: (HasHeader block1, HasHeader block2, HeaderHash block1 ~ HeaderHash block2)
  => ChainFragment block1
  -> ChainFragment block2
  -> Maybe (ChainFragment block1, ChainFragment block2,
            ChainFragment block1, ChainFragment block2)
intersectChainFragments initC1 initC2 =
    go initC1 initC2
  where
    go _   Empty    = Nothing
    go c1 (c2 :> b)
      | let p = blockPoint b
      , Just (l1, r1) <- splitAfterPoint c1     p
      , Just (l2, r2) <- splitAfterPoint initC2 p
                    -- splitAfterPoint initC2 p cannot fail,
                    -- since p comes out of initC2
                    = Just (l1, l2, r1, r2)
      | otherwise   = go c1 c2

-- This is the key operation on chains in this model
applyChainUpdate :: HasHeader block
                 => ChainUpdate block
                 -> ChainFragment block
                 -> Maybe (ChainFragment block)
applyChainUpdate (AddBlock b) c = Just (addBlock b c)
applyChainUpdate (RollBack p) c =       rollback p c

applyChainUpdates :: HasHeader block
                  => [ChainUpdate block]
                  -> ChainFragment block
                  -> Maybe (ChainFragment block)
applyChainUpdates []     c = Just c
applyChainUpdates (u:us) c = applyChainUpdates us =<< applyChainUpdate u c


-- | \( O(\max(n_1, n_2)) \). Check whether the first chain fragment is a
-- prefix of the second.
isPrefixOf :: (HasHeader block, Eq block)
           => ChainFragment block -> ChainFragment block -> Bool
a `isPrefixOf` b = toOldestFirst a `L.isPrefixOf` toOldestFirst b


-- | \( O(\log(\min(n_1, n_2))) \). Join two 'ChainFragment's if the first
-- (oldest) block of the second fragment is the successor of the last (newest)
-- block of the first fragment.
joinChainFragments :: HasHeader block
                   => ChainFragment block
                   -> ChainFragment block
                   -> Maybe (ChainFragment block)
joinChainFragments c1@(ChainFragment t1) c2@(ChainFragment t2) =
    case (FT.viewr t1, FT.viewl t2) of
      (FT.EmptyR, _)           -> Just c2
      (_,         FT.EmptyL)   -> Just c1
      (_ FT.:> b1, b2 FT.:< _) | b2 `isValidSuccessorOf` b1
                               -> Just (ChainFragment (t1 FT.>< t2))
      _                        -> Nothing

-- | Convert a 'ChainFragment' to a 'Chain'.
toChain :: HasHeader block => ChainFragment block -> Chain block
toChain = Chain.fromNewestFirst . toNewestFirst

-- | Convert a 'Chain' to a 'ChainFragment'.
fromChain :: HasHeader block => Chain block -> ChainFragment block
fromChain = fromNewestFirst . Chain.toNewestFirst

-- | Variant of 'fromChain' that assumes a valid chain and will not validate
-- the construct 'ChainFragment'.
unvalidatedFromChain :: HasHeader block => Chain block -> ChainFragment block
unvalidatedFromChain = unvalidatedFromNewestFirst . Chain.toNewestFirst
  where
    unvalidatedFromNewestFirst = ChainFragment . foldr (flip (FT.|>)) FT.empty

--
-- Serialisation
--

instance (HasHeader block, Serialise block)
  => Serialise (ChainFragment block) where

  encode c = encodeListLen (fromIntegral $ length c)
          <> foldChainFragment (\e b -> e <> encode b) mempty c

  decode = do
      n <- decodeListLen
      go Empty n
    where
      go c 0 = return c
      go c n = do b <- decode
                  go (c :> b) (n-1)

