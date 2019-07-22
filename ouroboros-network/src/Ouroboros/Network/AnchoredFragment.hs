{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}
module Ouroboros.Network.AnchoredFragment (
  -- * AnchoredFragment type and fundamental operations
  AnchoredFragment(Empty, (:>), (:<)),
  anchorPoint,
  unanchorFragment,
  mkAnchoredFragment,
  valid,
  validExtension,

  -- ** Block re-exports
  HasHeader(..),
  Point(..),
  castPoint,
  blockPoint,

  -- * AnchoredFragment construction and inspection
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
  takeOldest,
  dropWhileNewest,
  takeWhileOldest,
  length,
  null,

  -- ** Update type and operations
  ChainUpdate(..),
  addBlock,
  rollback,
  applyChainUpdate,
  applyChainUpdates,

  -- * Special operations
  pointOnFragment,
  withinFragmentBounds,
  findFirstPoint,
  successorBlock,
  selectPoints,
  isPrefixOf,
  splitAfterPoint,
  splitBeforePoint,
  join,
  intersect,
  intersectionPoint,
  mapAnchoredFragment,

  -- * Conversion to/from Chain
  fromChain,
  toChain,
  anchorNewest,

  -- * Helper functions
  prettyPrint
  ) where

import           Prelude hiding (head, last, length, null)

import           Control.Exception (assert)
import           Data.Functor ((<&>))
import           Data.List (find)
import           Data.Word (Word64)
import           GHC.Stack

import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain (Chain)
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.ChainFragment (ChainFragment)
import qualified Ouroboros.Network.ChainFragment as CF
import           Ouroboros.Network.Point (WithOrigin (At))

-- | An 'AnchoredFragment' is a 'ChainFragment' that is anchored to some
-- 'Point': the point right before the first, leftmost block in the fragment.
-- The anchor point can be thought of as a left exclusive bound.
--
-- For example, the following fragment is anchored at @a@ and contains @b1@,
-- @b2@, and @b3@, which is the head of the fragment.
--
-- > a ] b1 >: b2 >: b3
--
-- The fact that it is an /exclusive/ bound is particularly convenient when
-- dealing with Genesis. Genesis is the start of the chain, but not an actual
-- block, so we cannot use it an inclusive bound. However, there /is/ a
-- 'Point' that refers to Genesis ('Chain.genesisPoint'), which can be used as
-- the anchor point, acting as an exclusive bound.
--
-- An 'AnchoredFragment' anchored at Genesis, can thus be converted to a
-- 'Ouroboros.Network.Chain.Chain' ('toChain'), containing all blocks starting
-- from Genesis.
--
-- Without an anchor point, an empty 'ChainFragment' doesn't give us much more
-- information: is it empty because the whole chain is empty? Or, did we just
-- get an empty fragment that was split off from some later part of the chain?
--
-- Furthermore, an important criterion during chain selection is the length of
-- the chain. However, comparing the length of two 'ChainFragment's doesn't
-- make much sense, since they might start at different points. Hence the idea
-- to \"anchor\" fragments at some point, and require that fragments have the
-- same anchor when comparing their lengths.
--
-- Note: instead of comparing the lengths of two 'ChainFragment's, we could
-- try to compare the 'blockNo' of their heads, which is also a measure of the
-- total length of the chain. However, EBBs throw a(nother) spanner in the
-- works: each EBB shares its 'blockNo' with a regular block, so comparing a
-- 'ChainFragment' that only contains the EBB with a 'ChainFragment' that only
-- contains the regular block with the same 'blockNo' as the EBB, will not
-- give a conclusive preference to either fragment, while in reality one
-- fragment actually corresponds to a longer chain.
data AnchoredFragment block = AnchoredFragment
    { anchorPoint      :: !(Point block)
    , unanchorFragment :: !(ChainFragment block)
    } deriving (Show, Eq)

mkAnchoredFragment :: HasHeader block
                   => Point block -> ChainFragment block
                   -> AnchoredFragment block
mkAnchoredFragment a c = case CF.last c of
    Nothing -> AnchoredFragment a CF.Empty
    Just b  -> assert (validExtension (Empty a) b) $
               AnchoredFragment a c

-- | \( O(1) \). Pattern for matching on or creating an empty
-- 'AnchoredFragment'. An empty fragment has/needs an anchor point.
pattern Empty :: HasHeader block => Point block -> AnchoredFragment block
pattern Empty a <- (viewRight -> EmptyR a)
  where
    Empty a = AnchoredFragment a CF.Empty

-- | Auxiliary data type to define the pattern synonym
data ViewRight block
    = EmptyR (Point block)
    | ConsR  (AnchoredFragment block) block

viewRight :: HasHeader block => AnchoredFragment block -> ViewRight block
viewRight (AnchoredFragment a c) = case c of
    CF.Empty   -> EmptyR a
    c' CF.:> b -> ConsR (AnchoredFragment a c') b

-- | \( O(1) \). Add a block to the right of the anchored fragment.
pattern (:>) :: (HasHeader block, HasCallStack)
             => AnchoredFragment block -> block -> AnchoredFragment block
pattern af' :> b <- (viewRight -> ConsR af' b)
  where
    af@(AnchoredFragment a c) :> b = case c of
      -- When the chain fragment is empty, validate to check whether the block
      -- fits onto the anchor point.
      CF.Empty -> assert (validExtension af b) $
                  AnchoredFragment a (c CF.:> b)
      -- Don't validate when we're just appending a block to the chain
      -- fragment, as 'CF.:>' will already validate for us.
      _        -> AnchoredFragment a (c CF.:> b)

-- | Auxiliary data type to define the pattern synonym
data ViewLeft block
    = EmptyL (Point block)
    | ConsL  block (AnchoredFragment block)

viewLeft :: HasHeader block => AnchoredFragment block -> ViewLeft block
viewLeft (AnchoredFragment a c) = case c of
    CF.Empty   -> EmptyL a
    b CF.:< c' -> ConsL b (AnchoredFragment (blockPoint b) c')

-- | \( O(1) \). View the first, leftmost block of the anchored fragment.
--
-- This is only a view, not a constructor, as adding a block to the left would
-- change the anchor of the fragment.
pattern (:<) :: HasHeader block
             => block -> AnchoredFragment block -> AnchoredFragment block
pattern b :< af' <- (viewLeft -> ConsL b af')

infixl 5 :>, :<

{-# COMPLETE Empty, (:>) #-}
{-# COMPLETE Empty, (:<) #-}

prettyPrint :: HasHeader block
            => String
            -> (Point block -> String)
            -> (block -> String)
            -> AnchoredFragment block
            -> String
prettyPrint nl ppAnchor ppBlock (AnchoredFragment a c) =
    CF.foldChainFragment (\s b -> s ++ nl ++ "    " ++ ppBlock b)
    ("AnchoredFragment (" <> ppAnchor a <> "):") c


-- | \( O(n) \).
valid :: HasHeader block => AnchoredFragment block -> Bool
valid (Empty _) = True
valid (af :> b) = valid af && validExtension af b

-- | \( O(1) \).
validExtension :: HasHeader block => AnchoredFragment block -> block -> Bool
validExtension af bSucc =
    blockInvariant bSucc &&
    case head af of
      Left  p -> pointHash p == blockPrevHash bSucc &&
                 -- Note that this inequality would be strict, but for epoch
                 -- boundary blocks, which occupy the same slot as a regular
                 -- block.
                 pointSlot p <= At (blockSlot bSucc)
      Right b -> bSucc `CF.isValidSuccessorOf` b

-- | \( O(1) \). When the fragment is empty, return the anchor point,
-- otherwise the most recently added block.
head :: HasHeader block => AnchoredFragment block -> Either (Point block) block
head (_ :> b)  = Right b
head (Empty a) = Left a

-- | \( O(1) \). When the fragment is empty, the anchor point is returned.
headPoint :: HasHeader block => AnchoredFragment block -> Point block
headPoint = either id blockPoint . head

-- | \( O(1) \). When the fragment is empty, the slot of the anchor point is
-- returned, which may be origin (no slot).
headSlot :: HasHeader block => AnchoredFragment block -> WithOrigin SlotNo
headSlot = either pointSlot (At . blockSlot) . head

-- | \( O(1) \). When the fragment is empty, the hash of the anchor point is
-- returned.
headHash :: HasHeader block => AnchoredFragment block -> ChainHash block
headHash = either pointHash (BlockHash . blockHash) . head

-- | \( O(1) \). When the fragment is empty, 'Nothing' is returned, as the
-- anchor point has no 'BlockNo'.
headBlockNo :: HasHeader block => AnchoredFragment block -> Maybe BlockNo
headBlockNo = either (const Nothing) (Just . blockNo) . head

-- | \( O(1) \). When the fragment is empty, return the anchor point,
-- otherwise the leftmost block.
last :: HasHeader block => AnchoredFragment block -> Either (Point block) block
last (b :< _)  = Right b
last (Empty a) = Left a

-- | \( O(1) \). When the fragment is empty, the anchor point is returned.
lastPoint :: HasHeader block => AnchoredFragment block -> Point block
lastPoint = either id blockPoint . last

-- | \( O(1) \). When the fragment is empty, the slot of the anchor point is
-- returned, which may be the origin and therefore have no slot.
lastSlot :: HasHeader block => AnchoredFragment block -> WithOrigin SlotNo
lastSlot = either pointSlot (At . blockSlot) . last

-- | TODO. Make a list of blocks from a 'AnchoredFragment', in newest-to-oldest
-- order.
toNewestFirst :: HasHeader block => AnchoredFragment block -> [block]
toNewestFirst = CF.toNewestFirst . unanchorFragment

-- | \( O(n) \). Make a list of blocks from a 'AnchoredFragment', in
-- oldest-to-newest order.
toOldestFirst :: HasHeader block => AnchoredFragment block -> [block]
toOldestFirst = CF.toOldestFirst . unanchorFragment

-- | \( O(n) \). Make a 'AnchoredFragment' from a list of blocks in
-- newest-to-oldest order. The last block in the list must be the block
-- following the given anchor point.
fromNewestFirst :: HasHeader block
                => Point block  -- ^ Anchor
                -> [block] -> AnchoredFragment block
fromNewestFirst a = foldr (flip (:>)) (Empty a)

-- | \( O(n) \). Make a 'AnchoredFragment' from a list of blocks in
-- oldest-to-newest order. The first block in the list must be the block
-- following the given anchor point.
fromOldestFirst :: HasHeader block
                => Point block  -- ^ Anchor
                -> [block] -> AnchoredFragment block
fromOldestFirst a bs = mkAnchoredFragment a (CF.fromOldestFirst bs)

-- | \( O(\log(\min(i,n-i)) \). Drop the newest @n@ blocks from the
-- 'AnchoredFragment'. The anchor point is not changed.
dropNewest :: HasHeader block
           => Int  -- ^ @n@
           -> AnchoredFragment block -> AnchoredFragment block
dropNewest n (AnchoredFragment a c) = AnchoredFragment a $ CF.dropNewest n c

-- | \( O(\log(\min(i,n-i)) \). Take the oldest @n@ blocks from the
-- 'AnchoredFragment'. The anchor point is not changed.
takeOldest :: HasHeader block
           => Int  -- ^ @n@
           -> AnchoredFragment block -> AnchoredFragment block
takeOldest n (AnchoredFragment a c) = AnchoredFragment a $ CF.takeOldest n c

-- | \( O(n) \). Drop the newest blocks that satisfy the predicate, keeping
-- the remainder. The anchor point is not changed.
dropWhileNewest :: HasHeader block
                => (block -> Bool)
                -> AnchoredFragment block
                -> AnchoredFragment block
dropWhileNewest p (AnchoredFragment a c) =
    AnchoredFragment a (CF.dropWhileNewest p c)

-- | \( O(n) \). Take the oldest blocks that satisfy the predicate. The anchor
-- point is not changed.
takeWhileOldest :: HasHeader block
                => (block -> Bool)
                -> AnchoredFragment block
                -> AnchoredFragment block
takeWhileOldest p (AnchoredFragment a c) =
    AnchoredFragment a (CF.takeWhileOldest p c)

-- | \( O(1) \). Return the number of blocks. The anchor point is not counted.
length :: HasHeader block => AnchoredFragment block -> Int
length = CF.length . unanchorFragment

-- | \( O(1) \). The anchor point is not counted.
null :: AnchoredFragment block -> Bool
null = CF.null . unanchorFragment

-- | \( O(1) \). Add a block to the right of the anchored fragment.
--
-- Synonym for ':>'.
addBlock :: HasHeader block
         => block -> AnchoredFragment block -> AnchoredFragment block
addBlock b c = c :> b

-- | \( O(\log(\min(i,n-i)) \). If the 'Point' is within the bounds of the
-- 'AnchoredFragment' (see 'withinFragmentBounds'), roll back the anchored
-- fragment such that its head is the given point. In case the given point was
-- the anchor point, the returned anchored fragment will be empty.
--
-- In other words, remove blocks from the end of the 'AnchoredFragment' until
-- the given 'Point' is the head. If the given 'Point' is not within the
-- bounds of the 'AnchoredFragment', return 'Nothing'.
rollback :: HasHeader block
         => Point block -> AnchoredFragment block
         -> Maybe (AnchoredFragment block)
rollback p (AnchoredFragment a c)
    | p == a
    = Just (Empty a)
    | otherwise
    = (AnchoredFragment a) <$> CF.rollback p c

-- | \( O(o \log(\min(i,n-i))) \). See 'CF.selectPoints'.
--
-- The list of offsets must be increasing monotonically.
--
-- __Note__: offset @n@, where @n@ equals the length of the anchored fragment,
-- corresponds to the anchor point. When the fragment is empty, offset 0 will
-- thus correspond to the anchor point.
selectPoints :: HasHeader block
             => [Int] -> AnchoredFragment block -> [Point block]
selectPoints offsets (AnchoredFragment a c) =
    CF.selectPoints offsetsOnFrag c <> map (const a) anchorOffsets
  where
    len = CF.length c
    (offsetsOnFrag, offsetsAfterFrag) = span (< len) offsets
    anchorOffsets = takeWhile (== len) offsetsAfterFrag

-- | \( O(\log(\min(i,n-i)) \). Find the block after the given point. If the
-- given point is the anchor point, then the first block is returned (if there
-- is one).
successorBlock :: HasHeader block
               => Point block -> AnchoredFragment block -> Maybe block
successorBlock p af@(AnchoredFragment a c)
    | p == a
    = either (const Nothing) Just $ last af
    | otherwise
    = CF.successorBlock p c

-- | \( O(\log(\min(i,n-i)) \). Same as 'CF.pointOnChainFragment': does the
-- fragment contain a block with the given block? The anchor point is ignored.
pointOnFragment :: HasHeader block
                => Point block -> AnchoredFragment block -> Bool
pointOnFragment p (AnchoredFragment _ c) = CF.pointOnChainFragment p c

-- | \( O(\log(\min(i,n-i)) \). Is the point within the fragment bounds?
-- Either the point is the anchor point, or it corresponds to a block \"on\"
-- the fragment.
withinFragmentBounds :: HasHeader block
                     => Point block -> AnchoredFragment block -> Bool
withinFragmentBounds p (AnchoredFragment a c) =
    p == a || CF.pointOnChainFragment p c


-- | \( O(p \log(\min(i,n-i)) \). Find the first 'Point' in the list of points
-- that is within the fragment bounds. Return 'Nothing' if none of them are.
--
-- __Note__: in contrast to 'CF.findFirstPoint', this is based on
-- 'withinFragmentBounds' instead of 'CF.pointOnFragment'.
findFirstPoint
  :: HasHeader block
  => [Point block]
  -> AnchoredFragment block
  -> Maybe (Point block)
findFirstPoint ps c = find (`withinFragmentBounds` c) ps


applyChainUpdate :: HasHeader block
                 => ChainUpdate block block
                 -> AnchoredFragment block
                 -> Maybe (AnchoredFragment block)
applyChainUpdate (AddBlock b) c = Just (addBlock b c)
applyChainUpdate (RollBack p) c =       rollback p c

applyChainUpdates :: HasHeader block
                  => [ChainUpdate block block]
                  -> AnchoredFragment block
                  -> Maybe (AnchoredFragment block)
applyChainUpdates []     c = Just c
applyChainUpdates (u:us) c = applyChainUpdates us =<< applyChainUpdate u c

-- | Convert a 'Chain' to an 'AnchoredFragment'.
--
-- The anchor of the fragment will be 'Chain.genesisPoint'.
fromChain :: HasHeader block => Chain block -> AnchoredFragment block
fromChain = mkAnchoredFragment Chain.genesisPoint . CF.unvalidatedFromChain

-- | Convert an 'AnchoredFragment' to a 'Chain'.
--
-- The anchor of the fragment must be 'Chain.genesisPoint', otherwise
-- 'Nothing' is returned.
toChain :: HasHeader block => AnchoredFragment block -> Maybe (Chain block)
toChain af@(AnchoredFragment a _)
    | a == Chain.genesisPoint
    = Just $ Chain.fromNewestFirst $ toNewestFirst af
    | otherwise
    = Nothing

-- | Take the @n@ newest blocks from the fragment.
--
-- WARNING: this may change the anchor of the fragment!
--
-- When the fragment itself is shorter than @n@ blocks, the fragment will be
-- returned unmodified.
anchorNewest :: forall block. HasHeader block
             => Word64  -- ^ @n@
             -> AnchoredFragment block
             -> AnchoredFragment block
anchorNewest n c = case c of
    Empty _       -> c
    _ | n >= len  -> c
      | otherwise -> dropOldest (len - n) c
  where
    len = fromIntegral $ length c

    dropOldest :: Word64 -> AnchoredFragment block -> AnchoredFragment block
    dropOldest _ (Empty a) = Empty a
    dropOldest 0 c'        = c'
    dropOldest m (_ :< c') = dropOldest (m - 1) c'

-- | \( O(\max(n_1, n_2)) \). Check whether the first anchored fragment is a
-- prefix of the second.
--
-- The two 'AnchoredFragment's must have the same anchor point, otherwise the
-- first cannot be a prefix of the second.
isPrefixOf :: (HasHeader block, Eq block)
           => AnchoredFragment block -> AnchoredFragment block -> Bool
AnchoredFragment a1 c1 `isPrefixOf` AnchoredFragment a2 c2 =
    a1 == a2 && c1 `CF.isPrefixOf` c2

-- | \( O(\log(\min(i,n-i)) \). Split the 'AnchoredFragment' after the given
--  'Point'. Return 'Nothing' if given 'Point' is not within the fragment
--  bounds ('withinFragmentBounds').
--
-- The given 'Point' may be the anchor point of the fragment, in which case
-- the empty fragment with the given anchor point and the original fragment
-- are returned.
splitAfterPoint
   :: forall block1 block2.
      (HasHeader block1, HasHeader block2, HeaderHash block1 ~ HeaderHash block2)
   => AnchoredFragment block1
   -> Point block2
   -> Maybe (AnchoredFragment block1, AnchoredFragment block1)
splitAfterPoint c pt = case CF.splitAfterPoint (unanchorFragment c) pt of
   Just (cp, cs)
     -> let p = mkAnchoredFragment (anchorPoint c) cp
        in Just (p, mkAnchoredFragment (headPoint p) cs)
   Nothing
     | anchorPoint c == castPoint pt
     -> Just (mkAnchoredFragment (anchorPoint c) CF.Empty, c)
     | otherwise
     -> Nothing

-- | \( O(\log(\min(i,n-i)) \). Split the 'AnchoredFragment' before the given
--  'Point'. Return 'Nothing' if given 'Point' is not on the fragment
--  ('pointOnFragment').
--
-- This means that 'Nothing' is returned if the given 'Point' is the anchor
-- point of the fragment.
--
-- POSTCONDITION: joining ('join') the two fragments gives back the original
-- fragment.
--
-- POSTCONDITION: the last block (oldest) on the second fragment corresponds
-- to the given point.
splitBeforePoint
   :: forall block1 block2.
      (HasHeader block1, HasHeader block2, HeaderHash block1 ~ HeaderHash block2)
   => AnchoredFragment block1
   -> Point block2
   -> Maybe (AnchoredFragment block1, AnchoredFragment block1)
splitBeforePoint (AnchoredFragment ap cf) pt =
    CF.splitBeforePoint cf pt <&> \(cp, cs) ->
      let before = mkAnchoredFragment ap                 cp
          after  = mkAnchoredFragment (headPoint before) cs
      in (before, after)

-- | \( O(\log(\min(n_1, n_2))) \). Join two anchored fragments if the anchor
-- of the second fragment is the head (newest block) of the first fragment.
--
-- If the first fragment is empty, it can be joined if its anchor is the same
-- as the second fragment's anchor.
--
-- The returned fragment will have the same anchor as the first fragment.
join :: HasHeader block
     => AnchoredFragment block
     -> AnchoredFragment block
     -> Maybe (AnchoredFragment block)
join af1@(AnchoredFragment a1 c1) af2@(AnchoredFragment a2 c2) =
    case head af1 of
      -- First fragment is empty
      Left _
        | a1 == a2
        -> Just af2
        | otherwise
        -> Nothing
      Right b1Head
        | blockPoint b1Head == a2
        -> mkAnchoredFragment a1 <$> CF.joinChainFragments c1 c2
        | otherwise
        -> Nothing

-- | \( O(n_2 \log(n_1)) \). Look for the most recent intersection of two
-- 'AnchoredFragment's @c1@ and @c2@.
--
-- The fragments need not have the same anchor point.
--
-- If they intersect, i.e., share a common 'Point' (possibly the anchor
-- point), then return a tuple of:
--
-- * @p1@: the prefix of the first  fragment
-- * @p2@: the prefix of the second fragment
-- * @s1@: the suffix of the first  fragment
-- * @s2@: the suffix of the second fragment
--
-- @p1@ and @p2@ will have the same /head/ (possibly an anchor point), namely
-- the intersection point @i@. The original chain @c1@ can be obtained by
-- putting @s1@ after @p1@, similarly for @c2@: by putting @s2@ after @p2@:
--
-- @
-- Just c1 = 'join' p1 s1
-- Just c2 = 'join' p2 s2
-- @
--
-- Take for example the following two fragments that share blocks 4 and 5. The
-- two fragments are fragments of the same chain, but don't contain all blocks
-- of the original chain. The anchor points of the fragments are indicated
-- with an asterisk (*). The @-A@ and @-B@ suffixes denote that blocks are
-- part of a fork of the chain.
--
-- >
-- >
-- >     ┆ 1*┆
-- >     ├───┤
-- >     │ 2 │     ┆ 2*┆
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
-- >     ┆ 1*┆
-- >     ├───┤
-- >     │ 2 │     ┆ 2*┆
-- >     ├───┤     ├───┤
-- >     │ 4 │     │ 4 │
-- >     ├───┤     ├───┤
-- >     │ 5 │     │ 5 │      ┆ 5*┆     ┆ 5*┆
-- > ────┴───┴─────┴───┴──────┼───┼─────┼───┼──
-- >                          │ 6A│     │ 6B│
-- >                          └───┘     ├───┤
-- >                                    │ 8B│
-- >                                    └───┘
-- > Just (p1,       p2,        s1,       s2)
--
-- The intersection point will be the anchor point of fragments @s1@ and @s2@.
-- Fragment @p1@ will have the same anchor as @c1@ and @p2@ will have the same
-- anchor as @c2@.
--
-- Note that an empty fragment can still intersect another fragment, as its
-- anchor point can still intersect the other fragment. In that case the
-- respective prefix and suffix are both equal to original empty fragment.
-- Additionally, two empty fragments intersect if their anchor points are
-- equal, in which case all prefixes and suffixes are equal to the empty
-- fragment with the anchor point in question.
intersect
    :: forall block1 block2.
       (HasHeader block1, HasHeader block2, HeaderHash block1 ~ HeaderHash block2)
    => AnchoredFragment block1
    -> AnchoredFragment block2
    -> Maybe (AnchoredFragment block1, AnchoredFragment block2,
              AnchoredFragment block1, AnchoredFragment block2)
intersect c1 c2 = go c2
  where
    go :: AnchoredFragment block2
       -> Maybe (AnchoredFragment block1, AnchoredFragment block2,
                 AnchoredFragment block1, AnchoredFragment block2)
    go (Empty a2)
      | Just (p1, s1) <- splitAfterPoint c1 a2
      = Just (p1, mkAnchoredFragment a2 CF.Empty, s1, c2)
      | otherwise
      = Nothing
    go (c2' :> b)
      | let pt = blockPoint b
      , Just (p1, s1) <- splitAfterPoint c1 pt
      , Just (p2, s2) <- splitAfterPoint c2 pt
        -- splitAfterPoint c2 pt cannot fail,
        -- since pt comes out of c2
      = Just (p1, p2, s1, s2)
      | otherwise
      = go c2'

-- | \( O(n_2 \log(n_1)) \). Look for the most recent intersection point of
-- two 'AnchoredFragment's
--
-- The fragments need not have the same anchor point.
--
-- Reusing the example in the docstring of 'intersect': this function will
-- return the anchor point @5*@.
intersectionPoint
    :: (HasHeader block1, HasHeader block2, HeaderHash block1 ~ HeaderHash block2)
    => AnchoredFragment block1
    -> AnchoredFragment block2
    -> Maybe (Point block1)
intersectionPoint c1 c2 = case c1 `intersect` c2 of
    Just (_, _, s1, _) -> Just (anchorPoint s1)
    Nothing            -> Nothing

-- | \( O(n) \). Maps over the chain blocks. This is not allowed to change the
-- block `Point`s, or it would create an invalid chain. The 'anchorPoint' is
-- not affected.
--
mapAnchoredFragment :: (HasHeader block1, HasHeader block2,
                        HeaderHash block1 ~ HeaderHash block2)
                 => (block1 -> block2)
                 -> AnchoredFragment block1
                 -> AnchoredFragment block2
mapAnchoredFragment f (AnchoredFragment a c) =
    AnchoredFragment (castPoint a) (CF.mapChainFragment f c)
