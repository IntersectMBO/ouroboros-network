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
  join,
  intersect,
  intersectionPoint,

  -- * Conversion to/from Chain
  fromChain,
  toChain,
  anchorNewest,

  -- * Helper functions
  prettyPrint
  ) where

import           Prelude hiding (head, last, length, null)

import           Control.Exception (assert)
import           Data.List (find)
import           Data.Word (Word64)

import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain (Chain)
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.ChainFragment (ChainFragment)
import qualified Ouroboros.Network.ChainFragment as CF


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
pattern (:>) :: HasHeader block
             => AnchoredFragment block -> block -> AnchoredFragment block
pattern af' :> b <- (viewRight -> ConsR af' b)
  where
    af@(AnchoredFragment a c) :> b =
      assert (validExtension af b) $
      AnchoredFragment a (c CF.:> b)

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
                 pointSlot p <  blockSlot     bSucc
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
-- returned.
headSlot :: HasHeader block => AnchoredFragment block -> SlotNo
headSlot = either pointSlot blockSlot . head

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
-- returned.
lastSlot :: HasHeader block => AnchoredFragment block -> SlotNo
lastSlot = either pointSlot blockSlot . last

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
                 => ChainUpdate block
                 -> AnchoredFragment block
                 -> Maybe (AnchoredFragment block)
applyChainUpdate (AddBlock b) c = Just (addBlock b c)
applyChainUpdate (RollBack p) c =       rollback p c

applyChainUpdates :: HasHeader block
                  => [ChainUpdate block]
                  -> AnchoredFragment block
                  -> Maybe (AnchoredFragment block)
applyChainUpdates []     c = Just c
applyChainUpdates (u:us) c = applyChainUpdates us =<< applyChainUpdate u c

-- | Convert a 'Chain' to an 'AnchoredFragment'.
--
-- The anchor of the fragment will be 'Chain.genesisPoint'.
fromChain :: HasHeader block => Chain block -> AnchoredFragment block
fromChain = fromNewestFirst Chain.genesisPoint . Chain.toNewestFirst

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

-- | Take the @n@ newest blocks from the chain and turn them into an
-- 'AnchoredFragment'.
--
-- When the chain itself is shorter than @n@ blocks, the fragment will also be
-- shorter than @n@ blocks (and anchored at genesis).
anchorNewest :: forall block. HasHeader block
             => Word64  -- ^ @n@
             -> Chain block
             -> AnchoredFragment block
anchorNewest = go CF.Empty
  where
    -- Walk back over the chain, building up a chain fragment until k = 0 or
    -- we encountered genesis, then anchor the built-up chain fragment
    go :: ChainFragment block -> Word64 -> Chain block -> AnchoredFragment block
    go cf _ Chain.Genesis   = mkAnchoredFragment Chain.genesisPoint cf
    go cf 0 (_  Chain.:> b) = mkAnchoredFragment (blockPoint b)     cf
    go cf n (ch Chain.:> b) = go (b CF.:< cf) (n - 1) ch

-- | \( O(\max(n_1, n_2)) \). Check whether the first anchored fragment is a
-- prefix of the second.
--
-- The two 'AnchoredFragment's must have the same anchor point, otherwise the
-- first cannot be a prefix of the second.
isPrefixOf :: (HasHeader block, Eq block)
           => AnchoredFragment block -> AnchoredFragment block -> Bool
AnchoredFragment a1 c1 `isPrefixOf` AnchoredFragment a2 c2 =
    a1 == a2 && c1 `CF.isPrefixOf` c2

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
-- __Precondition__: the two 'AnchoredFragment's have the same anchor @a@.
--
-- We return:
--
-- * @p@ anchored at @a@: the common prefix shared between the fragments @c1@
--   and @c2@. The head (most recent) point of this prefix will become the
--   anchor of the suffixes; let's call it @a'@.
-- * @s1@ anchored at @a'@: the suffix of the first fragment @c1@ that comes
--   after @p@.
-- * @s2@ anchored at @a'@: the suffix of the second fragment @c2@ that comes
--   after @p@.
--
-- Since @c1@ and @c2@ have the same anchor, there /must/ always be at least
-- one intersection, i.e. the anchor point.
--
-- If the fragments fork off directly after the anchor point and don't share
-- any more points besides the anchor point, then @p@ is the empty fragment
-- anchored at @a@, @s1 = c1@, @s2 = c2@ and @a' = a@.
--
-- The original fragments @c1@ and @c2@ can be obtained by joining @p@ and
-- @s1@, and @p@ and @s2@ respectively:
--
-- @
-- Just c1 = 'join' p s1
-- Just c2 = 'join' p s2
-- @
--
-- For example:
--
-- >
-- >     ┆ A ┆     ┆ A ┆
-- >     ├───┤     ├───┤
-- >     │ b │     │ b │
-- >     ├───┤     ├───┤
-- >     │ c │     │ c │
-- > ────┼───┼─────┼───┼───
-- >     │ d │     │ d'│
-- >     └───┘     ├───┤
-- >               │ e'│
-- >               └───┘
-- >       c1        c2
--
-- Where @A@ is the anchor of both chains. The most recent intersection of
-- @c1@ and @c2@ is block @c@. We return the following fragments:
--
-- >
-- >     ┆ A ┆
-- >     ├───┤
-- >     │ b │
-- >     ├───┤
-- >     │ c │     ┆ C ┆     ┆ C ┆
-- > ────┴───┴─────┼───┼─────┼───┼───
-- >               │ d │     │ d'│
-- >               └───┘     ├───┤
-- >                         │ e'│
-- >                         └───┘
-- >      (p,       s1,       s2)
intersect
    :: (HasHeader block1, HasHeader block2, HeaderHash block1 ~ HeaderHash block2)
    => AnchoredFragment block1
    -> AnchoredFragment block2
    -> (AnchoredFragment block1, AnchoredFragment block1, AnchoredFragment block2)
intersect c1 c2 =
    assert (anchorPoint c1 == castPoint (anchorPoint c2)) $
    case CF.intersectChainFragments cf1 cf2 of
      Nothing                -> (Empty a, c1, c2)
      Just (cp, _, cs1, cs2) ->
        let p  = mkAnchoredFragment a cp
            a' = headPoint p
        in (p, mkAnchoredFragment a' cs1, mkAnchoredFragment (castPoint a') cs2)
  where
    a   = anchorPoint c1
    cf1 = unanchorFragment c1
    cf2 = unanchorFragment c2

-- | \( O(n_2 \log(n_1)) \). Look for the most recent intersection point of
-- two 'AnchoredFragment's
--
-- __Precondition__: the two 'AnchoredFragment's have the same anchor.
--
-- Reusing the example in the docstring of 'intersect': this function will
-- return the anchor point @C@.
intersectionPoint
    :: (HasHeader block1, HasHeader block2, HeaderHash block1 ~ HeaderHash block2)
    => AnchoredFragment block1
    -> AnchoredFragment block2
    -> Point block1
intersectionPoint c1 c2 = anchorPoint s1
  where
    (_, s1, _) = c1 `intersect` c2
