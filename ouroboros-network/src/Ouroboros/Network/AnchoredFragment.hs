{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}
module Ouroboros.Network.AnchoredFragment (
  -- * AnchoredFragment type and fundamental operations
  AnchoredFragment(Empty, (:>), (:<)),
  anchor,
  anchorPoint,
  anchorBlockNo,

  Anchor(..),
  anchorFromBlock,
  anchorFromPoint,
  anchorToPoint,
  anchorToSlotNo,
  anchorToBlockNo,
  anchorToHash,
  anchorIsGenesis,
  anchorToHeaderFields,
  anchorToTip,
  castAnchor,

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
  headAnchor,
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
  splitAt,
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
  sliceRange,
  join,
  intersect,
  intersectionPoint,
  mapAnchoredFragment,
  anchorNewest,
  filter,
  filterWithStop,

  -- * Helper functions
  prettyPrint,

  -- * Reference implementations for testing
  pointOnFragmentSpec,
  selectPointsSpec,
  filterWithStopSpec
  ) where

import           Prelude hiding (filter, head, last, length, null, splitAt)

import           Data.Either (isRight)
import           Data.FingerTree.Strict (StrictFingerTree)
import qualified Data.FingerTree.Strict as FT
import qualified Data.Foldable as Foldable
import qualified Data.List as L
import           Data.Maybe (fromMaybe)
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           GHC.Stack
import           NoThunks.Class (NoThunks)

import           Ouroboros.Network.Block
import           Ouroboros.Network.Point (WithOrigin (At, Origin), withOrigin)

-- | An 'AnchoredFragment' is a fragment of a chain that is anchored somewhere
-- in that chain. The 'Anchor' corresponds to the block immediately before the
-- first, leftmost block in the fragment. The block corresponding to the anchor
-- is not present in the fragment. The anchor can be thought of as a left
-- exclusive bound.
--
-- For example, the following fragment is anchored at @a@ and contains @b1@,
-- @b2@, and @b3@, which is the head of the fragment.
--
-- > a ] b1 >: b2 >: b3
--
-- The fact that it is an /exclusive/ bound is particularly convenient when
-- dealing with Genesis. Genesis is the start of the chain, but not an actual
-- block, so we cannot use it an inclusive bound. However, there /is/ an
-- 'Anchor' that refers to Genesis ('AnchorGenesis'), which can be used as the
-- anchor, acting as an exclusive bound.
--
-- An 'AnchoredFragment' anchored at Genesis, can thus be converted to a
-- 'Ouroboros.Network.MockChain.Chain' ('fromAnchoredFragment'), containing all
-- blocks starting from Genesis.
--
-- Without an anchor point, an empty fragment wouldn't give us much more
-- information: is it empty because the whole chain is empty? Or, did we just
-- get an empty fragment that was split off from some later part of the chain?
data AnchoredFragment block = AnchoredFragment {
      anchor           :: !(Anchor block)
    , unanchorFragment :: !(StrictFingerTree BlockMeasure block)
    }
  deriving (Show, Eq, Generic, NoThunks)

anchorPoint :: AnchoredFragment block -> Point block
anchorPoint = anchorToPoint . anchor

anchorBlockNo :: AnchoredFragment block -> WithOrigin BlockNo
anchorBlockNo = anchorToBlockNo . anchor

-- | Anchor of an 'AnchoredFragment'
data Anchor block =
    -- | The fragment is anchored at genesis
    AnchorGenesis

    -- | The fragment is anchored after genesis
    --
    -- We don't use the 'Point' type directly as that has its /own/ use of
    -- 'WithOrigin', and we want to enforce here that we have a block number
    -- if and only if the point is not 'Origin'.
    --
    -- TODO: Use 'HeaderField'?
  | Anchor !SlotNo !(HeaderHash block) !BlockNo
  deriving (Generic)

deriving instance StandardHash block => Show     (Anchor block)
deriving instance StandardHash block => Eq       (Anchor block)
deriving instance StandardHash block => NoThunks (Anchor block)

-- | The equivalent of 'castPoint' for 'Anchor'
castAnchor :: (HeaderHash a ~ HeaderHash b) => Anchor a -> Anchor b
castAnchor AnchorGenesis  = AnchorGenesis
castAnchor (Anchor s h b) = Anchor s h b

-- | Does this anchor represent genesis (i.e., empty chain)?
anchorIsGenesis :: Anchor block -> Bool
anchorIsGenesis AnchorGenesis = True
anchorIsGenesis Anchor{}      = False

-- | Construct anchor from a block
--
-- In other words, this would be the block immediately /before/ the other blocks
-- in the fragment.
anchorFromBlock :: HasHeader block => block -> Anchor block
anchorFromBlock b = Anchor sno hash bno
  where
    HeaderFields {
        headerFieldSlot    = sno
      , headerFieldBlockNo = bno
      , headerFieldHash    = hash
      } = getHeaderFields b

-- | Compute which 'Point' this anchor corresponds to
anchorToPoint :: Anchor block -> Point block
anchorToPoint AnchorGenesis   = genesisPoint
anchorToPoint (Anchor s h _b) = BlockPoint s h

-- | Construct an anchor /from/ a point
--
-- In this case, we must also be given the 'BlockNo'. This only makes sense
-- for points that aren't genesis.
anchorFromPoint :: Point block -> BlockNo -> Anchor block
anchorFromPoint GenesisPoint _     = error "anchorFromPoint: genesis point"
anchorFromPoint (BlockPoint s h) b = Anchor s h b

-- | Extract the 'BlockNo' from the anchor
--
-- NOTE: When the 'Anchor' is 'AnchorGenesis', this returns 'Origin'.
-- It does /not/ return 'genesisBlockNo', which is badly named, and is instead
-- the block number of the first block on the chain
-- (i.e., 'genesisPoint' and 'genesisBlockNo' don't go hand in hand!)
anchorToBlockNo :: Anchor block -> WithOrigin BlockNo
anchorToBlockNo AnchorGenesis    = Origin
anchorToBlockNo (Anchor _s _h b) = At b

-- | Extract the 'SlotNo' from the anchor
anchorToSlotNo :: Anchor block -> WithOrigin SlotNo
anchorToSlotNo AnchorGenesis    = Origin
anchorToSlotNo (Anchor s _h _b) = At s

-- | Extract the hash from the anchor
--
-- Returns 'GenesisHash' if the anchor is 'AnchorGenesis'.
anchorToHash :: Anchor block -> ChainHash block
anchorToHash AnchorGenesis    = GenesisHash
anchorToHash (Anchor _s h _b) = BlockHash h

anchorToHeaderFields :: Anchor block -> WithOrigin (HeaderFields block)
anchorToHeaderFields AnchorGenesis  = Origin
anchorToHeaderFields (Anchor s h b) = At (HeaderFields s b h)

-- | Translate 'Anchor' to 'Tip'
--
-- Right now this is in fact an isomorphism, but these two types are logically
-- independent.
anchorToTip :: (HeaderHash a ~ HeaderHash b) => Anchor a -> Tip b
anchorToTip AnchorGenesis  = TipGenesis
anchorToTip (Anchor s h b) = Tip s h b

-- | \( O(1) \). Pattern for matching on or creating an empty
-- 'AnchoredFragment'. An empty fragment has/needs an anchor point.
pattern Empty :: HasHeader block => Anchor block -> AnchoredFragment block
pattern Empty a <- (viewRight -> EmptyR a)
  where
    Empty a = AnchoredFragment a FT.empty

-- | Auxiliary data type to define the pattern synonym
data ViewRight block
    = EmptyR (Anchor block)
    | ConsR  (AnchoredFragment block) block

viewRight :: HasHeader block => AnchoredFragment block -> ViewRight block
viewRight (AnchoredFragment a ft) = case FT.viewr ft of
    FT.EmptyR   -> EmptyR a
    ft' FT.:> b -> ConsR (AnchoredFragment a ft') b

-- | \( O(1) \). Add a block to the right of the anchored fragment.
pattern (:>) :: HasHeader block
             => AnchoredFragment block -> block -> AnchoredFragment block
pattern af' :> b <- (viewRight -> ConsR af' b)
  where
    AnchoredFragment a ft :> b = AnchoredFragment a (ft FT.|> b)

-- | Auxiliary data type to define the pattern synonym
data ViewLeft block
    = EmptyL (Anchor block)
    | ConsL  block (AnchoredFragment block)

viewLeft :: HasHeader block => AnchoredFragment block -> ViewLeft block
viewLeft (AnchoredFragment a ft) = case FT.viewl ft of
    FT.EmptyL   -> EmptyL a
    b FT.:< ft' -> ConsL b (AnchoredFragment (anchorFromBlock b) ft')

-- | \( O(1) \). View the first, leftmost block of the anchored fragment.
--
-- This is only a view, not a constructor, as adding a block to the left would
-- change the anchor of the fragment, but we have no information about the
-- predecessor of the block we'd be prepending.
pattern (:<) :: HasHeader block
             => block -> AnchoredFragment block -> AnchoredFragment block
pattern b :< af' <- (viewLeft -> ConsL b af')

infixl 5 :>, :<

{-# COMPLETE Empty, (:>) #-}
{-# COMPLETE Empty, (:<) #-}

prettyPrint :: String
            -> (Point block -> String)
            -> (block -> String)
            -> AnchoredFragment block
            -> String
prettyPrint nl ppPoint ppBlock (AnchoredFragment a ft) =
    Foldable.foldl'
      (\s b -> s ++ nl ++ "    " ++ ppBlock b)
      ("AnchoredFragment (" <> ppPoint (anchorToPoint a) <> "):")
      ft

-- | \( O(n) \).
valid :: HasFullHeader block => AnchoredFragment block -> Bool
valid (Empty _) = True
valid (af :> b) = valid af && validExtension af b

-- | Checks whether the first block @bSucc@ is a valid successor of the second
-- block @b@ identified by an 'Anchor'.
--
-- * The 'blockPrevHash' of the @bSucc@ must match that of @b@.
-- * The 'blockSlot' of @bSucc@ must be strictly larger than that of @b@.
-- * The 'blockNo' of @bSucc@ must be 1 greater than that of @b@.
--
-- This function does not check whether @bSucc@ satisfies 'blockInvariant'.
isValidSuccessorOf :: HasFullHeader block
                   => block  -- ^ @bSucc@
                   -> Anchor block  -- ^ @b@
                   -> Bool
isValidSuccessorOf bSucc b = isRight $ isValidSuccessorOf' bSucc b

-- | Variation on 'isValidSuccessorOf' that provides more information
isValidSuccessorOf' :: HasFullHeader block
                    => block  -- ^ @bSucc@
                    -> Anchor block  -- ^ @b@
                    -> Either String ()
isValidSuccessorOf' bSucc b
  | anchorToHash b /= blockPrevHash bSucc
  = Left $ concat [
        "prevHash ("
      , show (blockPrevHash bSucc)
      , ") doesn't match hash of tip ("
      , show (anchorToHash b)
      , ") at "
      , prettyCallStack callStack
      ]
    -- Note that this inequality would be strict, but for epoch
    -- boundary blocks, which occupy the same slot as a regular
    -- block.
  | anchorToSlotNo b > At (blockSlot bSucc)
  = Left $ concat [
        "Slot of tip ("
      , show (anchorToSlotNo b)
      , ") > slot ("
      , show (blockSlot bSucc)
      , ")"
      ]
  -- The block number of the next block cannot be less than that of the tip,
  -- or more than that of the tip plus 1. It /can/ be the same as the tip,
  -- in the case of EBBs.
  | At (blockNo bSucc) < anchorToBlockNo b
  = Left $ concat [
        "BlockNo ("
      , show (blockNo bSucc)
      , ") is less than BlockNo of tip ("
      , show (anchorToBlockNo b)
      , ")"
      ]
  | blockNo bSucc > withOrigin (BlockNo 0) succ (anchorToBlockNo b)
  = Left $ concat [
        "BlockNo ("
      , show (blockNo bSucc)
      , ") is greater than BlockNo of tip ("
      , show (anchorToBlockNo b)
      , ") + 1"
      ]
  | otherwise
  = Right ()

-- | \( O(1) \).
validExtension :: HasFullHeader block => AnchoredFragment block -> block -> Bool
validExtension af bSucc =
    blockInvariant bSucc &&
    bSucc `isValidSuccessorOf` headAnchor af

-- | \( O(1) \). When the fragment is empty, return the anchor point,
-- otherwise the most recently added block.
head :: HasHeader block => AnchoredFragment block -> Either (Anchor block) block
head (_ :> b)  = Right b
head (Empty a) = Left a

-- | \( O(1) \). When the fragment is empty, the anchor point is returned.
headPoint :: HasHeader block => AnchoredFragment block -> Point block
headPoint = anchorToPoint . headAnchor

-- | \( O(1) \). The anchor corresponding to the most recently added block
-- (i.e., the anchor that would be needed for a fragment starting /after/ this)
headAnchor :: HasHeader block => AnchoredFragment block -> Anchor block
headAnchor = either id anchorFromBlock . head

-- | \( O(1) \). When the fragment is empty, the slot of the anchor point is
-- returned, which may be origin (no slot).
headSlot :: HasHeader block => AnchoredFragment block -> WithOrigin SlotNo
headSlot = either anchorToSlotNo (At . blockSlot) . head

-- | \( O(1) \). When the fragment is empty, the hash of the anchor point is
-- returned.
headHash :: HasHeader block => AnchoredFragment block -> ChainHash block
headHash = either anchorToHash (BlockHash . blockHash) . head

-- | \( O(1) \). When the fragment is empty, the block number of the anchor
-- point is returned.
headBlockNo :: HasHeader block => AnchoredFragment block -> WithOrigin BlockNo
headBlockNo = either anchorToBlockNo (At . blockNo) . head

-- | \( O(1) \). When the fragment is empty, return the anchor point,
-- otherwise the leftmost block.
last :: HasHeader block => AnchoredFragment block -> Either (Anchor block) block
last (b :< _)  = Right b
last (Empty a) = Left a

-- | \( O(1) \). When the fragment is empty, the anchor point is returned.
lastPoint :: HasHeader block => AnchoredFragment block -> Point block
lastPoint = either anchorToPoint blockPoint . last

-- | \( O(1) \). When the fragment is empty, the slot of the anchor point is
-- returned, which may be the origin and therefore have no slot.
lastSlot :: HasHeader block => AnchoredFragment block -> WithOrigin SlotNo
lastSlot = either anchorToSlotNo (At . blockSlot) . last

-- | TODO. Make a list of blocks from a 'AnchoredFragment', in newest-to-oldest
-- order.
toNewestFirst :: AnchoredFragment block -> [block]
toNewestFirst = Foldable.foldl' (flip (:)) [] . unanchorFragment

-- | \( O(n) \). Make a list of blocks from a 'AnchoredFragment', in
-- oldest-to-newest order.
toOldestFirst :: AnchoredFragment block -> [block]
toOldestFirst = Foldable.toList . unanchorFragment

-- | \( O(n) \). Make a 'AnchoredFragment' from a list of blocks in
-- newest-to-oldest order. The last block in the list must be the block
-- following the given anchor point.
fromNewestFirst :: HasHeader block
                => Anchor block  -- ^ Anchor
                -> [block] -> AnchoredFragment block
fromNewestFirst a = foldr (flip (:>)) (Empty a)

-- | \( O(n) \). Make a 'AnchoredFragment' from a list of blocks in
-- oldest-to-newest order. The first block in the list must be the block
-- following the given anchor point.
fromOldestFirst :: HasHeader block
                => Anchor block  -- ^ Anchor
                -> [block] -> AnchoredFragment block
fromOldestFirst a bs = AnchoredFragment a (FT.fromList bs)

-- | \( O(\log(\min(i,n-i)) \). Split the 'AnchoredFragment' at a given
--  position.
--
-- POSTCONDITION: @(before, after) = splitAt i f@, then:
-- * @anchorPoint before == anchorPoint f@
-- * @headPoint   before == anchorPoint after@
-- * @headPoint   after  == headPoint f@
-- * @join before after  == Just f@
splitAt ::
      forall block. HasHeader block
   => Int
   -> AnchoredFragment block
   -> (AnchoredFragment block, AnchoredFragment block)
splitAt i (AnchoredFragment a ft) = case FT.split (\v -> bmSize v > i) ft of
   (before, after) ->
     let before' = AnchoredFragment a before
     in (before', AnchoredFragment (headAnchor before') after)

-- | \( O(\log(\min(i,n-i)) \). Drop the newest @n@ blocks from the
-- 'AnchoredFragment'. The anchor point is not changed.
dropNewest :: HasHeader block
           => Int  -- ^ @n@
           -> AnchoredFragment block -> AnchoredFragment block
dropNewest n af@(AnchoredFragment a ft) =
    AnchoredFragment a $ FT.takeUntil (\v -> bmSize v > remainingLength) ft
  where
    remainingLength = length af - n

-- | \( O(\log(\min(i,n-i)) \). Take the oldest @n@ blocks from the
-- 'AnchoredFragment'. The anchor point is not changed.
takeOldest :: HasHeader block
           => Int  -- ^ @n@
           -> AnchoredFragment block -> AnchoredFragment block
takeOldest n (AnchoredFragment a ft) =
    AnchoredFragment a $ FT.takeUntil (\v -> bmSize v > n) ft

-- | \( O(n) \). Drop the newest blocks that satisfy the predicate, keeping
-- the remainder. The anchor point is not changed.
dropWhileNewest :: HasHeader block
                => (block -> Bool)
                -> AnchoredFragment block
                -> AnchoredFragment block
dropWhileNewest _ (Empty a) = Empty a
dropWhileNewest p af@(af' :> b)
    | p b       = dropWhileNewest p af'
    | otherwise = af

-- | \( O(n) \). Take the oldest blocks that satisfy the predicate. The anchor
-- point is not changed.
takeWhileOldest :: HasHeader block
                => (block -> Bool)
                -> AnchoredFragment block
                -> AnchoredFragment block
takeWhileOldest p = \(AnchoredFragment a ft) -> AnchoredFragment a (go ft)
  where
    go ft = case FT.viewl ft of
        FT.EmptyL
          -> FT.empty
        b FT.:< ft'
          | p b
          -> b FT.<| go ft'
          | otherwise
          -> FT.empty

-- | \( O(1) \). Return the number of blocks. The anchor point is not counted.
length :: HasHeader block => AnchoredFragment block -> Int
length = bmSize . FT.measure . unanchorFragment

-- | \( O(1) \). The anchor point is not counted.
null :: AnchoredFragment block -> Bool
null = FT.null . unanchorFragment

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
rollback p af@(AnchoredFragment a _)
    | p == anchorToPoint a
    = Just (Empty a)
    | otherwise
    = fst <$> splitAfterPoint af p

-- | \( O(\log(\min(i,n-i)) \). Internal variant of 'lookupBySlot' that
-- returns a 'FT.SearchResult'.
lookupBySlotFT :: HasHeader block
               => AnchoredFragment block
               -> SlotNo
               -> FT.SearchResult BlockMeasure block
lookupBySlotFT (AnchoredFragment _ ft) s =
    FT.search (\vl vr -> bmMaxSlot vl >= s && bmMinSlot vr > s) ft

-- | \( O(\log(\min(i,n-i) + s) \) where /s/ is the number of blocks with the
-- same slot. Return all blocks in the chain fragment with a slot equal to the
-- given slot. The blocks will be ordered from oldest to newest.
lookupBySlot :: HasHeader block
             => AnchoredFragment block
             -> SlotNo
             -> [block]
lookupBySlot af s = case lookupBySlotFT af s of
    FT.Position before b _after
      | blockSlot b == s
        -- We have found the rightmost block with the given slot, we still
        -- have to look at the blocks before it with the same slot.
      -> blocksBefore before [b]
    _ -> []
  where
    -- Look to the left of the block we found for more blocks with the same
    -- slot.
    blocksBefore before acc = case FT.viewr before of
      before' FT.:> b
        | blockSlot b == s
        -> blocksBefore before' (b:acc)
           -- Note that we're prepending an older block each time, so the
           -- final list of blocks will be ordered from oldest to newest. No
           -- need to reverse the accumulator.
      _ -> acc

-- | \( O(o \log(\min(i,n-i))) \). Select a bunch of 'Point's based on offsets
-- from the head of the anchored fragment. This is used in the chain consumer
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
-- Only for offsets within the bounds of the anchored fragment will there be
-- points in the returned list.
--
-- __Note__: offset @n@, where @n@ equals the length of the anchored fragment,
-- corresponds to the anchor point. When the fragment is empty, offset 0 will
-- thus correspond to the anchor point.
selectPoints ::
     forall block. HasHeader block
  => [Int]
  -> AnchoredFragment block
  -> [Point block]
selectPoints offsets = go relativeOffsets
  where
    relativeOffsets = zipWith (-) offsets (0:offsets)

    go :: [Int] -> AnchoredFragment block -> [Point block]
    go [] _
      = []
    go (off:offs) af
      | let i = length af - off
      , i >= 0
      , (af', _) <- splitAt i af
      = headPoint af' : go offs af'
      | otherwise
      = []

-- | \( O(o * n) \). Specification of 'selectPoints'.
--
-- Use 'selectPoints', as it should be faster.
--
-- This function is used to verify whether 'selectPoints' behaves as expected.
selectPointsSpec :: HasHeader block
                => [Int] -> AnchoredFragment block -> [Point block]
selectPointsSpec offsets c =
    [ ps !! offset
    | let ps = map blockPoint (toNewestFirst c) <> [anchorPoint c]
          len = L.length ps
    , offset <- offsets
    , offset < len
    ]

-- | \( O(\log(\min(i,n-i)) \). Find the block after the given point. If the
-- given point is the anchor point, then the first block is returned (if there
-- is one).
successorBlock :: HasHeader block
               => Point block -> AnchoredFragment block -> Maybe block
successorBlock p af@(AnchoredFragment a _)
    | p == anchorToPoint a
    = either (const Nothing) Just $ last af
    | otherwise
    = case splitAfterPoint af p of
        Just (_, b :< _) -> Just b
        _otherwise       -> Nothing

-- | \( O(\log(\min(i,n-i)) \). Does the fragment contain a block with the given
-- point? The anchor point is ignored.
pointOnFragment :: HasHeader block
                => Point block -> AnchoredFragment block -> Bool
pointOnFragment p af = case p of
    GenesisPoint       -> False
    BlockPoint bslot _ -> any ((== p) . blockPoint) $ lookupBySlot af bslot

-- | \( O(n) \). Specification of 'pointOnFragment'.
--
-- Use 'pointOnFragment', as it should be faster.
--
-- This function is used to verify whether 'pointOnFragment' behaves as
-- expected.
pointOnFragmentSpec :: HasHeader block
                    => Point block -> AnchoredFragment block -> Bool
pointOnFragmentSpec p = go
    where
      -- Recursively search the fingertree from the right
      go (Empty _) = False
      go (c' :> b) | blockPoint b == p = True
                   | otherwise         = go c'

-- | \( O(\log(\min(i,n-i)) \). Is the point within the fragment bounds?
-- Either the point is the anchor point, or it corresponds to a block \"on\"
-- the fragment.
withinFragmentBounds :: HasHeader block
                     => Point block -> AnchoredFragment block -> Bool
withinFragmentBounds p af@(AnchoredFragment a _) =
    p == anchorToPoint a || pointOnFragment p af

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
findFirstPoint ps c = L.find (`withinFragmentBounds` c) ps

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
anchorNewest n c
    | toDrop <= 0
    = c
    | toDrop < 5
      -- Hybrid approach: microbenchmarks have shown that a linear drop is
      -- faster when the number of elements is small. For a larger number of
      -- elements, the asymptotic complexity of 'splitAt' wins.
    = linearDrop toDrop c
    | otherwise
    = snd $ splitAt toDrop c
  where
    len, toDrop :: Int
    len    = length c
    toDrop = len - fromIntegral n

    linearDrop :: Int -> AnchoredFragment block -> AnchoredFragment block
    linearDrop !_ (Empty a) = Empty a
    linearDrop !0 c'        = c'
    linearDrop !m (_ :< c') = linearDrop (m - 1) c'

-- | \( O(\max(n_1, n_2)) \). Check whether the first anchored fragment is a
-- prefix of the second.
--
-- The two 'AnchoredFragment's must have the same anchor point, otherwise the
-- first cannot be a prefix of the second.
isPrefixOf :: (HasHeader block, Eq block)
           => AnchoredFragment block -> AnchoredFragment block -> Bool
af1 `isPrefixOf` af2 =
    anchor af1 == anchor af2 && toOldestFirst af1 `L.isPrefixOf` toOldestFirst af2

-- | \( O(\log(\min(i,n-i)) \). Split the 'AnchoredFragment' after the given
--  'Point'. Return 'Nothing' if given 'Point' is not within the fragment
--  bounds ('withinFragmentBounds').
--
-- The given 'Point' may be the anchor point of the fragment, in which case
-- the empty fragment with the given anchor point and the original fragment
-- are returned.
--
-- POSTCONDITION: when @Just (before, after) = splitAfterPoint f pt@, then:
-- * @anchorPoint before == anchorPoint f@
-- * @headPoint   before == pt@
-- * @anchorPoint after  == pt@
-- * @headPoint   after  == headPoint f@
-- * @join before after  == Just f@
splitAfterPoint
   :: forall block1 block2.
      (HasHeader block1, HeaderHash block1 ~ HeaderHash block2)
   => AnchoredFragment block1
   -> Point block2
   -> Maybe (AnchoredFragment block1, AnchoredFragment block1)
splitAfterPoint af@(AnchoredFragment a ft) p =
    case p of
      _ | anchorToPoint a == castPoint p
        -> Just (Empty a, af)
      BlockPoint slot _
        | (l, r) <- FT.split (\v -> bmMaxSlot v > slot) ft
          -- @l@ contains blocks with a slot <= the given slot. There could be
          -- multiple with the given slot, so try them one by one.
        -> go slot l r
      _otherwise
        -> Nothing
  where
    go slot l r = case FT.viewr l of
      l' FT.:> b
        | blockPoint b == castPoint p
        , let al = AnchoredFragment a l
        -> Just (al, AnchoredFragment (headAnchor al) r)
        | blockSlot b == slot
        -> go slot l' (b FT.<| r)
      -- Empty tree or the slot number doesn't match anymore
      _ -> Nothing

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
      (HasHeader block1, HeaderHash block1 ~ HeaderHash block2)
   => AnchoredFragment block1
   -> Point block2
   -> Maybe (AnchoredFragment block1, AnchoredFragment block1)
splitBeforePoint _ GenesisPoint = Nothing
splitBeforePoint (AnchoredFragment a ft) p@(BlockPoint bslot _)
    | (l, r) <- FT.split (\v -> bmMaxSlot v >= bslot) ft
      -- @r@ contains blocks with a slot >= the given slot. There could be
      -- multiple with the given slot, so try them one by one.
    = go l r
    | otherwise
    = Nothing
  where
    go l r = case FT.viewl r of
      b FT.:< r'
        | blockPoint b == castPoint p
        , let al = AnchoredFragment a l
        -> Just (al, AnchoredFragment (headAnchor al) r)
        | blockSlot b == bslot
        -> go (l FT.|> b) r'
      -- Empty tree or the slot number doesn't match anymore
      _ -> Nothing

-- | Select a slice of an anchored fragment between two points, inclusive.
--
-- Both points must exist on the chain, in order, or the result is @Nothing@.
--
sliceRange :: HasHeader block
           => AnchoredFragment block
           -> Point block
           -> Point block
           -> Maybe (AnchoredFragment block)
sliceRange af from to
  | Just (_, af') <- splitBeforePoint af  from
  , Just (af'',_) <- splitAfterPoint  af' to
  = Just af''

  | otherwise
  = Nothing

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
join af1@(AnchoredFragment a1 ft1) af2@(AnchoredFragment _ ft2)
    | headPoint af1 == anchorPoint af2
    = Just $ AnchoredFragment a1 (ft1 FT.>< ft2)
    | otherwise
    = Nothing

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
intersect c1 c2
    | length c2 > length c1
      -- Note that 'intersect' is linear in its second argument. It iterates
      -- over the elements in the second fragment, starting from the end,
      -- looking for a match in the first fragment (with a /O(log(n))/ cost).
      -- So by using the shortest fragment as the second argument, we get the
      -- same result with a lower cost than the other way around.
    = (\(p2, p1, s2, s1) -> (p1, p2, s1, s2)) <$> intersect c2 c1

    | pointSlot (headPoint c1) < pointSlot (anchorPoint c2) ||
      pointSlot (headPoint c2) < pointSlot (anchorPoint c1)
      -- If there is no overlap in slot numbers, there will be no overlap
    = Nothing

    | otherwise
    = go c2
  where
    go :: AnchoredFragment block2
       -> Maybe (AnchoredFragment block1, AnchoredFragment block2,
                 AnchoredFragment block1, AnchoredFragment block2)
    go (Empty a2)
      | Just (p1, s1) <- splitAfterPoint c1 (anchorToPoint a2)
      = Just (p1, Empty a2, s1, c2)
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
mapAnchoredFragment f (AnchoredFragment a ft) =
    AnchoredFragment (castAnchor a) (FT.fmap' f ft)

-- | \( O\(n\) \). Variation on 'filterWithStop' without a stop condition.
filter ::
     forall block. HasHeader block
  => (block -> Bool)  -- ^ Filtering predicate
  -> AnchoredFragment block
  -> [AnchoredFragment block]
filter p = filterWithStop p (const False)

-- | \( O(n + r * \log(\min(i,n-i)) \) where /r/ is the number of consecutive
-- ranges of blocks to be included in the result.
--
-- Filter out blocks that don't match the predicate.
--
-- As filtering removes blocks the result is a sequence of disconnected
-- fragments. The fragments are in the original order and are of maximum size.
--
-- As soon as the stop condition is true, the filtering stops and the remaining
-- fragment (starting with the first element for which the stop condition is
-- true) is the final fragment in the returned list.
--
-- The stop condition wins from the filtering predicate: if the stop condition
-- is true for an element, but the filter predicate not, then the element
-- still ends up in final fragment.
--
-- For example, given the fragment containing @[1, 2, 3, 4, 5, 6]@:
--
-- > filter         odd        -> [[1], [3], [5]]
-- > filterWithStop odd (>= 4) -> [[1], [3], [4, 5, 6]]
filterWithStop ::
     forall block. HasHeader block
  => (block -> Bool)  -- ^ Filtering predicate
  -> (block -> Bool)  -- ^ Stop condition
  -> AnchoredFragment block
  -> [AnchoredFragment block]
filterWithStop p stop c =
    map (applyFilterRange c) $ startRange (zip [0..] (toOldestFirst c))
  where
    startRange :: [(Int, block)] -> [FilterRange]
    startRange [] = []
    startRange ((i, blk):blks)
        | stop blk
         -- We can stop filtering, the last range is from @blk@ to the end of the
         -- fragment.
        = [FilterRange i (length c - 1)]

        | p blk
          -- We can use @blk@ to start a range, try extending it with the next
          -- block
        = extendRange i i blks

        | otherwise
          -- Not part of a range, try the next block
        = startRange blks

    extendRange :: Int -> Int -> [(Int, block)] -> [FilterRange]
    extendRange !start !end [] = [FilterRange start end]
    extendRange !start !end ((i, blk):blks)
        | stop blk
          -- We can stop filtering, the last range is from @start@ to the end of the
          -- fragment.
        = [FilterRange start (length c - 1)]

        | p blk
          -- Extend the open range with @blk@
        = extendRange start i blks

        | otherwise
          -- End the open range and try starting another one
        = FilterRange start end : startRange blks

-- | Range with /inclusive/ bounds, i.e., indices, that should be included in
-- the result of a filtering operation.
--
-- INVARIANT: the first lower bound <= the upper bound
--
-- When used in combination with a fragment, both indices should be in the [0,
-- size of fragment) range.
data FilterRange = FilterRange !Int !Int
  deriving (Show)

-- | \( O(\log(\min(i,n-i)) \). Apply a 'FilterRange' to a fragment, returning
-- the fragment matching the range.
--
-- For example, @FilterRange 0 0@ correspond to the first element of the
-- fragment. @FilterRange 0 1@ corresponds to the first two elements of the
-- fragment.
--
-- Since both bounds are inclusive, the fragment is never empty.
--
-- PRECONDITION: both indices are in the @[0, size of fragment)@ range.
applyFilterRange ::
     forall block. HasHeader block
  => AnchoredFragment block
  -> FilterRange
  -> AnchoredFragment block
applyFilterRange c (FilterRange start stop) = inRange
  where
    (_before, fromStart) = splitAt start c
    (inRange, _after)    = splitAt (stop - start + 1) fromStart

-- | \( O\(n\) \). Naive reference implementation of 'filterWithStop'.
--
-- While the asymptotic complexity of this function is better than that of
-- 'filterWithStop', the allocation cost is high. This function deconstructs and
-- reconstructs the fragment (until the stop condition is reached), even when no
-- blocks are removed.
filterWithStopSpec ::
     forall block. HasHeader block
  => (block -> Bool)  -- ^ Filtering predicate
  -> (block -> Bool)  -- ^ Stop condition
  -> AnchoredFragment block
  -> [AnchoredFragment block]
filterWithStopSpec p stop = goNext []
  where
    goNext :: [AnchoredFragment block]  -- Previously constructed fragments
           -> AnchoredFragment block    -- Fragment still to process
           -> [AnchoredFragment block]
    goNext cs af = go cs (Empty (anchor af)) af

    go :: [AnchoredFragment block]  -- Previously constructed fragments
       -> AnchoredFragment block    -- Currently accumulating fragment
       -> AnchoredFragment block    -- Fragment still to process
       -> [AnchoredFragment block]
    go cs c' af@(b :< c) | stop b = reverse (addToAcc (join' c' af) cs)
                         | p    b = go cs (c' :> b) c
    go cs c' (_ :< c)             = goNext (addToAcc c' cs) c
    go cs c' (Empty _)            = reverse (addToAcc c' cs)

    addToAcc :: AnchoredFragment block
             -> [AnchoredFragment block]
             -> [AnchoredFragment block]
    addToAcc (Empty _) acc =    acc
    addToAcc c'        acc = c':acc

    -- This is called with @c'@ and @(b : < c)@. @c'@ is the fragment
    -- containing the blocks before @b@, so they must be joinable.
    join' :: AnchoredFragment block
          -> AnchoredFragment block
          -> AnchoredFragment block
    join' a b = fromMaybe (error "could not join fragments") $ join a b
