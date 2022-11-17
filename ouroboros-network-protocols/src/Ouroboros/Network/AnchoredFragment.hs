{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
module Ouroboros.Network.AnchoredFragment
  ( -- * AnchoredFragment type and fundamental operations
    AnchoredFragment
  , AnchoredSeq (Empty, (:>), (:<))
  , anchor
  , anchorPoint
  , anchorBlockNo
    -- * Anchor
  , Anchor (..)
  , anchorFromBlock
  , anchorFromPoint
  , anchorToPoint
  , anchorToSlotNo
  , anchorToBlockNo
  , anchorToHash
  , anchorIsGenesis
  , anchorToHeaderFields
  , anchorToTip
  , castAnchor
  , valid
  , validExtension
    -- ** Block re-exports
  , HasHeader (..)
  , Point (..)
  , castPoint
  , blockPoint
    -- * AnchoredFragment construction and inspection
    -- ** Head inspection
  , headPoint
  , headAnchor
  , headSlot
  , headHash
  , headBlockNo
    -- ** Basic operations
  , head
  , last
  , lastPoint
  , lastSlot
  , toNewestFirst
  , toOldestFirst
  , fromNewestFirst
  , fromOldestFirst
  , splitAt
  , dropNewest
  , takeOldest
  , dropWhileNewest
  , takeWhileOldest
  , length
  , null
    -- ** Update type and operations
  , ChainUpdate (..)
  , addBlock
  , rollback
  , applyChainUpdate
  , applyChainUpdates
    -- * Special operations
  , pointOnFragment
  , withinFragmentBounds
  , findFirstPoint
  , successorBlock
  , selectPoints
  , isPrefixOf
  , splitAfterPoint
  , splitBeforePoint
  , sliceRange
  , join
  , intersect
  , intersectionPoint
  , mapAnchoredFragment
  , anchorNewest
  , filter
  , filterWithStop
    -- * Helper functions
  , prettyPrint
    -- * Reference implementations for testing
  , pointOnFragmentSpec
  , selectPointsSpec
  , filterWithStopSpec
  ) where

import           Prelude hiding (filter, head, last, length, map, null, splitAt)

import           Data.Either (isRight)
import qualified Data.List as L
import           GHC.Generics (Generic)
import           GHC.Stack
import           NoThunks.Class (NoThunks)

import           Ouroboros.Network.AnchoredSeq hiding (join, prettyPrint,
                     rollback)
import qualified Ouroboros.Network.AnchoredSeq as AS
import           Ouroboros.Network.Block
import           Ouroboros.Network.Point (WithOrigin (At, Origin), withOrigin)

{-------------------------------------------------------------------------------
  Anchor
-------------------------------------------------------------------------------}

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
    -- Note that we don't use 'HeaderFields' here because that is a view of a
    -- header with lazy fields and thus unfit for long-term in-memory storage.
    --
    -- Moreover, we don't reuse the 'Tip' type, because that type is sent across
    -- the network, while this type is not. This means we can freely change this
    -- type to suit our needs without worrying about binary compatibility.
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

{-------------------------------------------------------------------------------
  AnchoredFragment
-------------------------------------------------------------------------------}

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
-- An 'AnchoredFragment' anchored at Genesis can thus be converted to a
-- 'Ouroboros.Network.MockChain.Chain' ('fromAnchoredFragment'), containing all
-- blocks starting from Genesis.
--
-- Without an anchor point, an empty fragment wouldn't give us much more
-- information: is it empty because the whole chain is empty? Or, did we just
-- get an empty fragment that was split off from some later part of the chain?
type AnchoredFragment block = AnchoredSeq (WithOrigin SlotNo) (Anchor block) block

instance HasHeader block
      => Anchorable (WithOrigin SlotNo) (Anchor block) block where
  asAnchor = anchorFromBlock
  getAnchorMeasure _ = anchorToSlotNo

-- | Return the 'Point' corresponding to the anchor.
anchorPoint :: AnchoredFragment block -> Point block
anchorPoint = anchorToPoint . anchor

-- | Return the 'BlocKno' corresponding to the anchor.
anchorBlockNo :: AnchoredFragment block -> WithOrigin BlockNo
anchorBlockNo = anchorToBlockNo . anchor

prettyPrint ::
     String
  -> (Point block -> String)
  -> (block -> String)
  -> AnchoredFragment block
  -> String
prettyPrint nl ppPoint = AS.prettyPrint nl (ppPoint . anchorToPoint)

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

-- | \( O(1) \). When the fragment is empty, the anchor point is returned.
headPoint :: HasHeader block => AnchoredFragment block -> Point block
headPoint = anchorToPoint . headAnchor

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

-- | \( O(1) \). When the fragment is empty, the anchor point is returned.
lastPoint :: HasHeader block => AnchoredFragment block -> Point block
lastPoint = either anchorToPoint blockPoint . last

-- | \( O(1) \). When the fragment is empty, the slot of the anchor point is
-- returned, which may be the origin and therefore have no slot.
lastSlot :: HasHeader block => AnchoredFragment block -> WithOrigin SlotNo
lastSlot = either anchorToSlotNo (At . blockSlot) . last

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
rollback p = AS.rollback (pointSlot p) ((== p) . either anchorToPoint blockPoint)

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
selectPoints offsets =
    fmap (either anchorToPoint blockPoint) . AS.selectOffsets offsets

-- | \( O(o * n) \). Specification of 'selectPoints'.
--
-- Use 'selectPoints', as it should be faster.
--
-- This function is used to verify whether 'selectPoints' behaves as expected.
selectPointsSpec :: HasHeader block
                => [Int] -> AnchoredFragment block -> [Point block]
selectPointsSpec offsets c =
    [ ps !! offset
    | let ps = (blockPoint <$> toNewestFirst c) <> [anchorPoint c]
          len = L.length ps
    , offset <- offsets
    , offset < len
    ]

-- | \( O(\log(\min(i,n-i)) \). Find the block after the given point. If the
-- given point is the anchor point, then the first block is returned (if there
-- is one).
successorBlock :: HasHeader block
               => Point block -> AnchoredFragment block -> Maybe block
successorBlock p af
    | p == anchorPoint af
    = either (const Nothing) Just $ last af
    | otherwise
    = case splitAfterPoint af p of
        Just (_, b :< _) -> Just b
        _otherwise       -> Nothing

-- | \( O(\log(\min(i,n-i)) \). Does the fragment contain a block with the given
-- point? The anchor point is ignored.
pointOnFragment :: HasHeader block
                => Point block -> AnchoredFragment block -> Bool
pointOnFragment p = contains (pointSlot p) ((== p) . blockPoint)

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
withinFragmentBounds p =
    withinBounds
      (pointSlot p)
      ((== p) . either anchorToPoint blockPoint)

-- | \( O(p \log(\min(i,n-i)) \). Find the first 'Point' in the list of points
-- that is within the fragment bounds. Return 'Nothing' if none of them are.
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
splitAfterPoint af p =
    splitAfterMeasure
      (pointSlot p)
      ((== castPoint p) . either anchorToPoint blockPoint)
      af

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
splitBeforePoint af p =
    splitBeforeMeasure
      (pointSlot p)
      ((== castPoint p) . blockPoint)
      af

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
join = AS.join $ \aOrB a ->
    either anchorToPoint blockPoint aOrB == anchorToPoint a

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

-- | \( O(n) \). Maps over the chain's blocks. This is not allowed to change the
-- block 'Point's, or it would create an invalid chain. The 'anchorPoint' is not
-- affected.
--
mapAnchoredFragment ::
     (HasHeader block2, HeaderHash block1 ~ HeaderHash block2)
  => (block1 -> block2)
  -> AnchoredFragment block1
  -> AnchoredFragment block2
mapAnchoredFragment = bimap castAnchor
