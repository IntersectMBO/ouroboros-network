{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Utility functions on anchored fragments
--
-- Intended for qualified import
-- > import qualified Ouroboros.Consensus.Util.AnchoredFragment as AF
module Ouroboros.Consensus.Util.AnchoredFragment (
    compareAnchoredFragments
  , compareHeadBlockNo
  , forksAtMostKBlocks
  , preferAnchoredCandidate
  ) where

import           Control.Monad.Except (throwError)
import           Data.Function (on)
import           Data.Maybe (isJust)
import           Data.Word (Word64)
import           GHC.Stack

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment,
                     AnchoredSeq (Empty, (:>)))
import qualified Ouroboros.Network.AnchoredFragment as AF

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Util.Assert

{-------------------------------------------------------------------------------
  Utility functions on anchored fragments
-------------------------------------------------------------------------------}

-- | Compare the 'headBlockNo', which is a measure of the length of the chain,
-- of two anchored fragments.
--
-- A fragment with a head is always \"greater\" than one without. When both
-- fragments have no head (i.e. are empty), they are 'EQ'.
--
-- Note that an EBB can share its @BlockNo@ with another regular block. If
-- such an EBB is the head of one fragment and the regular block with the same
-- @BlockNo@ is the head of the other fragment, then this function will say
-- they are 'EQ', while in fact one fragment should be preferred over the
-- other.
--
-- This is not a big deal as we won't be seeing new EBBs, so they will not be
-- the head of a fragment very often anyway, only when catching up. As soon as
-- a new block/header is added to the fragment, the right decision will be
-- made again ('GT' or 'LT').
compareHeadBlockNo
  :: HasHeader b
  => AnchoredFragment b
  -> AnchoredFragment b
  -> Ordering
compareHeadBlockNo = compare `on` AF.headBlockNo

forksAtMostKBlocks
  :: HasHeader b
  => Word64              -- ^ How many blocks can it fork?
  -> AnchoredFragment b  -- ^ Our chain.
  -> AnchoredFragment b  -- ^ Their chain
  -> Bool                -- ^ Indicates whether their chain forks at most the
                         -- specified number of blocks.
forksAtMostKBlocks k ours theirs = case ours `AF.intersect` theirs of
    Nothing                   -> False
    Just (_, _, ourSuffix, _) -> fromIntegral (AF.length ourSuffix) <= k

-- | Lift 'compareChains' to 'AnchoredFragment'
--
-- PRECONDITION: Either both fragments are non-empty or they intersect.
--
-- For a detailed discussion of this precondition, and a justification for the
-- definition of this function, please refer to the Consensus Report.
--
-- Usage note: the primary user of this function is the chain database. It
-- establishes the precondition in two different ways:
--
-- * When comparing a candidate fragment to our current chain, the fragment is
--   guaranteed (by the chain sync client) to intersect with our chain (indeed,
--   within at  most @k@ blocks from our tp, although the exact distance does
--   not matter for 'compareAnchoredCandidates').
-- * It will only compare candidate fragments that it has previously verified
--   are preferable to our current chain. Since these fragments intersect with
--   our current chain, they must by transitivity also intersect each other.
compareAnchoredFragments ::
     forall blk. (BlockSupportsProtocol blk, HasCallStack)
  => BlockConfig blk
  -> AnchoredFragment (Header blk)
  -> AnchoredFragment (Header blk)
  -> Ordering
compareAnchoredFragments cfg frag1 frag2 =
    assertWithMsg precondition $
    case (frag1, frag2) of
      (Empty _, Empty _) ->
        -- The fragments intersect but are equal: their anchors must be equal,
        -- and hence the fragments represent the same chain. They are therefore
        -- equally preferable.
        EQ
      (Empty anchor, _ :> tip') ->
        -- Since the fragments intersect, but the first one is empty, its anchor
        -- must lie somewhere along the the second. If it is the tip, the two
        -- fragments represent the same chain and are equally preferable. If
        -- not, the second chain is a strict extension of the first and is
        -- therefore strictly preferable.
        if blockPoint tip' == AF.anchorToPoint anchor
          then EQ
          else LT
      (_ :> tip, Empty anchor') ->
        -- This case is symmetric to the previous
        if blockPoint tip == AF.anchorToPoint anchor'
          then EQ
          else GT
      (_ :> tip, _ :> tip') ->
        -- Case 4
        compare
          (selectView cfg tip)
          (selectView cfg tip')
  where
    precondition :: Either String ()
    precondition
      | not (AF.null frag1), not (AF.null frag2)
      = return ()
      | isJust (AF.intersectionPoint frag1 frag2)
      = return ()
      | otherwise
      = throwError
          "precondition violated: fragments both empty or don't intersect"

-- | Lift 'preferCandidate' to 'AnchoredFragment'
--
-- See discussion for 'compareAnchoredCandidates'.
preferAnchoredCandidate ::
     forall blk. (BlockSupportsProtocol blk, HasCallStack)
  => BlockConfig blk
  -> AnchoredFragment (Header blk)      -- ^ Our chain
  -> AnchoredFragment (Header blk)      -- ^ Candidate
  -> Bool
preferAnchoredCandidate cfg ours cand =
    compareAnchoredFragments cfg ours cand == LT
