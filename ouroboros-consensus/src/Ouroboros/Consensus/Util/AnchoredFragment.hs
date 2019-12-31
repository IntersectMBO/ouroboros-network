{-# LANGUAGE ScopedTypeVariables #-}

-- | Utility functions on anchored fragments
--
-- Intended for qualified import
-- > import qualified Ouroboros.Consensus.Util.AnchoredFragment as AF
module Ouroboros.Consensus.Util.AnchoredFragment (
    compareHeadBlockNo
  , forksAtMostKBlocks
  , preferAnchoredCandidate
  , compareAnchoredCandidates
  ) where

import           Data.Function (on)
import           Data.Word (Word64)
import           GHC.Stack

import           Ouroboros.Network.AnchoredFragment
                     (AnchoredFragment ((:>), Empty))
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (HasHeader, blockPoint)

import           Ouroboros.Consensus.Protocol.Abstract

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

-- | Lift 'preferCandidate' to 'AnchoredFragment'
--
-- PRECONDITION: The candidate must intersect with our chain within @k@ blocks
-- from our tip.
--
-- NOTE: In the discussion below we assume that the anchored fragments are
-- /suffixes/ of their chains.
--
-- A non-empty chain is always preferred over an empty one (see discussion for
-- 'preferCandidate'), but that does not necessarily mean that a non-empty
-- chain /fragment/ is necessary preferred over an empty one. After all, chain
-- fragments are suffixes of chains, and so in principle it's possible that we
-- might prefer the empty suffix of a longer chain over the non-empty suffix of
-- a shorter one.
--
-- We can distinguish between an empty fragment of a non-empty chain and a
-- (necessarily) empty fragment of an empty chain by looking at the anchor
-- point: if that is the genesis point, the chain must be empty. For emphasis,
-- we will refer to these chains/fragments as "genuinely empty".
--
-- Our own fragment will be empty in two cases only:
--
-- * It is genuinely empty. In this case, we prefer the candidate always,
--   unless it too is genuinely empty.
-- * We suffered from data loss, to such an extent that the volatile DB does
--   not contain any blocks anymore that fit onto our immutable chain.
--   This case will require more careful consideration.
--
-- Unfortunately, the candidate fragment can basically be empty at any point
-- due to the way that a switch-to-fork is implemented in terms of rollback
-- followed by roll forward; after a maximum rollback (and before the roll
-- forward), the candidate fragment is empty. (Note that a genuinely empty
-- fragment is /never/ preferred over our chain.)
--
-- We therefore have the following cases to consider:
--
-- 1. Both fragments are empty.
--
--    Since the two fragments must intersect, that intersection point can only
--    be the two anchor points, which must therefore be equal. This means that
--    two fragments represent the same chain: the candidate is not preferred.
--
-- 2. Our fragment is non-empty, the candidate fragment is empty.
--
--    Since the two fragments must intersect, that intersection can only be the
--    anchor of the candidate. That intersection point can lie anywhere on our
--    fragment.
--
--    a. If it lies at the tip of our fragment, the two fragments represent the
--       same chain.
--    b. If it lies before the tip of our fragment, the candidate chain is a
--       strict prefix of our chain.
--
--    In either case the candidate is not preferred.
--
-- 3. Our fragment is empty, the candidate fragment is non-empty.
--
--    Since the two fragments must intersect, that intersection can only be the
--    anchor of our fragment. Moreover, since we have ruled out the case that
--    our chain is genuinely empty, that anchor must be the tip of the immutable
--    DB. In other words, the tip of the immutable DB must lie somewhere on the
--    candidate chain.
--
--    a. If that is also the tip of the candidate fragment, the two fragments
--       represent the same chain, and the candidate is not preferred.
--    b. If it is /not/ the tip, the candidate is a strict extension of our
--       chain and is therefore preferred.
--
-- 4. Neither fragment is empty. In this case, we can simply call
--    'preferCandidate' on the two heads.
--
-- The case distinction between (3a) and (3b) could be avoided if we can
-- assume that the candidate fragment is anchored at our own anchor point
-- (which is guaranteed by trimming; see the ChainSync.md spec). In this case,
-- case (3a) becomes impossible. We /could/ require this as a stronger
-- precondition, at the cost of potentially requiring the caller to trim every
-- time before calling 'preferAnchoredCandidate'. We choose not to do this:
-- trimming in most cases is unnecessary (it is only required if our own
-- fragment is empty, which will be very rare indeed); moreover, the work we
-- need to do here to distinguish between (3a) and (3b) is the same amount of
-- work that a trimming would need to do (but we only need to do it rarely).
--
-- It is worth emphasizing that the above means that in the case that our
-- fragment or the candidate fragment is empty, we can decide whether or not
-- the candidate is preferred using only the "always extend" rule: we never
-- need the header that corresponds to the anchor point.
preferAnchoredCandidate :: forall p hdr. (
                             OuroborosTag p
                           , CanSelect p hdr
                           , HasHeader hdr
                           )
                        => NodeConfig p
                        -> AnchoredFragment hdr      -- ^ Our chain
                        -> AnchoredFragment hdr      -- ^ Candidate
                        -> Bool
preferAnchoredCandidate cfg ours theirs =
    case (ours, theirs) of
      (_, Empty _) ->
        -- Case 1 or 2
        False
      (Empty ourAnchor, _ :> theirTip) ->
        -- Case 3
        blockPoint theirTip /= ourAnchor
      (_ :> ourTip, _ :> theirTip) ->
        -- Case 4
        preferCandidate cfg ourTip theirTip

-- | Lift 'compareCandidates' to 'AnchoredFragment'
--
-- PRECONDITION: Both candidates must be preferred to our chain.
--
-- Implementation note: since the empty fragment is never preferred over our
-- chain, this is trivial. See discussion in 'preferAnchoredCandidate' for
-- details.
compareAnchoredCandidates :: ( OuroborosTag p
                             , CanSelect p hdr
                             , HasHeader hdr
                             , HasCallStack
                             )
                          => NodeConfig p
                          -> AnchoredFragment hdr
                          -> AnchoredFragment hdr
                          -> Ordering
compareAnchoredCandidates cfg ours theirs =
    case (ours, theirs) of
      (_ :> ourTip, _ :> theirTip) ->
        compareCandidates cfg ourTip theirTip
      _otherwise ->
        error "compareAnchoredCandidates: precondition violated"
