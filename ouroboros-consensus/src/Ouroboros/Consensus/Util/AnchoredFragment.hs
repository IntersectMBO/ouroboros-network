-- | Utility functions on anchored fragments
--
-- Intended for qualified import
-- > import qualified Ouroboros.Consensus.Util.AnchoredFragment as AF
module Ouroboros.Consensus.Util.AnchoredFragment (
    compareHeadBlockNo
  , forksAtMostKBlocks
  ) where

import           Prelude hiding (length)

import           Data.Function (on)
import           Data.Word (Word64)

import           Ouroboros.Network.AnchoredFragment

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
compareHeadBlockNo = compare `on` headBlockNo

forksAtMostKBlocks
  :: HasHeader b
  => Word64              -- ^ How many blocks can it fork?
  -> AnchoredFragment b  -- ^ Our chain.
  -> AnchoredFragment b  -- ^ Their chain
  -> Bool                -- ^ Indicates whether their chain forks at most the
                         -- specified number of blocks.
forksAtMostKBlocks k ours theirs = case ours `intersect` theirs of
    Nothing                   -> False
    Just (_, _, ourSuffix, _) -> fromIntegral (length ourSuffix) <= k
