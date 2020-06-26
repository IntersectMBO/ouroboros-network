{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Intended for qualified import
--
-- > import Ouroboros.Consensus.Fragment.Diff (ChainDiff (..))
-- > import qualified Ouroboros.Consensus.Fragment.Diff as Diff
module Ouroboros.Consensus.Fragment.Diff
  ( ChainDiff(ChainDiff)
    -- * Queries
  , getRollback
  , getSuffix
  , getTip
  , getAnchorPoint
    -- * Constructors
  , extend
  , diff
    -- * Application
  , apply
    -- * Manipulation
  , truncate
  , takeWhileOldest
  ) where

import           Prelude hiding (truncate)

import           Data.Word (Word64)
import           GHC.Stack (HasCallStack)

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment (..))
import qualified Ouroboros.Network.AnchoredFragment as AF

import           Ouroboros.Consensus.Block


-- | A diff of a chain (fragment).
--
-- INVARIANT: the length of the suffix must always be >= the rollback
--
-- Note: we allow the suffix with new headers to be empty, even though it is
-- rather pointless. Allowing empty ones makes working with them easier: fewer
-- cases to deal with. Without any headers, the rollback must be 0, so such a
-- diff would be an empty diff.
data ChainDiff blk = UnsafeChainDiff
    { getRollback :: !Word64
      -- ^ The number of headers to roll back the current chain
    , getSuffix   :: !(AnchoredFragment (Header blk))
      -- ^ The new headers to add after rolling back the current chain.
    }

deriving instance (HasHeader blk, Eq (Header blk))
               => Eq   (ChainDiff blk)
deriving instance (HasHeader blk, Show (Header blk))
               => Show (ChainDiff blk)

-- | Allow for pattern matching on a 'ChainDiff' without exposing the (unsafe)
-- constructor. Use 'extend' and 'diff' to construct a 'ChainDiff'.
pattern ChainDiff
  :: Word64 -> AnchoredFragment (Header blk) -> ChainDiff blk
pattern ChainDiff r s <- UnsafeChainDiff r s
{-# COMPLETE ChainDiff #-}

-- | Internal. Return 'Nothing' if the length of the suffix < the rollback.
mkRollback
  :: HasHeader (Header blk)
  => Word64
  -> AnchoredFragment (Header blk)
  -> Maybe (ChainDiff blk)
mkRollback nbRollback suffix
    | fromIntegral (AF.length suffix) >= nbRollback
    = Just $ UnsafeChainDiff nbRollback suffix
    | otherwise
    = Nothing

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

-- | Return the tip of the new suffix
getTip :: HasHeader (Header blk) => ChainDiff blk -> Point blk
getTip = castPoint . AF.headPoint . getSuffix

-- | Return the anchor point of the new suffix
getAnchorPoint :: ChainDiff blk -> Point blk
getAnchorPoint = castPoint . AF.anchorPoint . getSuffix

{-------------------------------------------------------------------------------
  Constructors
-------------------------------------------------------------------------------}

-- | Make an extension-only (no rollback) 'ChainDiff'.
extend :: AnchoredFragment (Header blk) -> ChainDiff blk
extend = UnsafeChainDiff 0

-- | Diff a candidate chain with the current chain.
--
-- If the candidate fragment is shorter than the current chain, 'Nothing' is
-- returned (this would violate the invariant of 'ChainDiff').
--
-- PRECONDITION: the candidate fragment must intersect with the current chain
-- fragment.
diff
  :: (HasHeader (Header blk), HasCallStack)
  => AnchoredFragment (Header blk)  -- ^ Current chain
  -> AnchoredFragment (Header blk)  -- ^ Candidate chain
  -> Maybe (ChainDiff blk)
diff curChain candChain =
  case AF.intersect curChain candChain of
    Just (_curChainPrefix, _candPrefix, curChainSuffix, candSuffix)
      -> mkRollback
           (fromIntegral (AF.length curChainSuffix))
           candSuffix
    -- Precondition violated.
    _ -> error "candidate fragment doesn't intersect with current chain"

{-------------------------------------------------------------------------------
  Application
-------------------------------------------------------------------------------}

-- | Apply the 'ChainDiff' on the given chain fragment.
--
-- The fragment is first rolled back a number of blocks before appending the
-- new suffix.
--
-- If the 'ChainDiff' doesn't fit (anchor point mismatch), 'Nothing' is
-- returned.
--
-- The returned fragment will have the same anchor point as the given
-- fragment.
apply
  :: HasHeader (Header blk)
  => AnchoredFragment (Header blk)
  -> ChainDiff blk
  -> Maybe (AnchoredFragment (Header blk))
apply curChain (ChainDiff nbRollback suffix) =
    AF.join (AF.dropNewest (fromIntegral nbRollback) curChain) suffix

{-------------------------------------------------------------------------------
  Manipulation
-------------------------------------------------------------------------------}

-- | Truncate the diff by rolling back the new suffix to the given point.
--
-- PRECONDITION: the given point must correspond to one of the new headers of
-- the new suffix or its anchor (i.e, @'AF.withinFragmentBounds' pt (getSuffix
-- diff)@).
--
-- If the length of the truncated suffix is shorter than the rollback,
-- 'Nothing' is returned.
truncate
  :: (HasHeader (Header blk), HasCallStack, HasHeader blk)
  => Point blk
  -> ChainDiff blk
  -> Maybe (ChainDiff blk)
truncate pt (ChainDiff nbRollback suffix)
    | Just suffix' <- AF.rollback (castPoint pt) suffix
    = mkRollback nbRollback suffix'
    | otherwise
    = error $ "rollback point not on the candidate suffix: " <> show pt

-- | Return the longest prefix of the suffix matching the given predicate,
-- starting from the left, i.e., the \"oldest\" blocks.
--
-- If the new suffix is shorter than the diff's rollback, return 'Nothing'.
takeWhileOldest
  :: HasHeader (Header blk)
  => (Header blk -> Bool)
  -> ChainDiff blk
  -> Maybe (ChainDiff blk)
takeWhileOldest accept (ChainDiff nbRollback suffix) =
    mkRollback nbRollback (AF.takeWhileOldest accept suffix)
