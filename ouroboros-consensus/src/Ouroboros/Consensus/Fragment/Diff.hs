{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Intended for qualified import
--
-- > import Ouroboros.Consensus.Fragment.Diff (ChainDiff (..))
-- > import qualified Ouroboros.Consensus.Fragment.Diff as Diff
module Ouroboros.Consensus.Fragment.Diff (
    ChainDiff (..)
    -- * Queries
  , getAnchorPoint
  , getTip
  , rollbackExceedsSuffix
    -- * Constructors
  , diff
  , extend
    -- * Application
  , apply
    -- * Manipulation
  , append
  , mapM
  , takeWhileOldest
  , truncate
  ) where

import           Prelude hiding (mapM, truncate)
import qualified Prelude

import           Data.Word (Word64)
import           GHC.Stack (HasCallStack)

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment,
                     AnchoredSeq (..))
import qualified Ouroboros.Network.AnchoredFragment as AF

import           Ouroboros.Consensus.Block


-- | A diff of a chain (fragment).
--
-- Typical instantiations of the type argument @b@: a block type @blk@,
-- @Header blk@, @HeaderFields@, ..., anything that supports 'HasHeader'.
--
-- Note: we allow the suffix to be shorter than the number of blocks to roll
-- back. In other words, applying a 'ChainDiff' can result in a chain shorter
-- than the chain to which the diff was applied.
data ChainDiff b = ChainDiff
    { getRollback :: !Word64
      -- ^ The number of blocks/headers to roll back the current chain
    , getSuffix   :: !(AnchoredFragment b)
      -- ^ The new blocks/headers to add after rolling back the current chain.
    }

deriving instance (StandardHash b, Eq   b) => Eq   (ChainDiff b)
deriving instance (StandardHash b, Show b) => Show (ChainDiff b)

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

-- | Return the tip of the new suffix
getTip :: HasHeader b => ChainDiff b -> Point b
getTip = castPoint . AF.headPoint . getSuffix

-- | Return the anchor point of the new suffix
getAnchorPoint :: ChainDiff b -> Point b
getAnchorPoint = castPoint . AF.anchorPoint . getSuffix

-- | Return 'True' iff applying the 'ChainDiff' to a chain @C@ will result in
-- a chain shorter than @C@, i.e., the number of blocks to roll back is
-- greater than the length of the new elements in the suffix to add.
rollbackExceedsSuffix :: HasHeader b => ChainDiff b -> Bool
rollbackExceedsSuffix (ChainDiff nbRollback suffix) =
    nbRollback > fromIntegral (AF.length suffix)

{-------------------------------------------------------------------------------
  Constructors
-------------------------------------------------------------------------------}

-- | Make an extension-only (no rollback) 'ChainDiff'.
extend :: AnchoredFragment b -> ChainDiff b
extend = ChainDiff 0

-- | Diff a candidate chain with the current chain.
--
-- If the candidate fragment is shorter than the current chain, 'Nothing' is
-- returned (this would violate the invariant of 'ChainDiff').
--
-- PRECONDITION: the candidate fragment must intersect with the current chain
-- fragment.
diff
  :: (HasHeader b, HasCallStack)
  => AnchoredFragment b  -- ^ Current chain
  -> AnchoredFragment b  -- ^ Candidate chain
  -> ChainDiff b
diff curChain candChain =
  case AF.intersect curChain candChain of
    Just (_curChainPrefix, _candPrefix, curChainSuffix, candSuffix)
      -> ChainDiff
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
  :: HasHeader b
  => AnchoredFragment b
  -> ChainDiff b
  -> Maybe (AnchoredFragment b)
apply curChain (ChainDiff nbRollback suffix) =
    AF.join (AF.dropNewest (fromIntegral nbRollback) curChain) suffix

{-------------------------------------------------------------------------------
  Manipulation
-------------------------------------------------------------------------------}

-- | Append a @b@ to a 'ChainDiff'.
--
-- PRECONDITION: it must fit onto the end of the suffix.
append :: HasHeader b => ChainDiff b -> b -> ChainDiff b
append (ChainDiff nbRollback suffix) b = (ChainDiff nbRollback (suffix :> b))

-- | Truncate the diff by rolling back the new suffix to the given point.
--
-- PRECONDITION: the given point must correspond to one of the new
-- blocks/headers of the new suffix or its anchor (i.e,
-- @'AF.withinFragmentBounds' pt (getSuffix diff)@).
--
-- If the length of the truncated suffix is shorter than the rollback,
-- 'Nothing' is returned.
truncate
  :: (HasHeader b, HasCallStack)
  => Point b
  -> ChainDiff b
  -> ChainDiff b
truncate pt (ChainDiff nbRollback suffix)
    | Just suffix' <- AF.rollback (castPoint pt) suffix
    = ChainDiff nbRollback suffix'
    | otherwise
    = error $ "rollback point not on the candidate suffix: " <> show pt

-- | Return the longest prefix of the suffix matching the given predicate,
-- starting from the left, i.e., the \"oldest\" blocks.
--
-- If the new suffix is shorter than the diff's rollback, return 'Nothing'.
takeWhileOldest
  :: HasHeader b
  => (b -> Bool)
  -> ChainDiff b
  -> ChainDiff b
takeWhileOldest accept (ChainDiff nbRollback suffix) =
    ChainDiff nbRollback (AF.takeWhileOldest accept suffix)

mapM
  :: forall a b m.
     ( HasHeader b
     , HeaderHash a ~ HeaderHash b
     , Monad m
     )
  => (a -> m b)
  -> ChainDiff a
  -> m (ChainDiff b)
mapM f (ChainDiff rollback suffix) =
       ChainDiff rollback
    .  AF.fromOldestFirst (AF.castAnchor (AF.anchor suffix))
   <$> Prelude.mapM f (AF.toOldestFirst suffix)
