{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Ouroboros.Consensus.Util.RedundantConstraints (
    keepRedundantConstraint
    -- * Convenience re-export
  , Proxy (..)
  ) where

import           Data.Proxy

-- | Can be used to silence individual "redundant constraint" warnings
--
-- > foo :: ConstraintUsefulForDebugging => ...
-- > foo =
-- >     ..
-- >   where
-- >     _ = keepRedundantConstraint (Proxy @ConstraintUsefulForDebugging))
keepRedundantConstraint :: c => proxy c -> ()
keepRedundantConstraint _ = ()
