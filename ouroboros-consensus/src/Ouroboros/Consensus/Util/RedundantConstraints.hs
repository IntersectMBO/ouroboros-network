{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Ouroboros.Consensus.Util.RedundantConstraints (
    keepRedundantConstraint
  ) where

-- | Can be used to silence individual "redundant constraint" warnings
--
-- > foo :: ConstraintUsefulForDebugging => ...
-- > foo =
-- >     ..
-- >   where
-- >     _ = keepRedundantConstraint (Proxy @ConstraintUsefulForDebugging))
keepRedundantConstraint :: c => proxy c -> ()
keepRedundantConstraint _ = ()
