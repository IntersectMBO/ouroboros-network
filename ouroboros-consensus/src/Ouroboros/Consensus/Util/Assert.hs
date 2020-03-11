{-# LANGUAGE CPP              #-}
{-# LANGUAGE TypeApplications #-}
module Ouroboros.Consensus.Util.Assert
  ( assertWithMsg
  ) where

import           GHC.Stack (HasCallStack)

import           Ouroboros.Consensus.Util.RedundantConstraints

assertWithMsg :: HasCallStack => Either String () -> a -> a
#if ENABLE_ASSERTIONS
assertWithMsg (Left msg) _ = error msg
#endif
assertWithMsg _          a = a
  where
    _ = keepRedundantConstraint (Proxy @HasCallStack)
