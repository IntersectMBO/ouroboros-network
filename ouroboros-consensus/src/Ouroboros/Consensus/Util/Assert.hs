{-# LANGUAGE CPP              #-}
{-# LANGUAGE TypeApplications #-}
module Ouroboros.Consensus.Util.Assert (
    assertEqWithMsg
  , assertWithMsg
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

assertEqWithMsg :: (Eq b, Show b, HasCallStack) => (b, b) -> a -> a
assertEqWithMsg (x, y) = assertWithMsg msg
  where
    msg :: Either String ()
    msg | x == y    = Right ()
        | otherwise = Left $ show x ++ " /= " ++ show y
