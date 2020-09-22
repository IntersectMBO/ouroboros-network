{-# LANGUAGE ScopedTypeVariables #-}

module Test.Ouroboros.Network.Utils where

import           Control.Monad.Class.MonadTime
import           Test.QuickCheck


newtype Delay = Delay { getDelay :: DiffTime }
  deriving Show


-- | This needs to be small, as we are using real time limits in block-fetch
-- examples.
--
instance Arbitrary Delay where
    arbitrary = do
      Positive (delay :: Float) <- resize 5 arbitrary
      return (Delay (realToFrac delay))
    shrink (Delay delay) | delay >= 0.1 = [ Delay (delay - 0.1) ]
                         | otherwise    = []

