{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Orphans.Arbitrary () where

import           Test.QuickCheck

import           Ouroboros.Consensus.Demo
import           Ouroboros.Consensus.Node


minNumCoreNodes, minNumSlots :: Int
minNumCoreNodes = 2
minNumSlots     = 1

instance Arbitrary NumCoreNodes where
  arbitrary = NumCoreNodes <$> choose (minNumCoreNodes, 5)
  shrink (NumCoreNodes n) = NumCoreNodes <$> (filter (>= minNumCoreNodes) $ shrink n)

-- TODO: We shouldn't really pick the number of slots independent from k
instance Arbitrary NumSlots where
  arbitrary = NumSlots <$> choose (minNumSlots, 100)
  shrink (NumSlots n) = NumSlots <$> (filter (>= minNumSlots) $ shrink n)
