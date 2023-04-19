{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Util.Slots (NumSlots (..)) where

import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import           Quiet (Quiet (..))
import qualified Test.QuickCheck as QC
import           Test.QuickCheck (Arbitrary (..))

-- | Number of slots
newtype NumSlots = NumSlots {unNumSlots :: Word64}
  deriving (Eq, Generic, NoThunks)
  deriving (Show) via (Quiet NumSlots)

{-------------------------------------------------------------------------------
  Arbitrary instances
-------------------------------------------------------------------------------}

-- TODO: We shouldn't really pick the number of slots independent from k
instance Arbitrary NumSlots where
  arbitrary = NumSlots <$> QC.choose (minNumSlots, 100)
  shrink (NumSlots n) = NumSlots <$> (filter (>= minNumSlots) $ shrink n)

minNumSlots :: Word64
minNumSlots = 1
