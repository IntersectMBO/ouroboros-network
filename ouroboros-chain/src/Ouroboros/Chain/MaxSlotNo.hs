{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Ouroboros.Chain.MaxSlotNo (
    MaxSlotNo (..)
  , maxSlotNoFromMaybe
  , maxSlotNoToMaybe
  , maxSlotNoFromWithOrigin
  ) where

import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Cardano.Slotting.Slot (SlotNo, WithOrigin, withOriginToMaybe)

-- | The highest slot number seen.
data MaxSlotNo
  = NoMaxSlotNo
    -- ^ No block/header has been seen yet, so we don't have a highest slot
    -- number.
  | MaxSlotNo !SlotNo
    -- ^ The highest slot number seen.
  deriving (Eq, Show, Generic, NoThunks)

-- The derived instances would do the same, but for clarity, we write it out
-- explicitly.
instance Ord MaxSlotNo where
  compare NoMaxSlotNo       (MaxSlotNo _) = LT
  compare NoMaxSlotNo       NoMaxSlotNo   = EQ
  compare (MaxSlotNo _)  NoMaxSlotNo      = GT
  compare (MaxSlotNo s1) (MaxSlotNo s2)   = compare s1 s2

maxSlotNoFromMaybe :: Maybe SlotNo -> MaxSlotNo
maxSlotNoFromMaybe = maybe NoMaxSlotNo MaxSlotNo

maxSlotNoToMaybe :: MaxSlotNo -> Maybe SlotNo
maxSlotNoToMaybe NoMaxSlotNo   = Nothing
maxSlotNoToMaybe (MaxSlotNo s) = Just s

maxSlotNoFromWithOrigin :: WithOrigin SlotNo -> MaxSlotNo
maxSlotNoFromWithOrigin = maxSlotNoFromMaybe . withOriginToMaybe

instance Semigroup MaxSlotNo where
  (<>) = max

instance Monoid MaxSlotNo where
  mempty  = NoMaxSlotNo
  mappend = (<>)
