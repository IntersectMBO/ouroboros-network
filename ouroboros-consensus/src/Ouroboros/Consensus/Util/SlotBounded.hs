{-# LANGUAGE DeriveFunctor #-}

-- | Intended for qualified import
module Ouroboros.Consensus.Util.SlotBounded (
    SlotBounded(..)
  , bounded
  , unbounded
  , at
  ) where

import           Ouroboros.Network.Block (SlotNo, TPoint (..))

-- | An item bounded to be valid within particular slots (inclusive)
data SlotBounded a = SlotBounded
  { sbLower   :: !(TPoint SlotNo)
  , sbUpper   :: !(TPoint SlotNo)
  , sbContent :: !a
  } deriving (Eq, Functor, Show)

-- | Construct a slot bounded item.
--
--   We choose not to validate that the slot bounds are reasonable here.
bounded :: SlotNo -> SlotNo -> a -> SlotBounded a
bounded l r = SlotBounded (Point l) (Point r)

unbounded :: a -> SlotBounded a
unbounded = SlotBounded Origin (Point maxBound)

at :: SlotBounded a -> TPoint SlotNo -> Maybe a
sb `at` slot =
  if (slot <= sbUpper sb && slot >= sbLower sb)
  then Just $ sbContent sb
  else Nothing
