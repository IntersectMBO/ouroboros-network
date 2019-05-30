{-# LANGUAGE DeriveFunctor #-}

-- | Intended for qualified import
module Ouroboros.Consensus.Util.SlotBounded (
    SlotBounded(..)
  , bounded
  , unbounded
  , at
  ) where

import           Ouroboros.Network.Block (SlotNo)

-- | An item bounded to be valid within particular slots
data SlotBounded a = SlotBounded
  { sbLower   :: !SlotNo
  , sbUpper   :: !SlotNo
  , sbContent :: !a
  } deriving (Eq, Functor, Show)

-- | Construct a slot bounded item.
--
--   We choose not to validate that the slot bounds are reasonable here.
bounded :: SlotNo -> SlotNo -> a -> SlotBounded a
bounded = SlotBounded

unbounded :: a -> SlotBounded a
unbounded = SlotBounded minBound maxBound

at :: SlotBounded a -> SlotNo -> Maybe a
sb `at` slot =
  if (slot <= sbUpper sb && slot >= sbLower sb)
  then Just $ sbContent sb
  else Nothing
