{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Intended for qualified import
module Ouroboros.Consensus.Util.SlotBounded (
    -- * Bounds
    Bounds(..)
  , InBounds(..)
    -- * Slot-bounded values
  , SlotBounded(..)
  , bounds
  , bounded
  , maximal
  , at
  , contains
  ) where

import           Codec.Serialise (Serialise)
import           Data.Proxy
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.Block (SlotNo)
--import           Ouroboros.Network.Point (WithOrigin (..))

{-------------------------------------------------------------------------------
  Bounds
-------------------------------------------------------------------------------}

data Bounds =
    -- | Both bounds are inclusive
    II

    -- | Lower bound is inclusive, upper bound is exclusive
  | IX

class InBounds (bounds :: Bounds) where
  inBounds :: proxy bounds -> SlotNo -> (SlotNo, SlotNo) -> Bool

instance InBounds II where
  inBounds _ x (lo, hi) = lo <= x && x <= hi

instance InBounds IX where
  inBounds _ x (lo, hi) = lo <= x && x < hi

{-------------------------------------------------------------------------------
  Slot-bounded values
-------------------------------------------------------------------------------}

-- | An item bounded to be valid within particular slots
data SlotBounded (bounds :: Bounds) a = SlotBounded
  { sbLower   :: !SlotNo
  , sbUpper   :: !SlotNo
  , sbContent :: !a
  } deriving (Eq, Functor, Show, Generic, Serialise, NoUnexpectedThunks)

bounds :: SlotBounded bounds a -> (SlotNo, SlotNo)
bounds (SlotBounded lo hi _) = (lo, hi)

contains :: forall bounds a. InBounds bounds
         => SlotBounded bounds a -> SlotNo -> Bool
sb `contains` slot = inBounds (Proxy @bounds) slot (bounds sb)

-- | Construct a slot bounded item.
--
-- We choose not to validate that the slot bounds are reasonable here.
bounded :: SlotNo -> SlotNo -> a -> SlotBounded bounds a
bounded = SlotBounded

maximal :: a -> SlotBounded bounds a
maximal = SlotBounded minBound maxBound

at :: InBounds bounds => SlotBounded bounds a -> SlotNo -> Maybe a
sb `at` slot =
    if sb `contains` slot
      then Just $ sbContent sb
      else Nothing
