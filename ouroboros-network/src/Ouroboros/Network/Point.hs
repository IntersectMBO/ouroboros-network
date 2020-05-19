{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia       #-}

module Ouroboros.Network.Point
  ( WithOrigin (..)
  , Block (..)
  , origin
  , at
  , block
  , fromWithOrigin
  , withOrigin
  , withOriginToMaybe
  , withOriginFromMaybe
  ) where

import           GHC.Generics (Generic)
import           Quiet (Quiet (..))

import           Cardano.Prelude (NoUnexpectedThunks)
import           Cardano.Slotting.Slot

data Block slot hash = Block
  { blockPointSlot :: !slot
  , blockPointHash :: !hash
  }
  deriving (Eq, Ord, Generic, NoUnexpectedThunks)
  deriving (Show) via (Quiet (Block slot hash))

block :: slot -> hash -> WithOrigin (Block slot hash)
block slot hash = at (Block slot hash)
