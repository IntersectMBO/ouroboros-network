{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving#-}

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

import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

import Cardano.Slotting.Slot
import Data.Aeson

data Block slot hash = Block
  { blockPointSlot :: !slot
  , blockPointHash :: !hash
  }
  deriving (Eq, Ord, Show, Generic, NoThunks)

deriving instance (ToJSON slot, ToJSON hash) => ToJSON (Block slot hash)
deriving instance (FromJSON slot, FromJSON hash) => FromJSON (Block slot hash)

block :: slot -> hash -> WithOrigin (Block slot hash)
block slot hash = at (Block slot hash)
