{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}

module Ouroboros.Network.Point
  ( WithOrigin (..)
  , Block (..)
  , origin
  , at
  , block
  ) where

import           GHC.Generics (Generic)

data WithOrigin t = Origin | At !t
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

data Block slot hash = Block
  { blockPointSlot :: !slot
  , blockPointHash :: !hash
  }
  deriving (Eq, Ord, Show)

at :: t -> WithOrigin t
at = At

origin :: WithOrigin t
origin = Origin

block :: slot -> hash -> WithOrigin (Block slot hash)
block slot hash = at (Block slot hash)
