module Ouroboros.Network.Point
  ( WithOrigin (..)
  , Block (..)
  , origin
  , at
  , block
  ) where

data WithOrigin t = Origin | At t
  deriving (Eq, Ord, Show)

instance Functor WithOrigin where
  fmap _ Origin    = Origin
  fmap f (At t) = At (f t)

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
