{-# LANGUAGE DeriveAnyClass    #-}
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
  , fromWithOrigin
  , withOrigin
  , withOriginToMaybe
  , withOriginFromMaybe
  ) where

import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Codec.Serialise (Serialise (..))

data WithOrigin t = Origin | At !t
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable,
            NoUnexpectedThunks, Serialise)

data Block slot hash = Block
  { blockPointSlot :: !slot
  , blockPointHash :: !hash
  }
  deriving (Eq, Ord, Show, Generic, NoUnexpectedThunks)

at :: t -> WithOrigin t
at = At

origin :: WithOrigin t
origin = Origin

block :: slot -> hash -> WithOrigin (Block slot hash)
block slot hash = at (Block slot hash)

fromWithOrigin :: t -> WithOrigin t -> t
fromWithOrigin t Origin = t
fromWithOrigin _ (At t) = t

withOrigin :: b -> (t -> b) -> WithOrigin t -> b
withOrigin a _ Origin = a
withOrigin _ f (At t) = f t

withOriginToMaybe :: WithOrigin t -> Maybe t
withOriginToMaybe Origin = Nothing
withOriginToMaybe (At t) = Just t

withOriginFromMaybe :: Maybe t -> WithOrigin t
withOriginFromMaybe Nothing  = Origin
withOriginFromMaybe (Just t) = At t
