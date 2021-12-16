{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module Ouroboros.Network.Protocol.Handshake.Version
  ( Versions (..)
  , Version (..)
  , Accept (..)
  , Acceptable (..)
  , VersionMismatch (..)
    -- * Simple or no versioning
  , simpleSingletonVersions
  , foldMapVersions
  , combineVersions
  ) where

import           Data.Foldable (toList)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import           GHC.Stack (HasCallStack)


-- | The version map supported by the local agent keyed on the version
-- identifier.
--
-- Each 'Version' contains a function which takes negotiated version data and
-- returns negotiated application (the 'r' type variable).
--
-- If one needs to combine multiple versions the simplest way is to use one of
-- the combinators: 'foldMapVersions', 'combineVersions' or the 'Semigroup'
-- instance directly:
--
-- >
-- > fold $ (simpleSingletonVersions ...)
-- >       :| [ (simpleSingletonVersions ...)
-- >          , (simpleSingletonVersions ...)
-- >          , ...
-- >          ]
-- >
--
newtype Versions vNum vData r = Versions
  { getVersions :: Map vNum (Version vData r)
  }
  deriving Semigroup

instance Functor (Versions vNum extra) where
    fmap f (Versions vs) = Versions $ Map.map (fmap f)  vs


-- | Useful for folding multiple 'Versions'.
--
-- A 'foldMap' restricted to the 'Versions' 'Semigroup'.
--
-- PRECONDITION: @f x@ is non-empty.
--
foldMapVersions :: (Ord vNum, Foldable f, HasCallStack)
                => (x -> Versions vNum extra r)
                -> f x
                -> Versions vNum extra r
foldMapVersions f fx = case toList fx of
    [] -> error "foldMapVersions: precondition violated"
    xs -> foldl1 (<>) (map f xs)

combineVersions :: (Ord vNum, Foldable f, HasCallStack)
                => f (Versions vNum extra r)
                -> Versions vNum extra r
combineVersions = foldMapVersions id

-- |
-- A @'Maybe'@ like type which better explains its purpose.
--
data Accept vData
  = Accept vData
  | Refuse !Text
  deriving (Eq, Show)

class Acceptable v where
  acceptableVersion :: v -> v -> Accept v


data Version vData r = Version
  { versionApplication :: vData -> r
  , versionData        :: vData
  }
  deriving Functor

data VersionMismatch vNum where
  NoCommonVersion     :: VersionMismatch vNum
  InconsistentVersion :: vNum -> VersionMismatch vNum

--
-- Simple version negotiation
--

-- | Singleton smart constructor for 'Versions'.
--
simpleSingletonVersions
  :: vNum
  -> vData
  -> r
  -> Versions vNum vData r
simpleSingletonVersions vNum vData r =
  Versions
    $ Map.singleton vNum
      (Version (\_ -> r) vData)
