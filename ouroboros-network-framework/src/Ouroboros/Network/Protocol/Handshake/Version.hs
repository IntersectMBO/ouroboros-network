{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module Ouroboros.Network.Protocol.Handshake.Version
  ( Versions (..)
  , Application (..)
  , Version (..)
  , Sigma (..)
  , Accept (..)
  , Acceptable (..)
  , Dict (..)
  , DictVersion (..)
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
import           Data.Typeable (Typeable)
import           GHC.Stack (HasCallStack)

import           Ouroboros.Network.CodecCBORTerm


-- Description of versions.
--
-- - Each particular version is a function from a pair of version data
--   (peculiar to that version, existentially-quantified) to some result
--   type (called `r` in this module).
-- - A version-numbering scheme, and the set of supported versions, is
--   defined simultaneously by a `Map` keyed on something, perhaps `Word32` in
--   a real-world instance. The `Sigma` GADT pairs the particular version data
--   with each version definition.

-- | The set of versions supported by the local agent are described by a map
-- keyed on the version identifier.
--
-- If one needs to combine multiple versions the simplest way is to use
-- one of the combinators: 'foldMapVersions', 'combineVersions' or the
-- 'Semigroup' instance directly:
--
-- >
-- > fold $ (simpleSingletonVersions ...)
-- >       :| [ (simpleSingletonVersions ...)
-- >          , (simpleSingletonVersions ...)
-- >          , ...
-- >          ]
-- >
--
newtype Versions vNum extra r = Versions
  { getVersions :: Map vNum (Sigma (Version extra r))
  }
  deriving (Semigroup)

instance Functor (Versions vNum extra) where
    fmap f (Versions vs) = Versions $ Map.map fmapSigma vs
      where
        fmapSigma (Sigma t (Version (Application app) extra)) = Sigma t (Version (Application $ \x -> f (app x)) extra)

data Sigma f where
  Sigma :: !t -> !(f t) -> Sigma f


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

-- | Takes a pair of version data: local then remote.
newtype Application r vData = Application
  { runApplication :: vData -> r
  }

data Version extra r vData = Version
  { versionApplication :: Application r vData
  , versionExtra       :: extra vData
  }

data VersionMismatch vNum where
  NoCommonVersion     :: VersionMismatch vNum
  InconsistentVersion :: vNum -> VersionMismatch vNum

data Dict constraint thing where
  Dict :: constraint thing => Dict constraint thing

-- | 'DictVersion' is used to instantiate the 'extra' param of 'Version'.
-- 'hanshakeParams' is instatiated in either "Ouroboros.Network.NodeToNode" or
-- "Ouroboros.Network.NodeToClient" to 'HandshakeParams'.
--
data DictVersion vNumber agreedOptions vData where
     DictVersion :: ( Typeable vData
                    , Acceptable vData
                    , Show vData
                    )
                 => CodecCBORTerm Text vData
                 -> (vNumber -> vData -> agreedOptions)
                 -- ^ agreed vData
                 -> DictVersion vNumber agreedOptions vData

--
-- Simple version negotation
--

-- | Singleton smart constructor for 'Versions'.
--
simpleSingletonVersions
  :: vNum
  -> vData
  -> extra vData
  -> r
  -> Versions vNum extra r
simpleSingletonVersions vNum vData extra r =
  Versions
    $ Map.singleton vNum
        (Sigma vData (Version (Application $ \_ -> r) extra))
