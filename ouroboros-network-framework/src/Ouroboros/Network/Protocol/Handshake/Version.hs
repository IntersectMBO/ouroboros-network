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
  , pickVersions
  , VersionMismatch (..)

  -- * Simple or no versioning
  , simpleSingletonVersions
  , foldMapVersions
  , combineVersions
  , mapWithVersion
  ) where

import           Data.Foldable (toList)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import           Data.Typeable ((:~:) (Refl), Typeable, eqT)
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
        fmapSigma (Sigma t (Version (Application app) extra)) = Sigma t (Version (Application $ \x y -> f (app x y)) extra)


mapWithVersion
    :: forall vNum extra a b.
       (vNum -> a -> b)
    -> Versions vNum extra a
    -> Versions vNum extra b
mapWithVersion f (Versions vs) = Versions $ Map.mapWithKey g vs
  where
    g :: vNum -> Sigma (Version extra a) -> Sigma (Version extra b)
    g vNum (Sigma vData (Version (Application app) extra)) =
      Sigma vData
            (Version
              (Application $ \vData' vData'' -> f vNum (app vData' vData''))
              extra)


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
data Accept
  = Accept
  | Refuse !Text
  deriving (Eq, Show)

class Acceptable v where
  acceptableVersion :: v -> v -> Accept

-- | Takes a pair of version data: local then remote.
newtype Application r vData = Application
  { runApplication :: vData -> vData -> r
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

data DictVersion vData where
     DictVersion :: ( Typeable vData
                    , Acceptable vData
                    , Show vData
                    )
                 => CodecCBORTerm Text vData
                 -> DictVersion vData

-- | Pick the version with the highest version number (by `Ord vNum`) common
-- in both maps.
--
-- This is a useful guide for comparison with a version negotiation scheme for
-- use in production between different processes. If the `Versions` maps
-- used by each process are given to `pickVersions`, it should come up with
-- the same result as the production version negotiation.
--
-- It is _assumed_ that if the maps agree on a key, then the existential
-- types in the `Sigma` value at the key are also equal.
--
-- So, the issue here is that they may not have the same version data type.
-- This becomes a non-issue on the network because the decoder/encoder
-- basically fills the role of a safe dynamic type cast.
pickVersions
  :: ( Ord vNum )
  => (forall vData . extra vData -> Dict Typeable vData)
  -> Versions vNum extra r
  -> Versions vNum extra r
  -> Either (VersionMismatch vNum) (r, r)
pickVersions isTypeable lversions rversions = case Map.toDescList commonVersions of
  [] -> Left NoCommonVersion
  (vNum, (Sigma (ldata :: ldata) lversion, Sigma (rdata :: rdata) rversion)) : _ ->
    case (isTypeable (versionExtra lversion), isTypeable (versionExtra rversion)) of
      (Dict, Dict) -> case eqT :: Maybe (ldata :~: rdata) of
        Nothing   -> Left $ InconsistentVersion vNum
        Just Refl ->
          let lapp = versionApplication lversion
              rapp = versionApplication rversion
          in  Right (runApplication lapp ldata rdata, runApplication rapp rdata rdata)
  where
  commonVersions = getVersions lversions `intersect` getVersions rversions
  intersect = Map.intersectionWith (,)

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
        (Sigma vData (Version (Application $ \_ _ -> r) extra))
