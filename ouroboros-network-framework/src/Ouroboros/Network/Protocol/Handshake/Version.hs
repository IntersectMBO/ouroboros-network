{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module Ouroboros.Network.Protocol.Handshake.Version
  ( Versions (..)
  --, VersionsX (..)
  , Application (..)
  --, ApplicationX (..)
  , Version (..)
  --, VersionX (..)
  , Sigma (..)
  , Accept (..)
  --, AcceptX (..)
  , Acceptable (..)
  -- , Dict (..)
  , DictVersion (..)
  -- , pickVersions
  , VersionMismatch (..)

  --, pickVersionsX
  --, VersionsX (..)
  --, VersionX (..)
  --, simpleSingletonVersionsX

  -- * Simple or no versioning
  , simpleSingletonVersions
  , foldMapVersions
  , combineVersions
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

newtype Versions vNum extra r vData agreedOptions = Versions {
    getVersions :: Map vNum (Version extra r vData agreedOptions)
  }
  deriving (Semigroup)

newtype VersionsZ vNum extra r vData = VersionsZ
  -- { getVersions :: Map vNum (Sigma (Version extra r))
  {  getVersionsZ :: Map vNum (VersionZ extra r vData)
  }
  deriving (Semigroup)

{-instance Functor (Versions vNum extra r) where
    fmap f (Versions vs) = Versions $ Map.map fmapSigma vs
      where
        fmapSigma (Sigma t (Version (Application app) extra)) = Sigma t (Version (Application $ \x y -> f (app x y)) extra)-}

data Sigma f where
  Sigma :: !t -> !(f t) -> Sigma f


-- | Useful for folding multiple 'Versions'.
--
-- A 'foldMap' restricted to the 'Versions' 'Semigroup'.
--
-- PRECONDITION: @f x@ is non-empty.
--
foldMapVersions :: (Ord vNum, Foldable f, HasCallStack)
                => (x -> Versions vNum extra r vData agreedOptions)
                -> f x
                -> Versions vNum extra r vData agreedOptions
foldMapVersions f fx = case toList fx of
    [] -> error "foldMapVersions: precondition violated"
    xs -> foldl1 (<>) (map f xs)

combineVersions :: (Ord vNum, Foldable f, HasCallStack)
                => f (Versions vNum extra r vData agreedOptions)
                -> Versions vNum extra r vData agreedOptions
combineVersions = foldMapVersions id

-- |
-- A @'Maybe'@ like type which better explains its purpose.
--
data AcceptZ
  = AcceptZ
  | RefuseZ !Text
  deriving (Eq, Show)

class AcceptableZ v where
  acceptableVersionZ :: v -> v -> AcceptZ

data Accept agreedOptions
  = Accept agreedOptions
  | Refuse !Text
  deriving (Eq, Show)

class Acceptable v agreedOptions where
  acceptableVersion :: v -> v -> Accept agreedOptions




-- | Takes a pair of version data: local then remote.
newtype ApplicationZ r vData = ApplicationZ
  { runApplicationZ :: vData -> vData -> r
  }

newtype Application r agreedOptions = Application
  { runApplication :: agreedOptions -> r
  }

data VersionZ extra r vData = VersionZ
  { versionApplicationZ :: Application r vData
  , versionExtraZ       :: extra vData
  , versionDataZ        :: vData
  }

data Version extra r vData agreedOptions = Version
  { versionApplication :: Application r agreedOptions
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
data DictVersion vNumber vData agreedOptions where
     DictVersion :: ( Typeable vData
                    , Acceptable vData agreedOptions
                    , Show vData
                    )
                 => CodecCBORTerm Text vData
                 -> (vNumber -> vData )
                 -- ^ local vData
                 -> DictVersion vNumber vData agreedOptions

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
{-pickVersions
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
  intersect = Map.intersectionWith (,)-}

{-pickVersionsX
  :: forall vNum r vData extra.
      ( Ord vNum
     , Acceptable vData)
  => VersionsX vNum extra r vData
  -> VersionsX vNum extra r vData
  -> Either (VersionMismatch vNum) (r, r)
pickVersionsX lversions rversions = undefined matchVersion (Map.toDescList commonVersions)
  where
  matchVersion :: [(vNum, (Version extra r vData, Version extra r vData))] -> Either (VersionMismatch vNum) (r, r)
  matchVersion [] = Left NoCommonVersion
  matchVersion ((_, (lversion, rversion)):vs) =
    let ldata = version lversion
        rdata = versionData rversion in
    case acceptableVersion (versionData lversion) (versionData rversion) of
      Refuse _ -> matchVersion vs
      Accept ->
         let lapp = versionApplication lversion
             rapp = versionApplication rversion in
         Right (runApplication lapp ldata rdata, runApplication rapp ldata rdata)


  commonVersions = getVersionsX lversions `intersect` getVersionsX rversions
  intersect = Map.intersectionWith (,)-}

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
  -> Versions vNum extra r vData agreedOptions
simpleSingletonVersions vNum vData extra r =
  Versions
    $ Map.singleton vNum
        (Version (Application $ \_ -> r) extra)

{-simpleSingletonVersionsX
  :: vNum
  -> vData
  -> extra vData
  -> r
  -> VersionsX vNum extra r vData
simpleSingletonVersionsX vNum vData extra r =
  VersionsX
    $ Map.singleton vNum
        ((Version (Application $ \_ _ -> r) extra))-}
