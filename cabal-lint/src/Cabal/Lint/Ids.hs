-- | Names used in this tool and its UX

module Cabal.Lint.Ids (
  CompId (..),
  DepId (..),
  PrjPkgId (..),
  ) where

import qualified Distribution.Types.ComponentName                          as C
import qualified Distribution.Types.LibraryName                            as C
import qualified Distribution.Types.PackageName                            as C

-----

-- | The global name of a dependency, ie something that occurs within a
-- `build-depends` field
data DepId = DepId C.PackageName C.LibraryName
  deriving (Eq, Ord)

-- | The global name of a component, ie something that has its own
-- `build-depends` field
data CompId puid = CompId (PrjPkgId puid) C.ComponentName
  deriving (Eq, Ord)

-- | The global name of a package in this project, ie a .cabal file being linted
data PrjPkgId puid = PrjPkgId {unPrjPkgId :: puid}
  deriving (Eq, Ord)
