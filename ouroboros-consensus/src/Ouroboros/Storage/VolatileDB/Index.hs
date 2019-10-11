{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}

module Ouroboros.Storage.VolatileDB.Index (
    Index -- opaque

  -- public api. To be used as qualified.
  , empty
  , lookup
  , insert
  , delete
  , toList
  , elems
  ) where

import           Prelude hiding (lookup)

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks(..))
import           Ouroboros.Storage.FS.API.Types (FsPath)
import           Ouroboros.Storage.VolatileDB.FileInfo (FileInfo)

-- For each file, we store the latest blockId, the number of blocks
-- and a Map for its contents.
newtype Index blockId = Index {unIndex :: Map FsPath (FileInfo blockId)}
  deriving (Generic, NoUnexpectedThunks)

modifyIndex :: (Map FsPath (FileInfo blockId) -> Map FsPath (FileInfo blockId))
            -> Index blockId
            -> Index blockId
modifyIndex mdf (Index mp) = Index (mdf mp)

empty :: Index blockId
empty = Index Map.empty

lookup :: FsPath -> Index blockId -> Maybe (FileInfo blockId)
lookup path (Index mp) = Map.lookup path mp

insert :: FsPath -> FileInfo blockId -> Index blockId -> Index blockId
insert path info = modifyIndex (Map.insert path info)

delete :: FsPath -> Index blockId -> Index blockId
delete path = modifyIndex (Map.delete path)

toList :: Index blockId -> [(FsPath, FileInfo blockId)]
toList (Index mp) = Map.toList mp

elems :: Index blockId -> [FileInfo blockId]
elems index = snd <$> toList index
