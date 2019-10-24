{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

-- | VolatileDB Index
--
-- Intended for qualified import
-- > import qualified Ouroboros.Storage.VolatileDB.Index as Index
module Ouroboros.Storage.VolatileDB.Index (
    Index -- opaque
  , empty
  , lookup
  , insert
  , delete
  , toList
  , elems
  , getHandle
  ) where

import           Prelude hiding (lookup)

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks (..))

import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.VolatileDB.FileInfo (FileInfo)
import qualified Ouroboros.Storage.VolatileDB.FileInfo as FileInfo
import           Ouroboros.Storage.VolatileDB.Types (FileId)

-- For each file, we store the latest blockId, the number of blocks
-- and a Map for its contents.
newtype Index blockId h = Index {unIndex :: IntMap (FileInfo blockId h)}
  deriving (Generic, NoUnexpectedThunks)

modifyIndex :: (IntMap (FileInfo blockId h) -> IntMap (FileInfo blockId h))
            -> Index blockId h
            -> Index blockId h
modifyIndex mdf (Index mp) = Index (mdf mp)

empty :: Index blockId h
empty = Index IM.empty

lookup :: FileId -> Index blockId h -> Maybe (FileInfo blockId h)
lookup fileId (Index mp) = IM.lookup fileId mp

insert :: FileId -> FileInfo blockId h -> Index blockId h -> Index blockId h
insert fileId info = modifyIndex (IM.insert fileId info)

delete :: FileId -> Index blockId h -> Index blockId h
delete fileId = modifyIndex (IM.delete fileId)

toList :: Index blockId h -> [(FileId, FileInfo blockId h)]
toList (Index mp) = IM.toList mp

elems :: Index blockId h -> [FileInfo blockId h]
elems (Index mp) = IM.elems mp

getHandle :: FileId -> Index blockId h -> Maybe (Handle h)
getHandle fileId index = FileInfo.getHandle <$> lookup fileId index
