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
  ) where

import           Prelude hiding (lookup)

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks(..))
import           Ouroboros.Storage.VolatileDB.Types (FileId)
import           Ouroboros.Storage.VolatileDB.FileInfo (FileInfo)

-- For each file, we store the latest blockId, the number of blocks
-- and a Map for its contents.
newtype Index blockId = Index {unIndex :: IntMap (FileInfo blockId)}
  deriving (Generic, NoUnexpectedThunks)

modifyIndex :: (IntMap (FileInfo blockId) -> IntMap (FileInfo blockId))
            -> Index blockId
            -> Index blockId
modifyIndex mdf (Index mp) = Index (mdf mp)

empty :: Index blockId
empty = Index IM.empty

lookup :: FileId -> Index blockId -> Maybe (FileInfo blockId)
lookup path (Index mp) = IM.lookup path mp

insert :: FileId -> FileInfo blockId -> Index blockId -> Index blockId
insert path info = modifyIndex (IM.insert path info)

delete :: FileId -> Index blockId -> Index blockId
delete path = modifyIndex (IM.delete path)

toList :: Index blockId -> [(FileId, FileInfo blockId)]
toList (Index mp) = IM.toList mp

elems :: Index blockId -> [FileInfo blockId]
elems (Index mp) = IM.elems mp
