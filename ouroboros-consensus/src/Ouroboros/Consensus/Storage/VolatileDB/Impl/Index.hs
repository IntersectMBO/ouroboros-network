{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

-- | VolatileDB Index
--
-- Intended for qualified import
-- > import qualified Ouroboros.Consensus.Storage.VolatileDB.Impl.Index as Index
module Ouroboros.Consensus.Storage.VolatileDB.Impl.Index (
    Index -- opaque
  , empty
  , lookup
  , insert
  , delete
  , toAscList
  , elems
  , lastFile
  ) where

import           Prelude hiding (lookup)

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Ouroboros.Consensus.Storage.VolatileDB.Impl.FileInfo (FileInfo)
import           Ouroboros.Consensus.Storage.VolatileDB.Types (FileId)

-- | Mapping from 'FileId' to 'FileInfo'
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

toAscList :: Index blockId -> [(FileId, FileInfo blockId)]
toAscList (Index mp) = IM.toAscList mp

elems :: Index blockId -> [FileInfo blockId]
elems (Index mp) = IM.elems mp

-- | Return the last, i.e. the /highest/, 'FileId' and corresponding
-- 'FileInfo' stored in the 'Index'. Return 'Nothing' when empty.
lastFile :: Index blockId -> Maybe (FileId, FileInfo blockId)
lastFile = fmap fst . IM.maxViewWithKey . unIndex
