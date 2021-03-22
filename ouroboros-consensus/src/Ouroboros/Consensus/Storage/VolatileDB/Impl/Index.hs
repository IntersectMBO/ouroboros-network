{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

-- | VolatileDB Index
--
-- Intended for qualified import
-- > import qualified Ouroboros.Consensus.Storage.VolatileDB.Impl.Index as Index
module Ouroboros.Consensus.Storage.VolatileDB.Impl.Index (
    delete
  , elems
  , empty
  , insert
  , lastFile
  , lookup
  , toAscList
    -- * opaque
  , Index
  ) where

import           Prelude hiding (lookup)

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks (..))

import           Ouroboros.Consensus.Storage.VolatileDB.Impl.FileInfo (FileInfo)
import           Ouroboros.Consensus.Storage.VolatileDB.Impl.Types (FileId)

-- | Mapping from 'FileId' to 'FileInfo'
newtype Index blk = Index { unIndex :: IntMap (FileInfo blk) }
  deriving (Generic, NoThunks)

modifyIndex ::
     (IntMap (FileInfo blk) -> IntMap (FileInfo blk))
  -> Index blk
  -> Index blk
modifyIndex f (Index index) = Index (f index)

empty :: Index blk
empty = Index IM.empty

lookup :: FileId -> Index blk -> Maybe (FileInfo blk)
lookup fileId (Index index) = IM.lookup fileId index

insert :: FileId -> FileInfo blk -> Index blk -> Index blk
insert fileId fileInfo = modifyIndex (IM.insert fileId fileInfo)

delete :: FileId -> Index blk -> Index blk
delete fileId = modifyIndex (IM.delete fileId)

toAscList :: Index blk -> [(FileId, FileInfo blk)]
toAscList (Index index) = IM.toAscList index

elems :: Index blk -> [FileInfo blk]
elems (Index index) = IM.elems index

-- | Return the last, i.e. the /highest/, 'FileId' and corresponding
-- 'FileInfo' stored in the 'Index'. Return 'Nothing' when empty.
lastFile :: Index blk -> Maybe (FileId, FileInfo blk)
lastFile = fmap fst . IM.maxViewWithKey . unIndex
