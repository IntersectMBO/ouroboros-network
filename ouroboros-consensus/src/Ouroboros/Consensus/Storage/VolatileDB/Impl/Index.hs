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
import           Ouroboros.Consensus.Storage.VolatileDB.Impl.Types (FileId)

-- | Mapping from 'FileId' to 'FileInfo'
--
-- Just like 'FileInfo', we parameterise by @hash@ instead of @blk@ because
-- @HeaderHash blk@ is a type family and would lead to ambiguity problems.
newtype Index hash = Index { unIndex :: IntMap (FileInfo hash) }
  deriving (Generic, NoUnexpectedThunks)

modifyIndex ::
     (IntMap (FileInfo hash) -> IntMap (FileInfo hash))
  -> Index hash
  -> Index hash
modifyIndex f (Index index) = Index (f index)

empty :: Index hash
empty = Index IM.empty

lookup :: FileId -> Index hash -> Maybe (FileInfo hash)
lookup fileId (Index index) = IM.lookup fileId index

insert :: FileId -> FileInfo hash -> Index hash -> Index hash
insert fileId fileInfo = modifyIndex (IM.insert fileId fileInfo)

delete :: FileId -> Index hash -> Index hash
delete fileId = modifyIndex (IM.delete fileId)

toAscList :: Index hash -> [(FileId, FileInfo hash)]
toAscList (Index index) = IM.toAscList index

elems :: Index hash -> [FileInfo hash]
elems (Index index) = IM.elems index

-- | Return the last, i.e. the /highest/, 'FileId' and corresponding
-- 'FileInfo' stored in the 'Index'. Return 'Nothing' when empty.
lastFile :: Index hash -> Maybe (FileId, FileInfo hash)
lastFile = fmap fst . IM.maxViewWithKey . unIndex
