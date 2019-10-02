module Ouroboros.Storage.VolatileDB.Index (
    Index -- TODO: Make opaque newtype
  ) where

import           Data.Map.Strict (Map)

import           Ouroboros.Storage.VolatileDB.FileInfo

-- For each file, we store the latest blockId, the number of blocks
-- and a Map for its contents.
type Index blockId = Map String (FileInfo blockId)
