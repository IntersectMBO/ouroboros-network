module LedgerOnDisk.FromKV where

import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap

class MonadKV1 k m | m -> k where
  data ReadSet m
  prepare :: DMap k Proxy -> m (ReadSet m)
  submit :: ReadSet m -> ()

class FromKV a where
  foo :: (DMap k I -> a) -> a
