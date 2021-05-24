{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module LedgerOnDisk.FromKV where

import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Proxy
import Data.Functor.Identity


class MonadKV1 k m | m -> k where
  data ReadSet m
  prepare :: DMap k Proxy -> m (ReadSet m)
  submit :: ReadSet m -> ()

class FromKV a where
  foo :: (DMap k Identity -> a) -> a
