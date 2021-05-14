{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module LedgerOnDisk.ClassWithExceptions
  ( module LedgerOnDisk.ClassWithExceptions
  , module X
  )

where

import qualified LedgerOnDisk.Class as Class
import Control.Monad.Catch

import Data.HashMap.Strict(HashMap,(!))
-- import qualified Data.HashMap.Strict as HashMap

-- for reexport
import LedgerOnDisk.Class as X(ResultSet, Err, D(..), querySingle, QueryScope)

type MonadKV k v m = (Class.MonadKV k v m, MonadThrow m, Exception (Class.Err m))

submitOperation :: MonadKV k v m => Class.ResultSet m -> (HashMap k (Maybe v) -> (Class.KVOperationResult k v, a)) -> m a
submitOperation rs op = Class.submitOperation rs op >>= either throwM pure

lookup :: MonadKV k v m => k -> m (Maybe v)
lookup k = do
  rs <- Class.prepareOperation . Class.querySingle $ k
  submitOperation rs $ \x -> (mempty, x ! k) -- guaranteed by laws to succeed

delete :: MonadKV k v m => k -> m (Maybe v)
delete k = Class.delete k >>= either throwM pure
