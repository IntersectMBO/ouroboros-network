{-# language DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ConstraintKinds #-}

module LedgerOnDisk.V2.Db where

import Data.Kind

-- import Control.Monad.Class.MonadAsync
-- import Control.Monad.Class.MonadSTM

import LedgerOnDisk.V2.OnDiskMappings
import LedgerOnDisk.V2.Query
import LedgerOnDisk.V2.Diff


type OdmConstMap state a = OnDiskMappings state (ConstMap a)
type OdmQuery state = OnDiskMappings state Query
type OdmPtMap state = OnDiskMappings state PTMap
type OdmDiffMap state = OnDiskMappings state DiffMap
type OdmQueryResult state = OnDiskMappings state QueryResult

-- type MonadDBIO m =  (MonadAsync m, MonadSTM m)

class HasOnDiskMappings (DbState dbhandle) => Db dbhandle where
  type DbState dbhandle :: StateKind MapFlavour
  type DbSeqId dbhandle :: Type

  readDb :: dbhandle
    -> OnDiskMappings (DbState dbhandle) Query
    -> IO (OnDiskMappings (DbState dbhandle) QueryResult, DbSeqId dbhandle)

  writeDb :: dbhandle
    -> DbSeqId dbhandle
    -> DbSeqId dbhandle
    -> OnDiskMappings (DbState dbhandle) DiffMap -> IO ()
