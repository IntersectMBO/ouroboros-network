{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module LedgerOnDisk.Haskey.KVHandleWithSnapshots where


import LedgerOnDisk.KVHandle.OnDiskMappings
import LedgerOnDisk.KVHandle.RangeQuery
import LedgerOnDisk.KVHandle.ClassWithSnapshots
import LedgerOnDisk.Haskey.Types hiding (KVHandle)
import qualified LedgerOnDisk.Haskey.Types (KVHandle(..))
import LedgerOnDisk.Haskey.Impl

import Data.Kind
import qualified Database.Haskey.Alloc.Concurrent as Haskey
import qualified Data.BTree.Impure as Haskey
import Data.Coerce
import qualified Data.Map.Strict as Map
import Control.Exception (throwIO)

newtype KVHandle state = WrappedKVHandle
  { _getKVHandle :: LedgerOnDisk.Haskey.Types.KVHandle state }

instance forall (state :: (Type -> Type -> Type) -> Type).
  ( HasConstrainedOnDiskMappings (SemigroupMap Query) state
  , HasConstrainedOnDiskMappings HaskeyDBKVConstraint state
  , Haskey.Root (OnDiskMappings state Haskey.Tree))
  => DB (KVHandle state) where

  type DBKVConstraint (KVHandle state) = HaskeyDBKVConstraint
  type T (KVHandle state) = state

  newtype ReadHandle (KVHandle state) = WrappedHaskeyReadSet (HaskeyReadSet state)

  newtype Snapshot (KVHandle state) = WrappedSnapshot (HaskeySnapshot state)
    deriving stock (Show)
    deriving newtype (Eq, Ord)


  prepare (WrappedKVHandle h) (DBRequest q) = let
    q' = Map.mapKeysMonotonic coerce q
    in coerce <$> haskeyPrepareQuery h q'

  -- submit (coerce -> rh) op = do
  --   (payload, coerce -> snapshot) <- haskeySubmitQuery rh $ \rangeQueryResults (Map.mapKeysMonotonic coerce -> pointQueryResults) -> case op DBOpArgs{..} of
  --     DBOpResult{..} -> (payload, snapshot, changes)
  --   pure DBResponse{..}

  rollback _ _ = throwIO $ userError "unimplemented"
