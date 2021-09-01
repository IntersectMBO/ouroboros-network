-- |

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module LedgerOnDisk.Haskey.Types where

import Data.Kind
import Control.Tracer
import qualified Database.Haskey.Store.InMemory as Haskey
import qualified Control.Monad.Haskey as Haskey
import Control.Concurrent.STM

import LedgerOnDisk.KVHandle.OnDiskMappings
import qualified Database.Haskey.Alloc.Concurrent as Haskey
import qualified Data.BTree.Impure as Haskey
import qualified Data.BTree.Primitives as Haskey
import Data.Proxy

type QueryId = Int

data KVHandle (state :: (Type -> Type -> Type) -> Type) = KVHandle
  { tracer :: !(Tracer IO HaskeyTrace)
  , nextQueryIdTV :: !(TVar Int)
  , outstandingQueriesTV :: !(TVar Int)
  , numTickets :: !Int -- ^ maximum outstanding queries
  , dbRoot :: !(Haskey.ConcurrentDb (OnDiskMappings state Haskey.Tree))
  , haskeyBackend :: !HaskeyBackend
  , concurrentHandles :: !Haskey.ConcurrentHandles
  , queryTimeoutMS :: !Int
  }

data HaskeyBackend
  = HBMemory
    { memConfig :: !Haskey.MemoryStoreConfig
    , memFiles :: !(Haskey.MemoryFiles FilePath)
    }
  | HBFile
    { fileConfig :: !Haskey.FileStoreConfig
    }

data HaskeyTrace
  = HT_Open HaskeyTraceOpen
  | HT_Prepare HaskeyTracePrepare
  | HT_Submit HaskeyTraceSubmit
  deriving stock (Show)
data HaskeyTraceOpen
  = HTO_Init FilePath
  | HTO_Created
  | HTO_Opened
  deriving stock (Show)

data HaskeyTracePrepare
  = HTP_Prepare
  | HTP_Preparing !QueryId
  | HTP_Timedout !QueryId
  | HTP_QueryStarted !QueryId
  | HTP_QueryComplete !QueryId
  deriving stock (Show)

data HaskeyTraceSubmit
  = HTS_Submit QueryId
  | HTS_Running QueryId
  | HTS_Complete QueryId
  deriving stock (Show)

class (Haskey.Key k, Haskey.Value v) => HaskeyDBKVConstraint k v
instance (Haskey.Key k, Haskey.Value v) => HaskeyDBKVConstraint k v

proxyConstraint :: Proxy HaskeyDBKVConstraint
proxyConstraint = Proxy

type HaskeyOnDiskMappings state =
  ( HasConstrainedOnDiskMappings HaskeyDBKVConstraint state
  , Haskey.Root (OnDiskMappings state Haskey.Tree)
  )
