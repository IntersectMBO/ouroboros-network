{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module LedgerOnDisk.Haskey.KVHandle
  ( inMemoryBackend
  , filestoreBackend
  , HaskeyBackend
  , withKVHandle
  , openKVHandle
  , closeKVHandle
  , LedgerOnDisk.Haskey.KVHandle.KVHandle
  , proxyConstraint
  )

where

import Data.Kind

import qualified Database.Haskey.Alloc.Concurrent as Haskey
import qualified Data.BTree.Impure as Haskey

import LedgerOnDisk.KVHandle.Class
import LedgerOnDisk.Haskey.Types hiding (KVHandle)
import qualified LedgerOnDisk.Haskey.Types (KVHandle(..))

import LedgerOnDisk.Haskey.Impl hiding (withKVHandle, openKVHandle, closeKVHandle)
import qualified LedgerOnDisk.Haskey.Impl (withKVHandle, openKVHandle, closeKVHandle)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Tracer
import Data.Coerce

newtype KVHandle state = WrappedKVHandle
  { _getKVHandle :: LedgerOnDisk.Haskey.Types.KVHandle state }

instance forall (state :: (Type -> Type -> Type) -> Type).
  ( HaskeyOnDiskMappings state
  , Haskey.Root (OnDiskMappings state Haskey.Tree))
  => DB state (KVHandle state) where
  type ReadSet state (KVHandle state) = HaskeyReadSet state
  type DBKVConstraint state = HaskeyDBKVConstraint

  prepare (WrappedKVHandle h) = haskeyPrepare h
  submit (WrappedKVHandle h) = haskeySubmit h

withKVHandle :: (MonadMask m, MonadIO m, HaskeyOnDiskMappings state)
  => Tracer IO HaskeyTrace
  -> Int
  -> HaskeyBackend
  -> FilePath
  -> (KVHandle state -> m a)
  -> m a
withKVHandle tracer n be f a = LedgerOnDisk.Haskey.Impl.withKVHandle tracer n be f (coerce a)

openKVHandle :: forall m state. (MonadMask m, MonadIO m, HaskeyOnDiskMappings state)
  => Tracer IO HaskeyTrace -- ^
  -> Int -- ^
  -> HaskeyBackend -- ^
  -> FilePath -- ^
  -> m (KVHandle state)
openKVHandle tracer numTickets haskeyBackend fp = coerce <$> LedgerOnDisk.Haskey.Impl.openKVHandle tracer numTickets haskeyBackend fp

closeKVHandle :: (MonadMask m, MonadIO m)
  => KVHandle state
  -> m ()
closeKVHandle = LedgerOnDisk.Haskey.Impl.closeKVHandle . coerce
