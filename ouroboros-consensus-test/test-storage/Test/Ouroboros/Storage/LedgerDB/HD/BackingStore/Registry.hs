{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- | A utility for storing and retrieving resources in a registry using handles
-- to identify resources in the registry.
module Test.Ouroboros.Storage.LedgerDB.HD.BackingStore.Registry (
    Handle
  , HandleRegistry
  , initHandleRegistry
  , readHandle
  , registerHandle
  ) where

import           Control.Monad.Class.MonadSTM.Internal as STM
                     (MonadSTM (TVar, atomically, newTVarIO, readTVar, writeTVar))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Ouroboros.Consensus.Util.IOLike (IOLike)

newtype Handle = Handle Word
  deriving stock (Show, Eq, Ord)
  deriving newtype Num

data HandleRegistry m a = HandleRegistry {
    handles    :: TVar m (Map Handle a)
  , nextHandle :: TVar m Handle
  }

initHandleRegistry :: IOLike m => m (HandleRegistry m a)
initHandleRegistry = do
  handles <- STM.newTVarIO Map.empty
  nextHandle <- STM.newTVarIO 0
  pure $ HandleRegistry { handles, nextHandle }

registerHandle ::
     IOLike m
  => HandleRegistry m a
  -> a
  -> m Handle
registerHandle HandleRegistry{handles, nextHandle} bsvh = STM.atomically $ do
  vhs <- STM.readTVar handles
  nh <- STM.readTVar nextHandle
  let
    vhs' = Map.insert nh bsvh vhs
  STM.writeTVar handles vhs'
  STM.writeTVar nextHandle (nh + 1)
  pure nh

readHandle ::
     IOLike m
  => HandleRegistry m a
  -> Handle
  -> m a
readHandle HandleRegistry{handles} h = STM.atomically $ do
  vhs <- STM.readTVar handles
  case Map.lookup h vhs of
    Nothing -> error "Handle not found"
    Just vh -> pure vh
