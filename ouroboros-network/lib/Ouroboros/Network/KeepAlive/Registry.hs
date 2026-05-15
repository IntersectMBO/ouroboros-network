{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.KeepAlive.Registry
  ( KeepAliveRegistry (..)
  , newKeepAliveRegistry
  , bracketKeepAliveClient
  , readPeerGSVs
  ) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (assert)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadThrow

import Ouroboros.Network.DeltaQ

-- | A registry which keeps `PeerGSV` information based on `keep-alive`
-- measurements.
--
data KeepAliveRegistry peer m = KeepAliveRegistry {
       dqRegistry
         :: StrictTVar  m (Map peer PeerGSV),
       keepRegistry
         :: StrictTVar  m (Map peer (ThreadId m, StrictTMVar m ())),
       dyingRegistry
         :: StrictTVar m (Set peer)
     }

newKeepAliveRegistry :: MonadSTM m
                     => m (KeepAliveRegistry peer m)
newKeepAliveRegistry = KeepAliveRegistry <$> newTVarIO Map.empty
                                         <*> newTVarIO Map.empty
                                         <*> newTVarIO Set.empty

bracketKeepAliveClient :: forall m a peer.
                              (MonadSTM m, MonadFork m, MonadMask m, Ord peer)
                       => KeepAliveRegistry peer m
                       -> peer
                       -> (StrictTVar m (Map peer PeerGSV) -> m a)
                       -> m a
bracketKeepAliveClient KeepAliveRegistry { dqRegistry, keepRegistry, dyingRegistry } peer action = do
    bracket_ register unregister (action dqRegistry)
  where
    -- the keepAliveClient will register a PeerGSV and the block fetch client will wait on it.
    register :: m ()
    register =
      atomically $ do
        -- Wait for previous keep alive client to cleanup
        dr <- readTVar dqRegistry
        check (peer `Map.notMember` dr)

        modifyTVar dqRegistry $ \m ->
          assert (peer `Map.notMember` m) $
          Map.insert peer defaultGSV m

    -- It is possible for the keepAlive client to keep running even without a fetch client, but
    -- a fetch client shouldn't run without a keepAlive client.
    unregister :: m ()
    unregister = uninterruptibleMask_ $ do
      fetchclient_m <- atomically $ do
        fetchclients <- readTVar keepRegistry
        case Map.lookup peer fetchclients of
             Nothing -> do
               -- If the fetch client is already dead we remove PeerGSV ourself directly.
               modifyTVar dqRegistry $ \m ->
                 assert (peer `Map.member` m) $
                 Map.delete peer m
               return Nothing
             Just rc -> do
               -- Prevent a new fetchclient from starting while we are killing the old one.
               modifyTVar dyingRegistry $ \s ->
                 assert (peer `Set.notMember` s) $
                 Set.insert peer s
               return $ Just rc
      case fetchclient_m of
           Nothing -> return ()
           Just (tid, doneVar) -> do
             -- Cancel the fetch client.
             throwTo tid AsyncCancelled
             atomically $ do
               -- wait for fetch client to exit.
               readTMVar doneVar
               modifyTVar dqRegistry $ \m ->
                 assert (peer `Map.member` m) $
                 Map.delete peer m
               modifyTVar dyingRegistry $ \s ->
                 assert (peer `Set.member` s) $
                 Set.delete peer s

-- | A read-only 'STM' action to get the 'PeerGSV's for all fetch
-- clients in the 'FetchClientRegistry'.
--
readPeerGSVs :: forall m peer.
                ( MonadSTM m, Ord peer)
             => KeepAliveRegistry peer m
             -> STM m (Map peer PeerGSV)
readPeerGSVs KeepAliveRegistry { dqRegistry, keepRegistry } = do
  dr <- readTVar dqRegistry
  kr <- readTVar keepRegistry
  -- The intersection gives us only the currently hot peers
  return $ Map.intersection dr kr
