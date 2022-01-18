{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.BlockFetch.ClientRegistry
  ( -- * Registry of block fetch clients
    FetchClientRegistry
  , newFetchClientRegistry
  , bracketFetchClient
  , bracketKeepAliveClient
  , bracketSyncWithFetchClient
  , setFetchClientContext
  , FetchClientPolicy (..)
  , readFetchClientsStatus
  , readFetchClientsStateVars
  , readPeerGSVs
  ) where

import           Data.Functor.Contravariant (contramap)
import           Data.Map (Map)
import qualified Data.Map as Map

import           Control.Exception (assert)
import           Control.Monad (unless)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork (MonadFork (throwTo),
                     MonadThread (ThreadId, myThreadId))
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Tracer (Tracer)

import           Ouroboros.Network.BlockFetch.ClientState
import           Ouroboros.Network.DeltaQ



-- | A registry for the threads that are executing the client side of the
-- 'BlockFetch' protocol to communicate with our peers.
--
-- The registry contains the shared variables we use to communicate with these
-- threads, both to track their status and to provide instructions.
--
-- The threads add\/remove themselves to\/from this registry when they start up
-- and shut down.
--
data FetchClientRegistry peer header block m =
     FetchClientRegistry
       (StrictTMVar m (Tracer m (TraceLabelPeer peer (TraceFetchClientState header)),
                 FetchClientPolicy header block m))
       (StrictTVar  m (Map peer (FetchClientStateVars m header)))
       (StrictTVar  m (Map peer (ThreadId m, StrictTMVar m ())))
       (StrictTVar  m (Map peer PeerGSV))
       (StrictTVar  m (Map peer (ThreadId m, StrictTMVar m ())))

newFetchClientRegistry :: MonadSTM m
                       => m (FetchClientRegistry peer header block m)
newFetchClientRegistry = FetchClientRegistry <$> newEmptyTMVarIO
                                             <*> newTVarIO Map.empty
                                             <*> newTVarIO Map.empty
                                             <*> newTVarIO Map.empty
                                             <*> newTVarIO Map.empty

-- | This is needed to start a block fetch client. It provides the required
-- 'FetchClientContext'. It registers and unregisters the fetch client on
-- start and end.
--
-- It also manages synchronisation with the corresponding chain sync client.
--
bracketFetchClient :: forall m a peer header block.
                      (MonadThrow m, MonadSTM m, MonadFork m, MonadMask m,
                       Ord peer)
                   => FetchClientRegistry peer header block m
                   -> peer
                   -> (FetchClientContext header block m -> m a)
                   -> m a
bracketFetchClient (FetchClientRegistry ctxVar
                      fetchRegistry syncRegistry dqRegistry keepRegistry) peer action = do
    ksVar <- newEmptyTMVarIO
    bracket (register ksVar) (uncurry (unregister ksVar)) (action . fst)
  where
    register :: StrictTMVar m ()
             -> m ( FetchClientContext header block m
                  , (ThreadId m, StrictTMVar m ()) )
    register ksVar = do
      tid <- myThreadId
      ctx <- atomically $ do
        -- blocks until setFetchClientContext is called
        (tracer, policy) <- readTMVar ctxVar

        -- wait for and register with keepAlive
        dqPeers <- readTVar dqRegistry
        check (peer `Map.member` dqPeers)
        modifyTVar keepRegistry $ \m ->
          assert (peer `Map.notMember` m) $
          Map.insert peer (tid, ksVar) m

        stateVars <- newFetchClientStateVars
        modifyTVar fetchRegistry $ \m ->
          assert (peer `Map.notMember` m) $
          Map.insert peer stateVars m
        return FetchClientContext {
          fetchClientCtxTracer    = contramap (TraceLabelPeer peer) tracer,
          fetchClientCtxPolicy    = policy,
          fetchClientCtxStateVars = stateVars
        }
      -- Now wait for the sync client to start up:
      syncclient <- atomically $ do
        syncclients <- readTVar syncRegistry
        case Map.lookup peer syncclients of
          Nothing         -> retry
          Just syncclient -> return syncclient
      return (ctx, syncclient)

    unregister :: StrictTMVar m ()
               -> FetchClientContext header block m
               -> (ThreadId m, StrictTMVar m ())
               -> m ()
    unregister ksVar FetchClientContext { fetchClientCtxStateVars = stateVars }
               (tid, doneVar)  = uninterruptibleMask_ $ do
      -- Signal we are shutting down
      atomically $
        writeTVar (fetchClientStatusVar stateVars) PeerFetchStatusShutdown
      -- Kill the sync client if it is still running
      throwTo tid AsyncCancelled
      -- Wait for the sync client to terminate and finally unregister ourselves
      atomically $ do
        -- Signal to keepAlive that we're going away
        putTMVar ksVar ()
        modifyTVar keepRegistry $ \m ->
          assert (peer `Map.member` m) $
          Map.delete peer m

        readTMVar doneVar
        modifyTVar fetchRegistry $ \m ->
          assert (peer `Map.member` m) $
          Map.delete peer m


-- | The block fetch and chain sync clients for each peer need to synchronise
-- their startup and shutdown. This bracket operation provides that
-- synchronisation for the chain sync client.
--
-- This must be used for the chain sync client /outside/ of its own state
-- registration and deregistration.
--
bracketSyncWithFetchClient :: forall m a peer header block.
                              (MonadThrow m, MonadSTM m, MonadFork m, Ord peer)
                           => FetchClientRegistry peer header block m
                           -> peer
                           -> m a
                           -> m a
bracketSyncWithFetchClient (FetchClientRegistry _ctxVar
                              fetchRegistry syncRegistry _dqRegistry _keepRegistry) peer action = do
    doneVar <- newEmptyTMVarIO
    bracket_ (register doneVar) (unregister doneVar) action
  where
    -- The goal here is that the block fetch client should be registered
    -- before the sync client starts running.
    --
    -- On the shutdown side, the sync client should stop before the block fetch
    -- is unregistered. This has to happen even if either client is terminated
    -- abnormally or being cancelled (which of course can happen in any order).

    register :: StrictTMVar m () -> m ()
    register doneVar = do
      tid <- myThreadId
      -- We wait for the fetch client to be registered, and register ourselves
      atomically $ do
        fetchclients <- readTVar fetchRegistry
        check (peer `Map.member` fetchclients)
        modifyTVar syncRegistry $ \m ->
          assert (peer `Map.notMember` m) $
          Map.insert peer (tid, doneVar) m

    unregister :: StrictTMVar m () -> m ()
    unregister doneVar =
      atomically $ do
        putTMVar doneVar ()
        modifyTVar syncRegistry $ \m ->
          assert (peer `Map.member` m) $
          Map.delete peer m

bracketKeepAliveClient :: forall m a peer header block.
                              (MonadThrow m, MonadSTM m, MonadFork m,
                               MonadMask m, Ord peer)
                       => FetchClientRegistry peer header block m
                       -> peer
                       -> ((StrictTVar  m (Map peer PeerGSV)) -> m a)
                       -> m a
bracketKeepAliveClient(FetchClientRegistry _ctxVar
                              _fetchRegistry _syncRegistry dqRegistry keepRegistry) peer action = do
    bracket_ register unregister (action dqRegistry)
  where
    -- the keepAliveClient will register a PeerGSV and the block fetch client will wait on it.
    register :: m ()
    register =
      atomically $
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
             Just rc -> return $ Just rc
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


setFetchClientContext :: MonadSTM m
                      => FetchClientRegistry peer header block m
                      -> Tracer m (TraceLabelPeer peer (TraceFetchClientState header))
                      -> FetchClientPolicy header block m
                      -> m ()
setFetchClientContext (FetchClientRegistry ctxVar _ _ _ _) tracer policy =
    atomically $ do
      ok <- tryPutTMVar ctxVar (tracer, policy)
      unless ok $ error "setFetchClientContext: called more than once"

-- | A read-only 'STM' action to get the current 'PeerFetchStatus' for all
-- fetch clients in the 'FetchClientRegistry'.
--
readFetchClientsStatus :: MonadSTM m
                       => FetchClientRegistry peer header block m
                       -> STM m (Map peer (PeerFetchStatus header))
readFetchClientsStatus (FetchClientRegistry _ registry _ _ _) =
  readTVar registry >>= traverse (readTVar . fetchClientStatusVar)

-- | A read-only 'STM' action to get the 'FetchClientStateVars' for all fetch
-- clients in the 'FetchClientRegistry'.
--
readFetchClientsStateVars :: MonadSTM m
                          => FetchClientRegistry peer header block m
                          -> STM m (Map peer (FetchClientStateVars m header))
readFetchClientsStateVars (FetchClientRegistry _ registry _ _ _) = readTVar registry

-- | A read-only 'STM' action to get the 'PeerGSV's for all fetch
-- clients in the 'FetchClientRegistry'.
--
readPeerGSVs :: MonadSTM m
             => FetchClientRegistry peer header block m
             -> STM m (Map peer PeerGSV)
readPeerGSVs (FetchClientRegistry _ _ _ registry _) = readTVar registry
