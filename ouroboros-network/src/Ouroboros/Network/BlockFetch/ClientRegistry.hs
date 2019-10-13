{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Ouroboros.Network.BlockFetch.ClientRegistry (
    -- * Registry of block fetch clients
    FetchClientRegistry,
    newFetchClientRegistry,
    bracketFetchClient,
    bracketSyncWithFetchClient,
    setFetchClientContext,
    FetchClientPolicy(..),
    readFetchClientsStatus,
    readFetchClientsStateVars,
  ) where

import           Data.Functor.Contravariant (contramap)
import qualified Data.Map as Map
import           Data.Map (Map)

import           Control.Monad (unless)
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
                   ( MonadThread(ThreadId, myThreadId), MonadFork(throwTo) )
import           Control.Exception (assert)
import           Control.Tracer (Tracer)

import           Ouroboros.Network.BlockFetch.ClientState



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

newFetchClientRegistry :: MonadSTM m
                       => m (FetchClientRegistry peer header block m)
newFetchClientRegistry = FetchClientRegistry <$> newEmptyTMVarM
                                             <*> newTVarM Map.empty
                                             <*> newTVarM Map.empty

-- | This is needed to start a block fetch client. It provides the required
-- 'FetchClientContext'. It registers and unregisters the fetch client on
-- start and end.
--
-- It also manages synchronisation with the corresponding chain sync client.
--
bracketFetchClient :: forall m a peer header block.
                      (MonadThrow m, MonadSTM m, MonadFork m, Ord peer)
                   => FetchClientRegistry peer header block m
                   -> peer
                   -> (FetchClientContext header block m -> m a)
                   -> m a
bracketFetchClient (FetchClientRegistry ctxVar
                      fetchRegistry syncRegistry) peer action =
    bracket register (uncurry unregister) (action . fst)
  where
    register :: m ( FetchClientContext header block m
                  , (ThreadId m, StrictTMVar m ()) )
    register = do
      ctx <- atomically $ do
        -- blocks until setFetchClientContext is called
        (tracer, policy) <- readTMVar ctxVar
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

    unregister :: FetchClientContext header block m
               -> (ThreadId m, StrictTMVar m ())
               -> m ()
    unregister FetchClientContext { fetchClientCtxStateVars = stateVars }
               (tid, doneVar)  = do
      -- Signal we are shutting down
      atomically $
        writeTVar (fetchClientStatusVar stateVars) PeerFetchStatusShutdown
      -- Kill the sync client if it is still running
      throwTo tid AsyncCancelled
      -- Wait for the sync client to terminate and finally unregister ourselves
      atomically $ do
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
                              fetchRegistry syncRegistry) peer action = do
    doneVar <- newEmptyTMVarM
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

setFetchClientContext :: MonadSTM m
                      => FetchClientRegistry peer header block m
                      -> Tracer m (TraceLabelPeer peer (TraceFetchClientState header))
                      -> FetchClientPolicy header block m
                      -> m ()
setFetchClientContext (FetchClientRegistry ctxVar _ _) tracer policy =
    atomically $ do
      ok <- tryPutTMVar ctxVar (tracer, policy)
      unless ok $ fail "setFetchClientContext: called more than once"

-- | A read-only 'STM' action to get the current 'PeerFetchStatus' for all
-- fetch clients in the 'FetchClientRegistry'.
--
readFetchClientsStatus :: MonadSTM m
                       => FetchClientRegistry peer header block m
                       -> STM m (Map peer (PeerFetchStatus header))
readFetchClientsStatus (FetchClientRegistry _ registry _) =
  readTVar registry >>= traverse (readTVar . fetchClientStatusVar)

-- | A read-only 'STM' action to get the 'FetchClientStateVars' for all fetch
-- clients in the 'FetchClientRegistry'.
--
readFetchClientsStateVars :: MonadSTM m
                          => FetchClientRegistry peer header block m
                          -> STM m (Map peer (FetchClientStateVars m header))
readFetchClientsStateVars (FetchClientRegistry _ registry _) = readTVar registry

