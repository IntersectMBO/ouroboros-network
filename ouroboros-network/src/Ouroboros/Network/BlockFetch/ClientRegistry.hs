{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.BlockFetch.ClientRegistry
  ( -- * Registry of block fetch clients
    FetchClientRegistry (..)
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

import Data.Functor.Contravariant (contramap)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (assert)
import Control.Monad (unless)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork (MonadFork (throwTo),
           MonadThread (ThreadId, myThreadId))
import Control.Monad.Class.MonadThrow
import Control.Tracer (Tracer)

import Ouroboros.Network.BlockFetch.ClientState
import Ouroboros.Network.DeltaQ



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
     FetchClientRegistry {
       fcrCtxVar
         :: StrictTMVar
              m ( Tracer m (TraceLabelPeer peer (TraceFetchClientState header))
                , STM m (FetchClientPolicy header block m)
                ),
       fcrFetchRegistry
         :: StrictTVar  m (Map peer (FetchClientStateVars m header)),
       fcrSyncRegistry
         :: StrictTVar  m (Map peer (ThreadId m, StrictTMVar m (), StrictTMVar m ())),
       fcrDqRegistry
         :: StrictTVar  m (Map peer PeerGSV),
       fcrKeepRegistry
         :: StrictTVar  m (Map peer (ThreadId m, StrictTMVar m ())),
       fcrDying
         :: StrictTVar m (Set peer)
                         }

newFetchClientRegistry :: MonadSTM m
                       => m (FetchClientRegistry peer header block m)
newFetchClientRegistry = FetchClientRegistry <$> newEmptyTMVarIO
                                             <*> newTVarIO Map.empty
                                             <*> newTVarIO Map.empty
                                             <*> newTVarIO Map.empty
                                             <*> newTVarIO Map.empty
                                             <*> newTVarIO Set.empty

-- | This is needed to start a block fetch client. It provides the required
-- 'FetchClientContext'. It registers and unregisters the fetch client on
-- start and end.
--
-- It also manages synchronisation with the corresponding chain sync client.
--
bracketFetchClient :: forall m a peer header block version.
                      (MonadSTM m, MonadFork m, MonadMask m, Ord peer)
                   => FetchClientRegistry peer header block m
                   -> version
                   -> peer
                   -> (FetchClientContext header block m -> m a)
                   -> m a
bracketFetchClient (FetchClientRegistry ctxVar
                      fetchRegistry syncRegistry dqRegistry keepRegistry dyingRegistry)
                   _version peer action = do
    ksVar <- newEmptyTMVarIO
    bracket (register ksVar) (uncurry (unregister ksVar)) (action . fst)
  where
    register :: StrictTMVar m ()
             -> m ( FetchClientContext header block m
                  , (ThreadId m, StrictTMVar m ()) )
    register ksVar = do
      tid <- myThreadId
      ctx <- atomically $ do
        -- wait for any potential older blockfetch to finish cleanup
        fr <- readTVar fetchRegistry
        check (peer `Map.notMember` fr)

        -- don't start if keepalive is attempting to die
        dr <- readTVar dyingRegistry
        check (peer `Set.notMember` dr)

        -- blocks until setFetchClientContext is called
        (tracer, mkPolicy) <- readTMVar ctxVar

        -- wait for and register with keepAlive
        dqPeers <- readTVar dqRegistry
        check (peer `Map.member` dqPeers)
        modifyTVar keepRegistry $ \m ->
          assert (peer `Map.notMember` m) $
          Map.insert peer (tid, ksVar) m

        -- allocate the policy specific for this peer's negotiated version
        policy <- mkPolicy

        stateVars <- newFetchClientStateVars
        modifyTVar fetchRegistry $ \m ->
          assert (peer `Map.notMember` m) $
          Map.insert peer stateVars m
        return FetchClientContext {
          fetchClientCtxTracer    = contramap (TraceLabelPeer peer) tracer,
          fetchClientCtxPolicy    = policy,
          fetchClientCtxStateVars = stateVars
          }

      -- Now wait for the sync client to start up.
      onException
        (atomically $ do
            syncclients <- readTVar syncRegistry
            case Map.lookup peer syncclients of
                 Nothing -> retry
                 Just (cTid, doneVar, startVar) -> do
                   putTMVar startVar ()
                   writeTVar (fetchClientStatusVar $ fetchClientCtxStateVars ctx)
                             (PeerFetchStatusReady Set.empty IsIdle)
                   return (ctx, (cTid, doneVar))
            )

        (atomically $ do
         -- we've been killed before the sync client started, cleanup
         writeTVar (fetchClientStatusVar $ fetchClientCtxStateVars ctx) PeerFetchStatusShutdown
         putTMVar ksVar ()
         modifyTVar keepRegistry $ \m ->
           assert (peer `Map.member` m) $
           Map.delete peer m

         modifyTVar fetchRegistry $ \m ->
           assert (peer `Map.member` m) $
           Map.delete peer m
         )

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
                              (MonadSTM m, MonadFork m, MonadCatch m,
                               Ord peer)
                           => FetchClientRegistry peer header block m
                           -> peer
                           -> m a
                           -> m a
bracketSyncWithFetchClient (FetchClientRegistry _ctxVar
                              _fetchRegistry syncRegistry _dqRegistry _keepRegistry _dyingRegistry) peer action = do
    doneVar <- newEmptyTMVarIO
    startVar <- newEmptyTMVarIO
    bracket_ (register doneVar startVar) (unregister doneVar) action
  where
    -- The goal here is that the block fetch client should be registered
    -- before the sync client starts running.
    --
    -- On the shutdown side, the sync client should stop before the block fetch
    -- is unregistered. This has to happen even if either client is terminated
    -- abnormally or being cancelled (which of course can happen in any order).

    register :: StrictTMVar m () -> StrictTMVar m () -> m ()
    register doneVar startVar = do
      tid <- myThreadId
      -- We first register ourselves
      atomically $ do
        -- wait for any potential older chainsync clients to finish cleanup
        sr <- readTVar syncRegistry
        check (peer `Map.notMember` sr)

        modifyTVar syncRegistry $ \m ->
          assert (peer `Map.notMember` m) $
          Map.insert peer (tid, doneVar, startVar) m
      -- Then we wait for fetch to notice us
      onException (atomically $ readTMVar startVar) (unregister doneVar)

    unregister :: StrictTMVar m () -> m ()
    unregister doneVar =
      atomically $ do
        putTMVar doneVar ()
        modifyTVar syncRegistry $ \m ->
          assert (peer `Map.member` m) $
          Map.delete peer m

bracketKeepAliveClient :: forall m a peer header block.
                              (MonadSTM m, MonadFork m, MonadMask m, Ord peer)
                       => FetchClientRegistry peer header block m
                       -> peer
                       -> (StrictTVar m (Map peer PeerGSV) -> m a)
                       -> m a
bracketKeepAliveClient(FetchClientRegistry _ctxVar
                              _fetchRegistry _syncRegistry dqRegistry keepRegistry dyingRegistry) peer action = do
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

setFetchClientContext :: MonadSTM m
                      => FetchClientRegistry peer header block m
                      -> Tracer m (TraceLabelPeer peer (TraceFetchClientState header))
                      -> STM m (FetchClientPolicy header block m)
                      -> m ()
setFetchClientContext (FetchClientRegistry ctxVar _ _ _ _ _) tracer mkPolicy =
    atomically $ do
      ok <- tryPutTMVar ctxVar (tracer, mkPolicy)
      unless ok $ error "setFetchClientContext: called more than once"

-- | A read-only 'STM' action to get the current 'PeerFetchStatus' for all
-- fetch clients in the 'FetchClientRegistry'.
--
readFetchClientsStatus :: MonadSTM m
                       => FetchClientRegistry peer header block m
                       -> STM m (Map peer (PeerFetchStatus header))
readFetchClientsStatus (FetchClientRegistry _ registry _ _ _ _) =
  readTVar registry >>= traverse (readTVar . fetchClientStatusVar)

-- | A read-only 'STM' action to get the 'FetchClientStateVars' for all fetch
-- clients in the 'FetchClientRegistry'.
--
readFetchClientsStateVars :: MonadSTM m
                          => FetchClientRegistry peer header block m
                          -> STM m (Map peer (FetchClientStateVars m header))
readFetchClientsStateVars (FetchClientRegistry _ registry _ _ _ _) = readTVar registry

-- | A read-only 'STM' action to get the 'PeerGSV's for all fetch
-- clients in the 'FetchClientRegistry'.
--
readPeerGSVs :: MonadSTM m
             => FetchClientRegistry peer header block m
             -> STM m (Map peer PeerGSV)
readPeerGSVs (FetchClientRegistry _ _ _ registry _ _) = readTVar registry
