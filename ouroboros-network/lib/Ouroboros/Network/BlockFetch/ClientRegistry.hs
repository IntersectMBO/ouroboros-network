{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.BlockFetch.ClientRegistry
  ( -- * Registry of block fetch clients
    FetchClientRegistry (..)
  , newFetchClientRegistry
  , bracketFetchClient
  , bracketSyncWithFetchClient
  , setFetchClientContext
  , FetchClientPolicy (..)
  , readFetchClientsStatus
  , readFetchClientsStateVars
    -- * KeepAlive registry
  , module KeepAlive
  ) where

import Data.Functor.Contravariant (contramap)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (assert)
import Control.Monad (unless)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork (MonadFork (throwTo),
           MonadThread (ThreadId, myThreadId))
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer)

import Ouroboros.Network.BlockFetch.ClientState
import Ouroboros.Network.Diffusion.Policies (deactivateTimeout)
import Ouroboros.Network.KeepAlive.Registry as KeepAlive



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
       ctxVar
         :: StrictTMVar
              m ( Tracer m (TraceLabelPeer peer (TraceFetchClientState header))
                , STM m (FetchClientPolicy header block m)
                ),
       fetchRegistry
         :: StrictTVar  m (Map peer (FetchClientStateVars m header)),
       syncRegistry
         :: StrictTVar  m (Map peer (ThreadId m, StrictTMVar m (), StrictTMVar m ()))
    }


newFetchClientRegistry :: MonadSTM m
                       => m (FetchClientRegistry peer header block m)
newFetchClientRegistry = FetchClientRegistry <$> newEmptyTMVarIO
                                             <*> newTVarIO Map.empty
                                             <*> newTVarIO Map.empty

-- | This is needed to start a block fetch client. It provides the required
-- 'FetchClientContext'. It registers and unregisters the fetch client on
-- start and end.
--
-- It also manages synchronisation with the corresponding chain sync client.
--
bracketFetchClient :: forall m a peer header block version.
                      (MonadFork m, MonadMask m, MonadTimer m, Ord peer)
                   => FetchClientRegistry peer header block m
                   -> KeepAliveRegistry peer m
                   -> version
                   -> peer
                   -> (FetchClientContext header block m -> m a)
                   -> m a
bracketFetchClient FetchClientRegistry { ctxVar, fetchRegistry, syncRegistry }
                   KeepAliveRegistry { dqRegistry, keepRegistry, dyingRegistry }
                   _version peer action = do
    ksVar <- newEmptyTMVarIO
    fst <$> generalBracket (register ksVar) (unregister ksVar) (action . fst)
  where
    onExceptionTimeout :: DiffTime
    onExceptionTimeout = 1

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
               -> ( FetchClientContext header block m
                  , (ThreadId m, StrictTMVar m ()) )
               -> ExitCase a
               -> m ()
    unregister ksVar (FetchClientContext { fetchClientCtxStateVars = stateVars },
                      (tid, doneVar)) exitCase  = uninterruptibleMask $ \unmask -> do
      let timeoutLimit = case exitCase of
                              ExitCaseSuccess _ -> deactivateTimeout
                              _                 -> onExceptionTimeout
      dead <- do
        -- Signal we are shutting down
        dieFast <- atomically $ do
          writeTVar (fetchClientStatusVar stateVars) PeerFetchStatusShutdown

          dr <- readTVar dyingRegistry
          return $ Set.member peer dr

        -- If keepAlive is dying we don't need to let chainsync exit cleanly
        if dieFast
           then do
             throwTo tid AsyncCancelled
             atomically $ readTMVar doneVar >> cleanup
             return True
           else return False

      if dead
         then return ()
         else do
           -- Give the sync client a chance to exit cleanly before killing it.
           res <- onException
                   (unmask $ timeout timeoutLimit $ atomically $ readTMVar doneVar)
                   (-- no time to wait, die die die!
                    uninterruptibleMask_ $ do
                    throwTo tid AsyncCancelled
                    atomically $ readTMVar doneVar >> cleanup
                   )
           case res of
                  Nothing -> do
                    throwTo tid AsyncCancelled
                    atomically $ readTMVar doneVar >> cleanup
                  Just _ -> atomically cleanup
     where
       cleanup = do
         modifyTVar fetchRegistry $ \m ->
           assert (peer `Map.member` m) $
           Map.delete peer m

         -- Signal to keepAlive that we're going away
         putTMVar ksVar ()
         modifyTVar keepRegistry $ \m ->
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
bracketSyncWithFetchClient FetchClientRegistry { syncRegistry } peer action = do
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

setFetchClientContext :: MonadSTM m
                      => FetchClientRegistry peer header block m
                      -> Tracer m (TraceLabelPeer peer (TraceFetchClientState header))
                      -> STM m (FetchClientPolicy header block m)
                      -> m ()
setFetchClientContext FetchClientRegistry { ctxVar } tracer mkPolicy =
    atomically $ do
      ok <- tryPutTMVar ctxVar (tracer, mkPolicy)
      unless ok $ error "setFetchClientContext: called more than once"

-- | A read-only 'STM' action to get the current 'PeerFetchStatus' for all
-- fetch clients in the 'FetchClientRegistry'.
--
readFetchClientsStatus :: MonadSTM m
                       => FetchClientRegistry peer header block m
                       -> STM m (Map peer (PeerFetchStatus header))
readFetchClientsStatus FetchClientRegistry { fetchRegistry } =
  readTVar fetchRegistry >>= traverse (readTVar . fetchClientStatusVar)

-- | A read-only 'STM' action to get the 'FetchClientStateVars' for all fetch
-- clients in the 'FetchClientRegistry'.
--
readFetchClientsStateVars :: MonadSTM m
                          => FetchClientRegistry peer header block m
                          -> STM m (Map peer (FetchClientStateVars m header))
readFetchClientsStateVars FetchClientRegistry { fetchRegistry } = readTVar fetchRegistry
