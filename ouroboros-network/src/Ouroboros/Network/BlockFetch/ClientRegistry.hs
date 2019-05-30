{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE BangPatterns               #-}

module Ouroboros.Network.BlockFetch.ClientRegistry (
    -- * Registry of block fetch clients
    FetchClientRegistry,
    newFetchClientRegistry,
    bracketFetchClient,
    setFetchClientContext,
    FetchClientPolicy(..),
    readFetchClientsStatus,
    readFetchClientsStateVars,
  ) where

import           Data.Functor.Contravariant (contramap)
import qualified Data.Map as Map
import           Data.Map (Map)

import           Control.Monad (unless)
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
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
       (TMVar m (Tracer m (TraceLabelPeer peer (TraceFetchClientState header)),
                 FetchClientPolicy header block m))
       (TVar  m (Map peer (FetchClientStateVars m header)))

newFetchClientRegistry :: MonadSTM m
                       => m (FetchClientRegistry peer header block m)
newFetchClientRegistry = FetchClientRegistry <$> newEmptyTMVarM
                                             <*> newTVarM Map.empty

bracketFetchClient :: (MonadThrow m, MonadSTM m, Ord peer)
                   => FetchClientRegistry peer header block m
                   -> peer
                   -> (FetchClientContext header block m -> m a)
                   -> m a
bracketFetchClient (FetchClientRegistry ctxVar registry) peer =
    bracket register unregister
  where
    register = atomically $ do
      (tracer, policy) <- readTMVar ctxVar -- blocks until setFetchClientContext
      stateVars <- newFetchClientStateVars
      modifyTVar' registry $ \m ->
        assert (peer `Map.notMember` m) $
        Map.insert peer stateVars m
      return FetchClientContext {
        fetchClientCtxTracer    = contramap (TraceLabelPeer peer) tracer,
        fetchClientCtxPolicy    = policy,
        fetchClientCtxStateVars = stateVars
      }

    unregister FetchClientContext { fetchClientCtxStateVars = stateVars } =
      atomically $ do
        writeTVar (fetchClientStatusVar stateVars) PeerFetchStatusShutdown
        modifyTVar' registry $ \m ->
          assert (peer `Map.member` m) $
          Map.delete peer m

setFetchClientContext :: MonadSTM m
                      => FetchClientRegistry peer header block m
                      -> Tracer m (TraceLabelPeer peer (TraceFetchClientState header))
                      -> FetchClientPolicy header block m
                      -> m ()
setFetchClientContext (FetchClientRegistry ctxVar _) tracer policy =
    atomically $ do
      ok <- tryPutTMVar ctxVar (tracer, policy)
      unless ok $ fail "setFetchClientContext: called more than once"

-- | A read-only 'STM' action to get the current 'PeerFetchStatus' for all
-- fetch clients in the 'FetchClientRegistry'.
--
readFetchClientsStatus :: MonadSTM m
                       => FetchClientRegistry peer header block m
                       -> STM m (Map peer (PeerFetchStatus header))
readFetchClientsStatus (FetchClientRegistry _ registry) =
  readTVar registry >>= traverse (readTVar . fetchClientStatusVar)

-- | A read-only 'STM' action to get the 'FetchClientStateVars' for all fetch
-- clients in the 'FetchClientRegistry'.
--
readFetchClientsStateVars :: MonadSTM m
                          => FetchClientRegistry peer header block m
                          -> STM m (Map peer (FetchClientStateVars m header))
readFetchClientsStateVars (FetchClientRegistry _ registry) = readTVar registry

