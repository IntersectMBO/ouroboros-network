{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE BangPatterns               #-}

module Ouroboros.Network.BlockFetch.ClientRegistry (
    -- * Registry of block fetch clients
    FetchClientRegistry(..),
    newFetchClientRegistry,
    bracketFetchClient,
    readFetchClientsStatus,
    readFetchClientsStateVars,
  ) where

import qualified Data.Map as Map
import           Data.Map (Map)

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Exception (assert)

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
newtype FetchClientRegistry peer header m =
        FetchClientRegistry (TVar m (Map peer (FetchClientStateVars m header)))

newFetchClientRegistry :: MonadSTM m => m (FetchClientRegistry peer header m)
newFetchClientRegistry = FetchClientRegistry <$> newTVarM Map.empty

bracketFetchClient :: (MonadThrow m, MonadSTM m, Ord peer)
                   => FetchClientRegistry peer header m
                   -> peer
                   -> (FetchClientStateVars m header -> m a)
                   -> m a
bracketFetchClient (FetchClientRegistry registry) peer =
    bracket register unregister
  where
    register = atomically $ do
      stateVars <- newFetchClientStateVars
      modifyTVar' registry $ \m ->
        assert (peer `Map.notMember` m) $
        Map.insert peer stateVars m
      return stateVars

    unregister FetchClientStateVars{fetchClientStatusVar} =
      atomically $ do
        writeTVar fetchClientStatusVar PeerFetchStatusShutdown
        modifyTVar' registry $ \m ->
          assert (peer `Map.member` m) $
          Map.delete peer m

-- | A read-only 'STM' action to get the current 'PeerFetchStatus' for all
-- fetch clients in the 'FetchClientRegistry'.
--
readFetchClientsStatus :: MonadSTM m
                       => FetchClientRegistry peer header m
                       -> STM m (Map peer (PeerFetchStatus header))
readFetchClientsStatus (FetchClientRegistry registry) =
  readTVar registry >>= traverse (readTVar . fetchClientStatusVar)

-- | A read-only 'STM' action to get the 'FetchClientStateVars' for all fetch
-- clients in the 'FetchClientRegistry'.
--
readFetchClientsStateVars :: MonadSTM m
                          => FetchClientRegistry peer header m
                          -> STM m (Map peer (FetchClientStateVars m header))
readFetchClientsStateVars (FetchClientRegistry registry) = readTVar registry

