{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Ouroboros.Network.Connections.Concurrent
  ( concurrent
  ) where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception (assert, bracket, finally, mask_)
import Control.Monad (forM_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Network.Socket (Socket, SockAddr)

import Ouroboros.Network.Connections.Types

data Reject (p :: Provenance) = Duplicate

data Accept (p :: Provenance) = Accept

data Connection
  = Remote (Async ())
  | Local (Async ())
  -- | First is remote, second is local
  | Both (Async ()) (Async ())

-- | Simple concurrent connection manager as a proof of concept.
-- It maintains at most 2 connections for each SockAddr: one outgoing and
-- one incoming. Rejects new ones only if there's already one known to it.
concurrent
  :: (Socket -> SockAddr -> IO ())
  -> (Connections SockAddr Socket Reject Accept IO -> IO t)
  -> IO t
concurrent withSocket k = do
  -- Cannot use an IORef for state because spawned threads will modify this
  -- when they finish.
  state :: MVar (Map SockAddr Connection) <- newMVar Map.empty
  let connections = Connections { include = includeOne state }
  k connections `finally` killThreads state

  where

  spawnIncoming
    :: MVar (Map SockAddr Connection)
    -> SockAddr
    -> Socket
    -> IO ()
    -> IO (Async ())
  spawnIncoming state addr sock closeSock = do
    rec let cleanup = do
              closeSock
              removeIncoming state addr async
        async <- asyncWithUnmask $ \unmask ->
          unmask (withSocket sock addr) `finally` cleanup
    return async

  -- When a spawned thread finishes, it removes itself from the shared state.
  -- See `spawnOne` where this is used in a `finally` continuation.
  removeIncoming
    :: MVar (Map SockAddr Connection)
    -> SockAddr
    -> Async ()
    -> IO ()
  removeIncoming state addr x = modifyMVar state $ \cmap -> do
    let alteration mentry = case mentry of
          -- Error cases would all be bugs in this implementation.
          Nothing -> error "entry vanished"
          Just (Local _) -> error "mismatched entry"
          Just (Remote y) -> assert (x == y) Nothing
          Just (Both y z) -> assert (x == y) (Just (Local z))
        !cmap' = Map.alter alteration addr cmap
    pure (cmap', ())

  -- FIXME should open the socket in this thread and reject if it fails?
  spawnOutgoing
    :: MVar (Map SockAddr Connection)
    -> SockAddr
    -> IO Socket
    -> (Socket -> IO ())
    -> IO (Async ())
  spawnOutgoing state addr openSock closeSock = do
    rec let cleanup = \sock -> do
              closeSock sock
              removeOutgoing state addr async
        async <- asyncWithUnmask $ \unmask ->
          bracket openSock cleanup (\sock -> unmask (withSocket sock addr))
    return async

  removeOutgoing
    :: MVar (Map SockAddr Connection)
    -> SockAddr
    -> Async ()
    -> IO ()
  removeOutgoing state addr x = modifyMVar state $ \cmap -> do
    let alteration mentry = case mentry of
          -- Error cases would all be bugs in this implementation.
          Nothing -> error "entry vanished"
          Just (Remote _) -> error "mismatched entry"
          Just (Local y) -> assert (x == y) Nothing
          Just (Both y z) -> assert (x == z) (Just (Remote y))
        !cmap' = Map.alter alteration addr cmap
    pure (cmap', ())

  includeOne
    :: MVar (Map SockAddr Connection)
    -> SockAddr
    -> Resource provenance IO Socket
    -> IO (Decision provenance Reject Accept)
  includeOne state addr resource = mask_ $ modifyMVar state $ \cmap -> case resource of
    Existing sock closeSock -> case Map.lookup addr cmap of
      Just (Remote _) -> pure (cmap, Rejected Duplicate)
      Just (Both _ _) -> pure (cmap, Rejected Duplicate)
      Just (Local outgoing) -> do
        incoming <- spawnIncoming state addr sock closeSock
        let !cmap' = Map.insert addr (Both incoming outgoing) cmap
        pure (cmap', Accepted Accept)
      Nothing -> do
        incoming <- spawnIncoming state addr sock closeSock
        let !cmap' = Map.insert addr (Remote incoming) cmap
        pure (cmap', Accepted Accept)
    New openSock closeSock -> case Map.lookup addr cmap of
      Just (Local _) -> pure (cmap, Rejected Duplicate)
      Just (Both _ _) -> pure (cmap, Rejected Duplicate)
      Just (Remote incoming) -> do
        outgoing <- spawnOutgoing state addr openSock closeSock
        let !cmap' = Map.insert addr (Both incoming outgoing) cmap
        pure (cmap', Accepted Accept)
      Nothing -> do
        outgoing <- spawnOutgoing state addr openSock closeSock
        let !cmap' = Map.insert addr (Local outgoing) cmap
        pure (cmap', Accepted Accept)

  killThreads :: MVar (Map SockAddr Connection) -> IO ()
  killThreads state = withMVar state $ \cmap ->
    forM_ (Map.toList cmap) $ \(_, conn) -> case conn of
      Remote x   -> cancel x
      Local  x   -> cancel x
      Both   x y -> cancel x >> cancel y
