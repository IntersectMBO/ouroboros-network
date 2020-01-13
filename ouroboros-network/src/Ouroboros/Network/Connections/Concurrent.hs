{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE BangPatterns #-}

module Ouroboros.Network.Connections.Concurrent
  ( concurrent
  ) where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception (assert, finally, mask_)
import Control.Monad (forM_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Network.Socket (Socket, SockAddr)
import qualified Network.Socket as Socket

import Ouroboros.Network.Connections.Types

data Reject = Duplicate

data Accept = Accept

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
  -> (Connections (Socket, SockAddr) Reject Accept IO -> IO t)
  -> IO t
concurrent withSocket k = do
  -- Cannot use an IORef for state because spawned threads will modify this
  -- when they finish.
  state :: MVar (Map SockAddr Connection) <- newMVar Map.empty
  let connections = Connections { include = includeOne state }
  k connections `finally` killThreads state

  where

  spawnOne
    :: MVar (Map SockAddr Connection)
    -> Provenance
    -> Socket
    -> SockAddr
    -> IO (Async ())
  spawnOne state provenance sock addr = do
    rec let cleanup = do
              Socket.close sock
              removeOne state provenance addr async
        async <- asyncWithUnmask $ \unmask ->
          unmask (withSocket sock addr) `finally` cleanup
    return async

  -- When a spawned thread finishes, it removes itself from the shared state.
  -- See `spawnOne` where this is used in a `finally` continuation.
  removeOne
    :: MVar (Map SockAddr Connection)
    -> Provenance
    -> SockAddr
    -> Async ()
    -> IO ()
  removeOne state provenance addr x = modifyMVar state $ \cmap -> do
    let alteration mentry = case (provenance, mentry) of
          -- Error cases would all be bugs in this implementation.
          (_, Nothing) -> error "entry vanished"
          (Incoming, Just (Local _)) -> error "mismatched entry"
          (Outgoing, Just (Remote _)) -> error "mismatched entry"
          (Incoming, Just (Remote y)) -> assert (x == y) Nothing
          (Outgoing, Just (Local y)) -> assert (x == y) Nothing
          (Incoming, Just (Both y z)) -> assert (x == y) (Just (Local z))
          (Outgoing, Just (Both y z)) -> assert (x == z) (Just (Remote y))
        !cmap' = Map.alter alteration addr cmap
    pure (cmap', ())


  includeOne
    :: MVar (Map SockAddr Connection)
    -> Provenance
    -> (Socket, SockAddr)
    -> IO (Decision Reject Accept)
  includeOne state provenance (sock, addr) = mask_ $ modifyMVar state $ \cmap -> do
    case (provenance, Map.lookup addr cmap) of
      -- NB: it's possible to send a message on the socket at this point, in
      -- case we wanted to inform the other end with some error code.
      (Incoming, Just (Remote _)) -> pure (cmap, Rejected Duplicate)
      (Outgoing, Just (Local _))  -> pure (cmap, Rejected Duplicate)
      (Incoming, Just (Both _ _)) -> pure (cmap, Rejected Duplicate)
      (Outgoing, Just (Both _ _)) -> pure (cmap, Rejected Duplicate)
      (Incoming, Just (Local y)) -> do
        x <- spawnOne state Incoming sock addr
        let !cmap' = Map.insert addr (Both x y) cmap
        pure (cmap', Accepted Accept)
      (Outgoing, Just (Remote x)) -> do
        y <- spawnOne state Outgoing sock addr
        let !cmap' = Map.insert addr (Both x y) cmap
        pure (cmap', Accepted Accept)
      (Incoming, Nothing) -> do
        x <- spawnOne state Incoming sock addr
        let !cmap' = Map.insert addr (Local x) cmap
        pure (cmap', Accepted Accept)
      (Outgoing, Nothing) -> do
        x <- spawnOne state Outgoing sock addr
        let !cmap' = Map.insert addr (Remote x) cmap
        pure (cmap', Accepted Accept)

  killThreads :: MVar (Map SockAddr Connection) -> IO ()
  killThreads state = withMVar state $ \cmap ->
    forM_ (Map.toList cmap) $ \(_, conn) -> case conn of
      Remote x   -> cancel x
      Local  x   -> cancel x
      Both   x y -> cancel x >> cancel y
