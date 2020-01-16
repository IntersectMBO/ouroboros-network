{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Exception (SomeException, throwIO)
import Control.Monad (forM)
import Data.Void (Void)
import qualified Debug.Trace as Debug (traceM)
import Network.Socket (Socket)
import qualified Network.Socket as Socket
import System.Environment (getArgs)

import Ouroboros.Network.Connections.Types
import Ouroboros.Network.Connections.Concurrent
import Ouroboros.Network.Connections.Socket.Types
import qualified Ouroboros.Network.Connections.Socket.Client as Client
import qualified Ouroboros.Network.Connections.Socket.Server as Server

-- Demo of the Connections system over sockets.
--
-- A number of nodes are created on a single host address (should use
-- localhost) with different ports. Each one brings up a concurrent
-- Connections term and then concurrently runs
-- - An accept loop into that Connections term
-- - Concurrent client connections to every address (including its own) into
--   that same Connections term
-- The continuation for that concurrent Connections term prints the local
-- and remote addresses and then sleeps for a sufficiently long time.
--
-- What we expect to see: if we have n nodes, then for each node, we should
-- see n unique print statements, one for each of the addresses. This shows
-- that connection re-use is working: whether the connection is initiated from
-- the given node or some other node, it is only brought up once.
--
-- Better way to do it: a `TVar (Map ConnectionId Word)` which is updated
-- by each connection callback to bump the count at that given ConnectionId.
-- If it passes 1, throw an error. Each thread can wait until the map is
-- 1 for every connection pair.

data Node = forall sockType . Node
  { address :: SockAddr sockType
  , server  :: Server ConnectionId Socket IO
  , client  :: SockAddr sockType -> Client ConnectionId Socket IO
  }

node :: Some SockAddr -> (Node -> IO t) -> IO t
node (Some bindAddr) k = Server.server bindAddr $ \server -> do
  let client = Client.client bindAddr
  k (Node bindAddr server client)

-- | What a node does when a new connection comes up. It'll just print a
-- debug trace (thread safe) and then wait for a very long time so that the
-- connection does not close.
withConnection :: ConnectionId -> Socket -> IO (Either () (Handler ()))
withConnection connId _socket = do
  Debug.traceM (show connId)
  pure $ Right (Handler () (threadDelay 1000000000))

-- | For each node, we want to run its accept loop, and concurrently connect
-- to every other address.
nodeAction :: [Some SockAddr] -> Node -> IO Void
nodeAction addrs (Node address server client) = concurrent withConnection $ \connections ->
  withAsync (Server.acceptLoop handleException connections server) $ \serverThread -> do
    forConcurrently_ addrs $ \addr -> case matchSockAddr address addr of
      Nothing -> error "mismatched families"
      Just peerAddr -> runClientWith connections (client peerAddr)
    -- Never finishes.
    wait serverThread
  where
  -- Any exception should kill our whole demo.
  handleException :: SomeException -> IO ()
  handleException = throwIO

-- forM but in continuation passing style.
--
--   Cont t = forall r . (t -> IO r) -> IO r
--
--   forK :: [i] -> (i -> Cont t) -> Cont [t]
--
forK :: [i] -> (i -> ((n -> IO r) -> IO r)) -> ([n] -> IO r) -> IO r
forK is mk k = go [] is mk k
  where
  go acc []     mk k = k (reverse acc)
  go acc (i:is) mk k = mk i $ \n -> go (n : acc) is mk k

main :: IO ()
main = do
  [host] <- getArgs
  -- Get addresses for the same host on a bunch of different consecutive ports.
  addrs <- forM [3000..3002] $ \port -> do
    (addrInfo : _) <- Socket.getAddrInfo Nothing (Just host) (Just (show port))
    pure (withSockType (Socket.addrAddress addrInfo))
  -- For each address, create servers and clients and package them up into
  -- `Node`s. Once the continuation is called, every node's server will be
  -- listening (but accept loops not running).
  forK addrs node $ \nodes ->
    forConcurrently_ nodes (nodeAction addrs)
  return ()
