{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Exception (IOException, SomeException, catch, throwIO)
import Control.Monad (forM, unless, void)
import Data.Void (Void, absurd)
import Data.Word (Word)
import qualified Debug.Trace as Debug (traceM)
import Network.Socket (Socket)
import qualified Network.Socket as Socket
import System.Environment (getArgs)
import System.Random (randomRIO)

import Ouroboros.Network.Connections.Types
import Ouroboros.Network.Connections.Util (forContinuation)
import Ouroboros.Network.Connections.Concurrent as Connection
import Ouroboros.Network.Connections.Socket.Types
import qualified Ouroboros.Network.Connections.Socket.Client as Client
import qualified Ouroboros.Network.Connections.Socket.Server as Server

import System.IO.Error (isAlreadyExistsError, isAlreadyInUseError, ioeGetErrorType)

-- Demo of the Connections system over sockets.
--
-- A number of nodes are created on a single host address (should use
-- localhost) with different ports. Each one brings up a concurrent
-- Connections term and then concurrently runs
-- - An accept loop into that Connections term
-- - Concurrent client connections to every address (except its own) into
--   that same Connections term
-- The continuation for that concurrent Connections term prints the local
-- and remote addresses and then sleeps for a sufficiently long time.
--
-- What we expect to see: if we have n nodes, then for each node, we should
-- see n unique debug statements, one for each of the addresses. This shows
-- that connection re-use is working: whether the connection is initiated from
-- the given node or some other node, it is only brought up once.
--
-- TODO Better way to do it: a `TVar (Map ConnectionId Word)` which is updated
-- by each connection callback to bump the count at that given ConnectionId.
-- If it passes 1, throw an error. Each thread can wait until the map is
-- 1 for every connection pair.
--
-- TODO problem with this demo: since it runs over one common TCP/IP stack, and
-- since we bind connecting sockets to a particular port, there is an inevitable
-- bug. When a connection is created from port A to B, we'll have:
-- - Thread `connect`ing establishes socket A:B.
-- - Thread `accept`ing for B establishes socket B:A
-- But, concurrently, the same thing happens for B to A.
-- There is always some interval of time between returning from `accept`, and
-- updating the shared state in the `Connections` term, during which the
-- program may attempt to create a _new_ socket B:A when the accept loop has
-- already got hold of one but not yet put it into the map.
--
-- In theory this could even happen between two TCP/IP stacks on different
-- machines, but it's highly unlikely.

data Request (provenance :: Provenance) where
  Request :: Request provenance

data Node = forall sockType . Node
  { address :: SockAddr sockType
  , server  :: Server ConnectionId Socket IO Request
  , client  :: SockAddr sockType -> Client ConnectionId Socket IO Request
  }

-- | When the continuation goes, there is a socket listening.
node :: Some SockAddr -> (Node -> IO t) -> IO t
node (Some (bindAddr :: SockAddr sockType)) k =
  Server.server bindAddr (const Request) $ \_boundAddr server -> do
    -- Type sig is required; GHC struggles with the higher-rank type (Client
    -- has foralls).
    let client :: SockAddr sockType -> Client ConnectionId Socket IO Request
        client = \remoteAddr -> Client.client (makeConnectionId bindAddr remoteAddr) Request
    k (Node bindAddr server client)

-- | What a node does when a new connection comes up. It'll just print a
-- debug trace (thread safe) and then wait for a very long time so that the
-- connection does not close.
withConnection
  :: Initiated provenance
  -> ConnectionId
  -> Socket
  -> Request provenance
  -> IO (Connection.Decision IO provenance CannotReject ())
withConnection initiated connId _socket Request = do
  Debug.traceM $ mconcat [show initiated, " : ", show connId]
  pure $ Connection.Accept $ \_ -> pure (Handler () (threadDelay 1000000000))

-- | For each node, we want to run its accept loop, and concurrently connect
-- to every other address.
nodeAction :: [Some SockAddr] -> Node -> IO ()
nodeAction addrs (Node address server client) = concurrent withConnection $ \connections -> do
  -- Run the server accept loop and the client threads using forConcurrently, so
  -- that exceptions from any of them will arise here and kill the entire
  -- program.
  let threads = concat
        [ [ fmap absurd (Server.acceptLoop (handleException "server") connections server) ]
        , flip fmap addrs $ \addr -> case matchSockAddr address addr of
            Nothing -> error "mismatched families"
            Just peerAddr ->
              -- Connecting to the bind address is problematic: the accept loop
              -- has already bound to it, and the client will bind to it as
              -- well, which seems to result in a deadlock.
              --
              -- Randomly delay to avoid the address in use problem describes
              -- at the top of this file.
              unless (peerAddr == address) $ 
                (randomRIO (1000, 1000000) >>= \t -> threadDelay t >> void (runClientWith connections (client peerAddr)))
                `catch`
                handleException ("client " ++ show peerAddr)
        ]
  forConcurrently_ threads id
  where
  -- Print the exception, prefixed with the address of the server which threw
  -- it.
  handleException :: String -> IOException -> IO ()
  handleException str e = do
    putStrLn $ mconcat
      [ str
      , " : "
      , show address
      , " : "
      , show (ioeGetErrorType e)
      , " : "
      , show e
      , "\n"
      ]
    throwIO e

main :: IO ()
main = do
  [host, portString, howManyString] <- getArgs >>= \args -> case args of
    [host, portString, howManyString] -> pure args
    _ -> error "args: <host> <port> <positive integer>"
  let port :: Word
      port = case reads portString of
        [(n, "")] -> n
        _ -> error "args: <host> <port> <positive integer>"
      howMany :: Word
      howMany = case reads howManyString of
        [(n, "")] -> n
        _ -> error "args: <host> <port> <positive integer>"
      range :: [Int]
      range = fmap fromIntegral [port..(port + howMany - 1)]
  -- Get addresses for the same host on a bunch of different consecutive ports.
  addrs <- forM range $ \port -> do
    (addrInfo : _) <- Socket.getAddrInfo Nothing (Just host) (Just (show port))
    pure (someSockType (Socket.addrAddress addrInfo))
  -- For each address, create servers and clients and package them up into
  -- `Node`s. Once the continuation is called, every node's server will be
  -- listening (but accept loops not running).
  forContinuation addrs node $ \nodes ->
    forConcurrently_ nodes (nodeAction addrs)
  return ()
