{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

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

data Node = forall sockType . Node
  { address :: SockAddr sockType
  , server  :: Server ConnectionId Socket IO
  , client  :: SockAddr sockType -> Client ConnectionId Socket IO
  }

-- | When the continuation goes, there is a socket listening.
node :: Some SockAddr -> (Node -> IO t) -> IO t
node addr@(Some bindAddr) k = Server.server addr $ \server -> do
  let client = Client.client bindAddr
  k (Node bindAddr server client)

-- | What a node does when a new connection comes up. It'll just print a
-- debug trace (thread safe) and then wait for a very long time so that the
-- connection does not close.
withConnection
  :: Provenance
  -> ConnectionId
  -> Socket
  -> IO (Connection.Decision () ())
withConnection provenance connId _socket = do
  Debug.traceM $ mconcat [show provenance, " : ", show connId]
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
              unless (peerAddr == address) $ 
                void (runClientWith connections (client peerAddr))
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
    pure (withSockType (Socket.addrAddress addrInfo))
  -- For each address, create servers and clients and package them up into
  -- `Node`s. Once the continuation is called, every node's server will be
  -- listening (but accept loops not running).
  forContinuation addrs node $ \nodes ->
    forConcurrently_ nodes (nodeAction addrs)
  return ()
