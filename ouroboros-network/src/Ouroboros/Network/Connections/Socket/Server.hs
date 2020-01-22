{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

module Ouroboros.Network.Connections.Socket.Server
  ( server
  , acceptLoop
  , acceptLoopOn
  ) where

import Control.Exception (Exception, bracket, bracketOnError, catch, mask,
         onException)
import Control.Monad (forever, when, void)
import Data.Void (Void)
import Network.Socket (Socket)
import qualified Network.Socket as Socket

import Ouroboros.Network.Connections.Socket.Types (ConnectionId, SockAddr (..),
         makeConnectionId, matchSockType, forgetSockType)
import Ouroboros.Network.Connections.Types

-- | Creates a socket of a given family, bound to a given address, and gives
-- a Server type which can be used to accept one connection by way of
-- `runServerWith` in the `Types` module (requires giving a `Connections` term).
--
-- Can also be used with `acceptLoop` to give the typical pattern of forever
-- accepting connections, and throwing away the decisions returned by the
-- `Connections` term.
server
  :: SockAddr addrType -- Bind address
  -> (SockAddr addrType -> request Remote)
  -> (Server ConnectionId Socket IO request -> IO t)
  -- ^ When this is called, the server is up and listening. When the callback
  -- returns or dies exceptionally, the listening socket is closed.
  -> IO t
server bindaddr mkRequest k = bracket openSocket closeSocket $ \sock ->
    k (acceptOne bindaddr sock mkRequest)
  where

  -- Use bracketOnError to ensure the socket is closed if any of the preparation
  -- (setting options, bind, listen) fails. If not, the socket is returned
  -- and the caller is responsible for closing it (the bracket at the top level
  -- of this server definition).
  openSocket :: IO Socket
  openSocket = bracketOnError createSocket closeSocket $ \sock -> do
    when isInet $ do
      Socket.setSocketOption sock Socket.ReuseAddr 1
      -- SO_REUSEPORT is not available on Windows.
      -- But fortunately, SO_REUSEADDR on Windows does what SO_REUSEPORT does
      -- on BSD-like implementations.
#if !defined(mingw32_HOST_OS)
      Socket.setSocketOption sock Socket.ReusePort 1
#endif
    when isInet6 $ Socket.setSocketOption sock Socket.IPv6Only 1
    Socket.bind sock (forgetSockType bindaddr)
    Socket.listen sock 1
    return sock

  createSocket :: IO Socket
  createSocket = Socket.socket family Socket.Stream Socket.defaultProtocol

  closeSocket :: Socket -> IO ()
  closeSocket = Socket.close

  isInet, isInet6 :: Bool
  family :: Socket.Family
  (isInet, isInet6, family) = case bindaddr of
    SockAddrIPv4 _ _     -> (True,  False, Socket.AF_INET)
    SockAddrIPv6 _ _ _ _ -> (True,  True,  Socket.AF_INET6)
    SockAddrUnix _       -> (False, False, Socket.AF_UNIX)

-- | Accepts one connection and include it, according to the parameter given
-- (see the `Server` type synonym).
-- Any exceptions thrown by accept will be re-thrown here, so be sure to
-- handle them.
acceptOne
  :: SockAddr addrType -- Bind address; needed to construct ConnectionId
  -> Socket
  -> (SockAddr addrType -> request Remote)
  -- ^ From the incoming socket address you must construct a remote request type
  -> Server ConnectionId Socket IO request
acceptOne bindaddr socket mkRequest = \includeConnection -> mask $ \restore -> do
  (sock, addr) <- restore (Socket.accept socket)
  case matchSockType bindaddr addr of
    -- Should never happen.
    Nothing -> error "mismatched socket address types"
    -- In this case, matchSockType has shown us that bindaddr and peeraddr
    -- have the same sockType parameter, so we can make a connection ID for
    -- the pair.
    Just peeraddr -> do
      -- Note the order of the addresses in the identifier: we always put our
      -- bind address first. The client (sibling module Client.hs) also puts
      -- the bind address first.
      let connId = makeConnectionId bindaddr peeraddr
      -- Including the connection could fail exceptionally, in which case we
      -- are still responsible for closing the socket.
      includeResult <- restore (includeConnection connId sock (Socket.close sock) (mkRequest peeraddr))
                       `onException`
                       Socket.close sock
      -- If it was rejected, we're responsible for closing. Otherwise, there's
      -- nothing to do now; the continuation `k` has taken responsibility for
      -- that socket.
      case includeResult of
        Rejected _ -> restore (Socket.close sock)
        Accepted _ -> pure ()
      pure includeResult

-- | A common pattern: accept in a loop, passing each connection through a
-- Connections term, and handling exceptions without necessarily dying.
-- The decision given by the Connections term is ignored.
--
-- Be prudent in choosing what to do in the exception handler.
-- Async exceptions should be re-thrown.
acceptLoop
  :: ( Exception e )
  => (e -> IO ())
  -> Connections ConnectionId Socket request accept reject IO
  -> Server ConnectionId Socket IO request
  -> IO Void
acceptLoop handleException connections accept = forever $
  void (runServerWith connections accept) `catch` handleException

-- | A convenient (and hopefully user-friendly) way to bring up a server and run
-- an accept loop on it. It creates a socket at the given address and runs an
-- accept loop with a given `Connections` term.
--
-- Here's an example of running concurrent IPv4 and IPv6 servers on a common
-- connection manager, using a request type to distinguish them (not necessary)
--
-- @
--   data Request p where
--     IPv6Request :: SockAddr IPv6 -> Request p
--     IPv4Request :: SockAddr IPv4 -> Request p
--
--   runServers connections = do
--     (sockAddr4, sockAddr6) <- resolveSocketAddresses ...
--     (void1, void2) <- Async.concurrently
--       (acceptLoopOn sockAddr4 IPv4Request handleException connections)
--       (acceptLoopOn sockAddr6 IPv6Request handleException connections)
--     absurd void1
--     where
--     handleException :: IOException -> IO ()
--     handleException _ = pure ()
-- @
acceptLoopOn
  :: ( Exception e )
  => SockAddr sockType
  -> (SockAddr sockType -> request Remote)
  -> (e -> IO ()) -- ^ Exception handling. Should not squelch async exceptions.
                  -- Probably should recover from IOExceptions such as no more
                  -- file descriptors. Re-throwing will bring the server accept
                  -- loop down.
  -> Connections ConnectionId Socket request accept reject IO
  -> IO Void
acceptLoopOn bindAddr mkRequest handleException connections =
  server bindAddr mkRequest $ \serv ->
    acceptLoop handleException connections serv
