{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

module Ouroboros.Network.Connections.Socket.Server
  ( server
  , acceptLoop
  ) where

import Control.Exception (Exception, bracket, catch, mask, onException)
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
  :: SockAddr sockType -- Bind address
  -> (Server ConnectionId Socket IO -> IO t)
  -- ^ When this is called, the server is up and listening. When the callback
  -- returns or dies exceptionally, the listening socket is closed.
  -> IO t
server bindaddr k = bracket
    openSocket
    closeSocket
    (\sock -> k (acceptOne bindaddr sock))
  where

  openSocket :: IO Socket
  openSocket = mask $ \restore -> do
    sock <- restore createSocket
    restore (prepareSocket sock) `onException` closeSocket sock
    return sock

  createSocket :: IO Socket
  createSocket = Socket.socket family Socket.Stream Socket.defaultProtocol

  prepareSocket :: Socket -> IO ()
  prepareSocket sock = do
    when isInet $ do
      Socket.setSocketOption sock Socket.ReuseAddr 1
      -- SO_REUSEPORT is not available on Windows.
      -- But fortunately, SO_REUSEADDR on Windows does what SO_REUSEPORT does
      -- on BSD-like implementations.
#if !defined(mingw32_HOST_OS)
      Socket.setSocketOption sock Socket.ReusePort 1
#endif
    when isInet6 $ Socket.setSocketOption sock Socket.IPv6Only 1
    when isInet  $ do
      Socket.bind sock (forgetSockType bindaddr)
      Socket.listen sock 1

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
  :: SockAddr sockType -- Bind address; needed to construct ConnectionId
  -> Socket
  -> Server ConnectionId Socket IO
acceptOne bindaddr socket = \includeConnection -> mask $ \restore -> do
  (sock, addr) <- restore (Socket.accept socket)
  case matchSockType bindaddr addr of
    -- Should never happen.
    Nothing -> error "mismatched socket address types"
    Just peeraddr -> do
      let connId = makeConnectionId bindaddr peeraddr
      -- Including the connection could fail exceptionally, in which case we
      -- are still responsible for closing the socket.
      includeResult <- restore (includeConnection connId sock (Socket.close sock))
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
  -> Connections ConnectionId Socket accept reject IO
  -> Server ConnectionId Socket IO
  -> IO Void
acceptLoop handleException connections accept = forever $
  void (runServerWith connections accept) `catch` handleException
