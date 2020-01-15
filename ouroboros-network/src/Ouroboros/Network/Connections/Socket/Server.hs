{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}

module Ouroboros.Network.Connections.Socket.Server
  ( server
  ) where

import Control.Exception (IOException, bracket, mask, onException, try)
import Control.Monad (forever, when)
import Data.Void (Void)
import Network.Socket (Socket)
import qualified Network.Socket as Socket

import Ouroboros.Network.Connections.Socket.Types (ConnectionId, SockAddr (..),
         makeConnectionId, matchSockType, forgetSockType)
import Ouroboros.Network.Connections.Types

-- | Creates a socket of a given family, bound to a given address, and forever
-- accepts connections, passing them to the `Connections` term via `include`.
-- That determines whether they are rejected (and immediately closed by this
-- server). Rate and resource limiting can be imposed by the `Connections`
-- term itself, or by the OS/firewall; it is not dealt with by this server.
--
-- TODO add a tracer parameter.
server
  :: (IOException -> IO ()) -- What to do in case of exception on accept.
                            -- Re-throwing will kill the server.
  -> SockAddr sockType      -- Bind address
  -> Server ConnectionId Socket reject accept IO Void
server acceptException bindaddr k = bracket
    openSocket
    closeSocket
    (acceptLoop acceptException k bindaddr)
  where
  openSocket = bracket createSocket closeSocket $ \sock -> do
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

acceptLoop
  :: (IOException -> IO ()) -- ^ Exception on `Socket.accept`.
  -> (ConnectionId -> Socket -> IO () -> IO (Decision Incoming reject acceptn))
  -> SockAddr sockType -- Bind address; needed to construct ConnectionId
  -> Socket
  -> IO x
acceptLoop acceptException k bindaddr socket =
  forever (acceptOne acceptException k bindaddr socket)

-- | Accepts one connection and includes it in the `Connections` term.
acceptOne
  :: (IOException -> IO ()) -- ^ Exception on `Socket.accept`.
  -> (ConnectionId -> Socket -> IO () -> IO (Decision Incoming reject accept))
  -> SockAddr sockType -- Bind address; needed to construct ConnectionId
  -> Socket
  -> IO ()
acceptOne acceptException k bindaddr socket = mask $ \restore -> do
  acceptResult <- try (restore (Socket.accept socket))
  case acceptResult :: Either IOException (Socket, Socket.SockAddr) of
    Left ex -> restore (acceptException ex)
    Right (sock, addr) -> case matchSockType bindaddr addr of
      Nothing -> error "mismatched socket address types"
      Just peeraddr -> do
        let connId = makeConnectionId bindaddr peeraddr
        -- Including the connection could fail exceptionally, in which case we
        -- are still responsible for closing the socket.
        includeResult <- restore (k connId sock (Socket.close sock))
                         `onException`
                         Socket.close sock
        -- If it was rejected, we're responsible for closing. Otherwise, there's
        -- nothing to do now; the continuation `k` has taken responsibility for
        -- that socket.
        case includeResult of
          Rejected _ -> restore (Socket.close sock)
          Accepted _ -> pure ()
