{-# LANGUAGE DataKinds #-}

module Ouroboros.Network.Connections.Server.Inet
  ( inetServer
  ) where

import Control.Exception (IOException, bracket, mask, try)
import Control.Monad (forever)
import Data.Void (Void)
import Network.Socket (Family, Socket, SockAddr)
import qualified Network.Socket as Socket

import Ouroboros.Network.Connections.Types

-- | Creates a socket of a given family, bound to a given address, and forever
-- accepts connections, passing them to the `Connections` term via `include`.
-- That determines whether they are rejected (and immediately closed by this
-- server). Rate and resource limiting can be imposed by the `Connections`
-- term itself, and is not dealt with by this server.
--
-- TODO add a tracer parameter.
inetServer
  :: (IOException -> IO ()) -- What to do in case of exception on accept.
                            -- Re-throwing will kill the server.
  -> Family -- INET or INET6. You can run inetServer concurrently, once for each
            -- family.
  -> SockAddr -- Bind address
  -> Server SockAddr Socket reject accept IO Void
inetServer acceptException family bindaddr k = bracket
    openSocket
    closeSocket
    (acceptLoop acceptException k)
  where
  openSocket = bracket createSocket closeSocket $ \sock -> do
    Socket.bind sock bindaddr
    Socket.listen sock 1
    return sock
    
  createSocket = Socket.socket family Socket.Stream Socket.defaultProtocol

  closeSocket = Socket.close

acceptLoop
  :: (IOException -> IO ()) -- ^ Exception on `Socket.accept`.
  -> (SockAddr -> Socket -> IO () -> IO (Decision Incoming reject acceptn))
  -> Socket
  -> IO x
acceptLoop acceptException k socket =
  forever (acceptOne acceptException k socket)

-- | Accepts one connection and includes it in the `Connections` term.
acceptOne
  :: (IOException -> IO ()) -- ^ Exception on `Socket.accept`.
  -> (SockAddr -> Socket -> IO () -> IO (Decision Incoming reject accept))
  -> Socket
  -> IO ()
acceptOne acceptException k socket = mask $ \restore -> do
  acceptResult <- try (restore (Socket.accept socket))
  case acceptResult :: Either IOException (Socket, SockAddr) of
    Left ex -> restore (acceptException ex)
    Right (sock, addr) -> do
      -- OK to run under mask.
      includeResult <- k addr sock (Socket.close sock)
      -- If it was rejected, we're responsible for closing. Otherwise, there's
      -- nothing to do now; the continuation `k` has taken responsibility for
      -- that socket.
      case includeResult of
        Rejected _ -> restore (Socket.close sock)
        Accepted _ -> pure ()
