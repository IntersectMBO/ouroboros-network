module Ouroboros.Network.Connections.Client.Inet
  ( inetClient
  ) where

import Control.Exception (mask, onException)
import Network.Socket (Family, Socket, SockAddr)
import qualified Network.Socket as Socket

import Ouroboros.Network.Connections.Types

-- | Given an address to bind to, this client will attempt to connect to the
-- other given address, and then include the socket in the `Connections`.
inetClient
  :: Connections (Socket, SockAddr) reject conn IO
  -> Family   -- AF_INET or AF_INET6
  -> SockAddr -- Our address (bind to this).
  -> Client SockAddr reject conn IO
inetClient connections family bindaddr = \sockaddr -> mask $ \restore -> do
  sock <- restore $ Socket.socket family Socket.Stream Socket.defaultProtocol
  let bindAndConnect = do
        Socket.bind sock bindaddr
        Socket.connect sock sockaddr
  restore bindAndConnect `onException` Socket.close sock
  -- It's OK to do this under mask; should just be some STM.
  result <- include connections Outgoing (sock, sockaddr)
  -- Must close the socket if it was rejected.
  -- If it was accepted, the `Connections` is now responsible for closing it.
  case result of
    Rejected _ -> restore (Socket.close sock)
    Accepted _ -> pure ()
  return result
