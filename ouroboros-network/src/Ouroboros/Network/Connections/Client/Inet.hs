{-# LANGUAGE DataKinds #-}

module Ouroboros.Network.Connections.Client.Inet
  ( inetClient
  ) where

import Control.Exception (bracket)
import Network.Socket (Family, Socket, SockAddr)
import qualified Network.Socket as Socket

import Ouroboros.Network.Connections.Types

-- | Given an address to bind to, this client will attempt to connect to the
-- other given address, and then include the socket in the `Connections`.
inetClient
  :: Family   -- AF_INET or AF_INET6
  -> SockAddr -- Our address (bind to this).
  -> SockAddr -- Remote address (connect to this).
  -> Client SockAddr Socket reject accept IO (Decision Outgoing reject accept)
inetClient family bindaddr sockaddr k = k sockaddr openSocket closeSocket

  where

  -- The Connections term is expected to take care of exception handling to
  -- ensure closeSocket is always called. But we need to do bracketing even
  -- here within openSocket, in case bind or connect fails
  openSocket :: IO Socket
  openSocket = bracket createSocket closeSocket $ \sock -> do
    Socket.bind sock bindaddr
    Socket.connect sock sockaddr
    return sock

  closeSocket :: Socket -> IO ()
  closeSocket = Socket.close

  createSocket :: IO Socket
  createSocket = Socket.socket family Socket.Stream Socket.defaultProtocol
