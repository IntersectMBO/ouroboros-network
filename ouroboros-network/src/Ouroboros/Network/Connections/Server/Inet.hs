{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}

-- `accept` is shadowed, but so what?
{-# OPTIONS_GHC "-fno-warn-name-shadowing" #-}

module Ouroboros.Network.Connections.Server.Inet
  ( inetServer
  ) where

import Control.Exception (IOException, bracket, mask, try)
import Control.Monad (forever)
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
  :: Connections (Socket, SockAddr) reject conn IO
  -> (IOException -> IO ()) -- What to do in case of exception on accept.
                            -- Re-throwing will kill the server.
  -> Family -- INET or INET6. You can run inetServer concurrently, once for each
            -- family.
  -> SockAddr -- Bind address
  -> Server IO
inetServer connections acceptException family bindaddr = bracket
    openSocket
    closeSocket
    (acceptLoop connections acceptException)
  where
  openSocket = do
    sock <- Socket.socket family Socket.Stream Socket.defaultProtocol
    Socket.bind sock bindaddr
    Socket.listen sock 1
    return sock
  closeSocket = Socket.close

acceptLoop
  :: Connections (Socket, SockAddr) reject conn IO
  -> (IOException -> IO ()) -- ^ Exception on `Socket.accept`.
  -> Socket
  -> IO x
acceptLoop connections acceptException socket =
  forever (acceptOne connections acceptException socket)

-- | Accepts one connection and includes it in the `Connections` term.
acceptOne
  :: Connections (Socket, SockAddr) reject conn IO
  -> (IOException -> IO ()) -- ^ Exception on `Socket.accept`.
  -> Socket
  -> IO ()
acceptOne connections acceptException socket = mask $ \restore -> do
  acceptResult <- try (restore (Socket.accept socket))
  case acceptResult :: Either IOException (Socket, SockAddr) of
    Left ex -> restore (acceptException ex)
    Right (sock, addr) -> do
      -- OK to run under mask.
      includeResult <- include connections Incoming (sock, addr)
      -- If it was rejected, we're responsible for closing. Otherwise, there's
      -- nothing to do now; the `Connections` is responsible for that socket.
      case includeResult of
        Rejected _ -> restore (Socket.close sock)
        Accepted _ -> pure ()
