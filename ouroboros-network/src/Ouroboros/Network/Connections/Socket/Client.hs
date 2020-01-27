{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

module Ouroboros.Network.Connections.Socket.Client
  ( client
  ) where

import Control.Exception (bracketOnError)
import Control.Monad (when)
import Network.Socket (Socket)
import qualified Network.Socket as Socket

import Ouroboros.Network.Connections.Socket.Types (ConnectionId (..),
         forgetSockType)
import Ouroboros.Network.Connections.Types

-- | Given an address to bind to, this client will attempt to connect to the
-- other given address, and then include the socket in the `Connections`.
--
-- A special SockAddr GADT is used to ensure you can't try to connect
-- incompatible addresses.
client
  :: ConnectionId -- ^ First component is the bind address, second the remote.
  -> request Local
  -> Client ConnectionId Socket IO request
client connid request = \k ->
  k connid openSocket closeSocket request

  where

  bindaddr :: Socket.SockAddr
  sockaddr :: Socket.SockAddr
  isInet, isInet6 :: Bool
  family :: Socket.Family
  (bindaddr, sockaddr, isInet, isInet6, family) = case connid of
    ConnectionIdIPv6 bind conn ->
      (forgetSockType bind, forgetSockType conn, True, True, Socket.AF_INET6)
    ConnectionIdIPv4 bind conn ->
      (forgetSockType bind, forgetSockType conn, True, False, Socket.AF_INET)
    ConnectionIdUnix bind conn ->
      (forgetSockType bind, forgetSockType conn, False, False, Socket.AF_UNIX)

  -- The Connections term is expected to take care of exception handling to
  -- ensure closeSocket is always called. But we need to do bracketing even
  -- here within openSocket, in case bind or connect fails.
  openSocket :: IO Socket
  openSocket = bracketOnError createSocket closeSocket $ \sock -> do
    when isInet $ do
      Socket.setSocketOption sock Socket.ReuseAddr 1
#if !defined(mingw32_HOST_OS)
      Socket.setSocketOption sock Socket.ReusePort 1
#endif
    when isInet6 $ Socket.setSocketOption sock Socket.IPv6Only 1
    Socket.bind sock bindaddr
    Socket.connect sock sockaddr
    return sock

  createSocket :: IO Socket
  createSocket = Socket.socket family Socket.Stream Socket.defaultProtocol

  closeSocket :: Socket -> IO ()
  closeSocket = Socket.close
