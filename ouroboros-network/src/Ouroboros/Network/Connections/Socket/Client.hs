{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}

module Ouroboros.Network.Connections.Socket.Client
  ( client
  ) where

import Control.Exception (bracket)
import Control.Monad (when)
import Network.Socket (Socket)
import qualified Network.Socket as Socket

import Ouroboros.Network.Connections.Socket.Types (SockAddr (..), forgetSockType)
import Ouroboros.Network.Connections.Types

-- | Given an address to bind to, this client will attempt to connect to the
-- other given address, and then include the socket in the `Connections`.
--
-- A special SockAddr GADT is used to ensure you can't try to connect
-- incompatible addresses.
client
  :: SockAddr sockType -- Our address (bind to this).
  -> SockAddr sockType -- Remote address (connect to this).
  -> Client Socket.SockAddr Socket reject accept IO (Decision Outgoing reject accept)
client bindaddr sockaddr k = k (forgetSockType sockaddr) openSocket closeSocket

  where

  -- The Connections term is expected to take care of exception handling to
  -- ensure closeSocket is always called. But we need to do bracketing even
  -- here within openSocket, in case bind or connect fails
  openSocket :: IO Socket
  openSocket = bracket createSocket closeSocket $ \sock -> do
    when isInet $ do
      Socket.setSocketOption sock Socket.ReuseAddr 1
#if !defined(mingw32_HOST_OS)
      Socket.setSocketOption sock Socket.ReusePort 1
#endif
    when isInet6 $ Socket.setSocketOption sock Socket.IPv6Only 1
    when isInet  $ Socket.bind sock (forgetSockType bindaddr)
    Socket.connect sock (forgetSockType sockaddr)
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
