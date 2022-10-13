{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE ScopedTypeVariables #-}

-- | Clients for the KES Agent.
module Cardano.KESAgent.ServiceClient
where

import Cardano.KESAgent.Driver (driver)
import Cardano.KESAgent.Peers (kesPusher, kesReceiver)
import Cardano.KESAgent.Protocol
import Cardano.Crypto.DirectSerialise

import Cardano.Crypto.KES.Class

import System.Socket (socket, SocketException, close, connect)
import System.Socket.Type.Stream
import System.Socket.Family.Unix
import System.Socket.Protocol.Default
import Network.TypedProtocol.Driver (runPeerWithDriver)
import Data.Proxy (Proxy (..))

import Control.Monad (forever, void)
import Control.Exception (bracket, catch)

data ServiceClientOptions =
  ServiceClientOptions
    { serviceClientSocketAddress :: SocketAddress Unix
    }

runServiceClient :: forall k
                  . KESSignAlgorithm IO k
                 => DirectDeserialise (SignKeyKES k)
                 => DirectSerialise (SignKeyKES k)
                 => VersionedProtocol (KESProtocol k)
                 => Proxy k
                 -> ServiceClientOptions
                 -> (SignKeyKES k -> IO ())
                 -> IO ()
runServiceClient proxy options handleKey = do
  void $ bracket
    (socket @Unix @Stream @Default)
    (\s -> do
      close s
      putStrLn "SERVICE_CLIENT: socket closed"
    )
    (\s -> do
      putStrLn $ "SERVICE_CLIENT: starting..."
      connect s (serviceClientSocketAddress options)
      putStrLn $ "SERVICE_CLIENT: connected to " ++ show (serviceClientSocketAddress options)
      void $ runPeerWithDriver
        (driver s)
        (kesReceiver handleKey)
        ()
    )
