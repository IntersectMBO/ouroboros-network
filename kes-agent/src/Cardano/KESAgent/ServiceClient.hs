{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE ScopedTypeVariables #-}

-- | Clients for the KES Agent.
module Cardano.KESAgent.ServiceClient
where

import Cardano.KESAgent.Driver (driver, DriverTrace)
import Cardano.KESAgent.Peers (kesPusher, kesReceiver)
import Cardano.KESAgent.Protocol
import Cardano.KESAgent.Logging
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

data ServiceClientTrace
  = ServiceClientDriverTrace DriverTrace
  | ServiceClientSocketClosed
  | ServiceClientConnected (SocketAddress Unix)
  | ServiceClientReceivedKey
  deriving (Show)

runServiceClient :: forall k
                  . KESSignAlgorithm IO k
                 => DirectDeserialise (SignKeyKES k)
                 => DirectSerialise (SignKeyKES k)
                 => VersionedProtocol (KESProtocol k)
                 => Proxy k
                 -> ServiceClientOptions
                 -> (SignKeyKES k -> IO ())
                 -> Tracer IO ServiceClientTrace
                 -> IO ()
runServiceClient proxy options handleKey tracer = do
  void $ bracket
    (socket @Unix @Stream @Default)
    (\s -> do
      close s
      traceWith tracer ServiceClientSocketClosed
    )
    (\s -> do
      connect s (serviceClientSocketAddress options)
      traceWith tracer $ ServiceClientConnected (serviceClientSocketAddress options)
      void $ runPeerWithDriver
        (driver s $ ServiceClientDriverTrace >$< tracer)
        (kesReceiver $ \k -> handleKey k <* traceWith tracer ServiceClientReceivedKey)
        ()
    )
