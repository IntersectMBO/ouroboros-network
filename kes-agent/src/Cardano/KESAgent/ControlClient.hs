{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE DeriveAnyClass #-}

-- | Clients for the KES Agent.
module Cardano.KESAgent.ControlClient
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
import Control.Exception (Exception, bracket, catch, throw)

data ControlClientOptions =
  ControlClientOptions
    { controlClientSocketAddress :: SocketAddress Unix
    }

data ControlClientTrace
  = ControlClientDriverTrace DriverTrace
  | ControlClientSocketClosed
  | ControlClientConnected (SocketAddress Unix)
  | ControlClientSendingKey
  deriving (Show)

-- | A simple control client: push one KES key, then exit.
runControlClient1 :: forall k
                   . KESSignAlgorithm IO k
                  => DirectDeserialise (SignKeyKES k)
                  => DirectSerialise (SignKeyKES k)
                  => VersionedProtocol (KESProtocol k)
                  => Proxy k
                  -> ControlClientOptions
                  -> SignKeyKES k
                  -> Tracer IO ControlClientTrace
                  -> IO ()
runControlClient1 proxy options key tracer = do
  void $ bracket
    (socket @Unix @Stream @Default)
    (\s -> do
      close s
      traceWith tracer $ ControlClientSocketClosed
    )
    (\s -> do
      connect s (controlClientSocketAddress options)
      traceWith tracer $ ControlClientConnected (controlClientSocketAddress options)
      void $ runPeerWithDriver
        (driver s $ ControlClientDriverTrace >$< tracer)
        (kesPusher (traceWith tracer ControlClientSendingKey >> return key) (return Nothing))
        ()
    )
