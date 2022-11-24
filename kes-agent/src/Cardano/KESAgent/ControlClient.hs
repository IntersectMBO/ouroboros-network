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
import Cardano.KESAgent.OCert
import Cardano.KESAgent.RetrySocket
import Cardano.KESAgent.RefCounting

import Cardano.Crypto.DirectSerialise
import Cardano.Crypto.KES.Class
import Cardano.Binary

import System.Socket (socket, SocketException, close, connect)
import System.Socket.Type.Stream
import System.Socket.Family.Unix
import System.Socket.Protocol.Default
import Network.TypedProtocol.Driver (runPeerWithDriver)
import Data.Proxy (Proxy (..))
import Data.Typeable

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
  | ControlClientAttemptReconnect Int
  | ControlClientSendingKey
  deriving (Show)

-- | A simple control client: push one KES key, then exit.
runControlClient1 :: forall c
                   . Crypto c
                  => Typeable c
                  => KESSignAlgorithm IO (KES c)
                  => DirectDeserialise (SignKeyKES (KES c))
                  => DirectSerialise (SignKeyKES (KES c))
                  => VersionedProtocol (KESProtocol c)
                  => Proxy c
                  -> ControlClientOptions
                  -> CRef (SignKeyWithPeriodKES (KES c))
                  -> OCert c
                  -> Tracer IO ControlClientTrace
                  -> IO ()
runControlClient1 proxy options key oc tracer = do
  void $ bracket
    (socket @Unix @Stream @Default)
    (\s -> do
      close s
      traceWith tracer $ ControlClientSocketClosed
    )
    (\s -> do
      retrySocket (\e n i -> traceWith tracer $ ControlClientAttemptReconnect n) $
        connect s (controlClientSocketAddress options)
      traceWith tracer $ ControlClientConnected (controlClientSocketAddress options)
      void $ runPeerWithDriver
        (driver s $ ControlClientDriverTrace >$< tracer)
        (kesPusher (traceWith tracer ControlClientSendingKey >> return (key, oc)) (return Nothing))
        ()
    )
