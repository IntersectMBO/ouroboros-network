{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE DeriveAnyClass #-}

-- | Clients for the KES Agent.
module Cardano.KESAgent.ControlClient
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
import Control.Exception (Exception, bracket, catch, throw)

data ControlClientOptions =
  ControlClientOptions
    { controlClientSocketAddress :: SocketAddress Unix
    }

-- | A simple control client: push one KES key, then exit.
runControlClient1 :: forall k
                   . KESSignAlgorithm IO k
                  => DirectDeserialise (SignKeyKES k)
                  => DirectSerialise (SignKeyKES k)
                  => VersionedProtocol (KESProtocol k)
                  => Proxy k
                  -> ControlClientOptions
                  -> SignKeyKES k
                  -> IO ()
runControlClient1 proxy options key = do
  void $ bracket
    (socket @Unix @Stream @Default)
    (\s -> do
      close s
      putStrLn "Control client socket closed"
    )
    (\s -> do
      connect s (controlClientSocketAddress options)
      putStrLn $ "Control client connected to " ++ show (controlClientSocketAddress options)
      void $ runPeerWithDriver
        (driver s)
        (kesPusher (putStrLn "Sending key" >> return key) (return Nothing))
        ()
    )
