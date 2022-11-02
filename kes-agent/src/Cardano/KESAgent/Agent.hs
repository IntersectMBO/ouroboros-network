{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE ScopedTypeVariables #-}

-- | The main Agent program.
-- The KES Agent opens two sockets:
--
-- - A \"control\" socket, to which a control server can connect in order to
--   push new keys into the agent.
-- - A \"service\" socket, to which a Node can connect in order to receive the
--   current KES key and future key updates.
module Cardano.KESAgent.Agent
where

import Cardano.KESAgent.Driver (driver, DriverTrace (..))
import Cardano.KESAgent.Peers (kesPusher, kesReceiver)
import Cardano.KESAgent.Protocol
import Cardano.KESAgent.Logging
import Cardano.Crypto.DirectSerialise
import Cardano.Binary

import Cardano.Crypto.KES.Class

import Data.ByteString (ByteString)
import Control.Concurrent.MVar (MVar, newEmptyMVar, tryTakeMVar, putMVar, readMVar)
import Control.Concurrent.Chan (Chan, newChan, dupChan, writeChan, readChan)
import Control.Concurrent.Async (concurrently)
import Control.Monad (forever, void)
import Control.Exception (bracket, catch)
import System.Socket (socket, SocketException, close, bind, listen, accept)
import System.Socket.Type.Stream
import System.Socket.Family.Unix
import System.Socket.Protocol.Default
import Network.TypedProtocol.Driver (runPeerWithDriver)
import Data.Proxy (Proxy (..))
import Data.Typeable

data AgentTrace
  = AgentServiceDriverTrace DriverTrace
  | AgentControlDriverTrace DriverTrace
  | AgentReplacingPreviousKey
  | AgentInstallingNewKey
  | AgentServiceSocketClosed
  | AgentListeningOnServiceSocket
  | AgentServiceClientConnected (SocketAddress Unix)
  | AgentServiceClientDisconnected (SocketAddress Unix)
  | AgentServiceSocketError SocketException
  | AgentControlSocketClosed
  | AgentListeningOnControlSocket
  | AgentControlClientConnected (SocketAddress Unix)
  | AgentControlClientDisconnected (SocketAddress Unix)
  | AgentControlSocketError SocketException
  deriving (Show)

data AgentOptions =
  AgentOptions
    { -- | Socket on which the agent will be listening for control messages,
      -- i.e., KES keys being pushed from a control server.
      agentControlSocketAddress :: SocketAddress Unix

      -- | Socket on which the agent will send KES keys to any connected nodes.
    , agentServiceSocketAddress :: SocketAddress Unix
    }

runAgent :: forall c
          . Crypto c
         => Typeable c
         => KESSignAlgorithm IO (KES c)
         => VersionedProtocol (KESProtocol c)
         => DirectSerialise (SignKeyKES (KES c))
         => DirectDeserialise (SignKeyKES (KES c))
         => Proxy c
         -> AgentOptions
         -> Tracer IO AgentTrace
         -> IO ()
runAgent proxy options tracer = do
  currentKeyVar <- newEmptyMVar
  nextKeyChan <- newChan

  let pushKey :: SignKeyKES (KES c) -> OCert c -> IO ()
      pushKey key oc = do
        -- Empty the var in case there's anything there already
        oldKeyOcMay <- tryTakeMVar currentKeyVar
        case oldKeyOcMay of
          Just _ -> traceWith tracer AgentReplacingPreviousKey
          Nothing -> traceWith tracer AgentInstallingNewKey
        -- The MVar is now empty; we write to the next key signal channel
        -- /before/ putting the new key in the MVar, because we want to make it
        -- such that when the consumer picks up the signal, the next update
        -- will be the correct one. Since the MVar is empty at this point, the
        -- consumers will block until we put the key back in.
        writeChan nextKeyChan ()
        -- Consumers have been notified, put the key to un-block them.
        putMVar currentKeyVar (key, oc)
        case oldKeyOcMay of
          Just (key, _) -> forgetSignKeyKES @IO @(KES c) key
          Nothing -> return ()

      currentKey =
        readMVar currentKeyVar

      nextKey = do
        () <- readChan nextKeyChan
        readMVar currentKeyVar

  let runService =
        void $ bracket
          (socket @Unix @Stream @Default)
          (\s -> do
              close s
              traceWith tracer AgentServiceSocketClosed
          )
          (\s -> do
            bind s (agentServiceSocketAddress options)
            listen s 0
            traceWith tracer AgentListeningOnServiceSocket
            let acceptAndHandle s = bracket
                  (accept s)
                  ( \(p, addr) -> do
                      close p
                      traceWith tracer $ AgentServiceClientDisconnected addr
                  )
                  ( \(p, addr) -> do
                      traceWith tracer $ AgentServiceClientConnected addr
                      myNextKeyChan <- dupChan nextKeyChan
                      void $ runPeerWithDriver
                        (driver p $ AgentServiceDriverTrace >$< tracer)
                        (kesPusher currentKey (Just <$> nextKey))
                        ()
                  )

            forever $ acceptAndHandle s `catch` \e -> do
              traceWith tracer $ AgentServiceSocketError e
          )

  let runControl =
        void $ bracket
          (socket @Unix @Stream @Default)
          (\s -> do
              close s
              traceWith tracer AgentControlSocketClosed
          )
          (\s -> do
            bind s (agentControlSocketAddress options)
            listen s 0
            traceWith tracer AgentListeningOnControlSocket
            let acceptAndHandle s = bracket
                  (accept s)
                  ( \(p, addr) -> do
                      close p
                      traceWith tracer $ AgentControlClientDisconnected addr
                  )
                  ( \(p, addr) -> do
                      traceWith tracer $ AgentControlClientConnected addr
                      void $ runPeerWithDriver
                        (driver p $ AgentControlDriverTrace >$< tracer)
                        (kesReceiver pushKey)
                        ()
                  )

            forever $ acceptAndHandle s `catch` \e -> do
              traceWith tracer $ AgentControlSocketError e
          )

  void $ concurrently runService runControl
