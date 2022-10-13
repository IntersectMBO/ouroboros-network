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

import Cardano.KESAgent.Driver (driver)
import Cardano.KESAgent.Peers (kesPusher, kesReceiver)
import Cardano.KESAgent.Protocol
import Cardano.Crypto.DirectSerialise

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

data AgentOptions =
  AgentOptions
    { -- | Socket on which the agent will be listening for control messages,
      -- i.e., KES keys being pushed from a control server.
      agentControlSocketAddress :: SocketAddress Unix

      -- | Socket on which the agent will send KES keys to any connected nodes.
    , agentServiceSocketAddress :: SocketAddress Unix
    }

runAgent :: forall k
          . KESSignAlgorithm IO k
         => VersionedProtocol (KESProtocol k)
         => DirectSerialise (SignKeyKES k)
         => DirectDeserialise (SignKeyKES k)
         => Proxy k
         -> AgentOptions
         -> IO ()
runAgent proxy options = do
  currentKeyVar <- newEmptyMVar
  nextKeyChan <- newChan

  let pushKey key = do
        -- Empty the var in case there's anything there already
        oldKeyMay <- tryTakeMVar currentKeyVar
        case oldKeyMay of
          Just _ -> putStrLn "AGENT: Replacing previous key"
          Nothing -> putStrLn "AGENT: Installing initial key"
        -- The MVar is now empty; we write to the next key signal channel
        -- /before/ putting the new key in the MVar, because we want to make it
        -- such that when the consumer picks up the signal, the next update
        -- will be the correct one. Since the MVar is empty at this point, the
        -- consumers will block until we put the key back in.
        writeChan nextKeyChan ()
        -- Consumers have been notified, put the key to un-block them.
        putMVar currentKeyVar key
        case oldKeyMay of
          Just key -> forgetSignKeyKES @IO @k key
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
              putStrLn "AGENT: Service socket closed"
          )
          (\s -> do
            bind s (agentServiceSocketAddress options)
            listen s 0
            putStrLn "AGENT: Listening on service socket"
            let acceptAndHandle s = bracket
                  (accept s)
                  ( \(p, addr) -> do
                      close p
                      putStrLn $ "AGENT: Closed service connection from " ++ show addr
                  )
                  ( \(p, addr) -> do
                      putStrLn $ "AGENT: Service client connected from " ++ show addr
                      myNextKeyChan <- dupChan nextKeyChan
                      void $ runPeerWithDriver
                        (driver p)
                        (kesPusher currentKey (Just <$> nextKey))
                        ()
                  )

            forever $ acceptAndHandle s `catch` \e -> do
              putStrLn "AGENT: Service socket acceptAndHandle: error"
              print (e :: SocketException)
          )

  let runControl =
        void $ bracket
          (socket @Unix @Stream @Default)
          (\s -> do
              close s
              putStrLn "AGENT: Control socket closed"
          )
          (\s -> do
            bind s (agentControlSocketAddress options)
            listen s 0
            putStrLn "AGENT: Listening on control socket"
            let acceptAndHandle s = bracket
                  (accept s)
                  ( \(p, addr) -> do
                      close p
                      putStrLn $ "AGENT: Closed control connection from " ++ show addr
                  )
                  ( \(p, addr) -> do
                      putStrLn $ "AGENT: Control client connected from " ++ show addr
                      void $ runPeerWithDriver
                        (driver p)
                        (kesReceiver pushKey)
                        ()
                  )

            forever $ acceptAndHandle s `catch` \e -> do
              putStrLn $ "AGENT: Control socket acceptAndHandle: " ++ show (e :: SocketException)
          )

  void $ concurrently runService runControl
