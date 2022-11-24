{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE NumericUnderscores #-}
{-#LANGUAGE OverloadedStrings #-}

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
import Cardano.KESAgent.OCert
import Cardano.KESAgent.RefCounting
import Cardano.KESAgent.Evolution

import Cardano.Crypto.DirectSerialise
import Cardano.Binary
import Cardano.Crypto.KES.Class

import Data.ByteString (ByteString)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, tryTakeMVar, putMVar, readMVar, withMVar)
import Control.Concurrent.Chan (Chan, newChan, dupChan, writeChan, readChan)
import Control.Concurrent.Async (concurrently)
import Control.Concurrent (threadDelay)
import Control.Monad (forever, void)
import Control.Exception (bracket, catch)
import System.Socket (socket, SocketException, close, bind, listen, accept)
import System.Socket.Type.Stream
import System.Socket.Family.Unix
import System.Socket.Protocol.Default
import Network.TypedProtocol.Driver (runPeerWithDriver)
import Data.Proxy (Proxy (..))
import Data.Typeable
import Data.Time (NominalDiffTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Maybe (fromJust)

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
  | AgentCheckEvolution KESPeriod
  | AgentUpdateKESPeriod KESPeriod KESPeriod
  | AgentKeyNotEvolved KESPeriod KESPeriod
  | AgentNoKeyToEvolve
  | AgentKeyEvolved
  | AgentKeyExpired
  | AgentLockRequest String
  | AgentLockAcquired String
  | AgentLockReleased String
  deriving (Show)

data AgentOptions =
  AgentOptions
    { -- | Socket on which the agent will be listening for control messages,
      -- i.e., KES keys being pushed from a control server.
      agentControlSocketAddress :: SocketAddress Unix

      -- | Socket on which the agent will send KES keys to any connected nodes.
    , agentServiceSocketAddress :: SocketAddress Unix

      -- | The genesis Unix timestamp; used to determine current KES period.
    , agentGenesisTimestamp :: Integer

      -- | Return the current POSIX time. Should normally be set to
      -- 'getPOSIXTime', but overriding this may be desirable for testing
      -- purposes.
    , agentGetCurrentTime :: IO NominalDiffTime
    }

defAgentOptions :: AgentOptions
defAgentOptions = AgentOptions
  { agentControlSocketAddress = fromJust $ socketAddressUnixAbstract "kes-agent-control.socket"
  , agentServiceSocketAddress = fromJust $ socketAddressUnixAbstract "kes-agent-service.socket"
  , agentGenesisTimestamp = 1506203091 -- real-world genesis on the production ledger
  , agentGetCurrentTime = getPOSIXTime
  }

runAgent :: forall c
          . Crypto c
         => Typeable c
         => KESSignAlgorithm IO (KES c)
         => ContextKES (KES c) ~ ()
         => VersionedProtocol (KESProtocol c)
         => DirectSerialise (SignKeyKES (KES c))
         => DirectDeserialise (SignKeyKES (KES c))
         => Proxy c
         -> AgentOptions
         -> Tracer IO AgentTrace
         -> IO ()
runAgent proxy options tracer = do
  currentKeyVar :: MVar (CRef (SignKeyWithPeriodKES (KES c)), OCert c) <- newEmptyMVar
  nextKeyChan <- newChan

  -- The key update lock is required because we need to distinguish between two
  -- different situations in which the currentKey MVar may be empty:
  -- - No key has been pushed yet (or the previous key has expired).
  -- - The key is currently being updated.
  -- In both cases, we want consumers to block until a key is present, but
  -- producers need to distinguish these cases:
  -- - If no key has been pushed yet, or the previous key has expired, the
  --   key pusher is allowed to install a new key; the key evolver can't do
  --   anything useful, so it will just sleep and try again later (it cannot
  --   block, because that would make it impossible for the pusher to install
  --   a key, resulting in a deadlock).
  -- - If a key has been pushed, but it is currently being evolved, then the
  --   pusher must wait until the evolution has finished, and then install the
  --   new key.
  -- - If the evolver is about to run, but the key is currently being
  --   overwritten, then the evolver should wait for the overwriting to finish,
  --   and then attempt to evolve the new key.
  -- The keyLock setup achieves this, by allowing only one producer at a
  -- time to manipulate the currentKeyVar, regardless of whether it is
  -- currently empty or not, while consumers are free to perform blocking reads
  -- on it without touching the keyLock.
  --
  -- Concretely, the rules for accessing currentKeyVar are:
  --
  -- - readMVar is allowed, and should be done without acquiring the
  --   keyLock.
  -- - tryReadMVar is always allowed, but may not be useful.
  -- - takeMVar is not allowed, since it would 1) block, and 2) require
  --   acquisition of keyLock, resulting in a deadlock.
  -- - tryTakeMVar is allowed, but only while holding the keyLock.
  keyLock :: MVar () <- newMVar ()
  let withKeyUpdateLock :: forall a. String -> IO a -> IO a
      withKeyUpdateLock context a = do
        traceWith tracer $ AgentLockRequest context
        withMVar keyLock $ \() -> do
          traceWith tracer (AgentLockAcquired context)
          result <- a
          traceWith tracer (AgentLockReleased context)
          return result

  let checkEvolution = withKeyUpdateLock "checkEvolution" $ do
        p' <- getCurrentKESPeriodWith (agentGetCurrentTime options) (agentGenesisTimestamp options)
        traceWith tracer $ AgentCheckEvolution p'
        keyOcMay <- tryTakeMVar currentKeyVar
        case keyOcMay of
          Nothing -> do
            traceWith tracer AgentNoKeyToEvolve
            return ()
          Just (keyVar, oc) -> withCRefValue keyVar $ \key -> do
            let p = KESPeriod $ unKESPeriod (ocertKESPeriod oc) + periodKES key
            if p /= p' then do
              keyMay' <- updateKESTo
                            ()
                            p'
                            oc
                            key
              case keyMay' of
                Nothing -> do
                  traceWith tracer AgentKeyExpired
                Just key' -> do
                  traceWith tracer AgentKeyEvolved
                  keyVar' <- newCRef (forgetSignKeyKES . skWithoutPeriodKES) key'
                  void $ putMVar currentKeyVar (keyVar', oc)
              releaseCRef keyVar
            else do
              traceWith tracer $ AgentKeyNotEvolved p p'
              void $ putMVar currentKeyVar (keyVar, oc)

  let pushKey :: CRef (SignKeyWithPeriodKES (KES c)) -> OCert c -> IO ()
      pushKey keyVar oc = do
        withKeyUpdateLock "pushKey" $ do
          acquireCRef keyVar
          -- Empty the var in case there's anything there already
          oldKeyOcMay <- tryTakeMVar currentKeyVar
          case oldKeyOcMay of
            Just (oldKeyVar, oldOC) -> do
              releaseCRef oldKeyVar
              traceWith tracer AgentReplacingPreviousKey
            Nothing ->
              traceWith tracer AgentInstallingNewKey
          -- The MVar is now empty; we write to the next key signal channel
          -- /before/ putting the new key in the MVar, because we want to make it
          -- such that when the consumer picks up the signal, the next update
          -- will be the correct one. Since the MVar is empty at this point, the
          -- consumers will block until we put the key back in.
          writeChan nextKeyChan ()
          -- Consumers have been notified, put the key to un-block them.
          putMVar currentKeyVar (keyVar, oc)

        checkEvolution

      currentKey = do
        readMVar currentKeyVar

      nextKey = do
        () <- readChan nextKeyChan
        readMVar currentKeyVar

  let runEvolution = do
        forever $ do
          -- Check time every 100 milliseconds, update key when period flips
          -- over.
          threadDelay 100_0000
          checkEvolution

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

  void $ runService `concurrently` runControl `concurrently` runEvolution
