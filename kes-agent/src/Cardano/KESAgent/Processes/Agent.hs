{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | The main Agent program.
-- The KES Agent opens two sockets:
--
-- - A \"control\" socket, to which a control server can connect in order to
--   push new keys into the agent.
-- - A \"service\" socket, to which a Node can connect in order to receive the
--   current KES key and future key updates.
--
-- The KES Agent has two slots, each of which can hold up to one KES key:
-- - The \"staged key\" slot can hold just a raw KES signing key; this key is
--   not shared with service clients, and there is no operational certificate
--   linked to it. This slot is used to hold a key while an operational
--   certificate for it is generated. Staged keys are not shared with other
--   agents, and are always lost when the KES agent restarts. Staged keys do
--   not have any validity attached to them; they become valid when an
--   operational certificate is attached and they are moved to the active slot.
--   Because of this, staged keys will never be evolved by the KES agent, and
--   when a staged key is installed into the active slot, it will always be at
--   evolution 0.
-- - The \"active\" or \"current\" slot may hold a 'TaggedBundle'. A tagged
--   bundle is always tagged with a timestamp for synchronization purposes, and
--   may hold a 'Bundle'. A 'Bundle', then, consists of a signing key and a
--   matching operational certificate. The active key is sent out to service
--   clients in two situations:
--   - When a service client first connects, the current key bundle is sent
--   - When a new key bundle is installed into the active slot, it is sent out
--     to all connected clients.
--
--  The KES Agent also takes care of evolving active keys to the current KES
--  period, based on genesis parameters ('EvolutionConfig') passed at agent
--  creation and the host's RTC. While active keys are evolved inside the
--  agent, these evolved keys will *not* be sent out to connected clients,
--  because those clients will already have an earlier evolution of the key,
--  and must evolve those themselves. The evolved key will, however, be served
--  once to any service client connecting at a later time.
module Cardano.KESAgent.Processes.Agent (
  Agent,
  AgentOptions (..),
  EvolutionConfig (..),
  ControlCrypto (..),
  ServiceCrypto (..),
  defEvolutionConfig,
  AgentTrace (..),
  ServiceClientTrace (..),
  defAgentOptions,
  newAgent,
  runAgent,
  finalizeAgent,
)
where

import Cardano.KESAgent.KES.Bundle (Bundle (..), TaggedBundle (..))
import Cardano.KESAgent.KES.Evolution (
  EvolutionConfig (..),
  defEvolutionConfig,
 )
import Cardano.KESAgent.Processes.Agent.CommonActions
import Cardano.KESAgent.Processes.Agent.Context
import Cardano.KESAgent.Processes.Agent.ControlDrivers
import Cardano.KESAgent.Processes.Agent.ServiceDrivers
import Cardano.KESAgent.Processes.Agent.Type
import Cardano.KESAgent.Processes.ServiceClient (
  ServiceClientDrivers (..),
  ServiceClientOptions (..),
  ServiceClientTrace (..),
  runServiceClientForever,
 )
import Cardano.KESAgent.Protocols.AgentInfo
import Cardano.KESAgent.Protocols.VersionHandshake.Driver (versionHandshakeDriver)
import Cardano.KESAgent.Protocols.VersionHandshake.Peers (versionHandshakeServer)
import Cardano.KESAgent.Protocols.VersionedProtocol
import Cardano.KESAgent.Util.PlatformPoison (poisonWindows)

import Ouroboros.Network.RawBearer
import Ouroboros.Network.Snocket (Accept (..), Accepted (..), Snocket (..))

import Control.Concurrent.Class.MonadMVar (MonadMVar)
import Control.Concurrent.Class.MonadSTM (MonadSTM, retry)
import Control.Concurrent.Class.MonadSTM.TChan (
  dupTChan,
  readTChan,
 )
import Control.Concurrent.Class.MonadSTM.TMVar (
  newTMVar,
  putTMVar,
  readTMVar,
  takeTMVar,
 )
import Control.Monad (forever, void)
import Control.Monad.Class.MonadAsync (
  MonadAsync,
  concurrently,
  concurrently_,
  mapConcurrently_,
 )
import Control.Monad.Class.MonadFork (labelThread, myThreadId)
import Control.Monad.Class.MonadST (MonadST)
import Control.Monad.Class.MonadSTM (atomically)
import Control.Monad.Class.MonadThrow (
  MonadCatch,
  MonadThrow,
  SomeException,
  catch,
  finally,
 )
import Control.Monad.Class.MonadTimer (threadDelay)
import Control.Tracer (Tracer (..), traceWith)
import Data.Functor.Contravariant ((>$<))
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import qualified Data.Text as Text
import Network.TypedProtocol.Core (PeerRole (..))
import Network.TypedProtocol.Driver (runPeerWithDriver)

-- | Helper function for listening on a socket.
runListener ::
  forall m c fd addr st (pr :: PeerRole) t a.
  MonadThrow m =>
  MonadFail m =>
  MonadST m =>
  MonadSTM m =>
  MonadMVar m =>
  MonadAsync m =>
  MonadCatch m =>
  Show fd =>
  Snocket m fd addr ->
  fd ->
  String ->
  MakeRawBearer m fd ->
  Tracer m AgentTrace ->
  (String -> AgentTrace) ->
  (String -> AgentTrace) ->
  (String -> String -> AgentTrace) ->
  (String -> AgentTrace) ->
  (String -> AgentTrace) ->
  (t -> AgentTrace) ->
  (RawBearer m -> Tracer m t -> m ()) ->
  m ()
runListener
  s
  -- \^ 'Snocket' to use for socket operations
  fd
  -- \^ File descriptor argument as required by the snocket implementation
  addrStr
  -- \^ String address of the socket to listen on
  mrb
  -- \^ Raw bearer factory
  tracer
  -- \^ Where to send trace logs
  tListeningOnSocket
  -- \^ How to trace listening-on-socket
  tSocketClosed
  -- \^ How to trace socket-closed
  tClientConnected
  -- \^ How to trace client-connected
  tClientDisconnected
  -- \^ How to trace client-disconnected
  tSocketError
  -- \^ How to trace socket errors
  tDriverTrace
  -- \^ How to wrap driver traces
  handle =
    -- \^ How to handle an incoming connection. This will run once for each
    -- accepted connection, in a separate thread.
    do
      listen s fd
      traceWith tracer (tListeningOnSocket addrStr)

      let handleConnection fd' = do
            traceWith tracer (tClientConnected (show fd) (show fd'))
            bearer <- getRawBearer mrb fd'
            handle bearer (tDriverTrace >$< tracer)

      let logAndContinue :: SomeException -> m ()
          logAndContinue e = traceWith tracer (tSocketError (show e))

      let loop :: Accept m fd addr -> m ()
          loop a = do
            accepted <- runAccept a
            case accepted of
              (AcceptFailure e, next) -> do
                traceWith tracer $ tSocketError (show e)
                loop next
              (Accepted fd' addr', next) ->
                concurrently_
                  (loop next)
                  ( handleConnection fd'
                      `catch` logAndContinue
                      `finally` (close s fd' >> traceWith tracer (tSocketClosed $ show fd'))
                  )

      (accept s fd >>= loop) `catch` logAndContinue

-- | Run an agent.
-- This will spawn the following threads:
-- - An evolution thread, which regularly checks whether the active key should
--   be evolved, and if so, evolves it.
-- - A service listener, listening on the configured service socket, spawning
--   a service handler for each accepted service connection
-- - A control listener, listening on the configured control socket, spawning
--   a control handler for each accepted control connection
-- - One bootstrap client thread for each configured bootstrapping address.
--   Each bootstrap client acts as a service client to another agent, taking
--   care of receiving incoming key updates, comparing them against the current
--   active key, and installing them if they are valid and newer.
runAgent ::
  forall c m fd addr.
  AgentContext m c =>
  ControlCrypto c =>
  ServiceCrypto c =>
  ServiceClientDrivers c =>
  Show addr =>
  Show fd =>
  Agent c m fd addr ->
  m ()
runAgent agent = do
  poisonWindows

  clientCounterVar <- atomically $ newTMVar (0 :: Int)

  let runEvolution = do
        forever $ do
          -- Check time every 100 milliseconds, update key when period flips
          -- over.
          threadDelay 100_000
          checkEvolution agent

  let runService :: m ()
      runService = do
        labelMyThread "service"

        let reportPushResult result =
              agentTrace agent $ AgentDebugTrace $ "Push result: " ++ show result

        runListener
          (agentSnocket agent)
          (agentServiceFD agent)
          (show $ agentServiceAddr (agentOptions agent))
          (agentMRB agent)
          (agentTracer . agentOptions $ agent)
          AgentListeningOnServiceSocket
          AgentServiceSocketClosed
          AgentServiceClientConnected
          AgentServiceClientDisconnected
          AgentServiceSocketError
          AgentServiceDriverTrace
          ( \bearer tracer' -> do
              serviceID <- atomically $ do
                i <- takeTMVar clientCounterVar
                putTMVar clientCounterVar (succ i)
                return i
              labelMyThread $ "service" ++ show serviceID

              nextKeyChanRcv <- atomically $ dupTChan (agentNextKeyChan agent)
              let currentKey =
                    atomically $ readTMVar (agentCurrentKeyVar agent) >>= maybe retry return
              let nextKey = do
                    tbundle <- atomically (readTChan nextKeyChanRcv)
                    agentTrace agent $
                      AgentPushingKeyUpdate
                        (formatMaybeKey (taggedBundleTimestamp tbundle) (bundleOC <$> taggedBundle tbundle))
                        ("service" ++ show serviceID)
                    return tbundle

              (protocolVersionMay :: Maybe VersionIdentifier, ()) <-
                runPeerWithDriver
                  ( versionHandshakeDriver
                      bearer
                      (AgentVersionHandshakeDriverTrace >$< (agentTracer . agentOptions $ agent))
                  )
                  (versionHandshakeServer (map serviceDriverVersionID (availableServiceDrivers @c @m)))
              case protocolVersionMay >>= (`lookupServiceDriver` (availableServiceDrivers @c @m)) of
                Nothing ->
                  traceWith (agentTracer . agentOptions $ agent) AgentServiceVersionHandshakeFailed
                Just run -> do
                  run bearer tracer' currentKey nextKey reportPushResult
          )

  let runBootstrap :: addr -> m ()
      runBootstrap addr = do
        labelMyThread $ "bootstrap-" ++ show addr
        let scOpts =
              ServiceClientOptions
                { serviceClientSnocket = agentSnocket agent
                , serviceClientAddress = addr
                }

        let label = Text.pack (show addr)

        let setConnectionStatus :: ConnectionStatus -> m ()
            setConnectionStatus status = atomically $ do
              m <- takeTMVar (agentBootstrapConnections agent)
              let m' = Map.insert label status m
              putTMVar (agentBootstrapConnections agent) m'

        let connStatTracer :: Tracer m ServiceClientTrace
            connStatTracer = Tracer $ \case
              ServiceClientAttemptReconnect {} ->
                setConnectionStatus ConnectionConnecting
              ServiceClientConnected {} ->
                setConnectionStatus ConnectionUp
              ServiceClientSocketClosed {} ->
                setConnectionStatus ConnectionDown
              ServiceClientAbnormalTermination {} ->
                setConnectionStatus ConnectionDown
              _ -> return ()

        let parentTracer :: Tracer m ServiceClientTrace
            parentTracer = AgentBootstrapTrace >$< agentTracer (agentOptions agent)

        setConnectionStatus ConnectionConnecting

        runServiceClientForever
          (Proxy @c)
          (agentMRB agent)
          scOpts
          (fmap pushKeyResultToRecvResult . pushKey agent)
          (parentTracer <> connStatTracer)

  let runBootstraps =
        mapConcurrently_ runBootstrap $ agentBootstrapAddr (agentOptions agent)

  let runControl = case agentControlFD agent of
        Nothing -> do
          let tracer = agentTracer . agentOptions $ agent
          traceWith tracer AgentControlSocketDisabled
          return ()
        Just fd -> do
          labelMyThread "control"
          runListener
            (agentSnocket agent)
            fd
            (maybe "<NO ADDRESS>" show $ agentControlAddr (agentOptions agent))
            (agentMRB agent)
            (agentTracer . agentOptions $ agent)
            AgentListeningOnControlSocket
            AgentControlSocketClosed
            AgentControlClientConnected
            AgentControlClientDisconnected
            AgentControlSocketError
            AgentControlDriverTrace
            ( \bearer tracer' -> do
                controlID <- atomically $ do
                  i <- takeTMVar clientCounterVar
                  putTMVar clientCounterVar (succ i)
                  return i
                labelMyThread $ "control" ++ show controlID
                (protocolVersionMay :: Maybe VersionIdentifier, ()) <-
                  runPeerWithDriver
                    ( versionHandshakeDriver
                        bearer
                        (AgentVersionHandshakeDriverTrace >$< (agentTracer . agentOptions $ agent))
                    )
                    (versionHandshakeServer (map fst (availableControlDrivers @c @m)))
                case protocolVersionMay >>= (`lookup` (availableControlDrivers @c @m)) of
                  Nothing ->
                    traceWith (agentTracer . agentOptions $ agent) AgentControlVersionHandshakeFailed
                  Just run ->
                    run bearer tracer' agent
            )

  void $
    runService
      `concurrently` runControl
      `concurrently` runEvolution
      `concurrently` runBootstraps

labelMyThread label = do
  tid <- myThreadId
  labelThread tid label
