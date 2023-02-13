{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE NumericUnderscores #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE MultiParamTypeClasses #-}

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
import Cardano.KESAgent.OCert (KESPeriod (..), KES, OCert (..))
import Cardano.KESAgent.RefCounting (CRef, withCRefValue, newCRef, acquireCRef, releaseCRef)
import Cardano.KESAgent.Evolution (getCurrentKESPeriodWith, updateKESTo)
import Cardano.KESAgent.DirectBearer (toDirectBearer)
import Cardano.KESAgent.Classes (MonadKES, MonadNetworking)

import Cardano.Crypto.KES.Class (SignKeyWithPeriodKES (..), forgetSignKeyKES)

import Data.ByteString (ByteString)
import Control.Monad (forever, void)
import Control.Tracer (Tracer, traceWith)
import Network.TypedProtocol.Driver (runPeerWithDriver)
import Data.Functor.Contravariant ((>$<))
import Data.Proxy (Proxy (..))
import Data.Time (NominalDiffTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Maybe (fromJust)
import Ouroboros.Network.Snocket (Snocket (..), Accept (..), Accepted (..))

import Control.Monad.Class.MonadTime (MonadTime (..))
import Control.Monad.Class.MonadMVar (MVar, newEmptyMVar, newMVar, withMVar, tryTakeMVar, putMVar, readMVar)
import Control.Monad.Class.MonadSTM (atomically)
import Control.Monad.Class.MonadAsync (concurrently)
import Control.Monad.Class.MonadTimer (threadDelay)
import Control.Monad.Class.MonadThrow (SomeException, bracket, throwIO, catch)
import Control.Concurrent.Class.MonadSTM.TChan (newTChan, writeTChan, readTChan)

{-HLINT ignore "Use underscore" -}

data AgentTrace
  = AgentServiceDriverTrace DriverTrace
  | AgentControlDriverTrace DriverTrace
  | AgentReplacingPreviousKey
  | AgentInstallingNewKey
  | AgentServiceSocketClosed
  | AgentListeningOnServiceSocket
  | AgentServiceClientConnected -- (SocketAddress Unix)
  | AgentServiceClientDisconnected -- (SocketAddress Unix)
  | AgentServiceSocketError String
  | AgentControlSocketClosed
  | AgentListeningOnControlSocket
  | AgentControlClientConnected -- (SocketAddress Unix)
  | AgentControlClientDisconnected -- (SocketAddress Unix)
  | AgentControlSocketError String
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

data AgentOptions m fd addr =
  AgentOptions
    { agentSnocket :: Snocket m fd addr
      -- | Socket on which the agent will be listening for control messages,
      -- i.e., KES keys being pushed from a control server.

    , agentControlAddr :: addr

      -- | Socket on which the agent will send KES keys to any connected nodes.
    , agentServiceAddr :: addr

      -- | The genesis Unix timestamp; used to determine current KES period.
    , agentGenesisTimestamp :: Integer

      -- | Return the current POSIX time. Should normally be set to
      -- 'getPOSIXTime', but overriding this may be desirable for testing
      -- purposes.
    , agentGetCurrentTime :: m NominalDiffTime
    }

defAgentOptions :: MonadTime m => AgentOptions m fd addr
defAgentOptions = AgentOptions
  { agentSnocket = error "default"
  , agentControlAddr = error "default"
  , agentServiceAddr = error "default"
  , agentGenesisTimestamp = 1506203091 -- real-world genesis on the production ledger
  , agentGetCurrentTime = utcTimeToPOSIXSeconds <$> getCurrentTime
  }

runAgent :: forall c m fd addr
          . MonadKES m c
         => MonadNetworking m fd
         => Proxy c
         -> AgentOptions m fd addr
         -> Tracer m AgentTrace
         -> m ()
runAgent proxy options tracer = do
  -- The key itself is stored as a 'CRef', rather than directly, which
  -- allows us to pass keys around and forget them exactly when the last
  -- reference is dropped. The downside to this is that we need to be explicit
  -- about those references, which is what the 'CRef' type achieves.
  currentKeyVar :: MVar m (CRef m (SignKeyWithPeriodKES (KES c)), OCert c) <- newEmptyMVar
  nextKeyChan <- atomically newTChan

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
  keyLock :: MVar m () <- newMVar ()
  let withKeyUpdateLock :: forall a. String -> m a -> m a
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

  let pushKey :: CRef m (SignKeyWithPeriodKES (KES c)) -> OCert c -> m ()
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
          atomically $ writeTChan nextKeyChan ()
          -- Consumers have been notified, put the key to un-block them.
          putMVar currentKeyVar (keyVar, oc)

        checkEvolution

      currentKey = do
        readMVar currentKeyVar

      nextKey = do
        () <- atomically $ readTChan nextKeyChan
        readMVar currentKeyVar

  let runEvolution = do
        forever $ do
          -- Check time every 100 milliseconds, update key when period flips
          -- over.
          threadDelay 100_0000
          checkEvolution

  let runService =
        let s = agentSnocket options
        in void $ bracket
          (open s (addrFamily s $ agentServiceAddr options))
          (\fd -> do
              close s fd
              traceWith tracer AgentServiceSocketClosed
          )
          (\fd -> do
            bind s fd (agentServiceAddr options)
            listen s fd
            traceWith tracer AgentListeningOnServiceSocket

            let loop :: Accept m fd addr -> m ()
                loop a = do
                  accepted <- runAccept a
                  case accepted of
                    (AcceptFailure e, next) ->
                      throwIO e
                    (Accepted fd' addr', next) -> do
                      traceWith tracer AgentServiceClientConnected
                      void $ runPeerWithDriver
                        (driver (toDirectBearer fd') $ AgentServiceDriverTrace >$< tracer)
                        (kesPusher currentKey (Just <$> nextKey))
                        ()
                      close s fd'
                      loop next
            
            (accept s fd >>= loop) `catch` \(e :: SomeException)-> do
              traceWith tracer $ AgentServiceSocketError (show e)
              throwIO e
          )

  let runControl =
        let s = agentSnocket options
        in void $ bracket
          (open s (addrFamily s $ agentServiceAddr options))
          (\fd -> do
              close s fd
              traceWith tracer AgentControlSocketClosed
          )
          (\fd -> do
            bind s fd (agentControlAddr options)
            listen s fd
            traceWith tracer AgentListeningOnControlSocket

            let loop :: Accept m fd addr -> m ()
                loop a = do
                  accepted <- runAccept a
                  case accepted of
                    (AcceptFailure e, next) ->
                      throwIO e
                    (Accepted fd' addr', next) -> do
                      traceWith tracer AgentControlClientConnected
                      void $ runPeerWithDriver
                        (driver (toDirectBearer fd') $ AgentControlDriverTrace >$< tracer)
                        (kesReceiver pushKey)
                        ()
                      close s fd'
                      loop next
            
            (accept s fd >>= loop) `catch` \(e :: SomeException)-> do
              traceWith tracer $ AgentControlSocketError (show e)
              throwIO e
          )

  void $ runService `concurrently` runControl `concurrently` runEvolution
