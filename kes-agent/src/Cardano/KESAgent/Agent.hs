{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | The main Agent program.
-- The KES Agent opens two sockets:
--
-- - A \"control\" socket, to which a control server can connect in order to
--   push new keys into the agent.
-- - A \"service\" socket, to which a Node can connect in order to receive the
--   current KES key and future key updates.
module Cardano.KESAgent.Agent
  where

import Cardano.KESAgent.Classes ( MonadKES )
import Cardano.KESAgent.Driver ( DriverTrace (..), driver )
import Cardano.KESAgent.Evolution ( getCurrentKESPeriodWith, updateKESTo )
import Cardano.KESAgent.OCert ( KES, KESPeriod (..), OCert (..) )
import Cardano.KESAgent.Peers ( kesPusher, kesReceiver )
import Cardano.KESAgent.Protocol ( KESProtocol )
import Cardano.KESAgent.RefCounting
  ( CRef
  , acquireCRef
  , newCRef
  , releaseCRef
  , withCRefValue
  )

import Cardano.Crypto.KES.Class
  ( SignKeyWithPeriodKES (..)
  , forgetSignKeyKES
  , rawSerialiseSignKeyKES
  )

import Ouroboros.Network.RawBearer
import Ouroboros.Network.Snocket ( Accept (..), Accepted (..), Snocket (..) )

import Control.Concurrent.Class.MonadMVar
  ( MVar
  , newEmptyMVar
  , newMVar
  , putMVar
  , readMVar
  , tryTakeMVar
  , withMVar
  )
import Control.Concurrent.Class.MonadSTM ( retry )
import Control.Concurrent.Class.MonadSTM.TChan
  ( newBroadcastTChan
  , dupTChan
  , readTChan
  , writeTChan
  )
import Control.Concurrent.Class.MonadSTM.TMVar
  ( TMVar
  , newEmptyTMVar
  , newTMVar
  , putTMVar
  , readTMVar
  , tryTakeTMVar
  , takeTMVar
  , tryReadTMVar
  )
import Control.Monad ( forever, void )
import Control.Monad.Class.MonadAsync ( concurrently, concurrently_ )
import Control.Monad.Class.MonadSTM ( atomically )
import Control.Monad.Class.MonadThrow
  ( SomeException
  , bracket
  , catch
  , finally
  , throwIO
  )
import Control.Monad.Class.MonadTime ( MonadTime (..) )
import Control.Monad.Class.MonadTimer ( threadDelay )
import Control.Tracer ( Tracer, traceWith )
import Data.ByteString ( ByteString )
import Data.Functor.Contravariant ( (>$<) )
import Data.Maybe ( fromJust )
import Data.Proxy ( Proxy (..) )
import Data.Time ( NominalDiffTime )
import Data.Time.Clock.POSIX ( utcTimeToPOSIXSeconds )
import Network.TypedProtocol.Core ( Peer (..), PeerRole (..) )
import Network.TypedProtocol.Driver ( runPeerWithDriver )
import Text.Printf

{-HLINT ignore "Use underscore" -}

data AgentTrace
  = AgentServiceDriverTrace DriverTrace
  | AgentControlDriverTrace DriverTrace
  | AgentReplacingPreviousKey String String
  | AgentInstallingNewKey String
  | AgentSkippingOldKey String String
  | AgentServiceSocketClosed String
  | AgentListeningOnServiceSocket String
  | AgentServiceClientConnected String
  | AgentServiceClientDisconnected String
  | AgentServiceSocketError String
  | AgentControlSocketClosed String
  | AgentListeningOnControlSocket String
  | AgentControlClientConnected String
  | AgentControlClientDisconnected String
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
         => Show addr
         => Show fd
         => Proxy c
         -> MakeRawBearer m fd
         -> AgentOptions m fd addr
         -> Tracer m AgentTrace
         -> m ()
runAgent proxy mrb options tracer = do
  -- The key itself is stored as a 'CRef', rather than directly, which
  -- allows us to pass keys around and forget them exactly when the last
  -- reference is dropped. The downside to this is that we need to be explicit
  -- about those references, which is what the 'CRef' type achieves.
  currentKeyVar :: TMVar m (CRef m (SignKeyWithPeriodKES (KES c)), OCert c) <- atomically newEmptyTMVar
  nextKeyChan <- atomically newBroadcastTChan

  -- The key update lock is required because we need to distinguish between two
  -- different situations in which the currentKey TMVar may be empty:
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
  -- - readTMVar is allowed, and should be done without acquiring the
  --   keyLock.
  -- - tryReadTMVar is always allowed, but may not be useful.
  -- - takeTMVar is not allowed, since it would 1) block, and 2) require
  --   acquisition of keyLock, resulting in a deadlock.
  -- - tryTakeTMVar is allowed, but only while holding the keyLock.
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
        keyOcMay <- atomically $ tryTakeTMVar currentKeyVar
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
                  void . atomically $ putTMVar currentKeyVar (keyVar', oc)
              releaseCRef keyVar
            else do
              traceWith tracer $ AgentKeyNotEvolved p p'
              void . atomically $ putTMVar currentKeyVar (keyVar, oc)

  let formatKey :: OCert c -> String
      formatKey ocert =
        let serialNumber = ocertN ocert
        in printf "%i" serialNumber

  let pushKey :: CRef m (SignKeyWithPeriodKES (KES c)) -> OCert c -> m ()
      pushKey keyVar oc = do
        withKeyUpdateLock "pushKey" $ do
          acquireCRef keyVar
          let keyStr = formatKey oc
          -- Empty the var in case there's anything there already
          oldKeyOcMay <- atomically $ tryTakeTMVar currentKeyVar

          case oldKeyOcMay of
            Just (oldKeyVar, oldOC) -> do
              let oldKeyStr = formatKey oldOC
              if ocertN oldOC >= ocertN oc then do
                releaseCRef keyVar
                traceWith tracer $ AgentSkippingOldKey oldKeyStr keyStr
                atomically $ putTMVar currentKeyVar (oldKeyVar, oldOC)
              else do
                releaseCRef oldKeyVar
                traceWith tracer $ AgentReplacingPreviousKey oldKeyStr keyStr
                atomically $ do
                  writeTChan nextKeyChan (keyVar, oc)
                  putTMVar currentKeyVar (keyVar, oc)
            Nothing -> do
              traceWith tracer $ AgentInstallingNewKey keyStr
              -- The TMVar is now empty; we write to the next key signal channel
              -- /before/ putting the new key in the MVar, because we want to make it
              -- such that when the consumer picks up the signal, the next update
              -- will be the correct one. Since the MVar is empty at this point, the
              -- consumers will block until we put the key back in.
              atomically $ do
                writeTChan nextKeyChan (keyVar, oc)
                putTMVar currentKeyVar (keyVar, oc)

        checkEvolution

  let runEvolution = do
        forever $ do
          -- Check time every 100 milliseconds, update key when period flips
          -- over.
          threadDelay 100_0000
          checkEvolution

  let runListener :: addr
                  -> (String -> AgentTrace)
                  -> (String -> AgentTrace)
                  -> (String -> AgentTrace)
                  -> (String -> AgentTrace)
                  -> (DriverTrace -> AgentTrace)
                  -> Peer (KESProtocol m c) (pr :: PeerRole) st m a
                  -> m ()
      runListener
        addr
        tListeningOnSocket
        tSocketClosed
        tClientConnected
        tSocketError
        tDriverTrace
        peer =
          let s = agentSnocket options
          in void $ bracket
            (open s (addrFamily s addr))
            (\fd -> do
                close s fd
                traceWith tracer (tSocketClosed $ show addr)
            )
            (\fd -> do
              bind s fd addr
              listen s fd
              traceWith tracer (tListeningOnSocket $ show addr)

              let handleConnection fd' = do
                    traceWith tracer (tClientConnected $ show fd')
                    bearer <- getRawBearer mrb fd'
                    void $ runPeerWithDriver
                      (driver bearer $ tDriverTrace >$< tracer)
                      peer
                      ()

              let loop :: Accept m fd addr -> m ()
                  loop a = do
                    accepted <- runAccept a
                    case accepted of
                      (AcceptFailure e, next) -> do
                        throwIO e
                      (Accepted fd' addr', next) ->
                        concurrently_
                          (loop next)
                          (handleConnection fd'
                            `finally`
                            close s fd' >> traceWith tracer (tSocketClosed $ show fd')
                          )

              (accept s fd >>= loop) `catch` \(e :: SomeException) -> do
                traceWith tracer $ tSocketError (show e)
                throwIO e
            )




  let runService = do
        nextKeyChanRcv <- atomically $ dupTChan nextKeyChan

        let currentKey = atomically $ readTMVar currentKeyVar
        let nextKey = atomically $ Just <$> readTChan nextKeyChanRcv

        runListener
          (agentServiceAddr options)
          AgentListeningOnServiceSocket
          AgentServiceSocketClosed
          AgentServiceClientConnected
          AgentServiceSocketError
          AgentServiceDriverTrace
          (kesPusher currentKey nextKey)

  let runControl =
        runListener
          (agentControlAddr options)
          AgentListeningOnControlSocket
          AgentControlSocketClosed
          AgentControlClientConnected
          AgentControlSocketError
          AgentControlDriverTrace
          (kesReceiver pushKey)

  void $ runService `concurrently` runControl -- `concurrently` runEvolution
