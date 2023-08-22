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
import Cardano.KESAgent.OCert ( KES, KESPeriod (..), OCert (..), Crypto (..) )
import Cardano.KESAgent.Peers ( kesPusher, kesReceiver )
import Cardano.KESAgent.Pretty ( Pretty (..), strLength )
import Cardano.KESAgent.Protocol ( KESProtocol, VersionedProtocol )
import Cardano.KESAgent.RefCounting
  ( CRef
  , acquireCRef
  , newCRefWith
  , releaseCRef
  , withCRefValue
  , CRefEvent (..)
  , CRefID
  )

import Cardano.Crypto.DirectSerialise
  ( DirectSerialise (..)
  , DirectDeserialise (..)
  )
import Cardano.Crypto.KES.Class
  ( KESAlgorithm (..)
  , SignKeyWithPeriodKES (..)
  , ContextKES
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
  , MonadMVar
  )
import Control.Monad.Class.MonadST ( MonadST )
import Control.Concurrent.Class.MonadSTM ( MonadSTM, retry )
import Control.Concurrent.Class.MonadSTM.TChan
  ( TChan
  , newBroadcastTChan
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
import Control.Monad.Class.MonadAsync ( MonadAsync, concurrently, concurrently_ )
import Control.Monad.Class.MonadSTM ( atomically )
import Control.Monad.Class.MonadFork ( labelThread, myThreadId )
import Control.Monad.Class.MonadThrow
  ( MonadThrow
  , MonadCatch
  , SomeException
  , bracket
  , catch
  , finally
  , throwIO
  )
import Control.Monad.Class.MonadTime ( MonadTime (..) )
import Control.Monad.Class.MonadTimer ( threadDelay )
import Control.Tracer ( Tracer, traceWith, nullTracer )
import Data.ByteString ( ByteString )
import Data.Functor.Contravariant ( (>$<), contramap )
import Data.Maybe ( fromJust )
import Data.Proxy ( Proxy (..) )
import Data.Time ( NominalDiffTime )
import Data.Time.Clock.POSIX ( utcTimeToPOSIXSeconds )
import Data.Typeable ( Typeable )
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
  | AgentKeyEvolved KESPeriod KESPeriod
  | AgentKeyExpired KESPeriod KESPeriod
  | AgentLockRequest String
  | AgentLockAcquired String
  | AgentLockReleased String
  | AgentCRefEvent CRefEvent
  deriving (Show)

instance Pretty AgentTrace where
  pretty (AgentServiceDriverTrace d) = "Agent: ServiceDriver: " ++ pretty d
  pretty (AgentControlDriverTrace d) = "Agent: ControlDriver: " ++ pretty d
  pretty (AgentServiceClientConnected a) = "Agent: ServiceClientConnected: " ++ a
  pretty (AgentServiceClientDisconnected a) = "Agent: ServiceClientDisconnected: " ++ a
  pretty (AgentServiceSocketError e) = "Agent: ServiceSocketError: " ++ e
  pretty (AgentControlClientConnected a) = "Agent: ControlClientConnected: " ++ a
  pretty (AgentControlClientDisconnected a) = "Agent: ControlClientDisconnected: " ++ a
  pretty (AgentControlSocketError e) = "Agent: ControlSocketError: " ++ e
  pretty x = "Agent: " ++ drop (strLength "Agent") (show x)

data AgentOptions m addr =
  AgentOptions
    { agentTracer :: Tracer m AgentTrace
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

defAgentOptions :: MonadTime m => AgentOptions m addr
defAgentOptions = AgentOptions
  { agentControlAddr = error "default"
  , agentServiceAddr = error "default"
  , agentGenesisTimestamp = 1506203091 -- real-world genesis on the production ledger
  , agentGetCurrentTime = utcTimeToPOSIXSeconds <$> getCurrentTime
  , agentTracer = nullTracer
  }

-- | A bundle of a KES key with a period, plus the matching op cert.
-- The key itself is stored as a 'CRef', rather than directly, which
-- allows us to pass keys around and forget them exactly when the last
-- reference is dropped. The downside to this is that we need to be
-- explicit about those references, which is what the 'CRef' type
-- achieves.
type KESBundle m c = (CRef m (SignKeyWithPeriodKES (KES c)), OCert c)

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
data Agent c m fd addr =
  Agent
    { agentSnocket :: Snocket m fd addr
    , agentMRB :: MakeRawBearer m fd
    , agentOptions :: AgentOptions m addr
    , agentCurrentKeyVar :: TMVar m (KESBundle m c)
    , agentNextKeyChan :: TChan m (KESBundle m c)
    , agentKeyLock :: MVar m ()
    , agentServiceFD :: fd
    , agentControlFD :: fd
    }

newAgent :: forall c m fd addr
          . MonadKES m c
         => Show addr
         => Show fd
         => Proxy c
         -> Snocket m fd addr
         -> MakeRawBearer m fd
         -> AgentOptions m addr
         -> m (Agent c m fd addr)
newAgent _p s mrb options = do
  currentKeyVar :: TMVar m (CRef m (SignKeyWithPeriodKES (KES c)), OCert c) <- atomically newEmptyTMVar
  nextKeyChan <- atomically newBroadcastTChan
  keyLock :: MVar m () <- newMVar ()

  serviceFD <- open s (addrFamily s (agentServiceAddr options))
  bind s serviceFD (agentServiceAddr options)

  controlFD <- open s (addrFamily s (agentControlAddr options))
  bind s controlFD (agentControlAddr options)

  return Agent
    { agentSnocket = s
    , agentMRB = mrb
    , agentOptions = options
    , agentCurrentKeyVar = currentKeyVar
    , agentNextKeyChan = nextKeyChan
    , agentKeyLock = keyLock
    , agentServiceFD = serviceFD
    , agentControlFD = controlFD
    }

finalizeAgent :: Monad m => Agent c m fd addr -> m ()
finalizeAgent agent = do
  let s = agentSnocket agent
  close s (agentServiceFD agent)
  close s (agentControlFD agent)

agentTrace :: Agent c m fd addr -> AgentTrace -> m ()
agentTrace agent = traceWith (agentTracer . agentOptions $ agent)

agentCRefTracer :: Agent c m fd addr -> Tracer m CRefEvent
agentCRefTracer = contramap AgentCRefEvent . agentTracer . agentOptions

withKeyUpdateLock :: MonadMVar m => Agent c m fd addr -> String -> m a -> m a
withKeyUpdateLock agent context a = do
  agentTrace agent $ AgentLockRequest context
  withMVar (agentKeyLock agent) $ \() -> do
    agentTrace agent (AgentLockAcquired context)
    result <- a
    agentTrace agent (AgentLockReleased context)
    return result

formatKey :: OCert c -> String
formatKey ocert =
  let serialNumber = ocertN ocert
  in printf "%i" serialNumber

checkEvolution :: (MonadThrow m, MonadST m, MonadSTM m, MonadMVar m, KESAlgorithm (KES c), ContextKES (KES c) ~ ()) => Agent c m fd addr -> m ()
checkEvolution agent = withKeyUpdateLock agent "checkEvolution" $ do
  p' <- getCurrentKESPeriodWith (agentGetCurrentTime $ agentOptions agent) (agentGenesisTimestamp $ agentOptions agent)
  agentTrace agent $ AgentCheckEvolution p'
  keyOcMay <- atomically $ tryTakeTMVar (agentCurrentKeyVar agent)
  case keyOcMay of
    Nothing -> do
      agentTrace agent AgentNoKeyToEvolve
      return ()
    Just (keyVar, oc) -> withCRefValue keyVar $ \key -> do
      let p = KESPeriod $ unKESPeriod (ocertKESPeriod oc) + periodKES key
      if p < p' then do
        keyMay' <- updateKESTo () p' oc key
        case keyMay' of
          Nothing -> do
            agentTrace agent $ AgentKeyExpired p p'
          Just key' -> do
            agentTrace agent $ AgentKeyEvolved p p'
            keyVar' <- newCRefWith (agentCRefTracer agent) (forgetSignKeyKES . skWithoutPeriodKES) key'
            void . atomically $ putTMVar (agentCurrentKeyVar agent) (keyVar', oc)
        releaseCRef keyVar
      else do
        agentTrace agent $ AgentKeyNotEvolved p p'
        void . atomically $ putTMVar (agentCurrentKeyVar agent) (keyVar, oc)

pushKey :: (MonadMVar m, MonadST m, MonadSTM m, KESAlgorithm (KES c), ContextKES (KES c) ~ (), MonadThrow m) => Agent c m fd addr -> CRef m (SignKeyWithPeriodKES (KES c)) -> OCert c -> m ()
pushKey agent keyVar oc = do
  withKeyUpdateLock agent "pushKey" $ do
    acquireCRef keyVar
    let keyStr = formatKey oc
    -- Empty the var in case there's anything there already
    oldKeyOcMay <- atomically $ tryTakeTMVar (agentCurrentKeyVar agent)

    case oldKeyOcMay of
      Just (oldKeyVar, oldOC) -> do
        let oldKeyStr = formatKey oldOC
        if ocertN oldOC >= ocertN oc then do
          releaseCRef keyVar
          agentTrace agent $ AgentSkippingOldKey oldKeyStr keyStr
          atomically $ putTMVar (agentCurrentKeyVar agent) (oldKeyVar, oldOC)
        else do
          releaseCRef oldKeyVar
          agentTrace agent $ AgentReplacingPreviousKey oldKeyStr keyStr
          atomically $ do
            writeTChan (agentNextKeyChan agent) (keyVar, oc)
            putTMVar (agentCurrentKeyVar agent) (keyVar, oc)
      Nothing -> do
        agentTrace agent $ AgentInstallingNewKey keyStr
        -- The TMVar is now empty; we write to the next key signal channel
        -- /before/ putting the new key in the MVar, because we want to make it
        -- such that when the consumer picks up the signal, the next update
        -- will be the correct one. Since the MVar is empty at this point, the
        -- consumers will block until we put the key back in.
        atomically $ do
          writeTChan (agentNextKeyChan agent) (keyVar, oc)
          putTMVar (agentCurrentKeyVar agent) (keyVar, oc)

  checkEvolution agent

runListener :: forall m c fd addr st (pr :: PeerRole) a
             . MonadThrow m
            => MonadFail m
            => MonadST m
            => MonadSTM m
            => MonadMVar m
            => MonadAsync m
            => MonadCatch m
            => Show fd
            => Crypto c
            => Typeable c
            => VersionedProtocol (KESProtocol m c)
            => DirectSerialise m (SignKeyKES (KES c))
            => DirectDeserialise m (SignKeyKES (KES c))
            => Snocket m fd addr
            -> fd
            -> String
            -> MakeRawBearer m fd
            -> Tracer m AgentTrace
            -> (String -> AgentTrace)
            -> (String -> AgentTrace)
            -> (String -> AgentTrace)
            -> (String -> AgentTrace)
            -> (DriverTrace -> AgentTrace)
            -> Peer (KESProtocol m c) pr st m a
            -> m ()
runListener
      s
      fd
      addrStr
      mrb
      tracer
      tListeningOnSocket
      tSocketClosed
      tClientConnected
      tSocketError
      tDriverTrace
      peer = do
  listen s fd
  traceWith tracer (tListeningOnSocket addrStr)

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

runAgent :: forall c m fd addr
          . MonadKES m c
         => Show addr
         => Show fd
         => Agent c m fd addr
         -> m ()
runAgent agent = do
  let runEvolution = do
        forever $ do
          -- Check time every 100 milliseconds, update key when period flips
          -- over.
          threadDelay 100_0000
          checkEvolution agent

  let runService = do
        labelMyThread "service"
        nextKeyChanRcv <- atomically $ dupTChan (agentNextKeyChan agent)

        let currentKey = atomically $ readTMVar (agentCurrentKeyVar agent)
        let nextKey = atomically $ Just <$> readTChan nextKeyChanRcv

        runListener
          (agentSnocket agent)
          (agentServiceFD agent)
          (show $ agentServiceAddr (agentOptions agent))
          (agentMRB agent)
          (agentTracer . agentOptions $ agent)
          AgentListeningOnServiceSocket
          AgentServiceSocketClosed
          AgentServiceClientConnected
          AgentServiceSocketError
          AgentServiceDriverTrace
          (kesPusher currentKey nextKey)

  let runControl = do
        labelMyThread "control"
        runListener
          (agentSnocket agent)
          (agentControlFD agent)
          (show $ agentControlAddr (agentOptions agent))
          (agentMRB agent)
          (agentTracer . agentOptions $ agent)
          AgentListeningOnControlSocket
          AgentControlSocketClosed
          AgentControlClientConnected
          AgentControlSocketError
          AgentControlDriverTrace
          (kesReceiver $ pushKey agent)

  void $ runService `concurrently` runControl `concurrently` runEvolution

labelMyThread label = do
  tid <- myThreadId
  labelThread tid label
