{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuantifiedConstraints #-}

-- | The main Agent program.
-- The KES Agent opens two sockets:
--
-- - A \"control\" socket, to which a control server can connect in order to
--   push new keys into the agent.
-- - A \"service\" socket, to which a Node can connect in order to receive the
--   current KES key and future key updates.
module Cardano.KESAgent.Processes.Agent
  ( Agent
  , AgentOptions (..)
  , EvolutionConfig (..)
  , defEvolutionConfig
  , AgentTrace (..)
  , ServiceClientTrace (..)
  , defAgentOptions
  , newAgent
  , runAgent
  , finalizeAgent
  )
  where

import Cardano.KESAgent.KES.Classes ( MonadKES )
import Cardano.KESAgent.KES.Crypto ( Crypto (..) )
import Cardano.KESAgent.KES.Bundle ( Bundle (..) )
import Cardano.KESAgent.KES.Evolution
  ( getCurrentKESPeriodWith
  , updateKESTo
  , EvolutionConfig (..)
  , defEvolutionConfig
  )
import Cardano.KESAgent.KES.OCert
  ( KESPeriod (..)
  , OCert (..)
  , OCertSignable
  , validateOCert
  )
import Cardano.KESAgent.Processes.ServiceClient
  ( runServiceClientForever
  , ServiceClientOptions (..)
  , ServiceClientTrace (..)
  )
import Cardano.KESAgent.Protocols.Control.Driver ( ControlDriverTrace (..), controlDriver )
import Cardano.KESAgent.Protocols.Control.Peers ( controlReceiver )
import Cardano.KESAgent.Protocols.Control.Protocol
  ( ControlProtocol
  , AgentInfo (..)
  , BundleInfo (..)
  , KeyInfo (..)
  , BootstrapInfo (..)
  , ConnectionStatus (..)
  )
import Cardano.KESAgent.Protocols.RecvResult ( RecvResult (..) )
import Cardano.KESAgent.Protocols.Service.Driver ( ServiceDriverTrace (..), serviceDriver, withDuplexBearer, BearerConnectionClosed )
import Cardano.KESAgent.Protocols.Service.Peers ( servicePusher )
import Cardano.KESAgent.Protocols.Service.Protocol ( ServiceProtocol )
import Cardano.KESAgent.Protocols.VersionedProtocol ( VersionedProtocol (..), NamedCrypto (..) )
import Cardano.KESAgent.Serialization.TextEnvelope ( decodeTextEnvelopeFile )
import Cardano.KESAgent.Serialization.Spec (HasSerInfo)
import Cardano.KESAgent.Util.Pretty ( Pretty (..), strLength )
import Cardano.KESAgent.Util.RefCounting
  ( CRef
  , CRefEvent (..)
  , CRefID
  , acquireCRef
  , newCRef
  , newCRefWith
  , readCRef
  , releaseCRef
  , withCRefValue
  )

import Cardano.Crypto.DSIGN.Class ( DSIGNAlgorithm (..), VerKeyDSIGN )
import qualified Cardano.Crypto.DSIGN.Class as DSIGN
import Cardano.Crypto.DirectSerialise
  ( DirectDeserialise (..)
  , DirectSerialise (..)
  )
import Cardano.Crypto.KES.Class
  ( ContextKES
  , KESAlgorithm (..)
  , SignKeyWithPeriodKES (..)
  , forgetSignKeyKES
  , rawSerialiseSignKeyKES
  , genKeyKES
  , deriveVerKeyKES
  )
import Cardano.Crypto.Libsodium.MLockedSeed

import Ouroboros.Network.RawBearer
import Ouroboros.Network.Snocket ( Accept (..), Accepted (..), Snocket (..) )

import Data.Coerce
import Control.Concurrent.Class.MonadMVar
  ( MVar
  , MonadMVar
  , newEmptyMVar
  , newMVar
  , putMVar
  , readMVar
  , tryTakeMVar
  , withMVar
  )
import Control.Concurrent.Class.MonadSTM ( MonadSTM, retry )
import Control.Concurrent.Class.MonadSTM.TChan
  ( TChan
  , dupTChan
  , newBroadcastTChan
  , readTChan
  , writeTChan
  )
import Control.Concurrent.Class.MonadSTM.TMVar
  ( TMVar
  , newEmptyTMVar
  , newTMVar
  , newTMVarIO
  , putTMVar
  , readTMVar
  , takeTMVar
  , tryReadTMVar
  , tryTakeTMVar
  )
import Control.Monad ( forever, void, when )
import Control.Monad.Class.MonadAsync
  ( MonadAsync
  , concurrently
  , concurrently_
  , mapConcurrently_
  )
import Control.Monad.Class.MonadFork ( labelThread, myThreadId )
import Control.Monad.Class.MonadST ( MonadST )
import Control.Monad.Class.MonadSTM ( atomically )
import Control.Monad.Class.MonadThrow
  ( MonadCatch
  , MonadThrow
  , SomeException
  , bracket
  , catch
  , finally
  , throwIO
  )
import Control.Monad.Class.MonadTime ( MonadTime (..) )
import Control.Monad.Class.MonadTimer ( threadDelay, MonadTimer )
import Control.Tracer ( Tracer (..), nullTracer, traceWith )
import Data.ByteString ( ByteString )
import Data.Functor.Contravariant ( contramap, (>$<) )
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Maybe ( fromJust )
import Data.Proxy ( Proxy (..) )
import Data.Text ( Text )
import qualified Data.Text as Text
import Data.Time ( UTCTime )
import Data.Typeable ( Typeable )
import Network.TypedProtocol.Core ( Peer (..), PeerRole (..) )
import Network.TypedProtocol.Driver ( runPeerWithDriver )
import Text.Printf

{-HLINT ignore "Use underscore" -}

data AgentTrace
  = AgentServiceDriverTrace ServiceDriverTrace
  | AgentControlDriverTrace ControlDriverTrace
  | AgentBootstrapTrace ServiceClientTrace
  | AgentReplacingPreviousKey String String
  | AgentRejectingKey String
  | AgentInstallingNewKey String
  | AgentSkippingOldKey String String
  | AgentServiceSocketClosed String
  | AgentListeningOnServiceSocket String
  | AgentServiceClientConnected String String
  | AgentServiceClientDisconnected String
  | AgentServiceSocketError String
  | AgentControlSocketClosed String
  | AgentListeningOnControlSocket String
  | AgentControlClientConnected String String
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
  pretty (AgentServiceSocketClosed a) = "Agent: ServiceSocketClosed: " ++ a
  pretty (AgentServiceClientConnected a b) = "Agent: ServiceClientConnected: " ++ a ++ " " ++ b
  pretty (AgentServiceClientDisconnected a) = "Agent: ServiceClientDisconnected: " ++ a
  pretty (AgentServiceSocketError e) = "Agent: ServiceSocketError: " ++ e

  pretty (AgentControlDriverTrace d) = "Agent: ControlDriver: " ++ pretty d
  pretty (AgentControlSocketClosed a) = "Agent: ControlSocketClosed: " ++ a
  pretty (AgentControlClientConnected a b) = "Agent: ControlClientConnected: " ++ a ++ " " ++ b
  pretty (AgentControlClientDisconnected a) = "Agent: ControlClientDisconnected: " ++ a
  pretty (AgentControlSocketError e) = "Agent: ControlSocketError: " ++ e
  pretty x = "Agent: " ++ drop (strLength "Agent") (show x)

data AgentOptions m addr c =
  AgentOptions
    { agentTracer :: Tracer m AgentTrace
      -- | Socket on which the agent will be listening for control messages,
      -- i.e., KES keys being pushed from a control server.
    , agentControlAddr :: addr

      -- | Socket on which the agent will send KES keys to any connected nodes.
    , agentServiceAddr :: addr

      -- | Sockets that the agent will use for bootstrapping from other agents.
    , agentBootstrapAddr :: [addr]

      -- | Evolution configuration: genesis, slot duration, slots per KES period
    , agentEvolutionConfig :: EvolutionConfig

      -- | Return the current time. Should normally be set to
      -- 'getPOSIXTime', but overriding this may be desirable for testing
      -- purposes.
    , agentGetCurrentTime :: m UTCTime

    , agentColdVerKey :: VerKeyDSIGN (DSIGN c)

    , agentGenSeed :: m (MLockedSeed (SeedSizeKES (KES c)))
    }

defAgentOptions :: MonadTime m => AgentOptions m addr c
defAgentOptions = AgentOptions
  { agentControlAddr = error "missing control address"
  , agentServiceAddr = error "missing service address"
  , agentBootstrapAddr = []
  , agentEvolutionConfig = defEvolutionConfig
  , agentGetCurrentTime = getCurrentTime
  , agentTracer = nullTracer
  , agentColdVerKey = error "missing cold verification key file"
  , agentGenSeed = error "missing seed generator"
  }

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
    , agentOptions :: AgentOptions m addr c
    , agentCurrentKeyVar :: TMVar m (Maybe (Bundle m c))
    , agentStagedKeyVar :: TMVar m (Maybe (CRef m (SignKeyWithPeriodKES (KES c))))
    , agentNextKeyChan :: TChan m (Bundle m c)
    , agentServiceFD :: fd
    , agentControlFD :: fd
    , agentBootstrapConnections :: TMVar m (Map Text ConnectionStatus)
    }

newAgent :: forall c m fd addr
          . MonadKES m c
         => Show addr
         => Show fd
         => Proxy c
         -> Snocket m fd addr
         -> MakeRawBearer m fd
         -> AgentOptions m addr c
         -> m (Agent c m fd addr)
newAgent _p s mrb options = do
  stagedKeyVar :: TMVar m (Maybe (CRef m (SignKeyWithPeriodKES (KES c))))
               <- newTMVarIO Nothing
  currentKeyVar :: TMVar m (Maybe (Bundle m c))
                <- newTMVarIO Nothing
  nextKeyChan <- atomically newBroadcastTChan
  bootstrapConnectionsVar <- newTMVarIO mempty

  serviceFD <- open s (addrFamily s (agentServiceAddr options))
  bind s serviceFD (agentServiceAddr options)

  controlFD <- open s (addrFamily s (agentControlAddr options))
  bind s controlFD (agentControlAddr options)

  return Agent
    { agentSnocket = s
    , agentMRB = mrb
    , agentOptions = options
    , agentStagedKeyVar = stagedKeyVar
    , agentCurrentKeyVar = currentKeyVar
    , agentNextKeyChan = nextKeyChan
    , agentServiceFD = serviceFD
    , agentControlFD = controlFD
    , agentBootstrapConnections = bootstrapConnectionsVar
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

alterBundle :: (MonadSTM m, MonadThrow m)
          => Agent c m fd addr
          -> String
          -> (Maybe (Bundle m c) -> m (Maybe (Bundle m c), a))
          -> m a
alterBundle agent context f = do
  agentTrace agent $ AgentLockRequest context
  atomically (takeTMVar (agentCurrentKeyVar agent)) >>= \bundle -> do
    agentTrace agent (AgentLockAcquired context)
    (bundle', retval) <- f bundle
    atomically $ putTMVar (agentCurrentKeyVar agent) bundle'
    agentTrace agent (AgentLockReleased context)
    return retval

withBundle :: (MonadSTM m, MonadThrow m)
          => Agent c m fd addr
          -> String
          -> (Maybe (Bundle m c) -> m a)
          -> m a
withBundle agent context f = do
  agentTrace agent $ AgentLockRequest context
  atomically (takeTMVar (agentCurrentKeyVar agent)) >>= \bundle -> do
    agentTrace agent (AgentLockAcquired context)
    retval <- f bundle
    atomically $ putTMVar (agentCurrentKeyVar agent) bundle
    agentTrace agent (AgentLockReleased context)
    return retval

alterStagedKey :: (MonadSTM m, MonadThrow m)
          => Agent c m fd addr
          -> String
          -> ( Maybe (CRef m (SignKeyWithPeriodKES (KES c)))
               -> m (Maybe (CRef m (SignKeyWithPeriodKES (KES c))), a)
             )
          -> m a
alterStagedKey agent context f = do
  agentTrace agent $ AgentLockRequest context
  atomically (takeTMVar (agentStagedKeyVar agent)) >>= \skp -> do
    agentTrace agent (AgentLockAcquired context)
    (skp', retval) <- f skp
    atomically $ putTMVar (agentStagedKeyVar agent) skp'
    agentTrace agent (AgentLockReleased context)
    return retval

withStagedKey :: (MonadSTM m, MonadThrow m)
          => Agent c m fd addr
          -> String
          -> (Maybe (CRef m (SignKeyWithPeriodKES (KES c))) -> m a)
          -> m a
withStagedKey agent context f = do
  agentTrace agent $ AgentLockRequest context
  atomically (takeTMVar (agentStagedKeyVar agent)) >>= \skp -> do
    agentTrace agent (AgentLockAcquired context)
    retval <- f skp
    atomically $ putTMVar (agentStagedKeyVar agent) skp
    agentTrace agent (AgentLockReleased context)
    return retval


formatKey :: OCert c -> String
formatKey ocert =
  let serialNumber = ocertN ocert
  in printf "%i" serialNumber

genKey :: (MonadThrow m, MonadST m, MonadSTM m, MonadMVar m, KESAlgorithm (KES c), ContextKES (KES c) ~ ())
       => Agent c m fd addr -> m (Maybe (VerKeyKES (KES c)))
genKey agent = do
  bracket
    (agentGenSeed . agentOptions $ agent)
    mlockedSeedFinalize $ \seed -> do
      sk <- genKeyKES seed
      oldSKVarMay <- atomically $ takeTMVar (agentStagedKeyVar agent)
      maybe (return ()) releaseCRef oldSKVarMay
      newSKVar <- newCRef (forgetSignKeyKES . skWithoutPeriodKES) (SignKeyWithPeriodKES sk 0)
      atomically $ putTMVar (agentStagedKeyVar agent) (Just newSKVar)
      vk <- deriveVerKeyKES sk
      return $ Just vk

dropKey :: (MonadThrow m, MonadST m, MonadSTM m, MonadMVar m, KESAlgorithm (KES c), ContextKES (KES c) ~ ())
        => Agent c m fd addr -> m (Maybe (VerKeyKES (KES c)))
dropKey agent = do
  keyMay <- atomically $ takeTMVar (agentStagedKeyVar agent)
  maybe (return ()) releaseCRef keyMay
  return Nothing
  `finally` do
    atomically $ putTMVar (agentStagedKeyVar agent) Nothing

queryKey :: (MonadThrow m, MonadST m, MonadSTM m, MonadMVar m, KESAlgorithm (KES c), ContextKES (KES c) ~ ())
         => Agent c m fd addr -> m (Maybe (VerKeyKES (KES c)))
queryKey agent = do
  withStagedKey agent "queryKey" $ \keyMay -> do
    case keyMay of
      Nothing -> return Nothing
      Just skpVar -> withCRefValue skpVar $ \skp -> do
        vk <- deriveVerKeyKES (skWithoutPeriodKES skp)
        return $ Just vk

installKey :: ( MonadThrow m
              , MonadST m
              , MonadSTM m
              , MonadMVar m
              , Crypto c
              , NamedCrypto c
              , ContextKES (KES c) ~ ()
              , ContextDSIGN (DSIGN c) ~ ()
              , DSIGN.Signable (DSIGN c) (OCertSignable c)
              )
         => Agent c m fd addr
         -> OCert c
         -> m RecvResult
installKey agent oc = do
  newKeyMay <- alterStagedKey agent "install staged key" $ \keyMay -> do
    case keyMay of
      Nothing -> do
        return (Nothing, Nothing)
      Just skpVar -> do
        return (Nothing, Just skpVar)
  maybe
    (return RecvErrorNoKey)
    (\newKey -> pushKey agent (Bundle newKey oc))
    newKeyMay

getInfo :: ( MonadThrow m
           , MonadST m
           , MonadSTM m
           , MonadMVar m
           , Crypto c
           , NamedCrypto c
           , ContextKES (KES c) ~ ()
           , ContextDSIGN (DSIGN c) ~ ()
           , DSIGN.Signable (DSIGN c) (OCertSignable c)
           )
        => Agent c m fd addr -> m (AgentInfo c)
getInfo agent = do
  bundleInfoMay <- do
      withBundle agent "get info" $ \case
        Nothing ->
          return Nothing
        Just bundle ->
          withCRefValue (bundleSKP bundle) $ \skp -> do
            return $ Just BundleInfo
                        { bundleInfoEvolution = fromIntegral $ periodKES skp
                        , bundleInfoStartKESPeriod = ocertKESPeriod (bundleOC bundle)
                        , bundleInfoOCertN = ocertN (bundleOC bundle)
                        , bundleInfoVK = ocertVkHot (bundleOC bundle)
                        , bundleInfoSigma = ocertSigma (bundleOC bundle)
                        }

  keyInfoMay <- do
      withStagedKey agent "get info" $ \case
        Nothing -> do
          return Nothing
        Just skpVar -> withCRefValue skpVar $ \skp -> do
          vk <- deriveVerKeyKES (skWithoutPeriodKES skp)
          return $ Just (KeyInfo vk)

  now <- agentGetCurrentTime (agentOptions agent)
  kesPeriod <- getCurrentKESPeriodWith
                (agentGetCurrentTime $ agentOptions agent)
                (agentEvolutionConfig $ agentOptions agent)
  bootstrapStatusesRaw <- Map.toAscList <$> atomically (readTMVar (agentBootstrapConnections agent))
  let bootstrapStatuses = map (uncurry BootstrapInfo) bootstrapStatusesRaw

  return AgentInfo
    { agentInfoCurrentBundle = bundleInfoMay
    , agentInfoStagedKey = keyInfoMay
    , agentInfoCurrentTime = now
    , agentInfoCurrentKESPeriod = kesPeriod
    , agentInfoBootstrapConnections = bootstrapStatuses
    }

checkEvolution :: (MonadThrow m, MonadST m, MonadSTM m, MonadMVar m, KESAlgorithm (KES c), ContextKES (KES c) ~ ()) => Agent c m fd addr -> m ()
checkEvolution agent = do
  p' <- getCurrentKESPeriodWith
          (agentGetCurrentTime $ agentOptions agent)
          (agentEvolutionConfig $ agentOptions agent)
  agentTrace agent $ AgentCheckEvolution p'
  alterBundle agent "checkEvolution" $ \bundleMay -> do
    case bundleMay of
      Nothing -> do
        agentTrace agent AgentNoKeyToEvolve
        return (Nothing, ())
      Just (Bundle keyVar oc) -> withCRefValue keyVar $ \key -> do
        let p = KESPeriod $ unKESPeriod (ocertKESPeriod oc) + periodKES key
        if p < p' then do
          keyMay' <- updateKESTo () p' oc key
          case keyMay' of
            Nothing -> do
              agentTrace agent $ AgentKeyExpired p p'
              releaseCRef keyVar
              return (Nothing, ())
            Just key' -> do
              agentTrace agent $ AgentKeyEvolved p p'
              keyVar' <- newCRefWith (agentCRefTracer agent) (forgetSignKeyKES . skWithoutPeriodKES) key'
              releaseCRef keyVar
              return (Just (Bundle keyVar' oc), ())
        else do
          agentTrace agent $ AgentKeyNotEvolved p p'
          return (bundleMay, ())

pushKey :: forall c m fd addr.
           ( MonadMVar m
           , MonadST m
           , MonadSTM m
           , Crypto c
           , DSIGN.Signable (DSIGN c) (OCertSignable c)
           , KESAlgorithm (KES c)
           , ContextKES (KES c) ~ ()
           , DSIGNAlgorithm (DSIGN c)
           , ContextDSIGN (DSIGN c) ~ ()
           , MonadThrow m
           )
        => Agent c m fd addr
        -> Bundle m c
        -> m RecvResult
pushKey agent bundle = do
  vkKES <- withCRefValue (bundleSKP bundle) (deriveVerKeyKES . skWithoutPeriodKES)

  let validationResult =
        validateOCert
          (agentColdVerKey (agentOptions agent))
          vkKES
          (bundleOC bundle)
  case validationResult of
    Left err ->  do
      agentTrace agent $ AgentRejectingKey err
      return RecvErrorInvalidOpCert
    Right () ->
      go

  where
    go = do
      result <- alterBundle agent "pushKey" $ \oldKeyOcMay -> do
        let keyVar = bundleSKP bundle
            oc = bundleOC bundle
        acquireCRef keyVar
        let keyStr = formatKey oc

        let report bundle = atomically $ do
              -- The TMVar is now empty; we write to the next key signal channel
              -- /before/ putting the new key in the MVar, because we want to make it
              -- such that when the consumer picks up the signal, the next update
              -- will be the correct one. Since the MVar is empty at this point, the
              -- consumers will block until we put the key back in.
              writeTChan (agentNextKeyChan agent) bundle
              return (Just bundle, RecvOK)

        case oldKeyOcMay of
          Just (Bundle oldKeyVar oldOC) -> do
            let oldKeyStr = formatKey oldOC
            if ocertN oldOC >= ocertN oc then do
              releaseCRef keyVar
              agentTrace agent $ AgentSkippingOldKey oldKeyStr keyStr
              return (oldKeyOcMay, RecvErrorKeyOutdated)
            else do
              releaseCRef oldKeyVar
              agentTrace agent $ AgentReplacingPreviousKey oldKeyStr keyStr
              report (Bundle keyVar oc)
          Nothing -> do
            agentTrace agent $ AgentInstallingNewKey keyStr
            report (Bundle keyVar oc)
      checkEvolution agent
      return result

runListener :: forall m c fd addr st (pr :: PeerRole) t a
             . MonadThrow m
            => MonadFail m
            => MonadST m
            => MonadSTM m
            => MonadMVar m
            => MonadAsync m
            => MonadCatch m
            => Show fd
            => Snocket m fd addr
            -> fd
            -> String
            -> MakeRawBearer m fd
            -> Tracer m AgentTrace
            -> (String -> AgentTrace)
            -> (String -> AgentTrace)
            -> (String -> String -> AgentTrace)
            -> (String -> AgentTrace)
            -> (String -> AgentTrace)
            -> (t -> AgentTrace)
            -> (RawBearer m -> Tracer m t -> m ())
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
      tClientDisconnected
      tSocketError
      tDriverTrace
      handle = do
  listen s fd
  traceWith tracer (tListeningOnSocket addrStr)

  let handleConnection fd' = do
        traceWith tracer (tClientConnected (show fd) (show fd'))
        bearer <- getRawBearer mrb fd'
        handle bearer (tDriverTrace >$< tracer)

  let logAndContinue :: SomeException -> m ()
      logAndContinue e = traceWith tracer (tSocketError (show e))

      handleClientDisconnect :: fd -> BearerConnectionClosed -> m ()
      handleClientDisconnect fd _ =
        traceWith tracer (tClientDisconnected $ show fd)

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
              (handleConnection fd'
                `catch` handleClientDisconnect fd'
                `catch` logAndContinue
                `finally` (close s fd' >> traceWith tracer (tSocketClosed $ show fd'))
              )

  (accept s fd >>= loop) `catch` logAndContinue

runAgent :: forall c m fd addr
          . (forall x y. Coercible x y => Coercible (m x) (m y))
         => MonadKES m c
         => MonadTimer m
         => ContextDSIGN (DSIGN c) ~ ()
         => DSIGN.Signable (DSIGN c) (OCertSignable c)
         => HasSerInfo (VerKeyKES (KES c))
         => HasSerInfo (SignKeyKES (KES c))
         => Crypto c
         => NamedCrypto c
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

        let currentKey = atomically $ do
              readTMVar (agentCurrentKeyVar agent) >>= maybe retry return
        let nextKey = atomically $ readTChan nextKeyChanRcv

        let reportPushResult = const (return ())

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
          (\bearer tracer' ->
            withDuplexBearer bearer $ \bearer' -> do
              void $
                runPeerWithDriver
                  (serviceDriver bearer' tracer')
                  (servicePusher currentKey nextKey reportPushResult)
                  ()
          )

  let runBootstrap :: addr -> m ()
      runBootstrap addr = do
        labelMyThread $ "bootstrap-" ++ show addr
        let scOpts = ServiceClientOptions
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
          (pushKey agent)
          (parentTracer <> connStatTracer)

  let runBootstraps =
        mapConcurrently_ runBootstrap $ agentBootstrapAddr (agentOptions agent)

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
          AgentControlClientDisconnected
          AgentControlSocketError
          AgentControlDriverTrace
          (\bearer tracer' ->
            void $
              runPeerWithDriver
                (controlDriver bearer tracer')
                (controlReceiver
                  (genKey agent)
                  (dropKey agent)
                  (queryKey agent)
                  (installKey agent)
                  (getInfo agent))
                ()
          )

  void $ runService
          `concurrently` runControl
          `concurrently` runEvolution
          `concurrently` runBootstraps

labelMyThread label = do
  tid <- myThreadId
  labelThread tid label
