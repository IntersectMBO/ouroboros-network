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
{-# LANGUAGE ConstraintKinds #-}

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
  , ControlCrypto (..)
  , ServiceCrypto (..)
  , defEvolutionConfig
  , AgentTrace (..)
  , ServiceClientTrace (..)
  , defAgentOptions
  , newAgent
  , runAgent
  , finalizeAgent
  )
  where

import Cardano.KESAgent.KES.Bundle ( Bundle (..) )
import Cardano.KESAgent.KES.Crypto ( Crypto (..) )
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
  , ServiceClientCrypto (..)
  )
import Cardano.KESAgent.Protocols.Types
import Cardano.KESAgent.Protocols.AgentInfo
import Cardano.KESAgent.Protocols.StandardCrypto
import Cardano.KESAgent.Protocols.BearerUtil
import qualified Cardano.KESAgent.Protocols.Control.V0.Driver as CP0
import qualified Cardano.KESAgent.Protocols.Control.V0.Peers as CP0
import qualified Cardano.KESAgent.Protocols.Control.V0.Protocol as CP0
import qualified Cardano.KESAgent.Protocols.Control.V1.Driver as CP1
import qualified Cardano.KESAgent.Protocols.Control.V1.Peers as CP1
import qualified Cardano.KESAgent.Protocols.Control.V1.Protocol as CP1
import Cardano.KESAgent.Protocols.RecvResult ( RecvResult (..) )
import qualified Cardano.KESAgent.Protocols.Service.V0.Driver as SP0
import qualified Cardano.KESAgent.Protocols.Service.V0.Peers as SP0
import qualified Cardano.KESAgent.Protocols.Service.V0.Protocol as SP0
import qualified Cardano.KESAgent.Protocols.Service.V1.Driver as SP1
import qualified Cardano.KESAgent.Protocols.Service.V1.Peers as SP1
import qualified Cardano.KESAgent.Protocols.Service.V1.Protocol as SP1
import Cardano.KESAgent.Protocols.VersionedProtocol
import Cardano.KESAgent.Protocols.VersionHandshake.Driver ( VersionHandshakeDriverTrace (..), versionHandshakeDriver )
import Cardano.KESAgent.Protocols.VersionHandshake.Peers ( versionHandshakeServer )
import Cardano.KESAgent.Protocols.VersionHandshake.Protocol ( VersionHandshakeProtocol )
import Cardano.KESAgent.Serialization.DirectCodec
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

import Data.Word
import Data.SerDoc.Info ( Description (..), aliasField, annField )
import Data.SerDoc.Class ( ViaEnum (..), Codec (..), HasInfo (..), Serializable (..), encodeEnum, decodeEnum, enumInfo )
import qualified Data.SerDoc.Info
import Data.SerDoc.TH (deriveSerDoc)
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
import Data.Coerce
import Data.Functor.Contravariant ( contramap, (>$<) )
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Maybe ( fromJust )
import Data.Proxy ( Proxy (..) )
import Data.Text ( Text )
import qualified Data.Text as Text
import Data.Time ( UTCTime )
import Data.Typeable ( Typeable )
import Network.TypedProtocol.Core ( PeerRole (..) )
import Network.TypedProtocol.Driver ( runPeerWithDriver )
import Text.Printf
import Data.Coerce

{-HLINT ignore "Use underscore" -}

data AgentTrace
  = AgentVersionHandshakeDriverTrace VersionHandshakeDriverTrace
  | AgentServiceDriverTrace ServiceDriverTrace
  | AgentControlDriverTrace ControlDriverTrace
  | AgentServiceVersionHandshakeFailed
  | AgentControlVersionHandshakeFailed
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
          . Monad m
         => MonadSTM m
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

genKey :: AgentContext m c
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

dropKey :: AgentContext m c
        => Agent c m fd addr -> m (Maybe (VerKeyKES (KES c)))
dropKey agent = do
  keyMay <- atomically $ takeTMVar (agentStagedKeyVar agent)
  maybe (return ()) releaseCRef keyMay
  return Nothing
  `finally` do
    atomically $ putTMVar (agentStagedKeyVar agent) Nothing

queryKey :: AgentContext m c
         => Agent c m fd addr -> m (Maybe (VerKeyKES (KES c)))
queryKey agent = do
  withStagedKey agent "queryKey" $ \keyMay -> do
    case keyMay of
      Nothing -> return Nothing
      Just skpVar -> withCRefValue skpVar $ \skp -> do
        vk <- deriveVerKeyKES (skWithoutPeriodKES skp)
        return $ Just vk

installKey :: AgentContext m c
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

class FromAgentInfo c a where
  fromAgentInfo :: AgentInfo c -> a

instance FromAgentInfo c (CP0.AgentInfo c) where
  fromAgentInfo info =
    CP0.AgentInfo
      { CP0.agentInfoCurrentBundle = convertBundleInfoCP0 <$> agentInfoCurrentBundle info
      , CP0.agentInfoStagedKey = convertKeyInfoCP0 <$> agentInfoStagedKey info
      , CP0.agentInfoCurrentTime = agentInfoCurrentTime info
      , CP0.agentInfoCurrentKESPeriod = agentInfoCurrentKESPeriod info
      , CP0.agentInfoBootstrapConnections = convertBootstrapInfoCP0 <$> agentInfoBootstrapConnections info
      }

convertBundleInfoCP0 :: BundleInfo c -> CP0.BundleInfo c
convertBundleInfoCP0 info =
  CP0.BundleInfo
    { CP0.bundleInfoEvolution = bundleInfoEvolution info
    , CP0.bundleInfoStartKESPeriod = bundleInfoStartKESPeriod info
    , CP0.bundleInfoOCertN = bundleInfoOCertN info
    , CP0.bundleInfoVK = bundleInfoVK info
    , CP0.bundleInfoSigma = bundleInfoSigma info
    }

convertKeyInfoCP0 :: KeyInfo c -> CP0.KeyInfo c
convertKeyInfoCP0 = coerce

convertBootstrapInfoCP0 :: BootstrapInfo -> CP0.BootstrapInfo
convertBootstrapInfoCP0 info =
  CP0.BootstrapInfo
    { CP0.bootstrapInfoAddress = bootstrapInfoAddress info
    , CP0.bootstrapInfoStatus = convertConnectionStatusCP0 $ bootstrapInfoStatus info
    }

convertConnectionStatusCP0 :: ConnectionStatus -> CP0.ConnectionStatus
convertConnectionStatusCP0 ConnectionUp = CP0.ConnectionUp
convertConnectionStatusCP0 ConnectionConnecting = CP0.ConnectionConnecting
convertConnectionStatusCP0 ConnectionDown = CP0.ConnectionDown


instance FromAgentInfo StandardCrypto CP1.AgentInfo where
  fromAgentInfo info =
    CP1.AgentInfo
      { CP1.agentInfoCurrentBundle = convertBundleInfoCP1 <$> agentInfoCurrentBundle info
      , CP1.agentInfoStagedKey = convertKeyInfoCP1 <$> agentInfoStagedKey info
      , CP1.agentInfoCurrentTime = agentInfoCurrentTime info
      , CP1.agentInfoCurrentKESPeriod = agentInfoCurrentKESPeriod info
      , CP1.agentInfoBootstrapConnections = convertBootstrapInfoCP1 <$> agentInfoBootstrapConnections info
      }

convertBundleInfoCP1 :: BundleInfo StandardCrypto -> CP1.BundleInfo
convertBundleInfoCP1 info =
  CP1.BundleInfo
    { CP1.bundleInfoEvolution = bundleInfoEvolution info
    , CP1.bundleInfoStartKESPeriod = bundleInfoStartKESPeriod info
    , CP1.bundleInfoOCertN = bundleInfoOCertN info
    , CP1.bundleInfoVK = bundleInfoVK info
    , CP1.bundleInfoSigma = bundleInfoSigma info
    }

convertKeyInfoCP1 :: KeyInfo StandardCrypto -> CP1.KeyInfo
convertKeyInfoCP1 = coerce

convertBootstrapInfoCP1 :: BootstrapInfo -> CP1.BootstrapInfo
convertBootstrapInfoCP1 info =
  CP1.BootstrapInfo
    { CP1.bootstrapInfoAddress = bootstrapInfoAddress info
    , CP1.bootstrapInfoStatus = convertConnectionStatusCP1 $ bootstrapInfoStatus info
    }

convertConnectionStatusCP1 :: ConnectionStatus -> CP1.ConnectionStatus
convertConnectionStatusCP1 ConnectionUp = CP1.ConnectionUp
convertConnectionStatusCP1 ConnectionConnecting = CP1.ConnectionConnecting
convertConnectionStatusCP1 ConnectionDown = CP1.ConnectionDown

getInfo :: AgentContext m c
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

checkEvolution :: AgentContext m c => Agent c m fd addr -> m ()
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
           AgentContext m c
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

class ServiceCrypto c where
  availableServiceDrivers :: forall m
                              . AgentContext m c
                             => [ ( VersionIdentifier
                                  , RawBearer m
                                    -> Tracer m ServiceDriverTrace
                                    -> m (Bundle m c)
                                    -> m (Bundle m c)
                                    -> (RecvResult -> m ())
                                    -> m ()
                                  )
                                ]

mkServiceDriverSP0 :: forall m c
                    . AgentContext m c
                   => ( VersionIdentifier
                      , RawBearer m
                        -> Tracer m ServiceDriverTrace
                        -> m (Bundle m c)
                        -> m (Bundle m c)
                        -> (RecvResult -> m ())
                        -> m ()
                      )
mkServiceDriverSP0 =
  ( versionIdentifier (Proxy @(SP0.ServiceProtocol _ c))
  , \bearer tracer currentKey nextKey reportPushResult ->
      void $
        runPeerWithDriver
          (SP0.serviceDriver bearer tracer)
          (SP0.servicePusher currentKey nextKey reportPushResult)
  )

mkServiceDriverSP1 :: forall m
                    . MonadAgent m
                   => ( VersionIdentifier
                      , RawBearer m
                        -> Tracer m ServiceDriverTrace
                        -> m (Bundle m StandardCrypto)
                        -> m (Bundle m StandardCrypto)
                        -> (RecvResult -> m ())
                        -> m ()
                      )
mkServiceDriverSP1 =
  ( versionIdentifier (Proxy @(SP1.ServiceProtocol _))
  , \bearer tracer currentKey nextKey reportPushResult ->
      void $
        runPeerWithDriver
          (SP1.serviceDriver bearer tracer)
          (SP1.servicePusher currentKey nextKey reportPushResult)
  )

instance ServiceCrypto StandardCrypto where
  availableServiceDrivers =
    [ mkServiceDriverSP1
    , mkServiceDriverSP0
    ]

instance ServiceCrypto MockCrypto where
  availableServiceDrivers =
    [ mkServiceDriverSP0 ]

instance ServiceCrypto SingleCrypto where
  availableServiceDrivers =
    [ mkServiceDriverSP0 ]

class ControlCrypto c where
  availableControlDrivers :: forall m fd addr
                              . AgentContext m c
                             => [ ( VersionIdentifier
                                  , RawBearer m
                                    -> Tracer m ControlDriverTrace
                                    -> Agent c m fd addr
                                    -> m ()
                                  )
                                ]

mkControlDriverCP0 :: forall m fd addr c
                    . AgentContext m c
                   => ( VersionIdentifier
                      , RawBearer m
                        -> Tracer m ControlDriverTrace
                        -> Agent c m fd addr
                        -> m ()
                      )
mkControlDriverCP0 =
    ( versionIdentifier (Proxy @(CP0.ControlProtocol _ c))
    , \bearer tracer agent ->
          void $
            runPeerWithDriver
              (CP0.controlDriver bearer tracer)
              (CP0.controlReceiver
                (genKey agent)
                (dropKey agent)
                (queryKey agent)
                (installKey agent)
                (fromAgentInfo <$> getInfo agent))
    )

mkControlDriverCP1 :: forall m fd addr
                    . AgentContext m StandardCrypto
                   => ( VersionIdentifier
                      , RawBearer m
                        -> Tracer m ControlDriverTrace
                        -> Agent StandardCrypto m fd addr
                        -> m ()
                      )
mkControlDriverCP1 =
    ( versionIdentifier (Proxy @(CP1.ControlProtocol _))
    , \bearer tracer agent ->
          void $
            runPeerWithDriver
              (CP1.controlDriver bearer tracer)
              (CP1.controlReceiver
                (genKey agent)
                (dropKey agent)
                (queryKey agent)
                (installKey agent)
                (fromAgentInfo <$> getInfo agent))
    )

instance ControlCrypto StandardCrypto where
  availableControlDrivers =
    [ mkControlDriverCP1, mkControlDriverCP0 ]

instance ControlCrypto MockCrypto where
  availableControlDrivers =
    [ mkControlDriverCP0 ]

instance ControlCrypto SingleCrypto where
  availableControlDrivers =
    [ mkControlDriverCP0 ]

type MonadAgent m =
      ( Monad m
      , MonadAsync m
      , MonadCatch m
      , MonadFail m
      , MonadMVar m
      , MonadST m
      , MonadTimer m
      )

type AgentCrypto c =
      ( ContextDSIGN (DSIGN c) ~ ()
      , ContextKES (KES c) ~ ()
      , DSIGN.Signable (DSIGN c) (OCertSignable c)
      , DirectSerialise (SignKeyKES (KES c))
      , DirectDeserialise (SignKeyKES (KES c))
      , Crypto c
      , NamedCrypto c
      , ControlCrypto c
      , ServiceCrypto c
      , ServiceClientCrypto c
      , Typeable c
      )

type AgentContext m c =
      ( MonadAgent m
      , AgentCrypto c
      , HasInfo (DirectCodec m) (VerKeyKES (KES c))
      , HasInfo (DirectCodec m) (SignKeyKES (KES c))
      )

runAgent :: forall c m fd addr
          . AgentContext m c
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
          (\bearer tracer' -> do
            (protocolVersionMay :: Maybe VersionIdentifier, ()) <-
                runPeerWithDriver
                  (versionHandshakeDriver bearer (AgentVersionHandshakeDriverTrace >$< (agentTracer . agentOptions $ agent)))
                  (versionHandshakeServer (map fst (availableServiceDrivers @c @m)))
            case protocolVersionMay >>= (`lookup` (availableServiceDrivers @c @m)) of
              Nothing ->
                traceWith (agentTracer . agentOptions $ agent) AgentServiceVersionHandshakeFailed
              Just run ->
                withDuplexBearer bearer $ \bearer' ->
                  run bearer' tracer' currentKey nextKey reportPushResult
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
          (\bearer tracer' -> do
            (protocolVersionMay :: Maybe VersionIdentifier, ()) <-
                runPeerWithDriver
                  (versionHandshakeDriver bearer (AgentVersionHandshakeDriverTrace >$< (agentTracer . agentOptions $ agent)))
                  (versionHandshakeServer (map fst (availableControlDrivers @c @m)))
            case protocolVersionMay >>= (`lookup` (availableControlDrivers @c @m)) of
              Nothing ->
                traceWith (agentTracer . agentOptions $ agent) AgentControlVersionHandshakeFailed
              Just run ->
                run bearer tracer' agent
          )

  void $ runService
          `concurrently` runControl
          `concurrently` runEvolution
          `concurrently` runBootstraps

labelMyThread label = do
  tid <- myThreadId
  labelThread tid label
