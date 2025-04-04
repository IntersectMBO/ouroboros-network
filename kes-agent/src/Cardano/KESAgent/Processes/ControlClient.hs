{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Clients for the KES Agent.
module Cardano.KESAgent.Processes.ControlClient
where

import Cardano.KESAgent.KES.Crypto (Crypto (..))
import Cardano.KESAgent.KES.OCert (OCert (..))
import Cardano.KESAgent.Protocols.AgentInfo
import qualified Cardano.KESAgent.Protocols.Control.V0.Driver as CP0
import qualified Cardano.KESAgent.Protocols.Control.V0.Peers as CP0
import qualified Cardano.KESAgent.Protocols.Control.V0.Protocol as CP0
import qualified Cardano.KESAgent.Protocols.Control.V1.Driver as CP1
import qualified Cardano.KESAgent.Protocols.Control.V1.Peers as CP1
import qualified Cardano.KESAgent.Protocols.Control.V1.Protocol as CP1
import qualified Cardano.KESAgent.Protocols.Control.V2.Driver as CP2
import qualified Cardano.KESAgent.Protocols.Control.V2.Peers as CP2
import qualified Cardano.KESAgent.Protocols.Control.V2.Protocol as CP2
import Cardano.KESAgent.Protocols.RecvResult (RecvResult (..))
import Cardano.KESAgent.Protocols.StandardCrypto
import Cardano.KESAgent.Protocols.Types
import Cardano.KESAgent.Protocols.VersionHandshake.Driver
import Cardano.KESAgent.Protocols.VersionHandshake.Peers
import Cardano.KESAgent.Protocols.VersionedProtocol (
  NamedCrypto,
  VersionIdentifier,
  VersionedProtocol (..),
 )
import Cardano.KESAgent.Serialization.DirectCodec
import Cardano.KESAgent.Util.Pretty (Pretty (..))
import Cardano.KESAgent.Util.RetrySocket (retrySocketWith)

import Cardano.Crypto.DirectSerialise
import Cardano.Crypto.KES.Class

import Ouroboros.Network.RawBearer
import Ouroboros.Network.Snocket (Snocket (..))

import Control.Concurrent.Class.MonadMVar (MonadMVar)
import Control.Monad.Class.MonadST (MonadST)
import Control.Monad.Class.MonadSTM (MonadSTM)
import Control.Monad.Class.MonadThrow (MonadCatch, MonadThrow, SomeException, bracket)
import Control.Monad.Class.MonadTimer (MonadDelay)
import Control.Tracer (Tracer, traceWith)
import Data.Coerce
import Data.Functor.Contravariant ((>$<))
import Data.Kind
import Data.SerDoc.Class (
  HasInfo (..),
  Serializable (..),
 )
import Data.Typeable
import Network.TypedProtocol.Driver (runPeerWithDriver)
import Network.TypedProtocol.Peer.Server (IsPipelined (..), Server)

data ControlClientTrace
  = ControlClientVersionHandshakeDriverTrace VersionHandshakeDriverTrace
  | ControlClientVersionHandshakeFailed
  | ControlClientDriverTrace ControlDriverTrace
  | ControlClientSocketClosed
  | ControlClientConnected -- (SocketAddress Unix)
  | ControlClientAttemptReconnect Int
  | ControlClientSendingKey
  | ControlClientAbnormalTermination String
  | ControlClientKeyAccepted
  | ControlClientKeyRejected RecvResult
  deriving (Show)

instance Pretty ControlClientTrace where
  pretty (ControlClientDriverTrace d) = "Control: ControlDriver: " ++ pretty d
  pretty ControlClientConnected = "Control: Connected"
  pretty x = "Control: " ++ drop (length "ControlClient") (show x)

data ControlClientOptions m fd addr
  = ControlClientOptions
  { controlClientSnocket :: Snocket m fd addr
  , controlClientAddress :: addr
  , controlClientLocalAddress :: Maybe addr
  , controlClientRetryDelay :: Int
  , controlClientRetryExponential :: Bool
  , controlClientRetryAttempts :: Int
  }

type ControlHandler m a =
  RawBearer m -> Tracer m ControlClientTrace -> m a

type MonadControlClient m =
  ( Monad m
  , MonadThrow m
  , MonadCatch m
  , MonadDelay m
  , MonadST m
  , MonadSTM m
  , MonadFail m
  , MonadMVar m
  )

type ControlClientCrypto c =
  ( Crypto c
  , NamedCrypto c
  , ControlClientDrivers c
  )

type ControlClientContext m c =
  ( MonadControlClient m
  , ControlClientCrypto c
  , HasInfo (DirectCodec m) (VerKeyKES (KES c))
  , Serializable (DirectCodec m) (VerKeyKES (KES c))
  , DirectSerialise (SignKeyKES (KES c))
  , DirectDeserialise (SignKeyKES (KES c))
  )

runControlClient1 ::
  forall c m fd addr a.
  ControlClientContext m c =>
  ControlClientDrivers c =>
  (ControlClient m c -> ControlHandler m a) ->
  Proxy c ->
  MakeRawBearer m fd ->
  ControlClientOptions m fd addr ->
  Tracer m ControlClientTrace ->
  m a
runControlClient1 handler proxy mrb options tracer = do
  let s = controlClientSnocket options
  bracket
    (openToConnect s (controlClientAddress options))
    ( \fd -> do
        close s fd
        traceWith tracer ControlClientSocketClosed
    )
    ( \fd -> do
        case controlClientLocalAddress options of
          Just addr -> bind s fd addr
          Nothing -> return ()
        retrySocketWith
          (if controlClientRetryExponential options then ((min 5000000) . (* 2)) else id)
          (controlClientRetryDelay options * 1000)
          (controlClientRetryAttempts options)
          (\(e :: SomeException) n i -> traceWith tracer $ ControlClientAttemptReconnect n)
          (connect s fd (controlClientAddress options))
        traceWith tracer ControlClientConnected
        bearer <- getRawBearer mrb fd
        let drivers = controlClientDrivers proxy
        (protocolVersionMay :: Maybe VersionIdentifier, ()) <-
          runPeerWithDriver
            (versionHandshakeDriver bearer (ControlClientVersionHandshakeDriverTrace >$< tracer))
            (versionHandshakeClient (map fst drivers))
        case protocolVersionMay >>= (`lookup` drivers) of
          Nothing ->
            error
              "Protocol handshake failed (control)"
              traceWith
              tracer
              ControlClientVersionHandshakeFailed
          Just controlClient ->
            handler controlClient bearer tracer
    )

class IsControlHandler proto (m :: Type -> Type) a where
  type InitialState proto :: proto
  toHandler :: Server proto NonPipelined (InitialState proto) m a -> ControlHandler m a

constHandler :: m a -> ControlHandler m a
constHandler a _ _ = a

type MonadControlHandler m =
  ( MonadThrow m
  , MonadST m
  , MonadSTM m
  , MonadMVar m
  , MonadFail m
  )

instance
  ( MonadControlHandler m
  , Crypto c
  , Typeable c
  , NamedCrypto c
  , HasInfo (DirectCodec m) (VerKeyKES (KES c))
  , DirectSerialise (SignKeyKES (KES c))
  , DirectDeserialise (SignKeyKES (KES c))
  ) =>
  IsControlHandler (CP0.ControlProtocol m c) m a
  where
  type InitialState (CP0.ControlProtocol m c) = CP0.InitialState
  toHandler peer bearer tracer = do
    fst
      <$> runPeerWithDriver
        (CP0.controlDriver bearer $ ControlClientDriverTrace >$< tracer)
        peer

instance MonadControlHandler m => IsControlHandler (CP1.ControlProtocol m) m a where
  type InitialState (CP1.ControlProtocol m) = CP1.InitialState
  toHandler peer bearer tracer = do
    fst
      <$> runPeerWithDriver
        (CP1.controlDriver bearer $ ControlClientDriverTrace >$< tracer)
        peer

instance MonadControlHandler m => IsControlHandler (CP2.ControlProtocol m) m a where
  type InitialState (CP2.ControlProtocol m) = CP2.InitialState
  toHandler peer bearer tracer = do
    fst
      <$> runPeerWithDriver
        (CP2.controlDriver bearer $ ControlClientDriverTrace >$< tracer)
        peer

toHandlerEntry ::
  forall proto m a.
  IsControlHandler proto m a =>
  VersionedProtocol proto =>
  Server proto NonPipelined (InitialState proto) m a ->
  (VersionIdentifier, ControlHandler m a)
toHandlerEntry peer = (versionIdentifier (Proxy @proto), toHandler peer)

data ControlClient m c
  = ControlClient
  { controlGenKey :: ControlHandler m (Maybe (VerKeyKES (KES c)))
  , controlQueryKey :: ControlHandler m (Maybe (VerKeyKES (KES c)))
  , controlDropStagedKey :: ControlHandler m (Maybe (VerKeyKES (KES c)))
  , controlInstallKey ::
      OCert c ->
      ControlHandler m RecvResult
  , controlDropKey :: ControlHandler m RecvResult
  , controlGetInfo :: ControlHandler m (AgentInfo c)
  }

class ControlClientDrivers c where
  controlClientDrivers ::
    forall m.
    ControlClientContext m c =>
    Proxy c ->
    [(VersionIdentifier, ControlClient m c)]

mkControlClientCP2 ::
  ControlClientContext m StandardCrypto =>
  (VersionIdentifier, ControlClient m StandardCrypto)
mkControlClientCP2 =
  ( versionIdentifier (Proxy @(CP2.ControlProtocol _))
  , ControlClient
      (toHandler CP2.controlGenKey)
      (toHandler CP2.controlQueryKey)
      (toHandler CP2.controlDropStagedKey)
      (toHandler <$> CP2.controlInstallKey)
      (toHandler CP2.controlDropKey)
      (fmap (fmap toAgentInfo) <$> toHandler CP2.controlGetInfo)
  )

mkControlClientCP1 ::
  ControlClientContext m StandardCrypto =>
  (VersionIdentifier, ControlClient m StandardCrypto)
mkControlClientCP1 =
  ( versionIdentifier (Proxy @(CP1.ControlProtocol _))
  , ControlClient
      (toHandler CP1.controlGenKey)
      (toHandler CP1.controlQueryKey)
      (toHandler CP1.controlDropKey)
      (toHandler <$> CP1.controlInstallKey)
      (constHandler $ pure RecvErrorUnsupportedOperation)
      (fmap (fmap toAgentInfo) <$> toHandler CP1.controlGetInfo)
  )

mkControlClientCP0 ::
  forall m c.
  ControlClientContext m c =>
  NamedCrypto c =>
  Typeable c =>
  (VersionIdentifier, ControlClient m c)
mkControlClientCP0 =
  ( versionIdentifier (Proxy @(CP0.ControlProtocol _ c))
  , ControlClient
      (toHandler (CP0.controlGenKey @c))
      (toHandler (CP0.controlQueryKey @c))
      (toHandler (CP0.controlDropKey @c))
      (toHandler <$> CP0.controlInstallKey)
      (constHandler $ pure RecvErrorUnsupportedOperation)
      (fmap (fmap toAgentInfo) <$> toHandler (CP0.controlGetInfo @c))
  )

instance ControlClientDrivers StandardCrypto where
  controlClientDrivers _ =
    [mkControlClientCP2, mkControlClientCP1, mkControlClientCP0]

instance ControlClientDrivers SingleCrypto where
  controlClientDrivers _ =
    [mkControlClientCP0]

instance ControlClientDrivers MockCrypto where
  controlClientDrivers _ =
    [mkControlClientCP0]

class ToAgentInfo c a where
  toAgentInfo :: a -> AgentInfo c

instance ToAgentInfo c (CP0.AgentInfo c) where
  toAgentInfo info =
    AgentInfo
      { agentInfoCurrentBundle = convertBundleInfoCP0 <$> CP0.agentInfoCurrentBundle info
      , agentInfoStagedKey = convertKeyInfoCP0 <$> CP0.agentInfoStagedKey info
      , agentInfoCurrentTime = CP0.agentInfoCurrentTime info
      , agentInfoCurrentKESPeriod = CP0.agentInfoCurrentKESPeriod info
      , agentInfoBootstrapConnections = convertBootstrapInfoCP0 <$> CP0.agentInfoBootstrapConnections info
      }

convertBundleInfoCP0 :: CP0.BundleInfo c -> TaggedBundleInfo c
convertBundleInfoCP0 info =
  TaggedBundleInfo
    { taggedBundleInfo =
        Just
          BundleInfo
            { bundleInfoEvolution = CP0.bundleInfoEvolution info
            , bundleInfoStartKESPeriod = CP0.bundleInfoStartKESPeriod info
            , bundleInfoOCertN = CP0.bundleInfoOCertN info
            , bundleInfoVK = CP0.bundleInfoVK info
            , bundleInfoSigma = CP0.bundleInfoSigma info
            }
    , taggedBundleInfoTimestamp =
        Nothing
    }

convertKeyInfoCP0 :: CP0.KeyInfo c -> KeyInfo c
convertKeyInfoCP0 = coerce

convertBootstrapInfoCP0 :: CP0.BootstrapInfo -> BootstrapInfo
convertBootstrapInfoCP0 info =
  BootstrapInfo
    { bootstrapInfoAddress = CP0.bootstrapInfoAddress info
    , bootstrapInfoStatus = convertConnectionStatusCP0 $ CP0.bootstrapInfoStatus info
    }

convertConnectionStatusCP0 :: CP0.ConnectionStatus -> ConnectionStatus
convertConnectionStatusCP0 CP0.ConnectionUp = ConnectionUp
convertConnectionStatusCP0 CP0.ConnectionConnecting = ConnectionConnecting
convertConnectionStatusCP0 CP0.ConnectionDown = ConnectionDown

instance ToAgentInfo StandardCrypto CP1.AgentInfo where
  toAgentInfo info =
    AgentInfo
      { agentInfoCurrentBundle = convertBundleInfoCP1 <$> CP1.agentInfoCurrentBundle info
      , agentInfoStagedKey = convertKeyInfoCP1 <$> CP1.agentInfoStagedKey info
      , agentInfoCurrentTime = CP1.agentInfoCurrentTime info
      , agentInfoCurrentKESPeriod = CP1.agentInfoCurrentKESPeriod info
      , agentInfoBootstrapConnections = convertBootstrapInfoCP1 <$> CP1.agentInfoBootstrapConnections info
      }

convertBundleInfoCP1 :: CP1.BundleInfo -> TaggedBundleInfo StandardCrypto
convertBundleInfoCP1 info =
  TaggedBundleInfo
    { taggedBundleInfo =
        Just
          BundleInfo
            { bundleInfoEvolution = CP1.bundleInfoEvolution info
            , bundleInfoStartKESPeriod = CP1.bundleInfoStartKESPeriod info
            , bundleInfoOCertN = CP1.bundleInfoOCertN info
            , bundleInfoVK = CP1.bundleInfoVK info
            , bundleInfoSigma = CP1.bundleInfoSigma info
            }
    , taggedBundleInfoTimestamp =
        Nothing
    }

convertKeyInfoCP1 :: CP1.KeyInfo -> KeyInfo StandardCrypto
convertKeyInfoCP1 = coerce

convertBootstrapInfoCP1 :: CP1.BootstrapInfo -> BootstrapInfo
convertBootstrapInfoCP1 info =
  BootstrapInfo
    { bootstrapInfoAddress = CP1.bootstrapInfoAddress info
    , bootstrapInfoStatus = convertConnectionStatusCP1 $ CP1.bootstrapInfoStatus info
    }

convertConnectionStatusCP1 :: CP1.ConnectionStatus -> ConnectionStatus
convertConnectionStatusCP1 CP1.ConnectionUp = ConnectionUp
convertConnectionStatusCP1 CP1.ConnectionConnecting = ConnectionConnecting
convertConnectionStatusCP1 CP1.ConnectionDown = ConnectionDown

instance ToAgentInfo StandardCrypto CP2.AgentInfo where
  toAgentInfo info =
    AgentInfo
      { agentInfoCurrentBundle = convertTaggedBundleInfoCP2 <$> CP2.agentInfoCurrentBundle info
      , agentInfoStagedKey = convertKeyInfoCP2 <$> CP2.agentInfoStagedKey info
      , agentInfoCurrentTime = CP2.agentInfoCurrentTime info
      , agentInfoCurrentKESPeriod = CP2.agentInfoCurrentKESPeriod info
      , agentInfoBootstrapConnections = convertBootstrapInfoCP2 <$> CP2.agentInfoBootstrapConnections info
      }

convertTaggedBundleInfoCP2 :: CP2.TaggedBundleInfo -> TaggedBundleInfo StandardCrypto
convertTaggedBundleInfoCP2 tinfo =
  TaggedBundleInfo
    { taggedBundleInfo = convertBundleInfoCP2 <$> CP2.taggedBundleInfo tinfo
    , taggedBundleInfoTimestamp = CP2.taggedBundleInfoTimestamp tinfo
    }

convertBundleInfoCP2 :: CP2.BundleInfo -> BundleInfo StandardCrypto
convertBundleInfoCP2 info =
  BundleInfo
    { bundleInfoEvolution = CP2.bundleInfoEvolution info
    , bundleInfoStartKESPeriod = CP2.bundleInfoStartKESPeriod info
    , bundleInfoOCertN = CP2.bundleInfoOCertN info
    , bundleInfoVK = CP2.bundleInfoVK info
    , bundleInfoSigma = CP2.bundleInfoSigma info
    }

convertKeyInfoCP2 :: CP2.KeyInfo -> KeyInfo StandardCrypto
convertKeyInfoCP2 = coerce

convertBootstrapInfoCP2 :: CP2.BootstrapInfo -> BootstrapInfo
convertBootstrapInfoCP2 info =
  BootstrapInfo
    { bootstrapInfoAddress = CP2.bootstrapInfoAddress info
    , bootstrapInfoStatus = convertConnectionStatusCP2 $ CP2.bootstrapInfoStatus info
    }

convertConnectionStatusCP2 :: CP2.ConnectionStatus -> ConnectionStatus
convertConnectionStatusCP2 CP2.ConnectionUp = ConnectionUp
convertConnectionStatusCP2 CP2.ConnectionConnecting = ConnectionConnecting
convertConnectionStatusCP2 CP2.ConnectionDown = ConnectionDown
