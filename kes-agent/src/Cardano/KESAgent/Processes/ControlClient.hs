{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

-- | Clients for the KES Agent.
module Cardano.KESAgent.Processes.ControlClient
  where

import Cardano.KESAgent.KES.Crypto ( Crypto (..) )
import Cardano.KESAgent.KES.OCert ( OCert (..) )
import Cardano.KESAgent.Util.Pretty ( Pretty (..) )
import Cardano.KESAgent.Protocols.Types
import Cardano.KESAgent.Protocols.AgentInfo
import Cardano.KESAgent.Protocols.VersionHandshake.Peers
import Cardano.KESAgent.Protocols.VersionHandshake.Protocol
import Cardano.KESAgent.Protocols.VersionHandshake.Driver
import Cardano.KESAgent.Protocols.StandardCrypto
import qualified Cardano.KESAgent.Protocols.Control.V0.Peers as CP0
import qualified Cardano.KESAgent.Protocols.Control.V0.Protocol as CP0
import qualified Cardano.KESAgent.Protocols.Control.V0.Driver as CP0
import qualified Cardano.KESAgent.Protocols.Control.V1.Peers as CP1
import qualified Cardano.KESAgent.Protocols.Control.V1.Protocol as CP1
import qualified Cardano.KESAgent.Protocols.Control.V1.Driver as CP1
import Cardano.KESAgent.Protocols.RecvResult ( RecvResult (..) )
import Cardano.KESAgent.Protocols.VersionedProtocol ( VersionedProtocol (..), NamedCrypto, VersionIdentifier )
import Cardano.KESAgent.Util.RefCounting ( CRef, withCRef )
import Cardano.KESAgent.Util.RetrySocket ( retrySocketWith )
import Cardano.KESAgent.Serialization.DirectCodec

import Cardano.Crypto.KES.Class
import Cardano.Crypto.DSIGN.Class
import Cardano.Crypto.DirectSerialise

import Ouroboros.Network.RawBearer
import Ouroboros.Network.Snocket ( Snocket (..) )

import Data.SerDoc.Info ( Description (..), aliasField, annField )
import Data.SerDoc.Class ( ViaEnum (..), Codec (..), HasInfo (..), Serializable (..), encodeEnum, decodeEnum, enumInfo )
import qualified Data.SerDoc.Info
import Data.SerDoc.TH (deriveSerDoc)
import Control.Monad ( forever, void )
import Control.Monad.Extra ( whenJust )
import Control.Monad.Class.MonadThrow ( MonadThrow, MonadCatch, SomeException, bracket )
import Control.Monad.Class.MonadST ( MonadST )
import Control.Monad.Class.MonadSTM ( MonadSTM )
import Control.Monad.Class.MonadTimer ( MonadDelay )
import Control.Concurrent.Class.MonadMVar ( MonadMVar )
import Control.Tracer ( Tracer, traceWith )
import Data.Functor.Contravariant ( (>$<) )
import Data.Proxy ( Proxy (..) )
import Network.TypedProtocol.Driver ( runPeerWithDriver )
import Network.TypedProtocol.Core ( Peer (..), PeerRole (..) )
import Data.Coerce
import Data.Kind
import Data.Typeable

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

data ControlClientOptions m fd addr =
  ControlClientOptions
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

runControlClient1 :: forall c m fd addr a
                   . ControlClientContext m c
                  => ControlClientDrivers c
                  => (ControlClient m c -> ControlHandler m a)
                  -> Proxy c
                  -> MakeRawBearer m fd
                  -> ControlClientOptions m fd addr
                  -> Tracer m ControlClientTrace
                  -> m a
runControlClient1 handler proxy mrb options tracer = do
  let s = controlClientSnocket options
  bracket
    (openToConnect s (controlClientAddress options))
    (\fd -> do
      close s fd
      traceWith tracer ControlClientSocketClosed
    )
    (\fd -> do
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
            ()
      case protocolVersionMay >>= (`lookup` drivers) of
        Nothing ->
          error "Protocol handshake failed (control)"
          traceWith tracer ControlClientVersionHandshakeFailed
        Just controlClient ->
          handler controlClient bearer tracer
    )

class IsControlHandler proto (m :: Type -> Type) a where
  type InitialState proto :: proto
  toHandler :: Peer proto AsServer (InitialState proto) m a -> ControlHandler m a

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
    ) => IsControlHandler (CP0.ControlProtocol m c) m a
  where
    type InitialState (CP0.ControlProtocol m c) = CP0.InitialState
    toHandler peer bearer tracer = do
      fst <$> runPeerWithDriver
                (CP0.controlDriver bearer $ ControlClientDriverTrace >$< tracer)
                peer
                ()

instance
    MonadControlHandler m => IsControlHandler (CP1.ControlProtocol m) m a
  where
    type InitialState (CP1.ControlProtocol m) = CP1.InitialState
    toHandler peer bearer tracer = do
      fst <$> runPeerWithDriver
                (CP1.controlDriver bearer $ ControlClientDriverTrace >$< tracer)
                peer
                ()

toHandlerEntry :: forall proto m a.
                  IsControlHandler proto m a
               => VersionedProtocol proto
               => Peer proto AsServer (InitialState proto) m a
               -> (VersionIdentifier, ControlHandler m a)
toHandlerEntry peer = (versionIdentifier (Proxy @proto), toHandler peer)

data ControlClient m c =
  ControlClient
  { controlGenKey :: ControlHandler m (Maybe (VerKeyKES (KES c)))
  , controlQueryKey :: ControlHandler m (Maybe (VerKeyKES (KES c)))
  , controlDropKey :: ControlHandler m (Maybe (VerKeyKES (KES c)))
  , controlInstallKey :: OCert c
                      -> ControlHandler m RecvResult
  , controlGetInfo :: ControlHandler m (AgentInfo c)
  }

class ControlClientDrivers c where
  controlClientDrivers :: forall m. ControlClientContext m c
                       => Proxy c -> [(VersionIdentifier, ControlClient m c)]

mkControlClientCP1 :: ControlClientContext m StandardCrypto
                   => (VersionIdentifier, ControlClient m StandardCrypto)
mkControlClientCP1 =
  ( versionIdentifier (Proxy @(CP1.ControlProtocol _))
  , ControlClient
      (toHandler CP1.controlGenKey)
      (toHandler CP1.controlQueryKey)
      (toHandler CP1.controlDropKey)
      (toHandler <$> CP1.controlInstallKey)
      (fmap (fmap toAgentInfo) <$> toHandler CP1.controlGetInfo)
  )

mkControlClientCP0 :: forall m c
                    . ControlClientContext m c
                   => NamedCrypto c
                   => Typeable c
                   => (VersionIdentifier, ControlClient m c)
mkControlClientCP0 =
  ( versionIdentifier (Proxy @(CP0.ControlProtocol _ c))
  , ControlClient
      (toHandler (CP0.controlGenKey @c))
      (toHandler (CP0.controlQueryKey @c))
      (toHandler (CP0.controlDropKey @c))
      (toHandler <$> CP0.controlInstallKey)
      (fmap (fmap toAgentInfo) <$> toHandler (CP0.controlGetInfo @c))
  )

instance ControlClientDrivers StandardCrypto where
  controlClientDrivers _ =
    [ mkControlClientCP1, mkControlClientCP0 ]

instance ControlClientDrivers SingleCrypto where
  controlClientDrivers _ =
    [ mkControlClientCP0 ]

instance ControlClientDrivers MockCrypto where
  controlClientDrivers _ =
    [ mkControlClientCP0 ]


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

convertBundleInfoCP0 :: CP0.BundleInfo c -> BundleInfo c
convertBundleInfoCP0 info =
  BundleInfo
    { bundleInfoEvolution = CP0.bundleInfoEvolution info
    , bundleInfoStartKESPeriod = CP0.bundleInfoStartKESPeriod info
    , bundleInfoOCertN = CP0.bundleInfoOCertN info
    , bundleInfoVK = CP0.bundleInfoVK info
    , bundleInfoSigma = CP0.bundleInfoSigma info
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

convertBundleInfoCP1 :: CP1.BundleInfo -> BundleInfo StandardCrypto
convertBundleInfoCP1 info =
  BundleInfo
    { bundleInfoEvolution = CP1.bundleInfoEvolution info
    , bundleInfoStartKESPeriod = CP1.bundleInfoStartKESPeriod info
    , bundleInfoOCertN = CP1.bundleInfoOCertN info
    , bundleInfoVK = CP1.bundleInfoVK info
    , bundleInfoSigma = CP1.bundleInfoSigma info
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

