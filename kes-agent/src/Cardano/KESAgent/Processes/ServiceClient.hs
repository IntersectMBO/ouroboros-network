{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Clients for the KES Agent.
module Cardano.KESAgent.Processes.ServiceClient
where

import Cardano.KESAgent.KES.Bundle (Bundle (..))
import Cardano.KESAgent.KES.Crypto (Crypto (..))
import Cardano.KESAgent.KES.OCert (OCert (..))
import Cardano.KESAgent.Protocols.AgentInfo
import Cardano.KESAgent.Protocols.RecvResult (RecvResult (..))
import qualified Cardano.KESAgent.Protocols.Service.V0.Driver as SP0
import qualified Cardano.KESAgent.Protocols.Service.V0.Peers as SP0
import qualified Cardano.KESAgent.Protocols.Service.V0.Protocol as SP0
import qualified Cardano.KESAgent.Protocols.Service.V1.Driver as SP1
import qualified Cardano.KESAgent.Protocols.Service.V1.Peers as SP1
import qualified Cardano.KESAgent.Protocols.Service.V1.Protocol as SP1
import Cardano.KESAgent.Protocols.StandardCrypto
import Cardano.KESAgent.Protocols.Types
import Cardano.KESAgent.Protocols.VersionHandshake.Driver
import Cardano.KESAgent.Protocols.VersionHandshake.Driver (
  VersionHandshakeDriverTrace,
  versionHandshakeDriver,
 )
import Cardano.KESAgent.Protocols.VersionHandshake.Peers
import Cardano.KESAgent.Protocols.VersionHandshake.Peers (versionHandshakeClient)
import Cardano.KESAgent.Protocols.VersionHandshake.Protocol
import Cardano.KESAgent.Protocols.VersionHandshake.Protocol (VersionHandshakeProtocol)
import Cardano.KESAgent.Protocols.VersionedProtocol
import Cardano.KESAgent.Serialization.DirectCodec
import Cardano.KESAgent.Util.PlatformPoison (poisonWindows)
import Cardano.KESAgent.Util.Pretty (Pretty (..))
import Cardano.KESAgent.Util.RefCounting (CRef)
import Cardano.KESAgent.Util.RetrySocket (retrySocket)

import Cardano.Crypto.DSIGN.Class
import Cardano.Crypto.DirectSerialise
import Cardano.Crypto.KES.Class (SignKeyKES, SignKeyWithPeriodKES (..), VerKeyKES)

import Ouroboros.Network.RawBearer
import Ouroboros.Network.Snocket (Snocket (..))

import Control.Concurrent.Class.MonadMVar
import Control.Concurrent.Class.MonadSTM
import Control.Monad (forever, void)
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadThrow (MonadCatch, MonadThrow, SomeException, bracket, catch)
import Control.Monad.Class.MonadTimer (MonadDelay, threadDelay)
import Control.Tracer (Tracer, traceWith)
import Data.Coerce
import Data.Functor.Contravariant ((>$<))
import Data.Proxy (Proxy (..))
import Data.SerDoc.Class (
  Codec (..),
  HasInfo (..),
  Serializable (..),
  ViaEnum (..),
  decodeEnum,
  encodeEnum,
  enumInfo,
 )
import Data.SerDoc.Info (Description (..), aliasField, annField)
import qualified Data.SerDoc.Info
import Data.SerDoc.TH (deriveSerDoc)
import Data.Typeable
import Data.Word (Word64)
import Network.TypedProtocol.Driver (runPeerWithDriver)

data ServiceClientOptions m fd addr
  = ServiceClientOptions
  { serviceClientSnocket :: Snocket m fd addr
  , serviceClientAddress :: addr
  }

data ServiceClientTrace
  = ServiceClientVersionHandshakeTrace !VersionHandshakeDriverTrace
  | ServiceClientVersionHandshakeFailed
  | ServiceClientDriverTrace !ServiceDriverTrace
  | ServiceClientSocketClosed
  | ServiceClientConnected !String
  | ServiceClientAttemptReconnect !Int !Int !String !String
  | ServiceClientReceivedKey
  | ServiceClientOpCertNumberCheck !Word64 !Word64
  | ServiceClientAbnormalTermination !String
  deriving (Show)

instance Pretty ServiceClientTrace where
  pretty (ServiceClientDriverTrace d) = "Service: ServiceDriver: " ++ pretty d
  pretty (ServiceClientConnected a) = "Service: Connected to " ++ a
  pretty x = "Service: " ++ drop (length "ServiceClient") (show x)

type MonadServiceClient m =
  ( Monad m
  , MonadFail m
  , MonadThrow m
  , MonadCatch m
  , MonadDelay m
  , MonadST m
  , MonadSTM m
  , MonadMVar m
  )

type ServiceClientCrypto c =
  ( Crypto c
  , NamedCrypto c
  , Typeable c
  , ServiceClientDrivers c
  , DirectSerialise (SignKeyKES (KES c))
  , DirectDeserialise (SignKeyKES (KES c))
  )

type ServiceClientContext m c =
  ( MonadServiceClient m
  , ServiceClientCrypto c
  , HasInfo (DirectCodec m) (VerKeyKES (KES c))
  , HasInfo (DirectCodec m) (SignKeyKES (KES c))
  )

class ServiceClientDrivers c where
  availableServiceClientDrivers ::
    forall m.
    ServiceClientContext m c =>
    [ ( VersionIdentifier
      , RawBearer m ->
        Tracer m ServiceClientTrace ->
        (Bundle m c -> m RecvResult) ->
        m ()
      )
    ]

mkServiceClientDriverSP0 ::
  forall m c.
  ServiceClientContext m c =>
  ( VersionIdentifier
  , RawBearer m ->
    Tracer m ServiceClientTrace ->
    (Bundle m c -> m RecvResult) ->
    m ()
  )
mkServiceClientDriverSP0 =
  ( versionIdentifier (Proxy @(SP0.ServiceProtocol _ StandardCrypto))
  , \bearer tracer handleKey ->
      void $
        runPeerWithDriver
          (SP0.serviceDriver bearer $ ServiceClientDriverTrace >$< tracer)
          (SP0.serviceReceiver $ \bundle -> handleKey bundle <* traceWith tracer ServiceClientReceivedKey)
  )

mkServiceClientDriverSP1 ::
  forall m.
  ServiceClientContext m StandardCrypto =>
  ( VersionIdentifier
  , RawBearer m ->
    Tracer m ServiceClientTrace ->
    (Bundle m StandardCrypto -> m RecvResult) ->
    m ()
  )
mkServiceClientDriverSP1 =
  ( versionIdentifier (Proxy @(SP1.ServiceProtocol _))
  , \bearer tracer handleKey ->
      void $
        runPeerWithDriver
          (SP1.serviceDriver bearer $ ServiceClientDriverTrace >$< tracer)
          (SP1.serviceReceiver $ \bundle -> handleKey bundle <* traceWith tracer ServiceClientReceivedKey)
  )

instance ServiceClientDrivers StandardCrypto where
  availableServiceClientDrivers =
    [mkServiceClientDriverSP1, mkServiceClientDriverSP0]

instance ServiceClientDrivers MockCrypto where
  availableServiceClientDrivers =
    [mkServiceClientDriverSP0]

instance ServiceClientDrivers SingleCrypto where
  availableServiceClientDrivers =
    [mkServiceClientDriverSP0]

-- | Run a Service Client indefinitely, restarting the connection once a
-- session ends.
-- In case of an abnormal session termination (via an exception), the exception
-- is logged via the provided 'Tracer', and another connection attempt is made.
runServiceClientForever ::
  forall c m fd addr.
  ServiceClientContext m c =>
  Show addr =>
  Proxy c ->
  MakeRawBearer m fd ->
  ServiceClientOptions m fd addr ->
  (Bundle m c -> m RecvResult) ->
  Tracer m ServiceClientTrace ->
  m ()
runServiceClientForever proxy mrb options handleKey tracer =
  forever $ do
    runServiceClient proxy mrb options handleKey tracer `catch` handle
    threadDelay 1000000
  where
    handle :: SomeException -> m ()
    handle e = do
      traceWith tracer $ ServiceClientAbnormalTermination (show e)
      return ()

-- | Run a single Service Client session. Once the peer closes the connection,
-- return.
runServiceClient ::
  forall c m fd addr.
  ServiceClientContext m c =>
  Show addr =>
  Proxy c ->
  MakeRawBearer m fd ->
  ServiceClientOptions m fd addr ->
  (Bundle m c -> m RecvResult) ->
  Tracer m ServiceClientTrace ->
  m ()
runServiceClient proxy mrb options handleKey tracer = do
  poisonWindows
  let s = serviceClientSnocket options
  latestOCNumVar <- newMVar Nothing
  let handleKey' bundle = do
        latestOCNumMay <- takeMVar latestOCNumVar
        case latestOCNumMay of
          Nothing -> do
            -- No key previously handled, so we need to accept this one
            putMVar latestOCNumVar (Just $ ocertN (bundleOC bundle))
            handleKey bundle
          Just latestOCNum -> do
            -- Have already handled a key before, so check that the received key
            -- is newer; if not, discard it.
            traceWith tracer $ ServiceClientOpCertNumberCheck (ocertN (bundleOC bundle)) latestOCNum
            if ocertN (bundleOC bundle) > latestOCNum
              then do
                putMVar latestOCNumVar (Just $ ocertN (bundleOC bundle))
                handleKey bundle
              else do
                putMVar latestOCNumVar (Just latestOCNum)
                return RecvErrorKeyOutdated

  void $
    bracket
      (openToConnect s (serviceClientAddress options))
      ( \fd -> do
          close s fd
          traceWith tracer ServiceClientSocketClosed
      )
      ( \fd -> do
          retrySocket
            ( \(e :: SomeException) n i ->
                traceWith tracer $ ServiceClientAttemptReconnect n i (show e) (show $ serviceClientAddress options)
            )
            $ connect s fd (serviceClientAddress options)
          traceWith tracer $ ServiceClientConnected (show $ serviceClientAddress options)
          bearer <- getRawBearer mrb fd
          (protocolVersionMay :: Maybe VersionIdentifier, ()) <-
            runPeerWithDriver
              (versionHandshakeDriver bearer (ServiceClientVersionHandshakeTrace >$< tracer))
              (versionHandshakeClient (map fst (availableServiceClientDrivers @c @m)))
          case protocolVersionMay >>= (`lookup` (availableServiceClientDrivers @c @m)) of
            Nothing ->
              traceWith tracer ServiceClientVersionHandshakeFailed
            Just run ->
              run bearer tracer handleKey'
      )
