{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeApplications #-}

-- | Clients for the KES Agent.
module Cardano.KESAgent.Processes.ServiceClient
  where

import Cardano.KESAgent.KES.Classes ( MonadKES )
import Cardano.KESAgent.KES.Crypto ( Crypto (..) )
import Cardano.KESAgent.KES.OCert ( OCert (..) )
import Cardano.KESAgent.KES.Bundle ( Bundle (..) )
import Cardano.KESAgent.Protocols.Service.V0.Driver ( ServiceDriverTrace, serviceDriver )
import Cardano.KESAgent.Protocols.Service.V0.Peers ( servicePusher, serviceReceiver )
import Cardano.KESAgent.Protocols.Service.V0.Protocol ( ServiceProtocol )
import Cardano.KESAgent.Protocols.RecvResult ( RecvResult (..) )
import Cardano.KESAgent.Protocols.VersionedProtocol
import Cardano.KESAgent.Protocols.VersionHandshake.Driver ( VersionHandshakeDriverTrace, versionHandshakeDriver )
import Cardano.KESAgent.Protocols.VersionHandshake.Peers ( versionHandshakeClient )
import Cardano.KESAgent.Protocols.VersionHandshake.Protocol ( VersionHandshakeProtocol )
import Cardano.KESAgent.Util.Pretty ( Pretty (..) )
import Cardano.KESAgent.Util.RefCounting ( CRef )
import Cardano.KESAgent.Util.RetrySocket ( retrySocket )
import Cardano.KESAgent.Serialization.DirectCodec

import Cardano.Crypto.KES.Class ( SignKeyWithPeriodKES (..), SignKeyKES, VerKeyKES )

import Ouroboros.Network.RawBearer
import Ouroboros.Network.Snocket ( Snocket (..) )

import Data.SerDoc.Info ( Description (..), aliasField, annField )
import Data.SerDoc.Class ( ViaEnum (..), Codec (..), HasInfo (..), Serializable (..), encodeEnum, decodeEnum, enumInfo )
import qualified Data.SerDoc.Info
import Data.SerDoc.TH (deriveSerDoc)
import Control.Monad ( forever, void )
import Control.Monad.Class.MonadThrow ( SomeException, bracket, catch )
import Control.Monad.Class.MonadTimer ( threadDelay )
import Control.Concurrent.Class.MonadMVar
import Control.Tracer ( Tracer, traceWith )
import Data.Functor.Contravariant ( (>$<) )
import Data.Proxy ( Proxy (..) )
import Data.Word ( Word64 )
import Network.TypedProtocol.Driver ( runPeerWithDriver )
import Data.Coerce

data ServiceClientOptions m fd addr =
  ServiceClientOptions
    { serviceClientSnocket :: Snocket m fd addr
    , serviceClientAddress :: addr
    }

data ServiceClientTrace
  = ServiceClientVersionHandshakeTrace !VersionHandshakeDriverTrace
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


availableServiceDrivers :: forall c m fd addr
                            . MonadKES m c
                           => HasInfo (DirectCodec m) (SignKeyKES (KES c))
                           => HasInfo (DirectCodec m) (VerKeyKES (KES c))
                           => NamedCrypto c
                           => [ ( VersionIdentifier
                                , RawBearer m
                                  -> Tracer m ServiceClientTrace
                                  -> (Bundle m c -> m RecvResult)
                                  -> m ()
                                )
                              ]
availableServiceDrivers =
  [ ( versionIdentifier (Proxy @(ServiceProtocol m c))
    , \bearer tracer handleKey ->
        void $
          runPeerWithDriver
            (serviceDriver bearer $ ServiceClientDriverTrace >$< tracer)
            (serviceReceiver $ \bundle -> handleKey bundle <* traceWith tracer ServiceClientReceivedKey)
            ()
    )
  ]

-- | Run a Service Client indefinitely, restarting the connection once a
-- session ends.
-- In case of an abnormal session termination (via an exception), the exception
-- is logged via the provided 'Tracer', and another connection attempt is made.
runServiceClientForever :: forall c m fd addr
                         . MonadKES m c
                        => MonadMVar m
                        => Show addr
                        => HasInfo (DirectCodec m) (SignKeyKES (KES c))
                        => HasInfo (DirectCodec m) (VerKeyKES (KES c))
                        => NamedCrypto c
                        => Proxy c
                        -> MakeRawBearer m fd
                        -> ServiceClientOptions m fd addr
                        -> (Bundle m c -> m RecvResult)
                        -> Tracer m ServiceClientTrace
                        -> m ()
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
runServiceClient :: forall c m fd addr
                  . MonadKES m c
                 => MonadMVar m
                 => Show addr
                 => HasInfo (DirectCodec m) (SignKeyKES (KES c))
                 => HasInfo (DirectCodec m) (VerKeyKES (KES c))
                 => NamedCrypto c
                 => Proxy c
                 -> MakeRawBearer m fd
                 -> ServiceClientOptions m fd addr
                 -> (Bundle m c -> m RecvResult)
                 -> Tracer m ServiceClientTrace
                 -> m ()
runServiceClient proxy mrb options handleKey tracer = do
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
            if ocertN (bundleOC bundle) > latestOCNum then do
              putMVar latestOCNumVar (Just $ ocertN (bundleOC bundle))
              handleKey bundle
            else do
              putMVar latestOCNumVar (Just latestOCNum)
              return RecvErrorKeyOutdated

  void $ bracket
    (openToConnect s (serviceClientAddress options))
    (\fd -> do
      close s fd
      traceWith tracer ServiceClientSocketClosed
    )
    (\fd -> do
      retrySocket (\(e :: SomeException) n i ->
          traceWith tracer $ ServiceClientAttemptReconnect n i (show e) (show $ serviceClientAddress options)) $
        connect s fd (serviceClientAddress options)
      traceWith tracer $ ServiceClientConnected (show $ serviceClientAddress options)
      bearer <- getRawBearer mrb fd
      (protocolVersionMay :: Maybe VersionIdentifier, ()) <-
          runPeerWithDriver
            (versionHandshakeDriver bearer (ServiceClientVersionHandshakeTrace >$< tracer))
            (versionHandshakeClient (map fst (availableServiceDrivers @c @m)))
            ()
      case protocolVersionMay >>= (`lookup` (availableServiceDrivers @c @m)) of
        Nothing ->
          error "Protocol handshake failed"
        Just run ->
          run bearer tracer handleKey'
    )
