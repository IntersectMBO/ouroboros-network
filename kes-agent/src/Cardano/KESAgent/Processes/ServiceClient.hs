{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | KES agent service client API
module Cardano.KESAgent.Processes.ServiceClient (
  ServiceClientTrace (..),
  ServiceClientDrivers (..),
  ServiceClientOptions (..),
  ServiceClientCrypto,
  MonadServiceClient,
  ServiceClientContext,
  runServiceClientForever,
  runServiceClient,
)
where

import Cardano.KESAgent.KES.Bundle (TaggedBundle (..))
import Cardano.KESAgent.KES.Crypto (Crypto (..))
import Cardano.KESAgent.Protocols.RecvResult (RecvResult (..))
import qualified Cardano.KESAgent.Protocols.Service.V0.Driver as SP0
import qualified Cardano.KESAgent.Protocols.Service.V0.Peers as SP0
import qualified Cardano.KESAgent.Protocols.Service.V0.Protocol as SP0
import qualified Cardano.KESAgent.Protocols.Service.V1.Driver as SP1
import qualified Cardano.KESAgent.Protocols.Service.V1.Peers as SP1
import qualified Cardano.KESAgent.Protocols.Service.V1.Protocol as SP1
import qualified Cardano.KESAgent.Protocols.Service.V2.Driver as SP2
import qualified Cardano.KESAgent.Protocols.Service.V2.Peers as SP2
import qualified Cardano.KESAgent.Protocols.Service.V2.Protocol as SP2
import Cardano.KESAgent.Protocols.StandardCrypto
import Cardano.KESAgent.Protocols.Types
import Cardano.KESAgent.Protocols.VersionHandshake.Driver
import Cardano.KESAgent.Protocols.VersionHandshake.Peers
import Cardano.KESAgent.Protocols.VersionedProtocol
import Cardano.KESAgent.Serialization.DirectCodec
import Cardano.KESAgent.Util.PlatformPoison (poisonWindows)
import Cardano.KESAgent.Util.Pretty (Pretty (..))
import Cardano.KESAgent.Util.RetrySocket (retrySocket)

import Cardano.Crypto.DirectSerialise
import Cardano.Crypto.KES.Class (SignKeyKES, VerKeyKES)

import Ouroboros.Network.RawBearer
import Ouroboros.Network.Snocket (Snocket (..))

import Control.Concurrent.Async (AsyncCancelled)
import Control.Concurrent.Class.MonadMVar
import Control.Concurrent.Class.MonadSTM
import Control.Exception (AsyncException)
import Control.Monad (unless, void)
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadThrow (
  Handler (..),
  MonadCatch,
  MonadThrow,
  SomeException,
  bracket,
  catches,
 )
import Control.Monad.Class.MonadTimer (MonadDelay, threadDelay)
import Control.Tracer (Tracer, traceWith)
import Data.Char (toLower)
import Data.Functor.Contravariant ((>$<))
import Data.SerDoc.Class (HasInfo (..))
import Data.Typeable
import Data.Word (Word64)
import Network.TypedProtocol.Driver (runPeerWithDriver)
import Text.Casing
import Text.Printf

-- | Configuration options for a service client.
data ServiceClientOptions m fd addr
  = ServiceClientOptions
  { serviceClientSnocket :: Snocket m fd addr
  -- ^ Snocket implementation to use for socket operations
  , serviceClientAddress :: addr
  -- ^ Socket address to connect to. A KES agent should be listening for
  -- service connections here.
  }

-- | Trace logging messages a service client can generate.
data ServiceClientTrace
  = ServiceClientVersionHandshakeTrace !VersionHandshakeDriverTrace
  | ServiceClientVersionHandshakeFailed
  | ServiceClientDriverTrace !ServiceDriverTrace
  | ServiceClientSocketClosed
  | ServiceClientConnected !String
  | ServiceClientAttemptReconnect !Int !Int !String !String
  | ServiceClientReceivedKey !TaggedBundleTrace
  | ServiceClientDeclinedKey !TaggedBundleTrace
  | ServiceClientDroppedKey
  | ServiceClientOpCertNumberCheck !Word64 !Word64
  | ServiceClientAbnormalTermination !String
  | ServiceClientStopped
  deriving (Show)

formatDelayMicro :: Int -> String
formatDelayMicro us
  | us < 1_000 =
      show us ++ "μs"
  | us < 1_000_000 =
      show (us `div` 1_000) ++ "ms"
  | us < 4_000_000 =
      printf "%0.2fs" ((fromIntegral us :: Double) / 1_000_000)
  | us < 10_000_000 =
      printf "%0.1fs" ((fromIntegral us :: Double) / 1_000_000)
  | us < 60_000_000 =
      printf "%0.0fs" ((fromIntegral us :: Double) / 1_000_000)
  | us < 3600_000_000 =
      printf "%i:$02i minutes" (us `div` 60_000_000) ((us `mod` 60_000_000) `div` 1_000_000)
  | otherwise =
      printf "%i:$02i hours" (us `div` 3600_000_000) ((us `mod` 3600_000_000) `div` 60_000_000)

instance Pretty ServiceClientTrace where
  pretty (ServiceClientDriverTrace d) = "Service: service driver: " ++ pretty d
  pretty (ServiceClientConnected a) = "Service: connected to " ++ a
  pretty (ServiceClientAttemptReconnect count delayMicro err addr) =
    "Service: service driver: failed to connect to "
      ++ addr
      ++ ": "
      ++ err
      ++ "; connection attempts left: "
      ++ show count
      ++ ", next connection attempt in "
      ++ formatDelayMicro delayMicro
  pretty (ServiceClientVersionHandshakeTrace a) = "Service: version handshake: " ++ pretty a
  pretty (ServiceClientReceivedKey k) = "Service: received key: " ++ pretty k
  pretty (ServiceClientDeclinedKey k) = "Service: declined key: " ++ pretty k
  pretty (ServiceClientAbnormalTermination err) = "Service: abnormal termination: " ++ err
  pretty x = "Service: " ++ prettify (drop (length "ServiceClient") (show x))
    where
      prettify str =
        case words str of
          [] -> ""
          x : xs -> unwords ((map toLower . toWords . fromAny) x : xs)

-- | Monadic typeclasses required to run service client actions.
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

-- | Crypto types that support service client operations.
type ServiceClientCrypto c =
  ( Crypto c
  , NamedCrypto c
  , Typeable c
  , ServiceClientDrivers c
  , DirectSerialise (SignKeyKES (KES c))
  , DirectDeserialise (SignKeyKES (KES c))
  )

-- | Typeclasses required for running service clients against a given crypto.
type ServiceClientContext m c =
  ( MonadServiceClient m
  , ServiceClientCrypto c
  , HasInfo (DirectCodec m) (VerKeyKES (KES c))
  , HasInfo (DirectCodec m) (SignKeyKES (KES c))
  )

-- | Crypto types for which service client drivers are available.
class ServiceClientDrivers c where
  availableServiceClientDrivers ::
    forall m.
    ServiceClientContext m c =>
    [ ( VersionIdentifier
      , RawBearer m ->
        Tracer m ServiceClientTrace ->
        (TaggedBundle m c -> m RecvResult) ->
        m ()
      )
    ]

mkServiceClientDriverSP0 ::
  forall m c.
  ServiceClientContext m c =>
  ( VersionIdentifier
  , RawBearer m ->
    Tracer m ServiceClientTrace ->
    (TaggedBundle m c -> m RecvResult) ->
    m ()
  )
mkServiceClientDriverSP0 =
  ( versionIdentifier (Proxy @(SP0.ServiceProtocol _ StandardCrypto))
  , \bearer tracer handleKey ->
      void $
        runPeerWithDriver
          (SP0.serviceDriver bearer $ ServiceClientDriverTrace >$< tracer)
          ( SP0.serviceReceiver $
              \bundle ->
                handleKey (TaggedBundle (Just bundle) 0)
                  <* traceWith
                    tracer
                    ( ServiceClientReceivedKey $
                        mkTaggedBundleTrace 0 (Just bundle)
                    )
          )
  )

mkServiceClientDriverSP1 ::
  forall m.
  ServiceClientContext m StandardCrypto =>
  ( VersionIdentifier
  , RawBearer m ->
    Tracer m ServiceClientTrace ->
    (TaggedBundle m StandardCrypto -> m RecvResult) ->
    m ()
  )
mkServiceClientDriverSP1 =
  ( versionIdentifier (Proxy @(SP1.ServiceProtocol _))
  , \bearer tracer handleKey ->
      void $
        runPeerWithDriver
          (SP1.serviceDriver bearer $ ServiceClientDriverTrace >$< tracer)
          ( SP1.serviceReceiver $ \bundle ->
              handleKey (TaggedBundle (Just bundle) 0)
                <* traceWith
                  tracer
                  ( ServiceClientReceivedKey $
                      mkTaggedBundleTrace 0 (Just bundle)
                  )
          )
  )

mkServiceClientDriverSP2 ::
  forall m.
  ServiceClientContext m StandardCrypto =>
  ( VersionIdentifier
  , RawBearer m ->
    Tracer m ServiceClientTrace ->
    (TaggedBundle m StandardCrypto -> m RecvResult) ->
    m ()
  )
mkServiceClientDriverSP2 =
  ( versionIdentifier (Proxy @(SP2.ServiceProtocol _))
  , \bearer tracer handleKey ->
      void $
        runPeerWithDriver
          (SP2.serviceDriver bearer $ ServiceClientDriverTrace >$< tracer)
          ( SP2.serviceReceiver
              ( \bundle -> do
                  result <- handleKey bundle
                  case result of
                    RecvOK ->
                      traceWith tracer $
                        ServiceClientReceivedKey
                          ( mkTaggedBundleTrace
                              (taggedBundleTimestamp bundle)
                              (taggedBundle bundle)
                          )
                    _ ->
                      traceWith tracer $
                        ServiceClientDeclinedKey
                          ( mkTaggedBundleTrace
                              (taggedBundleTimestamp bundle)
                              (taggedBundle bundle)
                          )
                  return result
              )
          )
  )

instance ServiceClientDrivers StandardCrypto where
  availableServiceClientDrivers =
    [ mkServiceClientDriverSP2
    , mkServiceClientDriverSP1
    , mkServiceClientDriverSP0
    ]

instance ServiceClientDrivers MockCrypto where
  availableServiceClientDrivers =
    [mkServiceClientDriverSP0]

instance ServiceClientDrivers SingleCrypto where
  availableServiceClientDrivers =
    [mkServiceClientDriverSP0]

-- | Run a service client indefinitely, restarting the connection once a
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
  (TaggedBundle m c -> m RecvResult) ->
  Tracer m ServiceClientTrace ->
  m ()
runServiceClientForever proxy mrb options handleKey tracer = do
  exitVar <- newMVar False
  let go :: m ()
      go = do
        runServiceClient proxy mrb options handleKey tracer
          `catches` [ Handler (handleAbort exitVar)
                    , Handler (handleAsyncCancelled exitVar)
                    , Handler handle
                    ]
        traceWith tracer $
          ServiceClientAttemptReconnect
            0
            10_000_000
            "Ran out of attempts"
            (show $ serviceClientAddress options)
        exit <- readMVar exitVar
        unless exit $ do
          threadDelay 10_000_000
          go
  go
  traceWith tracer ServiceClientStopped
  where
    handleAbort :: MVar m Bool -> AsyncException -> m ()
    handleAbort exitVar _ = do
      _ <- tryTakeMVar exitVar
      putMVar exitVar True

    handleAsyncCancelled :: MVar m Bool -> AsyncCancelled -> m ()
    handleAsyncCancelled exitVar _ = do
      _ <- tryTakeMVar exitVar
      putMVar exitVar True

    handle :: SomeException -> m ()
    handle e = do
      traceWith tracer $ ServiceClientAbnormalTermination (show e)

-- | Run a single service client session. Once the peer closes the connection,
-- or a failure occurs, return.
runServiceClient ::
  forall c m fd addr.
  ServiceClientContext m c =>
  Show addr =>
  Proxy c ->
  MakeRawBearer m fd ->
  ServiceClientOptions m fd addr ->
  (TaggedBundle m c -> m RecvResult) ->
  Tracer m ServiceClientTrace ->
  m ()
runServiceClient proxy mrb options handleKey tracer = do
  poisonWindows
  let s = serviceClientSnocket options
  latestTimestampVar <- newMVar Nothing
  let handleKey' tbundle@(TaggedBundle bundleMay timestamp) = do
        latestTimestamp <- takeMVar latestTimestampVar
        if Just timestamp <= latestTimestamp
          then do
            putMVar latestTimestampVar latestTimestamp
            return RecvErrorKeyOutdated
          else do
            putMVar latestTimestampVar (Just timestamp)
            handleKey tbundle

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
