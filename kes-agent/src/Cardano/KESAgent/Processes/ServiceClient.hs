{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Clients for the KES Agent.
module Cardano.KESAgent.Processes.ServiceClient
  where

import Cardano.KESAgent.KES.Classes ( MonadKES )
import Cardano.KESAgent.KES.Crypto ( Crypto (..) )
import Cardano.KESAgent.KES.OCert ( OCert (..) )
import Cardano.KESAgent.KES.Bundle ( Bundle (..) )
import Cardano.KESAgent.Protocols.Service.Driver ( ServiceDriverTrace, serviceDriver )
import Cardano.KESAgent.Protocols.Service.Peers ( servicePusher, serviceReceiver )
import Cardano.KESAgent.Protocols.RecvResult ( RecvResult (..) )
import Cardano.KESAgent.Util.Pretty ( Pretty (..) )
import Cardano.KESAgent.Util.RefCounting ( CRef )
import Cardano.KESAgent.Util.RetrySocket ( retrySocket )
import Cardano.KESAgent.Serialization.Spec ( HasSerInfo )

import Cardano.Crypto.KES.Class ( SignKeyWithPeriodKES (..), SignKeyKES )

import Ouroboros.Network.RawBearer
import Ouroboros.Network.Snocket ( Snocket (..) )

import Control.Monad ( forever, void )
import Control.Monad.Class.MonadThrow ( SomeException, bracket )
import Control.Monad.Class.MonadTimer ( threadDelay )
import Control.Concurrent.Class.MonadMVar
import Control.Tracer ( Tracer, traceWith )
import Data.Functor.Contravariant ( (>$<) )
import Data.Proxy ( Proxy (..) )
import Data.Word ( Word64 )
import Network.TypedProtocol.Driver ( runPeerWithDriver )

data ServiceClientOptions m fd addr =
  ServiceClientOptions
    { serviceClientSnocket :: Snocket m fd addr
    , serviceClientAddress :: addr
    }

data ServiceClientTrace
  = ServiceClientDriverTrace !ServiceDriverTrace
  | ServiceClientSocketClosed
  | ServiceClientConnected !String
  | ServiceClientAttemptReconnect !Int !Int !String
  | ServiceClientReceivedKey
  | ServiceClientOpCertNumberCheck !Word64 !Word64
  | ServiceClientAbnormalTermination !String
  deriving (Show)

instance Pretty ServiceClientTrace where
  pretty (ServiceClientDriverTrace d) = "Service: ServiceDriver: " ++ pretty d
  pretty (ServiceClientConnected a) = "Service: Connected to " ++ a
  pretty x = "Service: " ++ drop (length "ServiceClient") (show x)

-- | Run a Service Client indefinitely, restarting the connection once a
-- session ends.
runServiceClientForever :: forall c m fd addr
                         . MonadKES m c
                        => MonadMVar m
                        => Show addr
                        => HasSerInfo (SignKeyKES (KES c))
                        => Proxy c
                        -> MakeRawBearer m fd
                        -> ServiceClientOptions m fd addr
                        -> (Bundle m c -> m RecvResult)
                        -> Tracer m ServiceClientTrace
                        -> m ()
runServiceClientForever proxy mrb options handleKey tracer =
  forever $ do
    runServiceClient proxy mrb options handleKey tracer
    threadDelay 1000000

-- | Run a single Service Client session. Once the peer closes the connection,
-- return.
runServiceClient :: forall c m fd addr
                  . MonadKES m c
                 => MonadMVar m
                 => Show addr
                 => HasSerInfo (SignKeyKES (KES c))
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
      retrySocket (\(e :: SomeException) n i -> traceWith tracer $ ServiceClientAttemptReconnect n i (show e)) $
        connect s fd (serviceClientAddress options)
      traceWith tracer $ ServiceClientConnected (show $ serviceClientAddress options)
      bearer <- getRawBearer mrb fd
      void $ runPeerWithDriver
        (serviceDriver bearer $ ServiceClientDriverTrace >$< tracer)
        (serviceReceiver $ \bundle -> handleKey' bundle <* traceWith tracer ServiceClientReceivedKey)
        ()
    )
