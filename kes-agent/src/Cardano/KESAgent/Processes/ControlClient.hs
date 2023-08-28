{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Clients for the KES Agent.
module Cardano.KESAgent.Processes.ControlClient
  where

import Cardano.KESAgent.KES.Classes ( MonadKES )
import Cardano.KESAgent.Protocols.Service.Driver ( ServiceDriverTrace, serviceDriver )
import Cardano.KESAgent.KES.Crypto ( Crypto (..) )
import Cardano.KESAgent.KES.OCert ( OCert (..) )
import Cardano.KESAgent.Util.Pretty ( Pretty (..) )
import Cardano.KESAgent.Protocols.Service.Peers ( servicePusher, serviceReceiver )
import Cardano.KESAgent.Protocols.Service.Protocol ( RecvResult (..) )
import Cardano.KESAgent.Util.RefCounting ( CRef, withCRef )
import Cardano.KESAgent.Util.RetrySocket ( retrySocket )

import Cardano.Crypto.KES.Class ( SignKeyWithPeriodKES (..) )

import Ouroboros.Network.RawBearer
import Ouroboros.Network.Snocket ( Snocket (..) )

import Control.Monad ( forever, void )
import Control.Monad.Class.MonadThrow ( SomeException, bracket )
import Control.Tracer ( Tracer, traceWith )
import Data.Functor.Contravariant ( (>$<) )
import Data.Proxy ( Proxy (..) )
import Network.TypedProtocol.Driver ( runPeerWithDriver )

data ControlClientOptions m fd addr =
  ControlClientOptions
    { controlClientSnocket :: Snocket m fd addr
    , controlClientAddress :: addr
    , controlClientLocalAddress :: Maybe addr
    }

data ControlClientTrace
  = ControlClientDriverTrace ServiceDriverTrace
  | ControlClientSocketClosed
  | ControlClientConnected -- (SocketAddress Unix)
  | ControlClientAttemptReconnect Int
  | ControlClientSendingKey
  | ControlClientAbnormalTermination String
  | ControlClientKeyAccepted
  | ControlClientKeyRejected RecvResult
  deriving (Show)

instance Pretty ControlClientTrace where
  pretty (ControlClientDriverTrace d) = "Control: ServiceDriver: " ++ pretty d
  pretty ControlClientConnected = "Control: Connected"
  pretty x = "Control: " ++ drop (length "ControlClient") (show x)

-- | A simple control client: push one KES key, then exit.
runControlClient1 :: forall c m fd addr
                   . MonadKES m c
                  => Proxy c
                  -> MakeRawBearer m fd
                  -> ControlClientOptions m fd addr
                  -> CRef m (SignKeyWithPeriodKES (KES c))
                  -> OCert c
                  -> Tracer m ControlClientTrace
                  -> m ()
runControlClient1 proxy mrb options key oc tracer = withCRef key $ \key -> do
  let s = controlClientSnocket options
  void $ bracket
    (openToConnect s (controlClientAddress options))
    (\fd -> do
      close s fd
      traceWith tracer $ ControlClientSocketClosed
    )
    (\fd -> do
      case controlClientLocalAddress options of
        Just addr -> bind s fd addr
        Nothing -> return ()
      retrySocket (\(e :: SomeException) n i -> traceWith tracer $ ControlClientAttemptReconnect n) $
        connect s fd (controlClientAddress options)
      traceWith tracer $ ControlClientConnected -- (controlClientSocketAddress options)
      bearer <- getRawBearer mrb fd
      void $ runPeerWithDriver
        (serviceDriver bearer $ ControlClientDriverTrace >$< tracer)
        (servicePusher
            (traceWith tracer ControlClientSendingKey >> return (key, oc))
            (return Nothing)
            (\reason -> do
                case reason of
                  RecvOK -> traceWith tracer ControlClientKeyAccepted
                  err -> traceWith tracer (ControlClientKeyRejected err)
            )
        )
        ()
    )
