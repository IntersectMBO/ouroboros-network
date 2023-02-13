{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE MonoLocalBinds #-}

-- | Clients for the KES Agent.
module Cardano.KESAgent.ControlClient
where

import Cardano.KESAgent.Driver (driver, DriverTrace)
import Cardano.KESAgent.Peers (kesPusher, kesReceiver)
import Cardano.KESAgent.OCert (Crypto (..), OCert (..))
import Cardano.KESAgent.RetrySocket (retrySocket)
import Cardano.KESAgent.RefCounting (CRef)
import Cardano.KESAgent.DirectBearer (toDirectBearer)
import Cardano.KESAgent.Classes (MonadKES, MonadNetworking)

import Cardano.Crypto.KES.Class (SignKeyWithPeriodKES (..))

import Ouroboros.Network.Snocket (Snocket (..))

import Network.TypedProtocol.Driver (runPeerWithDriver)
import Control.Tracer (Tracer, traceWith)
import Data.Functor.Contravariant ((>$<))
import Data.Proxy (Proxy (..))

import Control.Monad (forever, void)
import Control.Monad.Class.MonadThrow (SomeException, bracket)
import Control.Monad.Class.MonadTimer (DiffTime)

data ControlClientOptions m fd addr =
  ControlClientOptions
    { controlClientSnocket :: Snocket m fd addr
    , controlClientAddress :: addr
    }

data ControlClientTrace
  = ControlClientDriverTrace DriverTrace
  | ControlClientSocketClosed
  | ControlClientConnected -- (SocketAddress Unix)
  | ControlClientAttemptReconnect Int
  | ControlClientSendingKey
  | ControlClientAbnormalTermination String
  deriving (Show)

-- | A simple control client: push one KES key, then exit.
runControlClient1 :: forall c m fd addr
                   . MonadKES m c
                  => MonadNetworking m fd
                  => Proxy c
                  -> ControlClientOptions m fd addr
                  -> CRef m (SignKeyWithPeriodKES (KES c))
                  -> OCert c
                  -> Tracer m ControlClientTrace
                  -> m ()
runControlClient1 proxy options key oc tracer = do
  let s = controlClientSnocket options
  void $ bracket
    (openToConnect s (controlClientAddress options))
    (\fd -> do
      close s fd
      traceWith tracer $ ControlClientSocketClosed
    )
    (\fd -> do
      retrySocket (\(e :: SomeException) n i -> traceWith tracer $ ControlClientAttemptReconnect n) $
        connect s fd (controlClientAddress options)
      traceWith tracer $ ControlClientConnected -- (controlClientSocketAddress options)
      void $ runPeerWithDriver
        (driver (toDirectBearer fd) $ ControlClientDriverTrace >$< tracer)
        (kesPusher (traceWith tracer ControlClientSendingKey >> return (key, oc)) (return Nothing))
        ()
    )
