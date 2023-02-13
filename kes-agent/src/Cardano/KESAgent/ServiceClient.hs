{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE MonoLocalBinds #-}

-- | Clients for the KES Agent.
module Cardano.KESAgent.ServiceClient
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

data ServiceClientOptions m fd addr =
  ServiceClientOptions
    { serviceClientSnocket :: Snocket m fd addr
    , serviceClientAddress :: addr
    }

data ServiceClientTrace
  = ServiceClientDriverTrace DriverTrace
  | ServiceClientSocketClosed
  | ServiceClientConnected -- (SocketAddress Unix)
  | ServiceClientAttemptReconnect Int DiffTime String
  | ServiceClientReceivedKey
  | ServiceClientAbnormalTermination String 
  deriving (Show)

runServiceClient :: forall c m fd addr
                  . MonadKES m c
                 => MonadNetworking m fd
                 => Proxy c
                 -> ServiceClientOptions m fd addr
                 -> (CRef m (SignKeyWithPeriodKES (KES c)) -> OCert c -> m ())
                 -> Tracer m ServiceClientTrace
                 -> m ()
runServiceClient proxy options handleKey tracer = do
  let s = serviceClientSnocket options
  void $ bracket
    (openToConnect s (serviceClientAddress options))
    (\fd -> do
      close s fd
      traceWith tracer ServiceClientSocketClosed
    )
    (\fd -> do
      retrySocket (\(e :: SomeException) n i -> traceWith tracer $ ServiceClientAttemptReconnect n i (show e)) $
        connect s fd (serviceClientAddress options)
      traceWith tracer ServiceClientConnected
      void $ runPeerWithDriver
        (driver (toDirectBearer fd) $ ServiceClientDriverTrace >$< tracer)
        (kesReceiver $ \k o -> handleKey k o <* traceWith tracer ServiceClientReceivedKey)
        ()
    )
