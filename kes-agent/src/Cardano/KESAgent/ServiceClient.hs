{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Clients for the KES Agent.
module Cardano.KESAgent.ServiceClient
  where

import Cardano.KESAgent.Classes ( MonadKES )
import Cardano.KESAgent.Driver ( DriverTrace, driver )
import Cardano.KESAgent.OCert ( Crypto (..), OCert (..) )
import Cardano.KESAgent.Peers ( kesPusher, kesReceiver )
import Cardano.KESAgent.RefCounting ( CRef )
import Cardano.KESAgent.RetrySocket ( retrySocket )

import Cardano.Crypto.KES.Class ( SignKeyWithPeriodKES (..) )

import Ouroboros.Network.RawBearer
import Ouroboros.Network.Snocket ( Snocket (..) )

import Control.Monad ( forever, void )
import Control.Monad.Class.MonadThrow ( SomeException, bracket )
import Control.Tracer ( Tracer, traceWith )
import Data.Functor.Contravariant ( (>$<) )
import Data.Proxy ( Proxy (..) )
import Network.TypedProtocol.Driver ( runPeerWithDriver )

data ServiceClientOptions m fd addr =
  ServiceClientOptions
    { serviceClientSnocket :: Snocket m fd addr
    , serviceClientAddress :: addr
    }

data ServiceClientTrace
  = ServiceClientDriverTrace DriverTrace
  | ServiceClientSocketClosed
  | ServiceClientConnected -- (SocketAddress Unix)
  | ServiceClientAttemptReconnect Int Int String
  | ServiceClientReceivedKey
  | ServiceClientAbnormalTermination String
  deriving (Show)

runServiceClient :: forall c m fd addr
                  . MonadKES m c
                 => Proxy c
                 -> MakeRawBearer m fd
                 -> ServiceClientOptions m fd addr
                 -> (CRef m (SignKeyWithPeriodKES (KES c)) -> OCert c -> m ())
                 -> Tracer m ServiceClientTrace
                 -> m ()
runServiceClient proxy mrb options handleKey tracer = do
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
      bearer <- getRawBearer mrb fd
      void $ runPeerWithDriver
        (driver bearer $ ServiceClientDriverTrace >$< tracer)
        (kesReceiver $ \k o -> handleKey k o <* traceWith tracer ServiceClientReceivedKey)
        ()
    )
