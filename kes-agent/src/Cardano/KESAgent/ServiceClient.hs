{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE ScopedTypeVariables #-}

-- | Clients for the KES Agent.
module Cardano.KESAgent.ServiceClient
where

import Cardano.KESAgent.Driver (driver, DriverTrace)
import Cardano.KESAgent.Peers (kesPusher, kesReceiver)
import Cardano.KESAgent.Protocol
import Cardano.KESAgent.OCert
import Cardano.KESAgent.RetrySocket
import Cardano.KESAgent.RefCounting
import Cardano.KESAgent.DirectBearer

import Cardano.Crypto.DirectSerialise
import Cardano.Crypto.KES.Class
import Cardano.Crypto.MonadMLock
import Cardano.Binary

import Ouroboros.Network.Snocket

import Network.TypedProtocol.Driver (runPeerWithDriver)
import Control.Tracer (Tracer, traceWith)
import Data.Functor.Contravariant ((>$<))
import Data.Proxy (Proxy (..))
import Data.Typeable

import Control.Monad (forever, void)
import Control.Exception (Exception)
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTimer
import Control.Monad.Class.MonadMVar
import Control.Monad.Class.MonadST

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
                  . Crypto c
                 => Typeable c
                 => KESSignAlgorithm m (KES c)
                 => DirectDeserialise m (SignKeyKES (KES c))
                 => DirectSerialise m (SignKeyKES (KES c))
                 => VersionedProtocol (KESProtocol m c)
                 => MonadThrow m
                 => MonadFail m
                 => MonadUnmanagedMemory m
                 => MonadByteStringMemory m
                 => MonadST m
                 => MonadCatch m
                 => MonadDelay m
                 => MonadMVar m
                 => ToDirectBearer m fd
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
      traceWith tracer $ ServiceClientConnected -- (serviceClientSocketAddress options)
      void $ runPeerWithDriver
        (driver (toDirectBearer fd) $ ServiceClientDriverTrace >$< tracer)
        (kesReceiver $ \k o -> handleKey k o <* traceWith tracer ServiceClientReceivedKey)
        ()
    )
