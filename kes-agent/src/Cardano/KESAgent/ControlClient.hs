{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE ScopedTypeVariables #-}

-- | Clients for the KES Agent.
module Cardano.KESAgent.ControlClient
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
