{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

-- | Clients for the KES Agent.
module Cardano.KESAgent.Processes.ControlClient
  where

import Cardano.KESAgent.KES.Classes ( MonadKES )
import Cardano.KESAgent.KES.Crypto ( Crypto (..) )
import Cardano.KESAgent.KES.OCert ( OCert (..) )
import Cardano.KESAgent.Util.Pretty ( Pretty (..) )
import Cardano.KESAgent.Protocols.Control.Peers
import Cardano.KESAgent.Protocols.Control.Protocol
import Cardano.KESAgent.Protocols.Control.Driver
import Cardano.KESAgent.Protocols.RecvResult ( RecvResult (..) )
import Cardano.KESAgent.Protocols.VersionedProtocol ( NamedCrypto )
import Cardano.KESAgent.Util.RefCounting ( CRef, withCRef )
import Cardano.KESAgent.Util.RetrySocket ( retrySocketWith )

import Cardano.Crypto.KES.Class ( SignKeyWithPeriodKES (..), VerKeyKES )

import Ouroboros.Network.RawBearer
import Ouroboros.Network.Snocket ( Snocket (..) )

import Control.Monad ( forever, void )
import Control.Monad.Extra ( whenJust )
import Control.Monad.Class.MonadThrow ( SomeException, bracket )
import Control.Tracer ( Tracer, traceWith )
import Data.Functor.Contravariant ( (>$<) )
import Data.Proxy ( Proxy (..) )
import Network.TypedProtocol.Driver ( runPeerWithDriver )
import Network.TypedProtocol.Core ( Peer (..), PeerRole (..) )

data ControlClientOptions m fd addr =
  ControlClientOptions
    { controlClientSnocket :: Snocket m fd addr
    , controlClientAddress :: addr
    , controlClientLocalAddress :: Maybe addr
    , controlClientRetryDelay :: Int
    , controlClientRetryExponential :: Bool
    , controlClientRetryAttempts :: Int
    }

data ControlClientTrace
  = ControlClientDriverTrace ControlDriverTrace
  | ControlClientSocketClosed
  | ControlClientConnected -- (SocketAddress Unix)
  | ControlClientAttemptReconnect Int
  | ControlClientSendingKey
  | ControlClientAbnormalTermination String
  | ControlClientKeyAccepted
  | ControlClientKeyRejected RecvResult
  deriving (Show)

instance Pretty ControlClientTrace where
  pretty (ControlClientDriverTrace d) = "Control: ControlDriver: " ++ pretty d
  pretty ControlClientConnected = "Control: Connected"
  pretty x = "Control: " ++ drop (length "ControlClient") (show x)

runControlClient1 :: forall c m fd addr a
                   . MonadKES m c
                  => Crypto c
                  => NamedCrypto c
                  => Peer (ControlProtocol m c) AsServer InitialState m a
                  -> Proxy c
                  -> MakeRawBearer m fd
                  -> ControlClientOptions m fd addr
                  -> Tracer m ControlClientTrace
                  -> m a
runControlClient1 peer proxy mrb options tracer = do
  let s = controlClientSnocket options
  bracket
    (openToConnect s (controlClientAddress options))
    (\fd -> do
      close s fd
      traceWith tracer $ ControlClientSocketClosed
    )
    (\fd -> do
      case controlClientLocalAddress options of
        Just addr -> bind s fd addr
        Nothing -> return ()
      retrySocketWith
        (if controlClientRetryExponential options then ((min 5000000) . (* 2)) else id)
        (controlClientRetryDelay options * 1000)
        (controlClientRetryAttempts options)
        (\(e :: SomeException) n i -> traceWith tracer $ ControlClientAttemptReconnect n)
        (connect s fd (controlClientAddress options))
      traceWith tracer $ ControlClientConnected
      bearer <- getRawBearer mrb fd
      fst <$> runPeerWithDriver
                (controlDriver bearer $ ControlClientDriverTrace >$< tracer)
                peer
                ()
    )
