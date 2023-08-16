{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
where

import Cardano.KESAgent.ServiceClient
import Cardano.KESAgent.Protocol
import Cardano.KESAgent.Pretty
import Cardano.KESAgent.RefCounting
import Cardano.KESAgent.OCert ( Crypto (..), OCert (..), KESPeriod (..) )

import Cardano.Crypto.KES.Class
import Cardano.Crypto.Libsodium (sodiumInit)

import Ouroboros.Network.RawBearer
import Ouroboros.Network.Snocket

import Control.Monad ( when, forever )
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadThrow ( bracket, finally, catch, SomeException )
import Control.Monad.Class.MonadTime ( getCurrentTime )
import Control.Monad.Class.MonadTimer ( threadDelay )
import Control.Concurrent.Class.MonadMVar
import Control.Tracer
import Data.Proxy (Proxy (..))
import Data.Maybe
import Data.Time.Clock.POSIX ( utcTimeToPOSIXSeconds )
import System.IOManager
import Network.Socket
import System.Environment
import System.Posix.Syslog.Priority as Syslog
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as Text
import Text.Printf

getOptions :: IOManager -> IO (ServiceClientOptions IO Socket SockAddr)
getOptions ioManager = do
  servicePath <- fromMaybe "/tmp/kes-agent-service.socket" <$> lookupEnv "KES_AGENT_SERVICE_PATH"
  return ServiceClientOptions
            { serviceClientSnocket = socketSnocket ioManager
            , serviceClientAddress = SockAddrUnix servicePath
            }

serviceTracePrio :: ServiceClientTrace -> Priority
serviceTracePrio ServiceClientDriverTrace {} = Syslog.Debug
serviceTracePrio ServiceClientSocketClosed {} = Syslog.Notice
serviceTracePrio ServiceClientConnected {} = Syslog.Notice
serviceTracePrio ServiceClientAttemptReconnect {} = Syslog.Info
serviceTracePrio ServiceClientReceivedKey {} = Syslog.Notice
serviceTracePrio ServiceClientAbnormalTermination {} = Syslog.Error

serviceTraceFormatBS :: ServiceClientTrace -> ByteString
serviceTraceFormatBS = encodeUtf8 . Text.pack . pretty

formatServiceTrace :: ServiceClientTrace -> (Priority, String)
formatServiceTrace msg =
  let prio = serviceTracePrio msg
  in (prio, pretty msg)

stdoutStringTracer :: Priority -> MVar IO () -> Tracer IO (Priority, String)
stdoutStringTracer maxPrio lock = Tracer $ \(prio, msg) -> do
  timestamp <- utcTimeToPOSIXSeconds <$> getCurrentTime
  when (prio <= maxPrio) $
        withMVar lock $ \_ -> do
          printf "%15.3f %-8s %s\n"
            (realToFrac timestamp :: Double)
            (show prio)
            msg
  
handleKey :: UnsoundKESAlgorithm (KES c)
          => CRef IO (SignKeyWithPeriodKES (KES c))
          -> OCert c
          -> IO ()
handleKey skpVar ocert = withCRefValue skpVar $ \skp -> do
  skSer <- rawSerialiseSignKeyKES (skWithoutPeriodKES skp)
  let period = periodKES skp
  let certN = ocertN ocert
  printf "KES key %i @%i: %s\n"
      certN period (hexShowBS skSer)

hexShowBS :: ByteString -> String
hexShowBS = concatMap (printf "%02x") . BS.unpack

main :: IO ()
main = do
  sodiumInit
  withIOManager $ \ioManager -> do
    logLock <- newMVar ()
    let maxPrio = Syslog.Debug
    serviceClientOptions <- getOptions ioManager
    let tracer = stdoutStringTracer maxPrio logLock
    let go = runServiceClient
                (Proxy @StandardCrypto)
                makeSocketRawBearer
                serviceClientOptions
                handleKey
                (contramap formatServiceTrace tracer)
                      `catch` (\(e :: AsyncCancelled) ->
                                  return ())
                      `catch` (\(e :: SomeException) ->
                                  traceWith tracer (Syslog.Emergency, show e))
    concurrently_ (forever $ threadDelay 1000) go
