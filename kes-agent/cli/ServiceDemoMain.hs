{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
where

import Cardano.KESAgent.Processes.ServiceClient
import Cardano.KESAgent.Protocols.Service.Protocol
import Cardano.KESAgent.Protocols.StandardCrypto
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Util.Pretty
import Cardano.KESAgent.Util.RefCounting
import Cardano.KESAgent.KES.Crypto ( Crypto (..) )
import Cardano.KESAgent.KES.OCert ( OCert (..), KESPeriod (..) )
import Cardano.KESAgent.KES.Bundle ( Bundle (..) )

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
import System.IO (hFlush, stdout)
import System.IOManager
import Network.Socket
import System.Environment
import System.Posix.Syslog.Priority as Syslog
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as Text
import Options.Applicative
import Text.Printf

data ServiceDemoOptions =
  ServiceDemoOptions
    { sdoServicePath :: Maybe String
    }

instance Semigroup ServiceDemoOptions where
  ServiceDemoOptions sp1 <> ServiceDemoOptions sp2 =
    ServiceDemoOptions (sp1 <|> sp2)

defServiceDemoOptions :: ServiceDemoOptions
defServiceDemoOptions =
  ServiceDemoOptions
    { sdoServicePath = Just "/tmp/kes-agent-service.socket"
    }

pServiceDemoOptions =
  ServiceDemoOptions
    <$> option (Just <$> str)
          (  long "service-address"
          <> short 's'
          <> value Nothing
          <> metavar "PATH"
          <> help "Socket address for 'service' connections"
          )

sdoFromEnv :: IO ServiceDemoOptions
sdoFromEnv = do
  servicePath <- lookupEnv "KES_AGENT_SERVICE_PATH"
  return defServiceDemoOptions
          { sdoServicePath = servicePath
          }

sdoToServiceClientOptions :: IOManager -> ServiceDemoOptions -> IO (ServiceClientOptions IO Socket SockAddr)
sdoToServiceClientOptions ioManager sdo = do
  servicePath <- maybe (error "Service address not configured") return $ sdoServicePath sdo
  return ServiceClientOptions
            { serviceClientSnocket = socketSnocket ioManager
            , serviceClientAddress = SockAddrUnix servicePath
            }

serviceTracePrio :: ServiceClientTrace -> Priority
serviceTracePrio ServiceClientVersionHandshakeTrace {} = Syslog.Debug
serviceTracePrio ServiceClientDriverTrace {} = Syslog.Debug
serviceTracePrio ServiceClientSocketClosed {} = Syslog.Notice
serviceTracePrio ServiceClientConnected {} = Syslog.Notice
serviceTracePrio ServiceClientAttemptReconnect {} = Syslog.Info
serviceTracePrio ServiceClientReceivedKey {} = Syslog.Notice
serviceTracePrio ServiceClientAbnormalTermination {} = Syslog.Error
serviceTracePrio ServiceClientOpCertNumberCheck {} = Syslog.Debug

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
          hFlush stdout

handleKey :: UnsoundKESAlgorithm (KES c)
          => Bundle IO c
          -> IO RecvResult
handleKey (Bundle skpVar ocert) = withCRefValue skpVar $ \skp -> do
  skSer <- rawSerialiseSignKeyKES (skWithoutPeriodKES skp)
  let period = periodKES skp
  let certN = ocertN ocert
  printf "KES key %i @%i: %s\n"
      certN period (hexShowBS skSer)
  return RecvOK

hexShowBS :: ByteString -> String
hexShowBS = concatMap (printf "%02x") . BS.unpack

programDesc = fullDesc

main :: IO ()
main = do
  sodiumInit
  sdo' <- execParser (info (pServiceDemoOptions <**> helper) programDesc)
  sdoEnv <- sdoFromEnv
  let sdo = sdo' <> sdoEnv <> defServiceDemoOptions

  withIOManager $ \ioManager -> do
    serviceClientOptions <- sdoToServiceClientOptions ioManager sdo
    logLock <- newMVar ()
    let maxPrio = Syslog.Debug
    let tracer = stdoutStringTracer maxPrio logLock
    forever $ do
      runServiceClient
        (Proxy @StandardCrypto)
        makeSocketRawBearer
        serviceClientOptions
        handleKey
        (contramap formatServiceTrace tracer)
              `catch` (\(e :: AsyncCancelled) ->
                          return ())
              `catch` (\(e :: SomeException) ->
                          traceWith tracer (Syslog.Emergency, show e))
      threadDelay 10000000
