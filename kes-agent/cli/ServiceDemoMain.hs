{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main
where

import Cardano.KESAgent.KES.Bundle (Bundle (..), TaggedBundle (..))
import Cardano.KESAgent.KES.Crypto (Crypto (..))
import Cardano.KESAgent.KES.OCert (OCert (..))
import Cardano.KESAgent.Priority
import Cardano.KESAgent.Processes.ServiceClient
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Protocols.StandardCrypto
import Cardano.KESAgent.Util.ColoredOutput
import Cardano.KESAgent.Util.Pretty
import Cardano.KESAgent.Util.RefCounting
import Cardano.KESAgent.Util.Version

import Cardano.Crypto.KES.Class
import Cardano.Crypto.Libsodium (sodiumInit)

import Ouroboros.Network.RawBearer
import Ouroboros.Network.Snocket

import Control.Concurrent.Class.MonadMVar
import Control.Exception (AsyncException (..))
import Control.Monad (when, unless)
import Control.Monad.Class.MonadThrow (SomeException, catch)
import Control.Monad.Class.MonadTime (getCurrentTime)
import Control.Monad.Class.MonadTimer (threadDelay)
import Control.Tracer
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Proxy (Proxy (..))
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word (Word64)
import Network.Socket hiding (Debug)
import Options.Applicative
import System.Environment
import System.IO (hFlush, stdout)
import System.IOManager
import Text.Printf

data ServiceDemoOptions
  = ServiceDemoOptions
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
    <$> option
      (Just <$> str)
      ( long "service-address"
          <> short 's'
          <> value Nothing
          <> metavar "PATH"
          <> help "Socket address for 'service' connections"
      )

sdoFromEnv :: IO ServiceDemoOptions
sdoFromEnv = do
  servicePath <- lookupEnv "KES_AGENT_SERVICE_PATH"
  return
    defServiceDemoOptions
      { sdoServicePath = servicePath
      }

sdoToServiceClientOptions ::
  IOManager -> ServiceDemoOptions -> IO (ServiceClientOptions IO Socket SockAddr)
sdoToServiceClientOptions ioManager sdo = do
  servicePath <- maybe (error "Service address not configured") return $ sdoServicePath sdo
  return
    ServiceClientOptions
      { serviceClientSnocket = socketSnocket ioManager
      , serviceClientAddress = SockAddrUnix servicePath
      }

serviceTracePrio :: ServiceClientTrace -> Priority
serviceTracePrio ServiceClientVersionHandshakeTrace {} = Debug
serviceTracePrio ServiceClientDriverTrace {} = Debug
serviceTracePrio ServiceClientVersionHandshakeFailed {} = Error
serviceTracePrio ServiceClientSocketClosed {} = Info
serviceTracePrio ServiceClientConnected {} = Info
serviceTracePrio ServiceClientAttemptReconnect {} = Info
serviceTracePrio ServiceClientReceivedKey {} = Info
serviceTracePrio ServiceClientDeclinedKey {} = Info
serviceTracePrio ServiceClientDroppedKey {} = Info
serviceTracePrio ServiceClientAbnormalTermination {} = Error
serviceTracePrio ServiceClientOpCertNumberCheck {} = Debug
serviceTracePrio ServiceClientStopped {} = Notice

serviceTraceFormatBS :: ServiceClientTrace -> ByteString
serviceTraceFormatBS = encodeUtf8 . Text.pack . pretty

formatServiceTrace :: ServiceClientTrace -> (Priority, String)
formatServiceTrace msg =
  let prio = serviceTracePrio msg
  in (prio, pretty msg)

stdoutStringTracer :: ColorMode -> Priority -> MVar IO () -> Tracer IO (Priority, String)
stdoutStringTracer mode maxPrio lock = Tracer $ \(prio, msg) -> do
  let color = prioColor prio
  timestamp <- utcTimeToPOSIXSeconds <$> getCurrentTime
  when (prio <= maxPrio) $
    withMVar lock $ \_ -> do
      hcPutStrLn mode stdout color $
        printf
          "%15.3f %-8s %s"
          (realToFrac timestamp :: Double)
          (show prio)
          msg
      hFlush stdout

handleKey ::
  UnsoundKESAlgorithm (KES c) =>
  (ServiceClientState -> IO ()) ->
  TaggedBundle IO c ->
  IO RecvResult
handleKey setState TaggedBundle {taggedBundle = Just (Bundle skpVar ocert)} = do
  withCRefValue skpVar $ \skp -> do
    skSer <- rawSerialiseSignKeyKES (skWithoutPeriodKES skp)
    let period = periodKES skp
    let certN = ocertN ocert
    setState $ ServiceClientBlockForging certN period (take 8 (hexShowBS skSer) ++ "...")
    return RecvOK
handleKey setState TaggedBundle {taggedBundle = Nothing} = do
  setState $ ServiceClientWaitingForCredentials
  return RecvOK

hexShowBS :: ByteString -> String
hexShowBS = concatMap (printf "%02x") . BS.unpack

programDesc = fullDesc

data ServiceClientState
  = ServiceClientNotRunning
  | ServiceClientWaitingForCredentials
  | ServiceClientBlockForging Word64 Period String
  deriving (Show, Eq, Ord)

main :: IO ()
main = do
  sodiumInit
  let parserPrefs = prefs $ subparserInline <> helpShowGlobals
      versionStr = "kes-agent-control " ++ libraryVersion
  sdo' <-
    customExecParser
      parserPrefs
      ( info
          ( pServiceDemoOptions
              <**> simpleVersioner versionStr
              <**> helper
          )
          programDesc
      )
  sdoEnv <- sdoFromEnv
  let sdo = sdo' <> sdoEnv <> defServiceDemoOptions

  let maxPrio = Debug
  logLock <- newMVar ()
  let tracer = stdoutStringTracer ColorsAuto maxPrio logLock

  let stateTracer = contramap (\(old, new) -> (Notice, "State: " ++ show old ++ " -> " ++ show new)) tracer
  stateVar <- newMVar ServiceClientNotRunning
  let setState new = do
        old <- takeMVar stateVar
        putMVar stateVar new
        traceWith stateTracer (old, new)

  withIOManager $ \ioManager -> do
    serviceClientOptions <- sdoToServiceClientOptions ioManager sdo
    exitVar <- newMVar False
    traceWith tracer (Notice, "RUN SERVICE CLIENT")
    let go = do
          setState ServiceClientWaitingForCredentials
          runServiceClient
            (Proxy @StandardCrypto)
            makeSocketRawBearer
            serviceClientOptions
            (handleKey setState)
            (contramap formatServiceTrace tracer)
            `catch` ( \(e :: AsyncException) -> do
                        traceWith tracer (Notice, "SERVICE CLIENT CANCELLED")
                        _ <- tryTakeMVar exitVar
                        putMVar exitVar True
                    )
            `catch` ( \(e :: SomeException) ->
                        traceWith tracer (Notice, show e)
                    )
          shouldExit <- readMVar exitVar
          unless shouldExit $ do
            threadDelay 100000
            go
    go
    traceWith tracer (Notice, "SERVICE CLIENT TERMINATED")
