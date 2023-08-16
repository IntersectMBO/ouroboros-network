{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
where

import Cardano.KESAgent.Agent
import Cardano.KESAgent.Protocol
import Cardano.KESAgent.Pretty

import Cardano.Crypto.Libsodium (sodiumInit)

import Ouroboros.Network.RawBearer
import Ouroboros.Network.Snocket

import Control.Monad ( when )
import Control.Monad.Class.MonadThrow ( bracket, finally )
import Control.Monad.Class.MonadTime ( getCurrentTime )
import Control.Concurrent.Class.MonadMVar
import Control.Tracer
import Data.Proxy (Proxy (..))
import Data.Maybe
import Data.Time.Clock.POSIX ( utcTimeToPOSIXSeconds )
import System.IOManager
import Network.Socket
import System.Environment
import System.Posix.Daemonize
import System.Posix.Syslog.Priority as Syslog
import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as Text
import Text.Printf
import Options.Applicative

data NormalModeOptions
  = NormalModeOptions
      { nmoServicePath :: Maybe String
      , nmoControlPath :: Maybe String
      , nmoLogLevel :: Maybe Priority
      }
  deriving (Show)

instance Semigroup NormalModeOptions where
  NormalModeOptions sp1 cp1 ll1 <> NormalModeOptions sp2 cp2 ll2 =
    NormalModeOptions (sp1 <|> sp2) (cp1 <|> cp2) (ll1 <|> ll2)

defNormalModeOptions :: NormalModeOptions
defNormalModeOptions =
  NormalModeOptions
    { nmoServicePath = Just "/tmp/kes-agent-service.socket"
    , nmoControlPath = Just "/tmp/kes-agent-control.socket"
    , nmoLogLevel = Just Syslog.Notice
    }

data ServiceModeOptions
  = ServiceModeOptions
      { smoServicePath :: Maybe String
      , smoControlPath :: Maybe String
      }
  deriving (Show)

instance Semigroup ServiceModeOptions where
  ServiceModeOptions sp1 cp1 <> ServiceModeOptions sp2 cp2 =
    ServiceModeOptions (sp1 <|> sp2) (cp1 <|> cp2)

defServiceModeOptions :: ServiceModeOptions
defServiceModeOptions =
  ServiceModeOptions
    { smoServicePath = Just "/tmp/kes-agent-service.socket"
    , smoControlPath = Just "/tmp/kes-agent-control.socket"
    }

data ProgramOptions
  = RunAsService ServiceModeOptions
  | RunNormally NormalModeOptions
  deriving (Show)

pProgramOptions = subparser
    (  command "start" (info (pure $ RunAsService defServiceModeOptions) idm)
    <> command "stop" (info (pure $ RunAsService defServiceModeOptions) idm)
    <> command "restart" (info (pure $ RunAsService defServiceModeOptions) idm)
    <> command "status" (info (pure $ RunAsService defServiceModeOptions) idm)
    <> command "run" (info (RunNormally <$> pNormalModeOptions) idm)
    )

pNormalModeOptions =
  NormalModeOptions
    <$> option (Just <$> str)
          (  long "service-address"
          <> short 's'
          <> value Nothing
          <> metavar "PATH"
          <> help "Socket address for 'service' connections"
          )
    <*> option (Just <$> str)
          (  long "control-address"
          <> short 'c'
          <> value Nothing
          <> metavar "PATH"
          <> help "Socket address for 'control' connections"
          )
    <*> option (Just <$> eitherReader readLogLevel)
          (  long "log-level"
          <> short 'l'
          <> value Nothing
          <> metavar "LEVEL"
          <> help "Logging level. One of 'debug', 'info', 'notice', 'warn', 'error', 'critical', 'emergency'."
          )

readLogLevel :: String -> Either String Priority
readLogLevel "debug" = Right Syslog.Debug
readLogLevel "info" = Right Syslog.Info
readLogLevel "warn" = Right Syslog.Warning
readLogLevel "notice" = Right Syslog.Notice
readLogLevel "error" = Right Syslog.Error
readLogLevel "critical" = Right Syslog.Critical
readLogLevel "emergency" = Right Syslog.Emergency
readLogLevel x = Left $ "Invalid log level " ++ show x

nmoFromEnv :: IO NormalModeOptions
nmoFromEnv = do
  servicePath <- lookupEnv "KES_AGENT_SERVICE_PATH"
  controlPath <- lookupEnv "KES_AGENT_CONTROL_PATH"
  logLevel <- fmap (either error id . readLogLevel) <$> lookupEnv "KES_AGENT_LOG_LEVEL"
  return NormalModeOptions
    { nmoServicePath = servicePath
    , nmoControlPath = controlPath
    , nmoLogLevel = logLevel
    }

smoFromEnv :: IO ServiceModeOptions
smoFromEnv = do
  servicePath <- lookupEnv "KES_AGENT_SERVICE_PATH"
  controlPath <- lookupEnv "KES_AGENT_CONTROL_PATH"
  return ServiceModeOptions
    { smoServicePath = servicePath
    , smoControlPath = controlPath
    }

nmoToAgentOptions :: NormalModeOptions -> IO (AgentOptions IO SockAddr)
nmoToAgentOptions nmo = do
  servicePath <- maybe (error "No service address") return (nmoServicePath nmo)
  controlPath <- maybe (error "No control address") return (nmoControlPath nmo)
  return defAgentOptions
            { agentServiceAddr = SockAddrUnix servicePath
            , agentControlAddr = SockAddrUnix controlPath
            }

smoToAgentOptions :: ServiceModeOptions -> IO (AgentOptions IO SockAddr)
smoToAgentOptions smo = do
  servicePath <- maybe (error "No service address") return (smoServicePath smo)
  controlPath <- maybe (error "No control address") return (smoControlPath smo)
  return defAgentOptions
            { agentServiceAddr = SockAddrUnix servicePath
            , agentControlAddr = SockAddrUnix controlPath
            }

agentTracePrio :: AgentTrace -> Priority
agentTracePrio AgentServiceDriverTrace {} = Syslog.Debug
agentTracePrio AgentControlDriverTrace {} = Syslog.Debug
agentTracePrio AgentReplacingPreviousKey {} = Syslog.Notice
agentTracePrio AgentInstallingNewKey {} = Syslog.Notice
agentTracePrio AgentSkippingOldKey {} = Syslog.Info
agentTracePrio AgentServiceSocketClosed {} = Syslog.Notice
agentTracePrio AgentListeningOnServiceSocket {} = Syslog.Notice
agentTracePrio AgentServiceClientConnected {} = Syslog.Notice
agentTracePrio AgentServiceClientDisconnected {} = Syslog.Notice
agentTracePrio AgentServiceSocketError {} = Syslog.Error
agentTracePrio AgentControlSocketClosed {} = Syslog.Notice
agentTracePrio AgentListeningOnControlSocket {} = Syslog.Notice
agentTracePrio AgentControlClientConnected {} = Syslog.Notice
agentTracePrio AgentControlClientDisconnected {} = Syslog.Notice
agentTracePrio AgentControlSocketError {} = Syslog.Error
agentTracePrio AgentCheckEvolution {} = Syslog.Info
agentTracePrio AgentUpdateKESPeriod {} = Syslog.Notice
agentTracePrio AgentKeyNotEvolved {} = Syslog.Info
agentTracePrio AgentNoKeyToEvolve {} = Syslog.Info
agentTracePrio AgentKeyEvolved {} = Syslog.Notice
agentTracePrio AgentKeyExpired {} = Syslog.Warning
agentTracePrio AgentLockRequest {} = Syslog.Debug
agentTracePrio AgentLockAcquired {} = Syslog.Debug
agentTracePrio AgentLockReleased {} = Syslog.Debug
agentTracePrio AgentCRefEvent {} = Syslog.Debug

agentTraceFormatBS :: AgentTrace -> ByteString
agentTraceFormatBS = encodeUtf8 . Text.pack . pretty

syslogAgentTracer :: Tracer IO AgentTrace
syslogAgentTracer = Tracer $ \event ->
  syslog (agentTracePrio event) (agentTraceFormatBS event)

stdoutAgentTracer :: Priority -> MVar IO () -> Tracer IO AgentTrace
stdoutAgentTracer maxPrio lock = Tracer $ \msg -> do
  timestamp <- utcTimeToPOSIXSeconds <$> getCurrentTime
  let prio = agentTracePrio msg
  when (prio <= maxPrio) $
        withMVar lock $ \_ -> do
          printf "%15.3f %-8s %s\n"
            (realToFrac timestamp :: Double)
            (show prio)
            (pretty msg)
  

runAsService :: ServiceModeOptions -> IO ()
runAsService smo' = withIOManager $ \ioManager -> do
  smoEnv <- smoFromEnv
  let smo = smo' <> smoEnv <> defServiceModeOptions
  agentOptions <- smoToAgentOptions smo
  serviced
    simpleDaemon
      { privilegedAction = do
          newAgent
            (Proxy @StandardCrypto)
            (socketSnocket ioManager)
            makeSocketRawBearer
            agentOptions { agentTracer = syslogAgentTracer }
      , program = \agent -> do
          runAgent agent `finally` finalizeAgent agent
      }

runNormally :: NormalModeOptions -> IO ()
runNormally nmo' = withIOManager $ \ioManager -> do
  nmoEnv <- nmoFromEnv
  let nmo = nmo' <> nmoEnv <> defNormalModeOptions
  agentOptions <- nmoToAgentOptions nmo
  maxPrio <- maybe (error "invalid priority") return $ nmoLogLevel nmo

  logLock <- newMVar ()
  bracket
    (newAgent
      (Proxy @StandardCrypto)
      (socketSnocket ioManager)
      makeSocketRawBearer
      agentOptions { agentTracer = stdoutAgentTracer maxPrio logLock }
    )
    finalizeAgent
    runAgent

programDesc = fullDesc

main = do
  sodiumInit
  programOptions <- execParser (info (pProgramOptions <**> helper) programDesc)
  case programOptions of
    RunNormally nmo -> runNormally nmo
    RunAsService smo -> runAsService smo
