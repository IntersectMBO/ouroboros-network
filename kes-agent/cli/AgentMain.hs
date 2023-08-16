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

getOptions :: IOManager -> IO (AgentOptions IO SockAddr)
getOptions ioManager = do
  servicePath <- fromMaybe "/tmp/kes-agent-service.socket" <$> lookupEnv "KES_AGENT_SERVICE_PATH"
  controlPath <- fromMaybe "/tmp/kes-agent-control.socket" <$> lookupEnv "KES_AGENT_CONTROL_PATH"
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
  

runAsService :: IO ()
runAsService = withIOManager $ \ioManager -> do
  agentOptions <- getOptions ioManager
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

runNormally :: IO ()
runNormally = withIOManager $ \ioManager -> do
  logLock <- newMVar ()
  let maxPrio = Syslog.Notice
  agentOptions <- getOptions ioManager
  bracket
    (newAgent
      (Proxy @StandardCrypto)
      (socketSnocket ioManager)
      makeSocketRawBearer
      agentOptions { agentTracer = stdoutAgentTracer maxPrio logLock }
    )
    finalizeAgent
    runAgent

main = do
  sodiumInit
  args <- getArgs
  case args of
    "-D":_ -> runNormally
    _ -> runAsService
