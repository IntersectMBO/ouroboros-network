{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
where

import Cardano.KESAgent.KES.Evolution
import Cardano.KESAgent.Processes.Agent
import Cardano.KESAgent.Protocols.Service.Protocol
import Cardano.KESAgent.Protocols.StandardCrypto
import Cardano.KESAgent.Util.Pretty
import Cardano.KESAgent.Serialization.TextEnvelope
import Cardano.KESAgent.Serialization.CBOR

import Cardano.Crypto.Libsodium (sodiumInit)
import Cardano.Crypto.Libsodium.MLockedSeed

import Ouroboros.Network.RawBearer
import Ouroboros.Network.Snocket

import Control.Monad ( when, (<=<) )
import Control.Monad.Class.MonadThrow ( bracket, finally, catch, SomeException )
import Control.Monad.Class.MonadTime ( getCurrentTime )
import Control.Concurrent.Class.MonadMVar
import Control.Tracer
import Data.Proxy (Proxy (..))
import Data.Maybe
import Data.Time.Clock.POSIX ( utcTimeToPOSIXSeconds )
import System.IO (hFlush, stdout)
import System.IOManager
import Network.Socket
import System.Environment
import System.Posix.Daemonize
import System.Posix.Syslog.Priority as Syslog
import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as Text
import Text.Printf
import Text.Read ( readMaybe )
import Options.Applicative
import System.Posix.Files as Posix
import System.Posix.Types as Posix
import System.Posix.User as Posix
import Data.Set (Set)
import qualified Data.Set as Set

data NormalModeOptions
  = NormalModeOptions
      { nmoServicePath :: Maybe String
      , nmoControlPath :: Maybe String
      , nmoBootstrapPaths :: Set String
      , nmoLogLevel :: Maybe Priority
      , nmoColdVerKeyFile :: Maybe FilePath
      , nmoGenesisFile :: Maybe FilePath
      }
  deriving (Show)

instance Semigroup NormalModeOptions where
  NormalModeOptions sp1 cp1 bps1 ll1 vkp1 gf1 <>
    NormalModeOptions sp2 cp2 bps2 ll2 vkp2 gf2 =
      NormalModeOptions
        (sp1 <|> sp2)
        (cp1 <|> cp2)
        (bps1 <> bps2)
        (ll1 <|> ll2)
        (vkp1 <|> vkp2)
        (gf1 <|> gf2)

defNormalModeOptions :: NormalModeOptions
defNormalModeOptions =
  NormalModeOptions
    { nmoServicePath = Just "/tmp/kes-agent-service.socket"
    , nmoControlPath = Just "/tmp/kes-agent-control.socket"
    , nmoBootstrapPaths = Set.empty
    , nmoLogLevel = Just Syslog.Notice
    , nmoColdVerKeyFile = Just "./cold.vkey"
    , nmoGenesisFile = Nothing
    }

data ServiceModeOptions
  = ServiceModeOptions
      { smoServicePath :: Maybe String
      , smoControlPath :: Maybe String
      , smoBootstrapPaths :: Set String
      , smoUser :: Maybe String
      , smoGroup :: Maybe String
      , smoGenesisFile :: Maybe FilePath
      }
  deriving (Show)

instance Semigroup ServiceModeOptions where
  ServiceModeOptions sp1 cp1 bps1 uid1 gid1 gf1 <>
    ServiceModeOptions sp2 cp2 bps2 uid2 gid2 gf2 =
      ServiceModeOptions
        (sp1 <|> sp2)
        (cp1 <|> cp2)
        (bps1 <> bps2)
        (uid1 <|> uid2)
        (gid1 <|> gid2)
        (gf1 <|> gf2)

defServiceModeOptions :: ServiceModeOptions
defServiceModeOptions =
  ServiceModeOptions
    { smoServicePath = Just "/tmp/kes-agent-service.socket"
    , smoControlPath = Just "/tmp/kes-agent-control.socket"
    , smoBootstrapPaths = Set.empty
    , smoUser = Just "kes-agent"
    , smoGroup = Just "kes-agent"
    , smoGenesisFile = Nothing
    }

nullServiceModeOptions :: ServiceModeOptions
nullServiceModeOptions =
  ServiceModeOptions
    { smoServicePath = Nothing
    , smoControlPath = Nothing
    , smoBootstrapPaths = Set.empty
    , smoUser = Nothing
    , smoGroup = Nothing
    , smoGenesisFile = Nothing
    }

data ProgramOptions
  = RunAsService ServiceModeOptions
  | RunNormally NormalModeOptions
  deriving (Show)

pProgramOptions = subparser
    (  command "start" (info (pure $ RunAsService nullServiceModeOptions) idm)
    <> command "stop" (info (pure $ RunAsService nullServiceModeOptions) idm)
    <> command "restart" (info (pure $ RunAsService nullServiceModeOptions) idm)
    <> command "status" (info (pure $ RunAsService nullServiceModeOptions) idm)
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
    <*> (Set.fromList <$> many (strOption
          (  long "bootstrap-address"
          <> short 'b'
          <> metavar "PATH"
          <> help "Socket address for 'bootstrapping' connections"
          )
        ))
    <*> option (Just <$> eitherReader readLogLevel)
          (  long "log-level"
          <> short 'l'
          <> value Nothing
          <> metavar "LEVEL"
          <> help "Logging level. One of 'debug', 'info', 'notice', 'warn', 'error', 'critical', 'emergency'."
          )
    <*> option (Just <$> str)
          (  long "cold-verification-key"
          <> value Nothing
          <> metavar "PATH"
          <> help "Cold verification key file, used to validate OpCerts upon receipt"
          )
    <*> option (Just <$> str)
          (  long "genesis-file"
          <> value Nothing
          <> metavar "PATH"
          <> help "Genesis file (mainnet-ERA-genesis.json)"
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

splitBy :: Ord a => a -> [a] -> [[a]]
splitBy sep [] = []
splitBy sep xs =
  let (lhs, rhs) = break (== sep) xs
  in lhs : splitBy sep (drop 1 rhs)

nmoFromEnv :: IO NormalModeOptions
nmoFromEnv = do
  servicePath <- lookupEnv "KES_AGENT_SERVICE_PATH"
  controlPath <- lookupEnv "KES_AGENT_CONTROL_PATH"
  bootstrapPathsRaw <- lookupEnv "KES_AGENT_BOOTSTRAP_PATHS"
  let bootstrapPaths =
        Set.fromList $ maybe [] (splitBy ':') bootstrapPathsRaw
  coldVerKeyPath <- lookupEnv "KES_AGENT_COLD_VK"
  genesisFile <- lookupEnv "KES_AGENT_GENESIS_FILE"
  logLevel <- fmap (either error id . readLogLevel) <$> lookupEnv "KES_AGENT_LOG_LEVEL"
  return NormalModeOptions
    { nmoServicePath = servicePath
    , nmoControlPath = controlPath
    , nmoBootstrapPaths = bootstrapPaths
    , nmoLogLevel = logLevel
    , nmoColdVerKeyFile = coldVerKeyPath
    , nmoGenesisFile = genesisFile
    }

smoFromEnv :: IO ServiceModeOptions
smoFromEnv = do
  servicePath <- lookupEnv "KES_AGENT_SERVICE_PATH"
  controlPath <- lookupEnv "KES_AGENT_CONTROL_PATH"
  bootstrapPathsRaw <- lookupEnv "KES_AGENT_BOOTSTRAP_PATHS"
  let bootstrapPaths =
        Set.fromList $ maybe [] (splitBy ':') bootstrapPathsRaw
  groupSpec <- lookupEnv "KES_AGENT_GROUP"
  userSpec <- lookupEnv "KES_AGENT_USER"
  genesisFile <- lookupEnv "KES_AGENT_GENESIS_FILE"
  return ServiceModeOptions
    { smoServicePath = servicePath
    , smoControlPath = controlPath
    , smoBootstrapPaths = bootstrapPaths
    , smoUser = userSpec
    , smoGroup = groupSpec
    , smoGenesisFile = genesisFile
    }

nmoToAgentOptions :: NormalModeOptions -> IO (AgentOptions IO SockAddr StandardCrypto)
nmoToAgentOptions nmo = do
  servicePath <- maybe (error "No service address") return (nmoServicePath nmo)
  controlPath <- maybe (error "No control address") return (nmoControlPath nmo)
  let bootstrapPaths = Set.toList $ nmoBootstrapPaths nmo
  coldVerKeyPath <- maybe (error "No cold verification key") return (nmoColdVerKeyFile nmo)
  (ColdVerKey coldVerKey) <- either error return =<< decodeTextEnvelopeFile coldVerKeyPath
  evolutionConfig <- maybe
                      (pure defEvolutionConfig)
                      (either error return <=< evolutionConfigFromGenesisFile)
                      (nmoGenesisFile nmo)
  return defAgentOptions
            { agentServiceAddr = SockAddrUnix servicePath
            , agentControlAddr = SockAddrUnix controlPath
            , agentBootstrapAddr = map SockAddrUnix bootstrapPaths
            , agentColdVerKey = coldVerKey
            , agentGenSeed = mlockedSeedNewRandom
            , agentEvolutionConfig = evolutionConfig
            }

smoToAgentOptions :: ServiceModeOptions -> IO (AgentOptions IO SockAddr StandardCrypto)
smoToAgentOptions smo = do
  servicePath <- maybe (error "No service address") return (smoServicePath smo)
  controlPath <- maybe (error "No control address") return (smoControlPath smo)
  let bootstrapPaths = Set.toList $ smoBootstrapPaths smo
  evolutionConfig <- maybe
                      (pure defEvolutionConfig)
                      (either error return <=< evolutionConfigFromGenesisFile)
                      (smoGenesisFile smo)
  return defAgentOptions
            { agentServiceAddr = SockAddrUnix servicePath
            , agentControlAddr = SockAddrUnix controlPath
            , agentBootstrapAddr = map SockAddrUnix bootstrapPaths
            , agentGenSeed = mlockedSeedNewRandom
            , agentEvolutionConfig = evolutionConfig
            }

agentTracePrio :: AgentTrace -> Priority
agentTracePrio AgentServiceDriverTrace {} = Syslog.Debug
agentTracePrio AgentControlDriverTrace {} = Syslog.Debug
agentTracePrio (AgentBootstrapTrace ServiceClientDriverTrace {}) = Syslog.Debug
agentTracePrio (AgentBootstrapTrace ServiceClientSocketClosed {}) = Syslog.Notice
agentTracePrio (AgentBootstrapTrace ServiceClientConnected {}) = Syslog.Notice
agentTracePrio (AgentBootstrapTrace ServiceClientAttemptReconnect {}) = Syslog.Info
agentTracePrio (AgentBootstrapTrace ServiceClientReceivedKey {}) = Syslog.Notice
agentTracePrio (AgentBootstrapTrace ServiceClientAbnormalTermination {}) = Syslog.Error
agentTracePrio (AgentBootstrapTrace ServiceClientOpCertNumberCheck {}) = Syslog.Debug
agentTracePrio AgentReplacingPreviousKey {} = Syslog.Notice
agentTracePrio AgentRejectingKey {} = Syslog.Warning
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
          hFlush stdout


runAsService :: ServiceModeOptions -> IO ()
runAsService smo' =
  go `catch` (\(e :: SomeException) ->
    syslog Syslog.Critical (encodeUtf8 . Text.pack $ show e))
  where
    go :: IO ()
    go =  withIOManager $ \ioManager -> do
      smoEnv <- smoFromEnv
      let smo = smo' <> smoEnv <> defServiceModeOptions
      agentOptions <- smoToAgentOptions smo
      groupName <- maybe (error "Invalid group") return $ smoGroup smo
      userName <- maybe (error "Invalid user") return $ smoUser smo
      servicePath <- maybe (error "Invalid service address") return $ smoServicePath smo
      controlPath <- maybe (error "Invalid control address") return $ smoControlPath smo

      serviced
        simpleDaemon
          { privilegedAction = do
              gid <- groupID <$> Posix.getGroupEntryForName groupName
              uid <- userID <$> Posix.getUserEntryForName userName
              Posix.setFileCreationMask 0770
              agent <- newAgent
                (Proxy @StandardCrypto)
                (socketSnocket ioManager)
                makeSocketRawBearer
                agentOptions { agentTracer = syslogAgentTracer }
              setOwnerAndGroup servicePath uid gid
              setOwnerAndGroup controlPath uid gid
              return agent
          , program = \agent -> do
              runAgent agent `finally` finalizeAgent agent
          , group = Just groupName
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
