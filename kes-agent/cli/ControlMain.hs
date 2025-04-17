{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The main @kes-agent-control@ program.
module Main
where

import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.Processes.ControlClient
import Cardano.KESAgent.Protocols.AgentInfo
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Protocols.StandardCrypto
import Cardano.KESAgent.Serialization.CBOR
import Cardano.KESAgent.Serialization.TextEnvelope
import Cardano.KESAgent.Util.HexBS
import Cardano.KESAgent.Util.Version

import Cardano.Crypto.DSIGN.Class
import Cardano.Crypto.KES.Class
import Cardano.Crypto.Libsodium (sodiumInit)
import Ouroboros.Network.RawBearer
import Ouroboros.Network.Snocket

import Control.Monad (forM_, unless, when)
import Control.Monad.Extra (whenJust)
import Control.Tracer
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Data.Proxy
import Network.Socket
import Options.Applicative
import System.Environment
import System.Exit
import System.IOManager
import Text.Printf
import Text.Read (readMaybe)

data CommonOptions
  = CommonOptions
  { optControlPath :: Maybe String
  , optVerbosity :: Maybe Int
  , optRetryDelay :: Maybe Int
  , optRetryExponential :: Maybe Bool
  , optRetryAttempts :: Maybe Int
  }
  deriving (Show, Eq)

instance Semigroup CommonOptions where
  CommonOptions p1 v1 ri1 re1 ra1 <> CommonOptions p2 v2 ri2 re2 ra2 =
    CommonOptions
      (p1 <|> p2)
      (v1 <|> v2)
      (ri1 <|> ri2)
      (re1 <|> re2)
      (ra1 <|> ra2)

defCommonOptions :: CommonOptions =
  CommonOptions
    { optControlPath = Just "/tmp/kes-agent-control.socket"
    , optVerbosity = Just 0
    , optRetryDelay = Nothing
    , optRetryExponential = Nothing
    , optRetryAttempts = Nothing
    }

optFromEnv :: IO CommonOptions
optFromEnv = do
  controlPath <- lookupEnv "KES_AGENT_CONTROL_PATH"
  retryDelay <- (>>= readMaybe) <$> lookupEnv "KES_AGENT_CONTROL_RETRY_INTERVAL"
  retryAttempts <- (>>= readMaybe) <$> lookupEnv "KES_AGENT_CONTROL_RETRY_ATTEMPTS"
  return
    CommonOptions
      { optControlPath = controlPath
      , optVerbosity = Nothing
      , optRetryDelay = retryDelay
      , optRetryAttempts = retryAttempts
      , optRetryExponential = Nothing
      }

class WithCommonOptions a where
  withCommonOptions :: CommonOptions -> a -> a

instance WithCommonOptions CommonOptions where
  withCommonOptions = (<>)

data GenKeyOptions
  = GenKeyOptions
  { gkoCommon :: CommonOptions
  , gkoKESVerificationKeyFile :: Maybe FilePath
  }
  deriving (Show, Eq)

instance Semigroup GenKeyOptions where
  GenKeyOptions c1 vk1 <> GenKeyOptions c2 vk2 =
    GenKeyOptions (c1 <> c2) (vk1 <|> vk2)

instance WithCommonOptions GenKeyOptions where
  withCommonOptions c o = o {gkoCommon = c <> gkoCommon o}

defGenKeyOptions :: GenKeyOptions =
  GenKeyOptions
    { gkoCommon = defCommonOptions
    , gkoKESVerificationKeyFile = Just "kes.vkey"
    }

gkoFromEnv :: IO GenKeyOptions
gkoFromEnv = do
  common <- optFromEnv
  return
    defGenKeyOptions
      { gkoCommon = common
      }

data QueryKeyOptions
  = QueryKeyOptions
  { qkoCommon :: CommonOptions
  , qkoKESVerificationKeyFile :: Maybe FilePath
  }
  deriving (Show, Eq)

instance Semigroup QueryKeyOptions where
  QueryKeyOptions c1 vk1 <> QueryKeyOptions c2 vk2 =
    QueryKeyOptions (c1 <> c2) (vk1 <|> vk2)

instance WithCommonOptions QueryKeyOptions where
  withCommonOptions c o = o {qkoCommon = c <> qkoCommon o}

defQueryKeyOptions :: QueryKeyOptions =
  QueryKeyOptions
    { qkoCommon = defCommonOptions
    , qkoKESVerificationKeyFile = Just "kes.vkey"
    }

qkoFromEnv :: IO QueryKeyOptions
qkoFromEnv = do
  common <- optFromEnv
  return
    defQueryKeyOptions
      { qkoCommon = common
      }

newtype DropStagedKeyOptions
  = DropStagedKeyOptions
  { dskoCommon :: CommonOptions
  }
  deriving (Show, Eq)

instance Semigroup DropStagedKeyOptions where
  DropStagedKeyOptions c1 <> DropStagedKeyOptions c2 =
    DropStagedKeyOptions (c1 <> c2)

instance WithCommonOptions DropStagedKeyOptions where
  withCommonOptions c o = o {dskoCommon = c <> dskoCommon o}

defDropStagedKeyOptions :: DropStagedKeyOptions =
  DropStagedKeyOptions
    { dskoCommon = defCommonOptions
    }

dskoFromEnv :: IO DropStagedKeyOptions
dskoFromEnv = do
  common <- optFromEnv
  return
    defDropStagedKeyOptions
      { dskoCommon = common
      }

newtype DropKeyOptions
  = DropKeyOptions
  { dkoCommon :: CommonOptions
  }
  deriving (Show, Eq)

instance Semigroup DropKeyOptions where
  DropKeyOptions c1 <> DropKeyOptions c2 =
    DropKeyOptions (c1 <> c2)

instance WithCommonOptions DropKeyOptions where
  withCommonOptions c o = o {dkoCommon = c <> dkoCommon o}

defDropKeyOptions :: DropKeyOptions =
  DropKeyOptions
    { dkoCommon = defCommonOptions
    }

dkoFromEnv :: IO DropKeyOptions
dkoFromEnv = do
  common <- optFromEnv
  return
    defDropKeyOptions
      { dkoCommon = common
      }

data InstallKeyOptions
  = InstallKeyOptions
  { ikoCommon :: CommonOptions
  , ikoOpCertFile :: Maybe FilePath
  }
  deriving (Show, Eq)

instance Semigroup InstallKeyOptions where
  InstallKeyOptions c1 vk1 <> InstallKeyOptions c2 vk2 =
    InstallKeyOptions (c1 <> c2) (vk1 <|> vk2)

instance WithCommonOptions InstallKeyOptions where
  withCommonOptions c o = o {ikoCommon = c <> ikoCommon o}

defInstallKeyOptions :: InstallKeyOptions =
  InstallKeyOptions
    { ikoCommon = defCommonOptions
    , ikoOpCertFile = Just "kes.vkey"
    }

ikoFromEnv :: IO InstallKeyOptions
ikoFromEnv = do
  common <- optFromEnv
  return
    defInstallKeyOptions
      { ikoCommon = common
      }

pCommonOptions =
  CommonOptions
    <$> option
      (Just <$> str)
      ( long "control-address"
          <> short 'c'
          <> value Nothing
          <> metavar "ADDR"
          <> help
            ( "Socket address for 'control' connections to a running kes-agent process. "
                <> "($KES_AGENT_CONTROL_PATH)"
            )
      )
    <*> option
      (Just <$> auto)
      ( long "verbose"
          <> short 'v'
          <> value (Just 1)
          <> help "Set verbosity"
      )
    <*> option
      (Just <$> auto)
      ( long "retry-interval"
          <> long "retry-delay"
          <> value Nothing
          <> help
            ( "Connection retry interval (milliseconds). "
                <> "($KES_AGENT_CONTROL_RETRY_INTERVAL)"
            )
      )
    <*> flag
      Nothing
      (Just True)
      ( long "retry-exponential"
          <> help "Exponentially increase retry interval"
      )
    <*> option
      (Just <$> auto)
      ( long "retry-attempts"
          <> value Nothing
          <> help
            ( "Number of connection retry attempts. "
                <> "($KES_AGENT_CONTROL_RETRY_ATTEMPTS)"
            )
      )

pGenKeyOptions =
  GenKeyOptions
    <$> pure defCommonOptions
    <*> pVerKeyFile

pQueryKeyOptions =
  QueryKeyOptions
    <$> pure defCommonOptions
    <*> pVerKeyFile

pDropStagedKeyOptions =
  DropStagedKeyOptions
    <$> pure defCommonOptions

pDropKeyOptions =
  DropKeyOptions
    <$> pure defCommonOptions

pInstallKeyOptions =
  InstallKeyOptions
    <$> pure defCommonOptions
    <*> pOpCertFile

pVerKeyFile =
  option
    (Just <$> str)
    ( long "kes-verification-key-file"
        <> value Nothing
        <> metavar "FILE"
        <> help "File to write KES verification key to"
    )

pOpCertFile =
  option
    (Just <$> str)
    ( long "opcert-file"
        <> value Nothing
        <> metavar "FILE"
        <> help "File to read OpCert from"
    )

data ProgramOptions
  = RunGenKey GenKeyOptions
  | RunQueryKey QueryKeyOptions
  | RunDropStagedKey DropStagedKeyOptions
  | RunInstallKey InstallKeyOptions
  | RunDropKey DropKeyOptions
  | RunGetInfo CommonOptions -- for now
  deriving (Show, Eq)

programOptionsWithCommonOptions :: CommonOptions -> ProgramOptions -> ProgramOptions
programOptionsWithCommonOptions c (RunGenKey o) = RunGenKey (withCommonOptions c o)
programOptionsWithCommonOptions c (RunQueryKey o) = RunQueryKey (withCommonOptions c o)
programOptionsWithCommonOptions c (RunDropStagedKey o) = RunDropStagedKey (withCommonOptions c o)
programOptionsWithCommonOptions c (RunInstallKey o) = RunInstallKey (withCommonOptions c o)
programOptionsWithCommonOptions c (RunDropKey o) = RunDropKey (withCommonOptions c o)
programOptionsWithCommonOptions c (RunGetInfo o) = RunGetInfo (withCommonOptions c o)

pProgramOptions :: Parser ProgramOptions
pProgramOptions =
  programOptionsWithCommonOptions
    <$> pCommonOptions
    <*> subparser
      ( command "gen-staged-key" (info (RunGenKey <$> pGenKeyOptions) idm)
          <> command "drop-staged-key" (info (RunDropStagedKey <$> pDropStagedKeyOptions) idm)
          <> command "export-staged-vkey" (info (RunQueryKey <$> pQueryKeyOptions) idm)
          <> command "install-key" (info (RunInstallKey <$> pInstallKeyOptions) idm)
          <> command "drop-key" (info (RunDropKey <$> pDropKeyOptions) idm)
          <> command "info" (info (pure $ RunGetInfo defCommonOptions) idm)
      )

eitherError :: Either String a -> IO a
eitherError (Left err) = error err
eitherError (Right x) = return x

-- | A tracer that prints control client log messages to 'stdout', formatting
-- them in a somewhat human friendly way.
humanFriendlyControlTracer :: Int -> Tracer IO ControlClientTrace
humanFriendlyControlTracer verbosity = Tracer $ \case
  ControlClientKeyAccepted -> putStrLn "Key accepted."
  ControlClientKeyRejected reason -> putStrLn $ "Key rejected: " ++ formatReason reason
  ControlClientAttemptReconnect 1 -> when (verbosity > 0) $ do
    printf "Connection to agent failed, will try again 1 more time\n"
  ControlClientAttemptReconnect n -> when (verbosity > 0) $ do
    printf "Connection to agent failed, will try again %i more times\n" n
  x -> when (verbosity > 1) (putStrLn $ "kes-agent-control:" ++ show x)

formatReason :: RecvResult -> String
formatReason RecvOK = "no error"
formatReason RecvErrorInvalidOpCert = "OpCert validation failed"
formatReason RecvErrorKeyOutdated = "KES key outdated"
formatReason RecvErrorNoKey = "No KES key found"
formatReason RecvErrorUnsupportedOperation = "Operation not supported"
formatReason RecvErrorUnknown = "unknown error"

mkControlClientOptions :: CommonOptions -> IOManager -> IO (ControlClientOptions IO Socket SockAddr)
mkControlClientOptions opts ioManager = do
  controlPath <- maybe (error "No control address") return (optControlPath opts)

  let retryDelay = fromMaybe 1000 $ optRetryDelay opts
  let retryExponential = fromMaybe False $ optRetryExponential opts
  let retryAttempts = fromMaybe 0 $ optRetryAttempts opts

  return
    ControlClientOptions
      { controlClientSnocket = socketSnocket ioManager
      , controlClientAddress = SockAddrUnix controlPath
      , controlClientLocalAddress = Nothing
      , controlClientRetryDelay = retryDelay
      , controlClientRetryExponential = retryExponential
      , controlClientRetryAttempts = retryAttempts
      }

runControlClientCommand ::
  CommonOptions ->
  IOManager ->
  (ControlClient IO StandardCrypto -> ControlHandler IO a) ->
  IO a
runControlClientCommand opts ioManager handler = do
  controlClientOptions <- mkControlClientOptions opts ioManager
  let verbosity = fromMaybe 0 $ optVerbosity opts
  runControlClient1 @StandardCrypto @IO
    handler
    (Proxy @StandardCrypto)
    makeSocketRawBearer
    controlClientOptions
    (humanFriendlyControlTracer verbosity)

runGenKey :: GenKeyOptions -> IO ()
runGenKey gko' = withIOManager $ \ioManager -> do
  putStrLn "Asking agent to generate a key..."
  gkoEnv <- gkoFromEnv
  let gko = gko' <> gkoEnv <> defGenKeyOptions
  let verKeyFilenameMay = gkoKESVerificationKeyFile gko
  vkKESMay <-
    runControlClientCommand
      (gkoCommon gko)
      ioManager
      controlGenKey
  case vkKESMay of
    Nothing -> do
      putStrLn "Key generation has failed. Please check KES agent log for details."
    Just vkKES -> do
      putStrLn "KES SignKey generated."
      case verKeyFilenameMay of
        Just "-" -> do
          BS.putStr $ encodeTextEnvelope (KESVerKey vkKES)
          putStrLn ""
        Just verKeyFilename -> do
          encodeTextEnvelopeFile verKeyFilename (KESVerKey vkKES)
          putStrLn $ "KES VerKey written to " ++ verKeyFilename
        Nothing ->
          return ()

runQueryKey :: QueryKeyOptions -> IO ()
runQueryKey qko' = withIOManager $ \ioManager -> do
  qkoEnv <- qkoFromEnv
  let qko = qko' <> qkoEnv <> defQueryKeyOptions
  let verKeyFilename = fromMaybe "-" (qkoKESVerificationKeyFile qko)
  vkKESMay <-
    runControlClientCommand
      (qkoCommon qko)
      ioManager
      controlQueryKey
  case vkKESMay of
    Nothing -> do
      putStrLn "No key available."
    Just vkKES -> do
      case verKeyFilename of
        "-" -> do
          BS.putStr $ encodeTextEnvelope (KESVerKey vkKES)
          putStrLn ""
        _ -> do
          encodeTextEnvelopeFile verKeyFilename (KESVerKey vkKES)
          putStrLn $ "KES VerKey written to " ++ verKeyFilename

runDropStagedKey :: DropStagedKeyOptions -> IO ()
runDropStagedKey dsko' = withIOManager $ \ioManager -> do
  dskoEnv <- dskoFromEnv
  let dsko = dsko' <> dskoEnv <> defDropStagedKeyOptions
  vkKESMay <-
    runControlClientCommand
      (dskoCommon dsko)
      ioManager
      controlDropStagedKey
  case vkKESMay of
    Nothing -> do
      putStrLn "Staged key dropped."
    Just vkKES -> do
      putStrLn "Staged key not dropped:"
      BS.putStr $ encodeTextEnvelope (KESVerKey vkKES)
      putStrLn ""

runInstallKey :: InstallKeyOptions -> IO ()
runInstallKey iko' = withIOManager $ \ioManager -> do
  ikoEnv <- ikoFromEnv
  let iko = iko' <> ikoEnv <> defInstallKeyOptions
  ikoKeyFile <- maybe (error "Missing OpCert file") return (ikoOpCertFile iko)

  opCertEither <- decodeTextEnvelopeFile ikoKeyFile
  case opCertEither of
    Left err -> do
      putStrLn $ "Error: " ++ err
    Right (OpCert oc _) -> do
      result <-
        runControlClientCommand
          (ikoCommon iko)
          ioManager
          (\c -> controlInstallKey c oc)
      if result == RecvOK
        then
          putStrLn "KES key installed."
        else do
          putStrLn $ "Error: " ++ formatReason result
          exitWith $ ExitFailure (fromEnum result)

runDropKey :: DropKeyOptions -> IO ()
runDropKey dko' = withIOManager $ \ioManager -> do
  dkoEnv <- dkoFromEnv
  let dko = dko' <> dkoEnv <> defDropKeyOptions
  result <-
    runControlClientCommand
      (dkoCommon dko)
      ioManager
      controlDropKey
  case result of
    RecvOK -> do
      putStrLn "KES key dropped."
    err -> do
      putStrLn "KES key not dropped:"
      print err

runGetInfo :: CommonOptions -> IO ()
runGetInfo opt' = withIOManager $ \ioManager -> do
  optEnv <- optFromEnv
  let opt = opt' <> optEnv <> defCommonOptions
  info <- runControlClientCommand opt ioManager controlGetInfo
  printf "Current time: %s\n" $ show (agentInfoCurrentTime info)
  printf "Current KES period: %u\n" (unKESPeriod $ agentInfoCurrentKESPeriod info)
  whenJust (agentInfoCurrentBundle info) $ \tbundleInfo -> do
    printf "--- Installed KES SignKey ---\n"
    printf "Timestamp: %s\n" (maybe "n/a" show $ taggedBundleInfoTimestamp tbundleInfo)
    case taggedBundleInfo tbundleInfo of
      Nothing -> do
        printf "{KEY DELETED}"
      Just bundleInfo -> do
        printf "VerKey: %s\n" (hexShowBS . rawSerialiseVerKeyKES $ bundleInfoVK bundleInfo)
        printf "Valid from period: %u\n" (unKESPeriod $ bundleInfoStartKESPeriod bundleInfo)
        printf
          "Current evolution: %u / %u\n"
          (bundleInfoEvolution bundleInfo)
          (totalPeriodsKES (Proxy @(KES StandardCrypto)))
        printf "OpCert number: %u\n" (bundleInfoOCertN bundleInfo)
        let (SignedDSIGN sig) = bundleInfoSigma bundleInfo
        printf "OpCert signature: %s\n" (hexShowBS . rawSerialiseSigDSIGN $ sig)
  whenJust (agentInfoStagedKey info) $ \keyInfo -> do
    printf "--- Staged KES SignKey ---\n"
    printf "VerKey: %s\n" (hexShowBS . rawSerialiseVerKeyKES $ keyInfoVK keyInfo)
  unless (null $ agentInfoBootstrapConnections info) $ do
    printf "--- Bootstrap Agents ---\n"
    forM_ (agentInfoBootstrapConnections info) $ \(BootstrapInfo addr status) -> do
      printf "%+30s %s\n" addr (show status)

programDesc = fullDesc

main :: IO ()
main = do
  sodiumInit
  let parserPrefs = prefs $ subparserInline <> helpShowGlobals
      versionStr = "kes-agent-control " ++ libraryVersion
  programOptions <- customExecParser
                      parserPrefs
                      (info
                        (pProgramOptions <**>
                          simpleVersioner versionStr <**>
                          helper
                        )
                        programDesc
                      )
  case programOptions of
    RunGenKey opts' -> runGenKey opts'
    RunQueryKey opts' -> runQueryKey opts'
    RunDropStagedKey opts' -> runDropStagedKey opts'
    RunDropKey opts' -> runDropKey opts'
    RunInstallKey opts' -> runInstallKey opts'
    RunGetInfo opts' -> runGetInfo opts'
