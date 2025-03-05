{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.KESAgent.Tests.EndToEnd (tests)
where

import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.Evolution (defEvolutionConfig, getCurrentKESPeriod)
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.Protocols.AgentInfo
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Protocols.StandardCrypto
import Cardano.KESAgent.Protocols.Types
import Cardano.KESAgent.Serialization.CBOR
import Cardano.KESAgent.Serialization.TextEnvelope
import Paths_kes_agent

import Cardano.Crypto.DSIGN.Class
import Cardano.Crypto.KES.Class

import Control.Concurrent.Async
import Control.Concurrent.Class.MonadMVar
import Control.Monad (forever, replicateM)
import Control.Monad.Class.MonadThrow (SomeException, bracket, catch, finally, throwIO)
import Control.Monad.Class.MonadTimer (threadDelay)
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO
import System.IO.Temp
import System.Process
import Test.Tasty
import Test.Tasty.HUnit

import Debug.Trace
import Text.Printf

defGenesisTimestamp :: Integer
defGenesisTimestamp = 1506203091 -- real-world genesis on the production ledger

tests :: TestTree
#if !defined(mingw32_HOST_OS)
tests =
  testGroup
    "end to end"
    [ testCase "kes-agent --help" kesAgentHelp
    , testCase "kes-agent --genesis-file" kesAgentGenesisFile
    , testCase "kes-agent-control --help" kesAgentControlHelp
    -- These tests take forever on Windows, and aren't really helpful anyway,
    -- since we don't support running kes-agent on Windows.
    , testGroup
        "kes-agent-control install-key"
        [ testCase "valid" kesAgentControlInstallValid
        , testCase "invalid opcert" kesAgentControlInstallInvalidOpCert
        , testCase "no key" kesAgentControlInstallNoKey
        , testCase "dropped key" kesAgentControlInstallDroppedKey
        , testCase "multiple nodes" kesAgentControlInstallMultiNodes
        ]
    , testGroup
        "evolution"
        [ testCase "key evolves forward" kesAgentEvolvesKey
        ]
    , testGroup
        "inter-agent"
        [ testCase "key propagated to other agent" kesAgentPropagate
        , testCase "self-healing 1 (agent 2 goes down)" kesAgentSelfHeal1
        , testCase "self-healing 2 (agent 1 goes down)" kesAgentSelfHeal2
        ]
    ]
#else
tests =
  testGroup
    "end to end"
    [ testCase "kes-agent --help" kesAgentHelp
    , testCase "kes-agent --genesis-file" kesAgentGenesisFile
    , testCase "kes-agent-control --help" kesAgentControlHelp
    ]
#endif

kesAgentHelp :: Assertion
kesAgentHelp = do
  (exitCode, stdout, stderr) <- readProcessWithExitCode "kes-agent" ["--help"] ""
  assertEqual (stdout ++ "\n" ++ stderr) ExitSuccess exitCode

kesAgentGenesisFile :: Assertion
kesAgentGenesisFile = do
  (agentOutLines, agentErrLines, exitCode) <- withSystemTempDirectory "kes-agent-tests" $ \tmpdir -> do
    let controlAddr = tmpdir </> "control.socket"
        serviceAddr = tmpdir </> "service.socket"
        kesKeyFile = tmpdir </> "kes.vkey"
        opcertFile = tmpdir </> "opcert.cert"
    coldSignKeyFile <- getDataFileName "fixtures/cold.skey"
    coldVerKeyFile <- getDataFileName "fixtures/cold.vkey"
    genesisFile <- getDataFileName "fixtures/mainnet-shelley-genesis.json"

    let args =
          [ "run"
          , "--genesis-file"
          , genesisFile
          , "--cold-verification-key"
          , coldVerKeyFile
          , "--service-address"
          , serviceAddr
          , "--control-address"
          , controlAddr
          ]
    withSpawnProcess "kes-agent" args $ \_ (Just hOut) (Just hErr) ph -> do
      threadDelay 100000
      terminateProcess ph
      (outT, errT) <-
        concurrently
          (Text.lines <$> Text.hGetContents hOut)
          (Text.lines <$> Text.hGetContents hErr)
      exitCode <- waitForProcess ph
      return (outT, errT, exitCode)
  assertMatchingOutputLines 3 ["ListeningOnControlSocket"] agentOutLines

kesAgentControlHelp :: Assertion
kesAgentControlHelp = do
  (exitCode, stdout, stderr) <- controlClient ["--help"]
  assertEqual (stdout ++ "\n" ++ stderr) ExitSuccess exitCode

prependMVar :: MVar IO [a] -> a -> IO ()
prependMVar var x = do
  xs <- takeMVar var
  let xs' = x : xs
  putMVar var xs'

logHandle :: Handle -> IO [String]
logHandle h = do
  var <- newMVar []
  let go = hGetLine h >>= prependMVar var
  forever go `catch` (\(_ :: SomeException) -> return ())
  reverse <$> readMVar var

kesAgentControlInstallValid :: Assertion
kesAgentControlInstallValid =
  withSystemTempDirectory "kes-agent-tests" $ \tmpdir -> do
    let controlAddr = tmpdir </> "control.socket"
        serviceAddr = tmpdir </> "service.socket"
        kesKeyFile = tmpdir </> "kes.vkey"
        opcertFile = tmpdir </> "opcert.cert"
    coldSignKeyFile <- getDataFileName "fixtures/cold.skey"
    coldVerKeyFile <- getDataFileName "fixtures/cold.vkey"

    let makeCert = do
          ColdSignKey coldSK <-
            either error return =<< decodeTextEnvelopeFile @(ColdSignKey (DSIGN StandardCrypto)) coldSignKeyFile
          ColdVerKey coldVK <-
            either error return =<< decodeTextEnvelopeFile @(ColdVerKey (DSIGN StandardCrypto)) coldVerKeyFile
          KESVerKey kesVK <-
            either error return =<< decodeTextEnvelopeFile @(KESVerKey (KES StandardCrypto)) kesKeyFile
          kesPeriod <- getCurrentKESPeriod defEvolutionConfig
          let ocert :: OCert StandardCrypto = makeOCert kesVK 0 kesPeriod coldSK
          encodeTextEnvelopeFile opcertFile (OpCert ocert coldVK)
          return ()

    (agentOutLines, serviceOutLines, ()) <-
      withAgentAndService controlAddr serviceAddr [] coldVerKeyFile $ do
        controlClientCheck
          [ "gen-staged-key"
          , "--kes-verification-key-file"
          , kesKeyFile
          , "--control-address"
          , controlAddr
          ]
          ExitSuccess
          [ "Asking agent to generate a key..."
          , "KES SignKey generated."
          , "KES VerKey written to " <> kesKeyFile
          ]
        makeCert
        controlClientCheck
          [ "install-key"
          , "--opcert-file"
          , opcertFile
          , "--control-address"
          , controlAddr
          ]
          ExitSuccess
          ["KES key installed."]
        -- Allow some time for service client to actually receive the key
        threadDelay 10000
    assertMatchingOutputLinesWith
      ("SERVICE OUTPUT CHECK\n" <> (Text.unpack . Text.unlines $ agentOutLines))
      0
      ["KES", "key", "0"]
      serviceOutLines

kesAgentControlInstallInvalidOpCert :: Assertion
kesAgentControlInstallInvalidOpCert =
  withSystemTempDirectory "kes-agent-tests" $ \tmpdir -> do
    let controlAddr = tmpdir </> "control.socket"
        serviceAddr = tmpdir </> "service.socket"
        kesKeyFile = tmpdir </> "kes.vkey"
    opcertFile <- getDataFileName "fixtures/opcert.cert"
    coldSignKeyFile <- getDataFileName "fixtures/cold.skey"
    coldVerKeyFile <- getDataFileName "fixtures/cold.vkey"

    (agentOutLines, serviceOutLines, ()) <-
      withAgentAndService controlAddr serviceAddr [] coldVerKeyFile $ do
        controlClientCheck
          [ "gen-staged-key"
          , "--kes-verification-key-file"
          , kesKeyFile
          , "--control-address"
          , controlAddr
          ]
          ExitSuccess
          [ "Asking agent to generate a key..."
          , "KES SignKey generated."
          , "KES VerKey written to " <> kesKeyFile
          ]
        controlClientCheck
          [ "install-key"
          , "--opcert-file"
          , opcertFile
          , "--control-address"
          , controlAddr
          ]
          (ExitFailure $ fromEnum RecvErrorInvalidOpCert)
          ["Error: OpCert validation failed"]

    assertNoMatchingOutputLines 0 ["KES", "key", "0"] serviceOutLines
    assertMatchingOutputLines 1 ["Warning", "Agent:", "RejectingKey"] agentOutLines

kesAgentControlInstallNoKey :: Assertion
kesAgentControlInstallNoKey =
  withSystemTempDirectory "kes-agent-tests" $ \tmpdir -> do
    let controlAddr = tmpdir </> "control.socket"
        serviceAddr = tmpdir </> "service.socket"
        opcertFile = tmpdir </> "opcert.cert"
    kesKeyFile <- getDataFileName "fixtures/kes.vkey"
    coldSignKeyFile <- getDataFileName "fixtures/cold.skey"
    coldVerKeyFile <- getDataFileName "fixtures/cold.vkey"

    let makeCert = do
          ColdSignKey coldSK <-
            either error return =<< decodeTextEnvelopeFile @(ColdSignKey (DSIGN StandardCrypto)) coldSignKeyFile
          ColdVerKey coldVK <-
            either error return =<< decodeTextEnvelopeFile @(ColdVerKey (DSIGN StandardCrypto)) coldVerKeyFile
          KESVerKey kesVK <-
            either error return =<< decodeTextEnvelopeFile @(KESVerKey (KES StandardCrypto)) kesKeyFile
          kesPeriod <- getCurrentKESPeriod defEvolutionConfig
          let ocert :: OCert StandardCrypto = makeOCert kesVK 0 kesPeriod coldSK
          encodeTextEnvelopeFile opcertFile (OpCert ocert coldVK)
          return ()

    (agentOutLines, serviceOutLines, ()) <-
      withAgentAndService controlAddr serviceAddr [] coldVerKeyFile $ do
        makeCert
        controlClientCheck
          [ "install-key"
          , "--opcert-file"
          , opcertFile
          , "--control-address"
          , controlAddr
          ]
          (ExitFailure $ fromEnum RecvErrorNoKey)
          ["Error: No KES key found"]

    assertNoMatchingOutputLines 0 ["KES", "key", "0"] serviceOutLines

kesAgentControlInstallDroppedKey :: Assertion
kesAgentControlInstallDroppedKey =
  withSystemTempDirectory "kes-agent-tests" $ \tmpdir -> do
    let controlAddr = tmpdir </> "control.socket"
        serviceAddr = tmpdir </> "service.socket"
        opcertFile = tmpdir </> "opcert.cert"
        kesKeyFile = tmpdir </> "kes.vkey"
    coldSignKeyFile <- getDataFileName "fixtures/cold.skey"
    coldVerKeyFile <- getDataFileName "fixtures/cold.vkey"

    let makeCert = do
          ColdSignKey coldSK <-
            either error return =<< decodeTextEnvelopeFile @(ColdSignKey (DSIGN StandardCrypto)) coldSignKeyFile
          ColdVerKey coldVK <-
            either error return =<< decodeTextEnvelopeFile @(ColdVerKey (DSIGN StandardCrypto)) coldVerKeyFile
          KESVerKey kesVK <-
            either error return =<< decodeTextEnvelopeFile @(KESVerKey (KES StandardCrypto)) kesKeyFile
          kesPeriod <- getCurrentKESPeriod defEvolutionConfig
          let ocert :: OCert StandardCrypto = makeOCert kesVK 0 kesPeriod coldSK
          encodeTextEnvelopeFile opcertFile (OpCert ocert coldVK)
          return ()

    (agentOutLines, serviceOutLines, ()) <-
      withAgentAndService controlAddr serviceAddr [] coldVerKeyFile $ do
        controlClientCheck
          [ "gen-staged-key"
          , "--kes-verification-key-file"
          , kesKeyFile
          , "--control-address"
          , controlAddr
          ]
          ExitSuccess
          [ "Asking agent to generate a key..."
          , "KES SignKey generated."
          , "KES VerKey written to " ++ kesKeyFile
          ]

        makeCert

        controlClientCheck
          [ "drop-staged-key"
          , "--control-address"
          , controlAddr
          ]
          ExitSuccess
          ["Staged key dropped."]

        controlClientCheck
          [ "install-key"
          , "--opcert-file"
          , opcertFile
          , "--control-address"
          , controlAddr
          ]
          (ExitFailure $ fromEnum RecvErrorNoKey)
          ["Error: No KES key found"]

    assertNoMatchingOutputLines 0 ["KES", "key", "0"] serviceOutLines

kesAgentControlInstallMultiNodes :: Assertion
kesAgentControlInstallMultiNodes =
  withSystemTempDirectory "kes-agent-tests" $ \tmpdir -> do
    let controlAddr = tmpdir </> "control.socket"
        serviceAddr = tmpdir </> "service.socket"
        kesKeyFile = tmpdir </> "kes.vkey"
        opcertFile = tmpdir </> "opcert.cert"
    coldSignKeyFile <- getDataFileName "fixtures/cold.skey"
    coldVerKeyFile <- getDataFileName "fixtures/cold.vkey"

    let makeCert = do
          ColdSignKey coldSK <-
            either error return =<< decodeTextEnvelopeFile @(ColdSignKey (DSIGN StandardCrypto)) coldSignKeyFile
          ColdVerKey coldVK <-
            either error return =<< decodeTextEnvelopeFile @(ColdVerKey (DSIGN StandardCrypto)) coldVerKeyFile
          KESVerKey kesVK <-
            either error return =<< decodeTextEnvelopeFile @(KESVerKey (KES StandardCrypto)) kesKeyFile
          kesPeriod <- getCurrentKESPeriod defEvolutionConfig
          let ocert :: OCert StandardCrypto = makeOCert kesVK 0 kesPeriod coldSK
          encodeTextEnvelopeFile opcertFile (OpCert ocert coldVK)
          return ()

    (agentOutLines, (serviceOutLines1, serviceOutLines2)) <-
      withAgent controlAddr serviceAddr [] coldVerKeyFile $ do
        (serviceOutLines1, ()) <- withService serviceAddr $ do
          -- Little bit of delay here to allow for the version handshake to
          -- finish
          threadDelay 10000
          return ()
        (serviceOutLines2, ()) <- withService serviceAddr $ do
          controlClientCheck
            [ "gen-staged-key"
            , "--kes-verification-key-file"
            , kesKeyFile
            , "--control-address"
            , controlAddr
            ]
            ExitSuccess
            [ "Asking agent to generate a key..."
            , "KES SignKey generated."
            , "KES VerKey written to " <> kesKeyFile
            ]
          makeCert
          controlClientCheck
            [ "install-key"
            , "--opcert-file"
            , opcertFile
            , "--control-address"
            , controlAddr
            ]
            ExitSuccess
            ["KES key installed."]
          -- Little bit of delay to allow the client to read the key.
          threadDelay 10000
        return (serviceOutLines1, serviceOutLines2)

    assertNoMatchingOutputLinesWith
      ("Service 1: NOT 'KES key 0'\n" ++ (Text.unpack . Text.unlines $ agentOutLines))
      0
      ["KES", "key", "0"]
      serviceOutLines1
    assertMatchingOutputLinesWith
      ("Service1: 'ReceivedVersionID'\n" ++ (Text.unpack . Text.unlines $ agentOutLines))
      4
      ["ReceivedVersionID"]
      serviceOutLines1
    assertMatchingOutputLinesWith
      ("Service2: 'KES key 0'\n" ++ (Text.unpack . Text.unlines $ agentOutLines))
      0
      ["KES", "key", "0"]
      serviceOutLines2

kesAgentEvolvesKey :: Assertion
kesAgentEvolvesKey =
  withSystemTempDirectory "kes-agent-tests" $ \tmpdir -> do
    let controlAddr = tmpdir </> "control.socket"
        serviceAddr = tmpdir </> "service.socket"
        kesKeyFile = tmpdir </> "kes.vkey"
        opcertFile = tmpdir </> "opcert.cert"
    coldSignKeyFile <- getDataFileName "fixtures/cold.skey"
    coldVerKeyFile <- getDataFileName "fixtures/cold.vkey"
    currentKesPeriod <- getCurrentKESPeriod defEvolutionConfig

    let makeCert = do
          ColdSignKey coldSK <-
            either error return =<< decodeTextEnvelopeFile @(ColdSignKey (DSIGN StandardCrypto)) coldSignKeyFile
          ColdVerKey coldVK <-
            either error return =<< decodeTextEnvelopeFile @(ColdVerKey (DSIGN StandardCrypto)) coldVerKeyFile
          KESVerKey kesVK <-
            either error return =<< decodeTextEnvelopeFile @(KESVerKey (KES StandardCrypto)) kesKeyFile
          let kesPeriod = KESPeriod $ unKESPeriod currentKesPeriod - 10
          let ocert :: OCert StandardCrypto = makeOCert kesVK 0 kesPeriod coldSK
          encodeTextEnvelopeFile opcertFile (OpCert ocert coldVK)
          return ()

    (agentOutLines, serviceOutLines, ()) <-
      withAgentAndService controlAddr serviceAddr [] coldVerKeyFile $ do
        controlClientCheck
          [ "gen-staged-key"
          , "--kes-verification-key-file"
          , kesKeyFile
          , "--control-address"
          , controlAddr
          ]
          ExitSuccess
          [ "Asking agent to generate a key..."
          , "KES SignKey generated."
          , "KES VerKey written to " <> kesKeyFile
          ]
        makeCert
        controlClientCheck
          [ "install-key"
          , "--opcert-file"
          , opcertFile
          , "--control-address"
          , controlAddr
          ]
          ExitSuccess
          ["KES key installed."]
        controlClientCheckP
          [ "info"
          , "--control-address"
          , controlAddr
          ]
          ExitSuccess
          (any (`elem` ["Current evolution: 10 / 64", "Current evolution: 11 / 64"]))
    assertMatchingOutputLinesWith
      ("SERVICE OUTPUT CHECK\n" <> (Text.unpack . Text.unlines $ agentOutLines))
      0
      ["KES", "key", "0"]
      serviceOutLines

kesAgentPropagate :: Assertion
kesAgentPropagate =
  withSystemTempDirectory "kes-agent-tests" $ \tmpdir -> do
    let controlAddr1 = tmpdir </> "control1.socket"
        serviceAddr1 = tmpdir </> "service1.socket"
        controlAddr2 = tmpdir </> "control2.socket"
        serviceAddr2 = tmpdir </> "service2.socket"
        kesKeyFile = tmpdir </> "kes.vkey"
        opcertFile = tmpdir </> "opcert.cert"
    coldSignKeyFile <- getDataFileName "fixtures/cold.skey"
    coldVerKeyFile <- getDataFileName "fixtures/cold.vkey"
    currentKesPeriod <- getCurrentKESPeriod defEvolutionConfig

    let makeCert = do
          ColdSignKey coldSK <-
            either error return =<< decodeTextEnvelopeFile @(ColdSignKey (DSIGN StandardCrypto)) coldSignKeyFile
          ColdVerKey coldVK <-
            either error return =<< decodeTextEnvelopeFile @(ColdVerKey (DSIGN StandardCrypto)) coldVerKeyFile
          KESVerKey kesVK <-
            either error return =<< decodeTextEnvelopeFile @(KESVerKey (KES StandardCrypto)) kesKeyFile
          let kesPeriod = KESPeriod $ unKESPeriod currentKesPeriod
          let ocert :: OCert StandardCrypto = makeOCert kesVK 0 kesPeriod coldSK
          encodeTextEnvelopeFile opcertFile (OpCert ocert coldVK)
          return ()

    (agentOutLines1, (agentOutLines2, serviceOutLines, ())) <-
      withAgent controlAddr1 serviceAddr1 [serviceAddr2] coldVerKeyFile $ do
        withAgentAndService controlAddr2 serviceAddr2 [serviceAddr1] coldVerKeyFile $ do
          controlClientCheck
            [ "gen-staged-key"
            , "--kes-verification-key-file"
            , kesKeyFile
            , "--control-address"
            , controlAddr1
            ]
            ExitSuccess
            [ "Asking agent to generate a key..."
            , "KES SignKey generated."
            , "KES VerKey written to " <> kesKeyFile
            ]
          makeCert
          controlClientCheck
            [ "install-key"
            , "--opcert-file"
            , opcertFile
            , "--control-address"
            , controlAddr1
            ]
            ExitSuccess
            ["KES key installed."]
          controlClientCheckP
            [ "info"
            , "--control-address"
            , controlAddr2
            ]
            ExitSuccess
            (any (`elem` ["Current evolution: 0 / 64", "Current evolution: 1 / 64"]))
    assertMatchingOutputLinesWith
      ("SERVICE OUTPUT CHECK\n" <> (Text.unpack . Text.unlines $ agentOutLines1))
      0
      ["KES", "key", "0"]
      serviceOutLines

kesAgentSelfHeal1 :: Assertion
kesAgentSelfHeal1 =
  withSystemTempDirectory "kes-agent-tests" $ \tmpdir -> do
    let controlAddr1 = tmpdir </> "control1.socket"
        serviceAddr1 = tmpdir </> "service1.socket"
        controlAddr2 = tmpdir </> "control2.socket"
        serviceAddr2 = tmpdir </> "service2.socket"
        kesKeyFile = tmpdir </> "kes.vkey"
        opcertFile = tmpdir </> "opcert.cert"
    coldSignKeyFile <- getDataFileName "fixtures/cold.skey"
    coldVerKeyFile <- getDataFileName "fixtures/cold.vkey"
    currentKesPeriod <- getCurrentKESPeriod defEvolutionConfig

    let makeCert = do
          ColdSignKey coldSK <-
            either error return =<< decodeTextEnvelopeFile @(ColdSignKey (DSIGN StandardCrypto)) coldSignKeyFile
          ColdVerKey coldVK <-
            either error return =<< decodeTextEnvelopeFile @(ColdVerKey (DSIGN StandardCrypto)) coldVerKeyFile
          KESVerKey kesVK <-
            either error return =<< decodeTextEnvelopeFile @(KESVerKey (KES StandardCrypto)) kesKeyFile
          let kesPeriod = KESPeriod $ unKESPeriod currentKesPeriod
          let ocert :: OCert StandardCrypto = makeOCert kesVK 0 kesPeriod coldSK
          encodeTextEnvelopeFile opcertFile (OpCert ocert coldVK)
          return ()

    (agentOutLines1, (agentOutLines2, ())) <-
      withAgent controlAddr1 serviceAddr1 [serviceAddr2] coldVerKeyFile $ do
        (agentOutLines2a, ()) <- withAgent controlAddr2 serviceAddr2 [serviceAddr1] coldVerKeyFile $ do
          threadDelay 1000000
          controlClientCheck
            [ "gen-staged-key"
            , "--kes-verification-key-file"
            , kesKeyFile
            , "--control-address"
            , controlAddr2
            ]
            ExitSuccess
            [ "Asking agent to generate a key..."
            , "KES SignKey generated."
            , "KES VerKey written to " <> kesKeyFile
            ]
          makeCert
          controlClientCheck
            [ "install-key"
            , "--opcert-file"
            , opcertFile
            , "--control-address"
            , controlAddr2
            ]
            ExitSuccess
            ["KES key installed."]
          controlClientCheckP
            [ "info"
            , "--control-address"
            , controlAddr1
            ]
            ExitSuccess
            (const True)
        controlClientCheckP
          [ "info"
          , "--control-address"
          , controlAddr2
          ]
          (ExitFailure 1)
          (any ("kes-agent-control: Network.Socket.connect: " `isPrefixOf`))
        (agentOutLines2b, ()) <- withAgent controlAddr2 serviceAddr2 [serviceAddr1] coldVerKeyFile $ do
          threadDelay 1000000
          controlClientCheckP
            [ "info"
            , "--control-address"
            , controlAddr2
            , "--retry-delay"
            , "100"
            , "--retry-attempts"
            , "10"
            ]
            ExitSuccess
            (any (`elem` ["Current evolution: 0 / 64", "Current evolution: 1 / 64"]))
        return (agentOutLines2a ++ ["------"] ++ agentOutLines2b, ())
    return ()

kesAgentSelfHeal2 :: Assertion
kesAgentSelfHeal2 =
  withSystemTempDirectory "kes-agent-tests" $ \tmpdir -> do
    let controlAddr1 = tmpdir </> "control1.socket"
        serviceAddr1 = tmpdir </> "service1.socket"
        controlAddr2 = tmpdir </> "control2.socket"
        serviceAddr2 = tmpdir </> "service2.socket"
        kesKeyFile = tmpdir </> "kes.vkey"
        opcertFile = tmpdir </> "opcert.cert"
    coldSignKeyFile <- getDataFileName "fixtures/cold.skey"
    coldVerKeyFile <- getDataFileName "fixtures/cold.vkey"
    currentKesPeriod <- getCurrentKESPeriod defEvolutionConfig

    let makeCert = do
          ColdSignKey coldSK <-
            either error return =<< decodeTextEnvelopeFile @(ColdSignKey (DSIGN StandardCrypto)) coldSignKeyFile
          ColdVerKey coldVK <-
            either error return =<< decodeTextEnvelopeFile @(ColdVerKey (DSIGN StandardCrypto)) coldVerKeyFile
          KESVerKey kesVK <-
            either error return =<< decodeTextEnvelopeFile @(KESVerKey (KES StandardCrypto)) kesKeyFile
          let kesPeriod = KESPeriod $ unKESPeriod currentKesPeriod
          let ocert :: OCert StandardCrypto = makeOCert kesVK 0 kesPeriod coldSK
          encodeTextEnvelopeFile opcertFile (OpCert ocert coldVK)
          return ()

    (agentOutLines1, (agentOutLines2, ())) <-
      withAgent controlAddr1 serviceAddr1 [serviceAddr2] coldVerKeyFile $ do
        (agentOutLines2a, ()) <- withAgent controlAddr2 serviceAddr2 [serviceAddr1] coldVerKeyFile $ do
          threadDelay 1000000
          controlClientCheck
            [ "gen-staged-key"
            , "--kes-verification-key-file"
            , kesKeyFile
            , "--control-address"
            , controlAddr1
            ]
            ExitSuccess
            [ "Asking agent to generate a key..."
            , "KES SignKey generated."
            , "KES VerKey written to " <> kesKeyFile
            ]
          makeCert
          controlClientCheck
            [ "install-key"
            , "--opcert-file"
            , opcertFile
            , "--control-address"
            , controlAddr1
            ]
            ExitSuccess
            ["KES key installed."]
          -- Allow some time for agent to shut down cleanly
          threadDelay 10000
        controlClientCheckP
          [ "info"
          , "--control-address"
          , controlAddr2
          ]
          (ExitFailure 1)
          (any ("kes-agent-control: Network.Socket.connect: " `isPrefixOf`))
        (agentOutLines2b, ()) <- withAgent controlAddr2 serviceAddr2 [serviceAddr1] coldVerKeyFile $ do
          threadDelay 10000
          controlClientCheckP
            [ "info"
            , "--control-address"
            , controlAddr2
            , "--retry-delay"
            , "100"
            , "--retry-attempts"
            , "10"
            ]
            ExitSuccess
            (any (`elem` ["Current evolution: 0 / 64", "Current evolution: 1 / 64"]))
          -- Allow some time for agent to shut down cleanly
          threadDelay 10000
        return (agentOutLines2a ++ ["------"] ++ agentOutLines2b, ())
    return ()

matchOutputLine :: Int -> [Text] -> Text -> Bool
matchOutputLine ignore pattern line =
  pattern `isPrefixOf` drop ignore (Text.words line)

matchOutputLines :: Int -> [Text] -> [Text] -> Bool
matchOutputLines ignore pattern =
  any (matchOutputLine ignore pattern)

matchNoOutputLines :: Int -> [Text] -> [Text] -> Bool
matchNoOutputLines ignore pattern =
  not . any (matchOutputLine ignore pattern)

assertMatchingOutputLines :: Int -> [Text] -> [Text] -> Assertion
assertMatchingOutputLines = assertMatchingOutputLinesWith ""

assertMatchingOutputLinesWith :: String -> Int -> [Text] -> [Text] -> Assertion
assertMatchingOutputLinesWith extraInfo ignore pattern lines =
  assertBool
    (extraInfo ++ (Text.unpack . Text.unlines $ lines))
    (matchOutputLines ignore pattern lines)

assertNoMatchingOutputLines :: Int -> [Text] -> [Text] -> Assertion
assertNoMatchingOutputLines = assertNoMatchingOutputLinesWith ""

assertNoMatchingOutputLinesWith :: String -> Int -> [Text] -> [Text] -> Assertion
assertNoMatchingOutputLinesWith extraInfo ignore pattern lines =
  assertBool
    (extraInfo ++ (Text.unpack . Text.unlines $ lines))
    (matchNoOutputLines ignore pattern lines)

controlClient :: [String] -> IO (ExitCode, String, String)
controlClient args =
  readProcessWithExitCode "kes-agent-control" args ""

controlClientCheck :: [String] -> ExitCode -> [String] -> IO ()
controlClientCheck args expectedExitCode expectedOutput = do
  (exitCode, outStr, errStr) <- controlClient args
  let outLines = lines outStr ++ lines errStr
  let cmd = show args
  assertEqual (cmd ++ "\nCONTROL CLIENT OUTPUT\n" ++ outStr ++ errStr) expectedOutput outLines
  assertEqual ("CONTROL CLIENT EXIT CODE\n" ++ outStr ++ errStr) expectedExitCode exitCode

controlClientCheckP :: [String] -> ExitCode -> ([String] -> Bool) -> IO ()
controlClientCheckP args expectedExitCode outputAsExpected = do
  (exitCode, outStr, errStr) <- controlClient args
  let outLines = lines outStr ++ lines errStr
  let cmd = show args
  assertBool (cmd ++ "\nCONTROL CLIENT OUTPUT\n" ++ outStr ++ errStr) (outputAsExpected outLines)
  assertEqual ("CONTROL CLIENT EXIT CODE\n" ++ outStr ++ errStr) expectedExitCode exitCode

withAgentAndService ::
  FilePath -> FilePath -> [FilePath] -> FilePath -> IO a -> IO ([Text.Text], [Text.Text], a)
withAgentAndService controlAddr serviceAddr bootstrapAddrs coldVerKeyFile action = do
  (agentOutLines, (serviceOutLines, retval)) <-
    withAgent controlAddr serviceAddr bootstrapAddrs coldVerKeyFile $
      withService
        serviceAddr
        action
  return (agentOutLines, serviceOutLines, retval)

withAgent :: FilePath -> FilePath -> [FilePath] -> FilePath -> IO a -> IO ([Text.Text], a)
withAgent controlAddr serviceAddr bootstrapAddrs coldVerKeyFile action =
  withSpawnProcess "kes-agent" args $ \_ (Just hOut) (Just hErr) ph -> go hOut hErr ph
  where
    go hOut hErr ph = do
      result <- (Right <$> action) `catch` handler
      terminateProcess ph
      outT <- Text.lines <$> Text.hGetContents hOut
      errT <- Text.lines <$> Text.hGetContents hErr
      case result of
        Right retval ->
          return (outT, retval)
        Left (HUnitFailure srcLocMay msg) -> do
          let msg' =
                ( Text.unpack . Text.unlines $
                    ["--- AGENT TRACE ---"]
                      ++ outT
                      ++ ["--- AGENT STDERR ---"]
                      ++ errT
                      ++ ["--- END OF AGENT TRACE ---"]
                )
                  ++ msg

          throwIO $ HUnitFailure srcLocMay msg'

    handler :: HUnitFailure -> IO (Either HUnitFailure a)
    handler err = return (Left err)

    args =
      [ "run"
      , "--cold-verification-key"
      , coldVerKeyFile
      , "--service-address"
      , serviceAddr
      , "--control-address"
      , controlAddr
      , "--log-level"
      , "debug"
      ]
        ++ concat [["--bootstrap-address", a] | a <- bootstrapAddrs]

withService :: FilePath -> IO a -> IO ([Text.Text], a)
withService serviceAddr action =
  withSpawnProcess "kes-service-client-demo" args $ \_ (Just hOut) _ ph -> do
    -- The service clients may start up faster than the agent, in which case
    -- the first connection attempt will fail. It will try again 100
    -- milliseconds later, so we wait 110 milliseconds before launching the
    -- payload action.
    threadDelay 110000
    retval <- action
    terminateProcess ph
    outT <- Text.lines <$> Text.hGetContents hOut
    return (outT, retval)
  where
    args = ["--service-address", serviceAddr]

withSpawnProcess ::
  FilePath ->
  [String] ->
  (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a) ->
  IO a
withSpawnProcess cmd args action = do
  let cp' = proc cmd args
      cp =
        cp'
          { std_in = CreatePipe
          , std_out = CreatePipe
          , std_err = CreatePipe
          , new_session = True
          }
  withCreateProcess cp action
