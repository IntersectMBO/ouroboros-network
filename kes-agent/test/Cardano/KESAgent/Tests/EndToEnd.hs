{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.KESAgent.Tests.EndToEnd
  ( tests )
  where
import Cardano.KESAgent.Serialization.TextEnvelope
import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.Protocols.Service.Protocol
import Cardano.KESAgent.Protocols.StandardCrypto
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Serialization.CBOR
import Cardano.KESAgent.KES.Evolution ( getCurrentKESPeriod )
import Paths_kes_agent

import Cardano.Crypto.KES.Class
import Cardano.Crypto.DSIGN.Class

import Control.Concurrent.Async
import Control.Concurrent.Class.MonadMVar
import Control.Monad (replicateM, forever)
import Control.Monad.Class.MonadTimer (threadDelay)
import Control.Monad.Class.MonadThrow (catch, finally, bracket, SomeException)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Test.Tasty
import Test.Tasty.HUnit
import System.Process
import System.Exit ( ExitCode (..) )
import System.Environment ( lookupEnv, setEnv, unsetEnv )
import System.IO
import System.IO.Temp
import System.FilePath ( (</>) )

defGenesisTimestamp :: Integer
defGenesisTimestamp = 1506203091 -- real-world genesis on the production ledger

tests :: TestTree
tests =
  testGroup "end to end"
    [ testCase "kes-agent --help" kesAgentHelp
    , testCase "kes-agent-control --help" kesAgentControlHelp
    , testGroup "kes-agent-control install-key"
      [ testCase "valid" kesAgentControlInstallValid
      , testCase "invalid opcert" kesAgentControlInstallInvalidOpCert
      , testCase "no key" kesAgentControlInstallNoKey
      , testCase "dropped key" kesAgentControlInstallDroppedKey
      ]
    ]

kesAgentHelp :: Assertion
kesAgentHelp = do
  (exitCode, stdout, stderr) <- readProcessWithExitCode "kes-agent" [ "--help" ] ""
  assertEqual (stdout ++ "\n" ++ stderr) ExitSuccess exitCode

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
          ColdSignKey coldSK <- either error return =<< decodeTextEnvelopeFile @(ColdSignKey (DSIGN StandardCrypto)) coldSignKeyFile
          ColdVerKey coldVK <- either error return =<< decodeTextEnvelopeFile @(ColdVerKey (DSIGN StandardCrypto)) coldVerKeyFile
          KESVerKey kesVK <- either error return =<< decodeTextEnvelopeFile @(KESVerKey (KES StandardCrypto)) kesKeyFile
          kesPeriod <- getCurrentKESPeriod defGenesisTimestamp
          let ocert :: OCert StandardCrypto = makeOCert kesVK 0 kesPeriod coldSK
          encodeTextEnvelopeFile opcertFile (OpCert ocert coldVK)
          return ()

    withAgent controlAddr serviceAddr coldVerKeyFile $ \_ _ _ agentPH -> do
      withService serviceAddr $ \_ (Just serviceOut) _ servicePH -> do
        (e1, out1, err1) <- controlClient [ "gen-staged-key", "--kes-verification-key-file", kesKeyFile, "--control-address", controlAddr ]
        assertEqual (err1 ++ out1) ExitSuccess e1
        let outLines = lines out1
        assertEqual out1
          [ "Asking agent to generate a key..."
          , "KES SignKey generated."
          , "KES VerKey written to " ++ kesKeyFile
          ]
          outLines
        makeCert
        (e2, out2, err2) <- controlClient [ "install-key", "--opcert-file", opcertFile, "--control-address", controlAddr ]
        let outLines = lines out2
        assertEqual out2
          [ "KES key installed."
          ]
          outLines
        assertEqual (err2 ++ out2) ExitSuccess e2
        terminateProcess servicePH
        serviceOutLines <- Text.lines <$> Text.hGetContents serviceOut
        let interestingLines = filter ("KES key 0 " `Text.isPrefixOf`) serviceOutLines
        assertBool (Text.unpack . Text.unlines $ serviceOutLines) (not . null $ interestingLines)

kesAgentControlInstallInvalidOpCert :: Assertion
kesAgentControlInstallInvalidOpCert =
  withSystemTempDirectory "kes-agent-tests" $ \tmpdir -> do
    let controlAddr = tmpdir </> "control.socket"
        serviceAddr = tmpdir </> "service.socket"
        kesKeyFile = tmpdir </> "kes.vkey"
    opcertFile <- getDataFileName "fixtures/opcert.cert"
    coldSignKeyFile <- getDataFileName "fixtures/cold.skey"
    coldVerKeyFile <- getDataFileName "fixtures/cold.vkey"

    withAgent controlAddr serviceAddr coldVerKeyFile $ \_ (Just agentOut) _ agentPH -> do
      withService serviceAddr $ \_ (Just serviceOut) _ servicePH -> do
        (e1, out1, err1) <- controlClient [ "gen-staged-key", "--kes-verification-key-file", kesKeyFile, "--control-address", controlAddr ]
        assertEqual (err1 ++ out1) ExitSuccess e1
        let outLines = lines out1
        assertEqual out1
          [ "Asking agent to generate a key..."
          , "KES SignKey generated."
          , "KES VerKey written to " ++ kesKeyFile
          ]
          outLines
        (e2, out2, err2) <- controlClient [ "install-key", "--opcert-file", opcertFile, "--control-address", controlAddr ]
        assertEqual (err2 ++ out2) (ExitFailure (fromEnum RecvErrorInvalidOpCert)) e2
        let outLines = lines out2
        assertEqual out2
          [ "Error: OpCert validation failed"
          ]
          outLines
        terminateProcess servicePH
        serviceOutLines <- Text.lines <$> Text.hGetContents serviceOut
        let interestingLines = filter ("KES key 0 " `Text.isPrefixOf`) serviceOutLines
        assertBool (Text.unpack . Text.unlines $ serviceOutLines) (null interestingLines)
      terminateProcess agentPH
      agentOutLines <- Text.lines <$> Text.hGetContents agentOut
      let interestingLines =
            [ l | l <- agentOutLines, let (_ : "Warning" : "Agent:" : "RejectingKey" : _) = Text.words l ]
      assertBool (Text.unpack . Text.unlines $ agentOutLines) (not . null $ interestingLines)

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
          ColdSignKey coldSK <- either error return =<< decodeTextEnvelopeFile @(ColdSignKey (DSIGN StandardCrypto)) coldSignKeyFile
          ColdVerKey coldVK <- either error return =<< decodeTextEnvelopeFile @(ColdVerKey (DSIGN StandardCrypto)) coldVerKeyFile
          KESVerKey kesVK <- either error return =<< decodeTextEnvelopeFile @(KESVerKey (KES StandardCrypto)) kesKeyFile
          kesPeriod <- getCurrentKESPeriod defGenesisTimestamp
          let ocert :: OCert StandardCrypto = makeOCert kesVK 0 kesPeriod coldSK
          encodeTextEnvelopeFile opcertFile (OpCert ocert coldVK)
          return ()

    withAgent controlAddr serviceAddr coldVerKeyFile $ \_ (Just agentOut) _ agentPH -> do
      withService serviceAddr $ \_ (Just serviceOut) _ servicePH -> do
        makeCert
        (e2, out2, err2) <- controlClient [ "install-key", "--opcert-file", opcertFile, "--control-address", controlAddr ]
        assertEqual (err2 ++ out2) (ExitFailure (fromEnum RecvErrorNoKey)) e2
        let outLines = lines out2
        assertEqual out2
          [ "Error: No KES key found"
          ]
          outLines
        terminateProcess servicePH
        serviceOutLines <- Text.lines <$> Text.hGetContents serviceOut
        let interestingLines = filter ("KES key 0 " `Text.isPrefixOf`) serviceOutLines
        assertBool (Text.unpack . Text.unlines $ serviceOutLines) (null interestingLines)
      terminateProcess agentPH
      agentOutLines <- Text.lines <$> Text.hGetContents agentOut
      let interestingLines =
            [ l | l <- agentOutLines, let (_ : "Warning" : "Agent:" : "RejectingKey" : _) = Text.words l ]
      assertBool (Text.unpack . Text.unlines $ agentOutLines) (not . null $ interestingLines)

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
          ColdSignKey coldSK <- either error return =<< decodeTextEnvelopeFile @(ColdSignKey (DSIGN StandardCrypto)) coldSignKeyFile
          ColdVerKey coldVK <- either error return =<< decodeTextEnvelopeFile @(ColdVerKey (DSIGN StandardCrypto)) coldVerKeyFile
          KESVerKey kesVK <- either error return =<< decodeTextEnvelopeFile @(KESVerKey (KES StandardCrypto)) kesKeyFile
          kesPeriod <- getCurrentKESPeriod defGenesisTimestamp
          let ocert :: OCert StandardCrypto = makeOCert kesVK 0 kesPeriod coldSK
          encodeTextEnvelopeFile opcertFile (OpCert ocert coldVK)
          return ()

    withAgent controlAddr serviceAddr coldVerKeyFile $ \_ (Just agentOut) _ agentPH -> do
      withService serviceAddr $ \_ (Just serviceOut) _ servicePH -> do
        (e1, out1, err1) <- controlClient [ "gen-staged-key", "--kes-verification-key-file", kesKeyFile, "--control-address", controlAddr ]
        assertEqual (err1 ++ out1) ExitSuccess e1
        let outLines = lines out1
        assertEqual out1
          [ "Asking agent to generate a key..."
          , "KES SignKey generated."
          , "KES VerKey written to " ++ kesKeyFile
          ]
          outLines

        makeCert

        (e2, out2, err2) <- controlClient [ "drop-staged-key", "--control-address", controlAddr ]
        assertEqual (err2 ++ out2) ExitSuccess e2
        let outLines = lines out2
        assertEqual out2
          [ "Staged key dropped." ]
          outLines

        (e3, out3, err3) <- controlClient [ "install-key", "--opcert-file", opcertFile, "--control-address", controlAddr ]
        assertEqual (err3 ++ out3) (ExitFailure (fromEnum RecvErrorNoKey)) e3
        let outLines = lines out3
        assertEqual out3
          [ "Error: No KES key found"
          ]
          outLines
        terminateProcess servicePH
        serviceOutLines <- Text.lines <$> Text.hGetContents serviceOut
        let interestingLines = filter ("KES key 0 " `Text.isPrefixOf`) serviceOutLines
        assertBool (Text.unpack . Text.unlines $ serviceOutLines) (null interestingLines)
      terminateProcess agentPH
      agentOutLines <- Text.lines <$> Text.hGetContents agentOut
      let interestingLines =
            [ l | l <- agentOutLines, let (_ : "Warning" : "Agent:" : "RejectingKey" : _) = Text.words l ]
      assertBool (Text.unpack . Text.unlines $ agentOutLines) (not . null $ interestingLines)


controlClient :: [String] -> IO (ExitCode, String, String)
controlClient args =
  readProcessWithExitCode "kes-agent-control" args ""

withAgent :: FilePath -> FilePath -> FilePath -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a) -> IO a
withAgent controlAddr serviceAddr coldVerKeyFile =
  withSpawnProcess "kes-agent"
              [ "run"
              , "--cold-verification-key", coldVerKeyFile
              , "--service-address", serviceAddr
              , "--control-address", controlAddr
              ]

withService :: FilePath -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a) -> IO a
withService serviceAddr =
  withSpawnProcess "kes-service-client-demo"
              [ "--service-address", serviceAddr
              ]

withSpawnProcess :: FilePath -> [String] -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a) -> IO a
withSpawnProcess cmd args action = do
  let cp' = proc cmd args
      cp = cp'
            { std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = Inherit
            , new_session = True
            }
  withCreateProcess cp action

