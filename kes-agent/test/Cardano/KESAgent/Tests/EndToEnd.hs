{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.KESAgent.Tests.EndToEnd
  ( tests )
  where
import Cardano.KESAgent.TextEnvelope
import Cardano.KESAgent.OCert
import Cardano.KESAgent.Protocol
import Cardano.KESAgent.Serialization
import Cardano.KESAgent.Evolution ( getCurrentKESPeriod )
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
    , testGroup "kes-agent-control new-key"
      [ testCase "valid" kesAgentControlNewKeyValid
      , testCase "invalid opcert" kesAgentControlNewKeyInvalidOpCert
      ]
    ]

kesAgentHelp :: Assertion
kesAgentHelp = do
  (exitCode, stdout, stderr) <- readProcessWithExitCode "kes-agent" [ "--help" ] ""
  assertEqual (stdout ++ "\n" ++ stderr) ExitSuccess exitCode

kesAgentControlHelp :: Assertion
kesAgentControlHelp = do
  (exitCode, stdout, stderr) <- readProcessWithExitCode "kes-agent-control" [ "--help" ] ""
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


kesAgentControlNewKeyValid :: Assertion
kesAgentControlNewKeyValid = 
  withSystemTempDirectory "kes-agent-tests" $ \tmpdir -> do
    let kesKeyFile = tmpdir </> "kes.vkey"
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

    (agentOutLines, serviceOutLines, controlOutLines) <-
      runNetwork tmpdir kesKeyFile opcertFile coldVerKeyFile makeCert

    let log =
          "--- AGENT ---\n" ++
          unlines agentOutLines ++
          "--- NODE ---\n" ++
          unlines serviceOutLines ++
          "--- CONTROL ---\n" ++
          unlines controlOutLines

    assertEqual log
      [ "KES VerKey written to " ++ kesKeyFile
      , "OpCert will be read from " ++ opcertFile
      , "Press ENTER to continue..."
      , "Key accepted."
      ]
      controlOutLines

kesAgentControlNewKeyInvalidOpCert :: Assertion
kesAgentControlNewKeyInvalidOpCert = 
  withSystemTempDirectory "kes-agent-tests" $ \tmpdir -> do
    let kesKeyFile = tmpdir </> "kes.vkey"
    opcertFile <- getDataFileName "fixtures/opcert.cert"
    coldVerKeyFile <- getDataFileName "fixtures/cold.vkey"

    (agentOutLines, serviceOutLines, controlOutLines) <-
      runNetwork tmpdir kesKeyFile opcertFile coldVerKeyFile (return ())

    let log =
          "--- AGENT ---\n" ++
          unlines agentOutLines ++
          "--- NODE ---\n" ++
          unlines serviceOutLines ++
          "--- CONTROL ---\n" ++
          unlines controlOutLines

    assertEqual log
      [ "KES VerKey written to " ++ kesKeyFile
      , "OpCert will be read from " ++ opcertFile
      , "Press ENTER to continue..."
      , "Key rejected: OpCert validation failed"
      ]
      controlOutLines

runNetwork :: FilePath -> FilePath -> FilePath -> FilePath -> IO () -> IO ([String], [String], [String])
runNetwork tmpdir kesKeyFile opcertFile coldVerKeyFile makeCert = do
  let controlAddr = tmpdir </> "control.socket"
      serviceAddr = tmpdir </> "service.socket"
  (agentOutLines, (serviceOutLines, controlOutLines)) <- withAgent controlAddr serviceAddr coldVerKeyFile $ \_ (Just agentOut) _ agentPH -> do
    concurrently (logHandle agentOut) $ do
      withService serviceAddr $ \_ (Just serviceOut) _ servicePH -> do
        concurrently (logHandle serviceOut) $ do
          withControlClient controlAddr opcertFile kesKeyFile $ \(Just controlIn) (Just controlOut) _ controlPH -> do
            hSetBuffering controlIn NoBuffering
            hSetBuffering controlOut NoBuffering

            line1 <- hGetLine controlOut
            line2 <- hGetLine controlOut
            line3 <- hGetLine controlOut

            makeCert

            hPutStrLn controlIn ""
            hFlush controlIn

            line4 <- either id (const "*** timeout ***") <$>
                        race (hGetLine controlOut) (threadDelay 100000 >> terminateProcess controlPH)
            waitForProcess controlPH
            terminateProcess servicePH
            terminateProcess agentPH

            return [line1, line2, line3, line4]
  return (agentOutLines, serviceOutLines, controlOutLines)


withControlClient :: FilePath -> FilePath -> FilePath -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a) -> IO a
withControlClient controlAddr opcertFile kesKeyFile =
  withSpawnProcess "kes-agent-control"
              [ "new-key"
              , "--opcert-file", opcertFile
              , "--kes-verification-key-file", kesKeyFile
              , "--control-address", controlAddr
              ]

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

