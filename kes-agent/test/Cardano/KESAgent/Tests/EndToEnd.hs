module Cardano.KESAgent.Tests.EndToEnd
  ( tests )
  where

import Paths_kes_agent

import Control.Concurrent.Async
import Control.Monad (replicateM)
import Control.Monad.Class.MonadTimer (threadDelay)
import Control.Monad.Class.MonadThrow (finally, bracket)
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

tests :: TestTree
tests =
  testGroup "end to end"
    [ testCase "kes-agent --help" kesAgentHelp
    , testCase "kes-agent-control --help" kesAgentControlHelp
    , testCase "kes-agent-control new-key" kesAgentControlNewKey
    ]

kesAgentHelp :: Assertion
kesAgentHelp = do
  (exitCode, stdout, stderr) <- readProcessWithExitCode "kes-agent" [ "--help" ] ""
  assertEqual (stdout ++ "\n" ++ stderr) ExitSuccess exitCode

kesAgentControlHelp :: Assertion
kesAgentControlHelp = do
  (exitCode, stdout, stderr) <- readProcessWithExitCode "kes-agent-control" [ "--help" ] ""
  assertEqual (stdout ++ "\n" ++ stderr) ExitSuccess exitCode

-- | This test starts an actual KES agent process, a simulated Node process
-- (@kes-service-client-demo@), and a KES control client, runs the control
-- client through the new-key procedure, and checks that the service client
-- reports a received key.
-- TODO: the opcert is currently loaded from a fixture, which means the KES
-- period encoded in it will soon be outdated, which will cause the test to
-- fail, because the agent will reject the cert instead of forwarding it.
kesAgentControlNewKey :: Assertion
kesAgentControlNewKey = 
  withSystemTempDirectory "kes-agent-tests" $ \tmpdir -> do
    let serviceLogFile = tmpdir </> "service.log"
    let kesKeyFile = tmpdir </> "kes.vkey"
    opcertFile <- getDataFileName "fixtures/opcert.cert"
    controlOutStr <- withRunningAgent $
      withServiceDemo serviceLogFile $ \servicePH -> do
        withControlClient opcertFile kesKeyFile $ \(Just controlIn) (Just controlOut) _ controlPH -> do
          hPutStrLn controlIn ""
          line1 <- Text.hGetLine controlOut
          Text.putStrLn line1
          line2 <- Text.hGetLine controlOut
          Text.putStrLn line2
          line3 <- Text.hGetLine controlOut
          Text.putStrLn line3
          let controlOutStr = Text.unpack $ Text.unlines [line1, line2, line3]
          waitForProcess controlPH
          threadDelay 100000
          return controlOutStr

    serviceOutStr <- Text.unpack <$> Text.readFile serviceLogFile
    let serviceOutLines = lines serviceOutStr

    assertEqual "control out"
      [ "KES VerKey written to " ++ kesKeyFile
      , "OpCert will be read from " ++ opcertFile
      , "Press ENTER to continue..."
      ]
      (lines controlOutStr)

    let keyReceivedLines = [ l | l <- serviceOutLines, "ReceivedKey" `elem` words l ]

    assertBool serviceOutStr (not . null $ keyReceivedLines)


withRunningAgent :: IO a -> IO a
withRunningAgent action = do
  coldVerKeyFile <- getDataFileName "fixtures/cold.vkey"
  bracket
    (spawnProcess "kes-agent"
                    [ "run"
                    , "--cold-verification-key", coldVerKeyFile
                    ])
    terminateProcess
    (const action)

withControlClient :: FilePath -> FilePath -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a) -> IO a
withControlClient opcertFile kesKeyFile action = do
  let cp' = proc "kes-agent-control"
              [ "new-key"
              , "--opcert-file", opcertFile
              , "--kes-verification-key-file", kesKeyFile
              , "--control-address", "/tmp/kes-agent-control.socket"
              ]
      cp = cp'
            { std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = Inherit
            }
  withCreateProcess cp action
      

spawnProcessDuplex :: String -> [String] -> IO (Handle, Handle, ProcessHandle)
spawnProcessDuplex cmd args = do
  (readIn, writeIn) <- createPipe
  (readOut, writeOut) <- createPipe
  hSetBuffering writeOut NoBuffering
  let cp' = proc cmd args
      cp = cp'
            { std_in = UseHandle readIn
            , std_out = UseHandle writeOut
            , std_err = UseHandle writeOut
            }
  (_, _, _, ph) <- createProcess cp
  return (writeIn, readOut, ph)

spawnProcessToFile :: String -> [String] -> FilePath -> IO ProcessHandle
spawnProcessToFile cmd args outPath = do
  withFile outPath WriteMode $ \hOut ->  do
    let cp' = proc cmd args
        cp = cp'
              { std_in = Inherit
              , std_out = UseHandle hOut
              , std_err = UseHandle hOut
              }
    (_, _, _, ph) <- createProcess cp
    return ph

withServiceDemo :: FilePath -> (ProcessHandle -> IO a) -> IO a
withServiceDemo logPath =
  bracket
    create
    terminateProcess
  where
    create :: IO ProcessHandle
    create = spawnProcessToFile "kes-service-client-demo" [] logPath
