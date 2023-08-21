module Cardano.KESAgent.Tests.EndToEnd
  ( tests )
  where

import Paths_kes_agent

import Control.Concurrent.Async
import Control.Monad.Class.MonadThrow (finally)
import Test.Tasty
import Test.Tasty.HUnit
import System.Process
import System.Exit ( ExitCode (..) )
import System.Environment ( lookupEnv, setEnv, unsetEnv )
import System.IO

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

kesAgentControlNewKey :: Assertion
kesAgentControlNewKey = withRunningAgent $ do
  opcertFile <- getDataFileName "fixtures/opcert.cert"
  let cp' = proc "kes-agent-control" [ "new-key", "--opcert-file", opcertFile ]
      cp = cp'
            { std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = CreatePipe
            }
  (Just hIn, Just hOut, Just hErr, ph) <- createProcess cp
  (_, (err, out)) <-
    hPutStrLn hIn "" `concurrently` (hGetContents hErr `concurrently` hGetContents hOut)

  assertEqual "out"
    [ "KES VerKey written to kes.vkey"
    , "OpCert will be read from " ++ opcertFile
    ]
    (lines out)
  assertEqual "err"
    [ "Press ENTER to continue..."
    ]
    (lines err)

withRunningAgent :: IO a -> IO a
withRunningAgent action = do
  agentProcess <- spawnProcess "kes-agent" [ "run" ]
  action `finally` terminateProcess agentProcess
