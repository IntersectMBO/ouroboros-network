module Cardano.KESAgent.Tests.EndToEnd
  ( tests )
  where

import Test.Tasty
import Test.Tasty.HUnit
import System.Process
import System.Exit ( ExitCode (..) )
import System.Environment ( lookupEnv, setEnv, unsetEnv )

tests :: TestTree
tests =
  testGroup "end to end"
    [ testCase "kes-agent --help" kesAgentHelp
    ]

kesAgentHelp :: Assertion
kesAgentHelp = do
  kesAgentPath <- readProcess "which" [ "kes-agent" ] ""
  print kesAgentPath
  (exitCode, stdout, stderr) <- readProcessWithExitCode "kes-agent" [ "--help" ] ""
  assertEqual (stdout ++ "\n" ++ stderr) ExitSuccess exitCode
