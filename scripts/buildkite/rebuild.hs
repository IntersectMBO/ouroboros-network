{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception
import Control.Monad.Trans.Maybe
import qualified Data.Text as T
import Safe
import System.Exit (exitWith)
import Turtle


-- | Run build and upload coverage information when successful
main :: IO ()
main = do
  buildResult <- buildStep Nothing

  when (buildResult == ExitSuccess) coverageUploadStep

  exitWith buildResult


-- | Build and test all packages using stack
buildStep :: Maybe [Text] -> IO ExitCode
buildStep testArgs = do
  echo "+++ Build and test"
  run "stack" $ cfg ++ ["build", "--fast"] ++ buildArgs
 where
  cfg = ["--dump-logs", "--color", "always"]
  -- No Haddock here due to https://github.com/commercialhaskell/stack/issues/4838
  buildArgs =
    [ "--bench"
      , "--no-run-benchmarks"
      , "--test"
      , "--skip", "test-Win32-network"
      , "--skip", "test-cddl"
      , "--coverage"
      ]
      ++ maybe [] ("--ta" :) testArgs


-- | Upload coverage information to coveralls
coverageUploadStep :: IO ()
coverageUploadStep = do
  echo "--- Uploading Coverage Information"
  need "OUROBOROS_NETWORK_COVERALLS_REPO_TOKEN" >>= \case
    Nothing -> printf
      "Missing coverall repo token. Not uploading coverage information.\n"
    Just repoToken -> do
      result <- proc
        "shc"
        [ "--repo-token"
        , repoToken
        , "--exclude-dirs"
        , "crypto/test"
        , "combined"
        , "all"
        ]
        empty
      case result of
        ExitSuccess   -> printf "Coverage information upload successful.\n"
        ExitFailure _ -> printf "Coverage information upload failed.\n"


run :: Text -> [Text] -> IO ExitCode
run cmd args = do
  printf (s % " " % s % "\n") cmd (T.unwords args)
  res <- proc cmd args empty
  case res of
    ExitSuccess      -> pure ()
    ExitFailure code -> eprintf
      ("error: Command exited with code " % d % "!\nContinuing...\n")
      code
  pure res
