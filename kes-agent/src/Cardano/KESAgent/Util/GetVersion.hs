{-# LANGUAGE TypeApplications #-}

module Cardano.KESAgent.Util.GetVersion
where

import Control.Exception (throwIO, try)
import Data.List (intercalate, isPrefixOf)
import Data.Maybe
import Data.Version
import Paths_kes_agent (version)
import System.Exit
import System.IO.Error (isDoesNotExistError)
import System.Process

checkGit :: IO Bool
checkGit = do
  r <- try $ readProcessWithExitCode "git" ["status"] ""
  case r of
    Right (exitCode, _, _) ->
      return (exitCode == ExitSuccess)
    Left e
      | isDoesNotExistError e ->
          do
            print e
            return False
      | otherwise ->
          throwIO e

git :: [String] -> IO String
git args = readProcess "git" args ""

getTimestamp :: IO String
getTimestamp = filter (/= '\n') <$> readProcess "date" ["+%Y%m%d%H%M%S"] ""

emptyToNothing :: [a] -> Maybe [a]
emptyToNothing [] = Nothing
emptyToNothing xs = Just xs

-- TODO: we should use `cardano-git-revision` and only as a dependency of the
-- binary, not the library.  Furthermore consider using the same approach as
-- `nix/set-git-rev.nix` in `cardano-node` repo.
getProgramVersion :: IO String
getProgramVersion = do
  let baseVersion = showVersion version

  isGit <- checkGit

  components <-
    if isGit
      then do
        gitTag <- filter (/= '\n') <$> git ["tag", "--points-at"]
        gitCommit <- filter (/= '\n') <$> git ["log", "-n1", "--format=%H"]
        if ("v" ++ baseVersion) `isPrefixOf` gitTag
          then
            return
              [ Just (drop 1 gitTag)
              ]
          else
            return
              [ Just baseVersion
              , Just "git"
              , emptyToNothing gitTag
              , emptyToNothing gitCommit
              ]
      else do
        r <- try @IOError getTimestamp
        case r of
          Right timestamp ->
            return
              [ Just baseVersion
              , Just "dev"
              , Just timestamp
              ]
          Left e -> do
            print e
            return
              [ Just baseVersion
              , Just "dev"
              ]

  return . intercalate "-" . catMaybes $ components
