module Cardano.KESAgent.Util.GetVersion
where

import System.Process
import System.Exit
import Paths_kes_agent (version)
import Data.Version
import Data.Maybe
import Data.List (intercalate, isPrefixOf)

checkGit :: IO Bool
checkGit = do
  (exitCode, _, _) <- readProcessWithExitCode "git" ["status"] ""
  return (exitCode == ExitSuccess)

git :: [String] -> IO String
git args = readProcess "git" args ""

getTimestamp :: IO String
getTimestamp = filter (/= '\n') <$> readProcess "date" ["+%Y%m%d%H%M%S"] ""

emptyToNothing :: [a] -> Maybe [a]
emptyToNothing [] = Nothing
emptyToNothing xs = Just xs

getProgramVersion :: IO String
getProgramVersion = do
  let baseVersion = showVersion version

  isGit <- checkGit
  
  components <- if isGit then do
    gitTag <- filter (/= '\n') <$> git ["tag", "--points-at"]
    gitCommit <- filter (/= '\n') <$> git ["log", "-n1", "--format=%H"]
    if ("v" ++ baseVersion) `isPrefixOf` gitTag then
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
    timestamp <- getTimestamp
    return
      [ Just baseVersion
      , Just "dev"
      , Just timestamp
      ]
  return . intercalate "-" . catMaybes $ components
