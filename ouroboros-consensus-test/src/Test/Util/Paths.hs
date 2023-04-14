-- | Utility function for finding a directory relative to the package root
-- directory.
--
-- Copied from
-- <https://github.com/input-output-hk/cardano-wallet/blob/master/lib/test-utils/src/Test/Utils/Paths.hs>
module Test.Util.Paths (
    getGoldenDir
  , getRelPath
  , inNixBuild
  ) where


import           Control.Monad.IO.Class (liftIO)
import           Data.FileEmbed (makeRelativeToProject)
import           Language.Haskell.TH.Syntax (Exp, Q, liftData)
import           System.Environment (lookupEnv)

-- | A TH function to get the path corresponding to the golden output
-- directory relative to the package root directory.
getGoldenDir :: Q Exp
getGoldenDir = getRelPath "golden"

-- | A TH function to get the path corresponding to the given 'FilePath' which
-- is relative to the package root directory.
--
-- It combines the current source file location and cabal file to locate the
-- package directory in such a way that works in both the stack/cabal package
-- build and ghci.
--
-- For the Nix build, rather than baking in a path that starts with @/build@, it
-- makes the test data path relative to the current directory.
getRelPath :: FilePath -> Q Exp
getRelPath relPath = do
    absPath <- makeRelativeToProject relPath
    useRel <- liftIO inNixBuild
    liftData (if useRel then relPath else absPath)

-- | Infer from environment variables whether we are running within a Nix build
-- (and not just a nix-shell).
inNixBuild :: IO Bool
inNixBuild = do
    let testEnv = fmap (maybe False (not . null)) . lookupEnv
    haveNixBuildDir <- testEnv "NIX_BUILD_TOP"
    inNixShell <- testEnv "IN_NIX_SHELL"
    pure (haveNixBuildDir && not inNixShell)
