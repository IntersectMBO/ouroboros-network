{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
-- | A @tasty@ command-line option for enabling nightly tests
module Test.Util.TestEnv (
    TestEnv (..)
  , askTestEnv
  , defaultMainWithTestEnv
  , defaultTestEnvConfig
  ) where

import           Data.Proxy (Proxy (..))
import           Options.Applicative (metavar)
import           Test.Tasty
import           Test.Tasty.Ingredients
import           Test.Tasty.Options
import           Test.Tasty.QuickCheck

-- | 'defaultMain' extended with 'iohkTestEnvIngredient'
defaultMainWithTestEnv :: TestEnvConfig -> TestTree -> IO ()
defaultMainWithTestEnv testConfig testTree =
    defaultMainWithIngredients (testEnvIngredient : defaultIngredients) $ withTestEnv testConfig testTree

    where
      testEnvIngredient :: Ingredient
      testEnvIngredient = includingOptions [Option (Proxy :: Proxy TestEnv)]

-- | Set the appropriate options for the test environment
withTestEnv :: TestEnvConfig -> TestTree -> TestTree
withTestEnv TestEnvConfig{..} testTree = askOption $ \case
      Nightly -> localOption (QuickCheckTests nightly) testTree
      CI      -> localOption (QuickCheckTests ci) testTree
      Dev     -> testTree

-- | Query and adjust options for `TestEnv`
askTestEnv :: (TestEnv -> TestTree) -> TestTree
askTestEnv = askOption

-- | Test configurations for test environment
data TestEnvConfig = TestEnvConfig { nightly :: Int, ci :: Int }

-- | Default set of tests for each environment
defaultTestEnvConfig :: TestEnvConfig
defaultTestEnvConfig = TestEnvConfig { nightly = 100000, ci = 10000 }

-- | An 'Option' that indicates the environment in which to run tests.
data TestEnv = Nightly | CI | Dev

safeReadTestEnv :: String -> Maybe TestEnv
safeReadTestEnv "nightly" = Just Nightly
safeReadTestEnv "ci"      = Just CI
safeReadTestEnv "dev"     = Just Dev
safeReadTestEnv _         = Nothing

instance IsOption TestEnv where
  defaultValue = Dev
  parseValue = safeReadTestEnv
  optionName = pure "test-env"
  optionHelp = pure "Enable a test mode. \
      \ The 'dev' env sets the default number of quickcheck tests to 100, \
      \ 'nightly' env sets it to 100_000 quickcheck tests, and \
      \ 'ci' env sets it to 10_000 quickcheck tests. \
      \ Individual tests are adjusted to run a number of tests proportional to the value above depending \
      \ on the time it takes to run them."

  -- Set of choices for test environment
  optionCLParser = mkOptionCLParser $ metavar "nightly|ci|dev"
