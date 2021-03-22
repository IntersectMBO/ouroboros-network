-- | A @tasty@ command-line option for enabling nightly tests
module Test.Util.Nightly (
    IohkNightlyEnabled (..)
  , askIohkNightlyEnabled
  , defaultMainWithIohkNightly
  , iohkNightlyIngredient
  ) where

import           Data.Proxy (Proxy (..))
import           Test.Tasty
import           Test.Tasty.Ingredients
import           Test.Tasty.Options

-- | 'defaultMain' extended with 'iohkNightlyIngredient'
defaultMainWithIohkNightly :: TestTree -> IO ()
defaultMainWithIohkNightly =
    defaultMainWithIngredients (iohkNightlyIngredient : defaultIngredients)

-- | This ingredient merely adds the 'IohkNightlyEnabled' 'Option' to the
-- @tasty@ command-line parser.
iohkNightlyIngredient :: Ingredient
iohkNightlyIngredient =
    TestManager [Option (Proxy :: Proxy IohkNightlyEnabled)] $
    \_optionSet _testTree -> Nothing

-- | Query if the 'IohkNightlyEnabled' 'Option' is enabled
askIohkNightlyEnabled :: (Bool -> TestTree) -> TestTree
askIohkNightlyEnabled f = askOption $ \(IohkNightlyEnabled b) -> f b

-- | An 'Option' that indicates the test suite should run more tests, run
-- longer tests, etc
newtype IohkNightlyEnabled = IohkNightlyEnabled Bool

instance IsOption IohkNightlyEnabled where
  defaultValue = IohkNightlyEnabled False
  parseValue = fmap IohkNightlyEnabled . safeReadBool
  optionName = pure "iohk-enable-nightly-tests"
  optionHelp = pure "Enable more expensive tests (specific to IOHK)"

  -- Use typical Un*x syntax for Boolean flags
  optionCLParser = flagCLParser Nothing (IohkNightlyEnabled True)
