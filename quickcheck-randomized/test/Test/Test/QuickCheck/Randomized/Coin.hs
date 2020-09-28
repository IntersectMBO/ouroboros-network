module Test.Test.QuickCheck.Randomized.Coin (tests) where

import           Test.Tasty

import qualified Test.Test.QuickCheck.Randomized.Coin.Direct (tests)
import qualified Test.Test.QuickCheck.Randomized.Coin.Meta (tests)

tests :: TestTree
tests = testGroup "Coin" $
    [ Test.Test.QuickCheck.Randomized.Coin.Direct.tests
    , Test.Test.QuickCheck.Randomized.Coin.Meta.tests
    ]
