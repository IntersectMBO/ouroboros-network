module Test.Ouroboros.Network.Testing.Data.AbsBearerInfo (tests) where

import qualified Data.List.NonEmpty as NonEmpty

import           Ouroboros.Network.Testing.Data.AbsBearerInfo (AbsBearerInfo,
                     AbsBearerInfoScript (..),
                     NonFailingAbsBearerInfoScript (..), canFail)
import           Ouroboros.Network.Testing.Data.Script (Script (Script))

import           Test.QuickCheck (Arbitrary (..), Fixed (..))
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
    testGroup "Ouroboros.Testing.Data.AbsBearerInfo"
    [ testGroup "generators"
      [ testProperty "shrinker AbsBearerInfo" prop_shrinker_AbsBearerInfo
      , testProperty "shrinker BearerInfoScript" prop_shrinker_BearerInfoScript
      , testProperty "generator NonFailingBearerInfoScript"
          prop_generator_NonFailingBeararInfoScript
      ]
    ]

prop_shrinker_AbsBearerInfo :: Fixed AbsBearerInfo -> Bool
prop_shrinker_AbsBearerInfo (Fixed abi) =
    abi `notElem` shrink abi

prop_shrinker_BearerInfoScript :: Fixed AbsBearerInfoScript -> Bool
prop_shrinker_BearerInfoScript (Fixed bis) =
    all (\bis'@(AbsBearerInfoScript (Script s)) ->
                  bis /= bis'
               && not (canFail (NonEmpty.last s))
        )
        (shrink bis)

prop_generator_NonFailingBeararInfoScript :: NonFailingAbsBearerInfoScript
                                          -> Bool
prop_generator_NonFailingBeararInfoScript (NonFailingAbsBearerInfoScript s) =
  not (any canFail s)
