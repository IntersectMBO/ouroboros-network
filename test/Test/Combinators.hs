{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
module Test.Combinators (
    tests
    ) where

import Control.Monad.State

import Ouroboros
import Test.GlobalState (initialBftState)

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Chain (frequencySt, genNonNegative)
import Test.ArbitrarySt


tests :: TestTree
tests =
  testGroup "Combinators"
  [ testGroup "frequencySt"
    [ testProperty "correctly threads the state across" $ prop_frequencySt_state
    , testProperty "picks different frequencies" $ prop_frequencySt_choices
    ]
  ]


prop_frequencySt_state :: Property
prop_frequencySt_state = property $ do
    -- Creates a small frequency list, each of which increment the state by 1
    -- and yield the given frequency.
    n <- choose (1, 6)
    let xs = map (\freq -> (freq, modify (+1) >> return freq)) [0..n]
    -- Start from an initial non-negative state and assert that frequencySt
    -- /does/ modify the state.
    initialState <- genNonNegative
    finalState <- execStateT (frequencySt xs) initialState
    return $   counterexample "Final state /= initialState" (finalState =/= initialState)
          .&&. counterexample "State is threaded" (finalState === initialState + 1)


newtype Choice = Choice Int deriving (Show, Eq)

instance ArbitrarySt 'OuroborosBFT Choice where
    arbitrarySt = frequencySt [
        (1, return $ Choice 1)
      , (2, return $ Choice 2)
      , (3, return $ Choice 3)
      ]

prop_frequencySt_choices :: Property
prop_frequencySt_choices = 
  forAll (evalStateT arbitrarySt initialBftState) $ \(Choice i) ->
         classify (i == 1) "frequency == 1" True
    .&&. classify (i == 2) "frequency == 2" True
    .&&. classify (i == 3) "frequency == 3" True


