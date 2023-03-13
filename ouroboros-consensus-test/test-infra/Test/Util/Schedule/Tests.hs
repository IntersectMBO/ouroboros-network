{-# LANGUAGE ScopedTypeVariables #-}

module Test.Util.Schedule.Tests (tests) where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.Schedule

tests :: TestTree
tests = testGroup "Test.Util.Schedule"
    [ testProperty "joinSchedule/genSchedule" $ prop_joinSchedule_genSchedule
    ]

prop_joinSchedule_genSchedule :: Property
prop_joinSchedule_genSchedule =
    forAll genElementsAndSpread $ \(as, spread) ->
      joinSchedule spread === as
  where
    genElementsAndSpread = do
      -- generate elements of some type with an Ord instance
      as :: [Int] <- arbitrary
      spread      <- genSchedule as
      return (as, spread)
