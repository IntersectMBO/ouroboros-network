{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Data.Map.Diff.Strict (tests) where

import           Data.Group
import           Data.Proxy (Proxy (Proxy))

import           Data.Map.Diff.Strict (Diff (..), DiffHistory (..))
import qualified Data.Map.Diff.Strict as MapDiff

import           Test.Tasty (TestTree, testGroup)

import           Test.QuickCheck (Positive (..), Property, (===), (==>))
import           Test.Tasty.QuickCheck (Testable (property), testProperty)
import           Test.Util.Laws (testGroupLaws, testMonoidLaws,
                     testSemigroupLaws)
import           Test.Util.Orphans.DiffSeq.Arbitrary ()


-- | Testing @'Semigroup'@, @'Monoid'@ and @'Group'@ laws for diff datatypes.
tests :: TestTree
tests = testGroup "Strict" [
    testGroup "Diff Int Int" [
        testSemigroupLaws p1
      , testMonoidLaws p1
      , testGroupLaws p1
      ]
  , testGroup "DiffHistory Int" [
        testSemigroupLaws p2
      , testMonoidLaws p2
      , testGroupLaws p2
      , testProperty "Inverted prefix" $ prop_invertedPrefix @Int
      , testProperty "Inverted suffix" $ prop_invertedSuffix @Int
      , testProperty "Undo sum left"  $ prop_undoSumLeft @Int
      , testProperty "Undo sum right" $ prop_undoSumRight @Int
      ]
  ]
    where
      p1 = Proxy @(Diff Int Int)
      p2 = Proxy @(DiffHistory Int)

-- | Subtracting an inverted prefix @l@ of a diff history @h@ from the left end
-- of @h@ gives us exactly @h@ without the prefix @l@.
prop_invertedPrefix ::
     (Eq v, Show v)
  => DiffHistory v -> Positive Int -> Property
prop_invertedPrefix h (Positive n) = property $
    n <= MapDiff.length h ==> invert l <> h === r
  where
    (l, r) = MapDiff.splitAt n h

-- | Subtracting an inverted suffix @r@ of a diff history @h@ from the right end
-- of @h@ gives us exactly @h@ without the suffix @r@.
prop_invertedSuffix ::
     (Eq v, Show v)
  => DiffHistory v -> Positive Int -> Property
prop_invertedSuffix h (Positive n) = property $
    n <= MapDiff.length h ==> l === h ~~ r
  where
    (l, r) = MapDiff.splitAt n h

-- | Summing @h1@ and @h2@, and subtracting @h1@ from the left end of the result
-- should give us exactly @h2@.
prop_undoSumLeft ::
     (Eq v, Show v)
  => DiffHistory v -> DiffHistory v -> Property
prop_undoSumLeft h1 h2 = property $ invert h1 <> (h1 <> h2) === h2

-- | Summing @h1@ and @h2@, and subtracting @h2@ from the right end of the result
-- should give us exactly @h1@.
prop_undoSumRight ::
     (Eq v, Show v)
  => DiffHistory v -> DiffHistory v -> Property
prop_undoSumRight h1 h2 = property $ (h1 <> h2) <> invert h2 === h1
