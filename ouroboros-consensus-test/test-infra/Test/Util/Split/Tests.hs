{-# LANGUAGE LambdaCase #-}
module Test.Util.Split.Tests (
    tests
  ) where

import           Data.Either (isLeft, isRight)
import           Data.Maybe (mapMaybe)
import           Data.Word (Word64)

import           Test.Util.Split

import           Test.Tasty
import           Test.Tasty.QuickCheck


{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Util.Split" $
    [ testProperty "prop_spanLeft"    prop_spanLeft
    , testProperty "prop_splitAtJust" prop_splitAtJust
    ]

prop_spanLeft :: [Either Int Char] -> Property
prop_spanLeft xs =
    rebuild .&&.
    count .&&.
    tickIsReversed
  where
    result@(pre, mbPost) = spanLeft id xs

    rebuild = counterexample "does not rebuild" $
              counterexample (show result) $
              actual === expected
      where
        expected = xs
        actual   = case mbPost of
          Nothing        -> map Left pre
          Just (b, post) -> map Left pre ++ Right b : post

    count = counterexample "wrong count" $
            counterexample (show result) $
            actual === expected
      where
        expected = length $ takeWhile isLeft xs
        actual   = length pre

    tickIsReversed = counterexample "is not reverse" $
                     reverse pre === fst (spanLeft' id xs)

prop_splitAtJust :: [Either Int Char] -> Word64 -> Property
prop_splitAtJust xs rawN =
    rebuild .&&.
    count
  where
    prj = prjRight
    lim = wlength $ mapMaybe prj xs
    n = if 0 == lim then rawN else rawN `mod` (2 * lim)

    result@(mbPre, post) = splitAtJust prj n xs

    rebuild = counterexample "does not rebuild" $
              counterexample (show result) $
              actual === expected
      where
        expected = xs
        actual   = case mbPre of
          Nothing       -> post
          Just (pre, b) -> pre ++ Right b : post

    count = counterexample "wrong count" $
            counterexample (show result) $
            actual === expected
      where
        expected = min n (wlength $ filter isRight xs)
        actual   = case mbPre of
          Nothing       -> 0
          Just (pre, _) -> succ (wlength $ filter isRight pre)

{-------------------------------------------------------------------------------
  Auxiliaries
-------------------------------------------------------------------------------}

wlength :: [a] -> Word64
wlength = fromIntegral . length

prjRight :: Either a b -> Maybe b
prjRight = \case
  Left{}  -> Nothing
  Right x -> Just x
