{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Util.Split (
    spanLeft,
    spanLeft',
    splitAtJust,
    -- * Testing
    prop_spanLeft,
    prop_splitAtJust,
    tests,
  ) where

import           Data.Bifunctor (first)
import           Data.Either (isLeft, isRight)
import           Data.Maybe (mapMaybe)
import           Data.Word (Word64)
import           GHC.Stack (HasCallStack)

import           Test.Tasty
import           Test.Tasty.QuickCheck

{-------------------------------------------------------------------------------
  spanLeft
-------------------------------------------------------------------------------}

-- | The returned @b@ is the first in the list.
--
-- INVARIANT The output data is a segmentation of the given list.
spanLeft
  :: forall x a b.
     HasCallStack
  => (x -> Either a b) -> [x] -> ([a], Maybe (b, [x]))
spanLeft prj xs = (reverse acc, mbBxs)
  where
    (acc, mbBxs) = spanLeft' prj xs

-- | As 'spanLeft', but the @[a]@ is reversed.
spanLeft'
  :: forall x a b.
     HasCallStack
  => (x -> Either a b) -> [x] -> ([a], Maybe (b, [x]))
spanLeft' prj = go []
  where
    go acc = \case
      []     -> (acc, Nothing)
      x : xs -> case prj x of
        Left a  -> go (a : acc) xs
        Right b -> (acc, Just (b, xs))

{-------------------------------------------------------------------------------
  splitAtJust
-------------------------------------------------------------------------------}

-- | INVARIANT: The second is a function of the first.
data Prj a b = Prj !a !b

-- | The returned @b@ is either the @n@th @b@ or else the last in the given
-- list.
--
-- INVARIANT The output data is a segmentation of the given list.
splitAtJust
  :: forall x b.
     HasCallStack
  => (x -> Maybe b) -> Word64 -> [x] -> (Maybe ([x], b), [x])
splitAtJust prj = \n xs ->
  if 0 == n then (Nothing, xs)
  else case peel xs of
    (pre, Just (xb, xs')) -> Just `first` go pre xb (n - 1) xs'
    (_, Nothing)          -> (Nothing, xs)
  where
    peel :: [x] -> ([x], Maybe (Prj x b, [x]))
    peel = spanLeft' prj'
      where
        prj' x = case prj x of
          Nothing -> Left x
          Just b  -> Right (Prj x b)

    go pre (Prj x b) n xs
      | 0 == n    = ((reverse pre, b), xs)
      | otherwise = case peel xs of
      (pre', Nothing       ) -> ((reverse pre, b), reverse pre')
      (pre', Just (xb, xs')) -> go (pre' ++ x : pre) xb (n - 1) xs'

{-------------------------------------------------------------------------------
  Auxiliaries
-------------------------------------------------------------------------------}

wlength :: [a] -> Word64
wlength = fromIntegral . length

prjRight :: Either a b -> Maybe b
prjRight = \case
  Left{}  -> Nothing
  Right x -> Just x

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
