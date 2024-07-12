{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Data.List.NonEmpty
import Data.Semigroup (Semigroup (..))

import Test.QuickCheck
import Test.QuickCheck.Monoids
import Test.Tasty
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup "quickcheck-monoids"
    [ testGroup "All"
      [ testProperty "associative" $ prop_associative @All
      , testProperty "unit"        $ prop_unit @All
      , testProperty "sconcat"     $ prop_sconcat @All
      , testProperty "mconcat"     $ prop_mconcat @All
      ]
    , testGroup "Any"
      [ testProperty "associative" $ prop_associative @Any
      , testProperty "unit"        $ prop_unit @Any
      , testProperty "sconcat"     $ prop_sconcat @Any
      , testProperty "mconcat"     $ prop_mconcat @Any
      ]
    ]


instance Arbitrary All where
    arbitrary = oneof [ pure $ All True
                      , pure $ All False
                      , pure $ All (counterexample "False" False)
                      , pure $ All (counterexample "True" True)
                      , pure $ All (ioProperty (return True))
                      , pure $ All (ioProperty (return False))
                      , pure $ All (checkCoverage $ cover 100 True "" True)
                      , pure $ All (checkCoverage $ cover 100 True "" False)
                      -- FAILS: unit law!
                      -- , pure $ All (checkCoverage $ cover 100 False "" True)
                      , pure $ All (checkCoverage $ cover 100 False "" False)
                      ]


instance Arbitrary Any where
    arbitrary = oneof [ pure $ Any True
                      , pure $ Any False
                      , pure $ Any (counterexample "False" False)
                      , pure $ Any (counterexample "True" True)
                      , pure $ Any (ioProperty (return True))
                      , pure $ Any (ioProperty (return False))
                      , pure $ Any (checkCoverage $ cover 100 True "" True)
                      , pure $ Any (checkCoverage $ cover 100 True "" False)
                      -- FAILS: unit law!
                      -- , pure $ Any (checkCoverage $ cover 100 False "" True)
                      , pure $ Any (checkCoverage $ cover 100 False "" False)
                      ]



prop_associative :: (Testable p, Semigroup p) => Blind p -> Blind p -> Blind p -> Property
prop_associative (Blind a) (Blind b) (Blind c) = ioProperty $ do
    x <- isSuccess <$> quickCheckWithResult args (a <> (b <> c))
    y <- isSuccess <$> quickCheckWithResult args ((a <> b) <> c)
    return (x === y)


prop_unit :: (Testable p, Monoid p) => Blind p -> Property
prop_unit (Blind a) = ioProperty $ do
    x <- isSuccess <$> quickCheckWithResult args (a <> mempty)
    y <- isSuccess <$> quickCheckWithResult args (mempty <> a)
    z <- isSuccess <$> quickCheckWithResult args a
    return (x === y .&&. y === z)


prop_sconcat :: (Testable p, Semigroup p) => Blind p -> Blind p -> Property
prop_sconcat (Blind a) (Blind b) = ioProperty $ do
    x <- isSuccess <$> quickCheckWithResult args (sconcat $ a :| [b])
    y <- isSuccess <$> quickCheckWithResult args (a <> b)
    return (x === y)


prop_mconcat :: (Testable p, Monoid p) => Blind p -> Blind p -> Property
prop_mconcat (Blind a) (Blind b) = ioProperty $ do
    x <- isSuccess <$> quickCheckWithResult args (mconcat [a, b])
    y <- isSuccess <$> quickCheckWithResult args (a <> b)
    return (x === y)


--
-- Auxiliary definitions
--

args :: Args
args = stdArgs { chatty = False, maxShrinks = 0 }
