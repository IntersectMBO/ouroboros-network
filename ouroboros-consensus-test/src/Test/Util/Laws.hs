{-# LANGUAGE ScopedTypeVariables #-}

module Test.Util.Laws (
    -- * Test tree makers
    testGroupLaws
  , testMonoidLaws
  , testSemigroupLaws
    -- * Properties for @'Semigroup'@ laws
  , associativity
    -- * Properties for @'Monoid'@ laws
  , concatenation
  , leftIdentity
  , rightIdentity
    -- * Properties for @'Group'@ laws
  , leftInverse
  , rightInverse
  ) where

import           Data.Group
import           Data.Proxy

import           Test.QuickCheck (Arbitrary, Property)
import qualified Test.QuickCheck as QC
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

{------------------------------------------------------------------------------
  Semigroup laws
------------------------------------------------------------------------------}

testSemigroupLaws ::
     ( Semigroup a, Arbitrary a
     , Eq a, Show a
     )
  => Proxy a
  -> TestTree
testSemigroupLaws p =
  testGroup "Semigroup laws" [
      testProperty "Associativity" $
        associativity p
    ]

associativity ::
     ( Semigroup a
     , Show a, Eq a
     )
  => Proxy a
  -> a
  -> a
  -> a
  -> Property
associativity _ x y z = x <> (y <> z) QC.=== (x <> y) <> z

{------------------------------------------------------------------------------
  Monoid laws
------------------------------------------------------------------------------}

testMonoidLaws ::
     ( Monoid a, Arbitrary a
     , Eq a, Show a
     )
  => Proxy a
  -> TestTree
testMonoidLaws p =
  testGroup "Monoid laws" [
      testProperty "Right identity" $
        rightIdentity p
    , testProperty "Left identity" $
        leftIdentity p
    , testProperty "Concatenation" $
        concatenation p
    ]

rightIdentity ::
     ( Monoid a
     , Show a, Eq a
     )
  => Proxy a
  -> a
  -> Property
rightIdentity _ x = x <> mempty QC.=== x

leftIdentity ::
     ( Monoid a
     , Show a, Eq a
     )
  => Proxy a
  -> a
  -> Property
leftIdentity _ x = mempty <> x QC.=== x

concatenation ::
     ( Monoid a
     , Show a, Eq a
     )
  => Proxy a
  -> [a]
  -> Property
concatenation _ xs = mconcat xs QC.=== foldr (<>) mempty xs

{------------------------------------------------------------------------------
  Group laws
------------------------------------------------------------------------------}

testGroupLaws ::
     ( Group a, Arbitrary a
     , Eq a, Show a
     )
  => Proxy a
  -> TestTree
testGroupLaws p =
  testGroup "Group laws" [
      testProperty "Right inverse" $
        rightInverse p
    , testProperty "Left inverse" $
        leftInverse p
    ]

rightInverse ::
     ( Group a
     , Show a, Eq a
     )
  => Proxy a
  -> a
  -> Property
rightInverse _ x = x <> invert x QC.=== mempty

leftInverse ::
     ( Group a
     , Show a, Eq a
     )
  => Proxy a
  -> a
  -> Property
leftInverse _ x = invert x <> x QC.=== mempty
