-- | Test trees and QuickCheck properties that test typeclass laws.
--
-- Laws included for typeclasses defining group-like and groupoid-like
-- structures.
module Data.Semigroupoid.Laws (
    -- * @Test.Tasty@ combinators
    testGroupWithProxy
    -- * Test tree makers
  , testGroupLaws
  , testGroupoidLaws
  , testMonoidLaws
  , testSemigroupLaws
  , testSemigroupoidLaws
    -- * Properties for @'Semigroup'@ laws
  , associativity
    -- * Properties for @'Monoid'@ laws
  , concatenation
  , concatenation'
  , leftIdentity
  , rightIdentity
    -- * Properties for @'Group'@ laws
  , leftInverse
  , rightInverse
    -- * Properties for @'Semigroupoid'@ laws
  , prop_associativity
    -- * Properties for @'Groupoid'@ laws
  , prop_identity
  , prop_inverse
  ) where

import           Data.Group
import           Data.Maybe (fromJust)
import           Data.Proxy
import           Data.Semigroupoid
import           Data.Typeable

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)



{------------------------------------------------------------------------------
  @Test.Tasty@ combinators
------------------------------------------------------------------------------}

testGroupWithProxy :: Typeable a => Proxy a -> [Proxy a -> TestTree] -> TestTree
testGroupWithProxy p tts = testGroup (show $ typeOf p) (fmap ($ p) tts)

{------------------------------------------------------------------------------
  @'Semigroup'@ laws
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
associativity _ x y z = x <> (y <> z) === (x <> y) <> z

{------------------------------------------------------------------------------
  @'Monoid'@ laws
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
    , testProperty "Concatenation'" $
        concatenation' p
    ]

rightIdentity ::
     ( Monoid a
     , Show a, Eq a
     )
  => Proxy a
  -> a
  -> Property
rightIdentity _ x = x <> mempty === x

leftIdentity ::
     ( Monoid a
     , Show a, Eq a
     )
  => Proxy a
  -> a
  -> Property
leftIdentity _ x = mempty <> x === x

concatenation ::
     ( Monoid a
     , Show a, Eq a
     )
  => Proxy a
  -> [a]
  -> Property
concatenation _ xs = mconcat xs === foldr (<>) mempty xs

concatenation' ::
     ( Monoid a
     , Show a, Eq a
     )
  => Proxy a
  -> [a]
  -> Property
concatenation' _ xs = foldr (<>) mempty xs === foldl (<>) mempty xs

{------------------------------------------------------------------------------
  @'Group'@ laws
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
rightInverse _ x = x <> invert x === mempty

leftInverse ::
     ( Group a
     , Show a, Eq a
     )
  => Proxy a
  -> a
  -> Property
leftInverse _ x = invert x <> x === mempty

{------------------------------------------------------------------------------
  @'Semigroupoid'@ laws
------------------------------------------------------------------------------}

testSemigroupoidLaws ::
     ( Semigroupoid a, Arbitrary a
     , Eq a, Show a
     )
  => Proxy a
  -> TestTree
testSemigroupoidLaws p =
  testGroup "Semigroupoid laws" [
      testProperty "Associativity 1" $ \x y z ->
        fst $ prop_associativity p x y z
    , testProperty "Associativity 2" $ \x y z ->
        snd $ prop_associativity p x y z
    ]

prop_associativity ::
     ( Semigroupoid a
     , Eq a, Show a
     )
  => Proxy a
  -> a
  -> a
  -> a
  -> (Property, Property)
prop_associativity _ x y z =
    ( withCounterexamples prop1
    , withCounterexamples prop2
    )
  where
    withCounterexamples =
        counterexample ("xy " <> show pxy)
      . counterexample ("yz " <> show pyz)
      . counterexample ("xyz1 " <> show pxyz1)
      . counterexample ("xyz2 " <> show pxyz2)

    pxy = x <>? y
    pyz = y <>? z

    pxyz1 = pxy `pappendM` Just z
    pxyz2 = Just x `pappendM` pyz

    prop1 = counterexample "assoc1" $
      case (pxy, pyz) of
        (Just _xy, Just _yz) ->
               (Nothing =/= pxyz1)
          .&&. (Nothing =/= pxyz2)
          .&&. (fromJust pxyz1 === fromJust pxyz2)
        (_      , _      ) -> label "trivial assoc1" ()

    prop2 = counterexample "assoc2" $
      case (pxyz1, pxyz2) of
        (Just xyz1, Just xyz2) ->
               (Nothing =/= pxy)
          .&&. (Nothing =/= pyz)
          .&&. (xyz1 === xyz2)
        (_        , _) -> label "trivial assoc2" ()

{------------------------------------------------------------------------------
  @'Groupoid'@ laws
------------------------------------------------------------------------------}

testGroupoidLaws ::
     ( Groupoid a, Arbitrary a
     , Eq a, Show a
     )
  => Proxy a
  -> TestTree
testGroupoidLaws p =
  testGroup "Groupoid laws" [
      testProperty "Identity" $
        prop_identity p
    , testProperty "Inverse" $
        prop_inverse p
    ]

prop_identity ::
     ( Groupoid a
     , Eq a, Show a
     )
  => Proxy a
  -> a
  -> a
  -> Property
prop_identity _ x y = case x <>? y of
  Nothing -> label "trivial identity" ()
  Just xy ->
         counterexample "right identity" (xy <>? pinv y === Just x)
    .&&. counterexample "left identity" (pinv x <>? xy === Just y)

prop_inverse ::
     ( Groupoid a
     , Eq a, Show a
     )
  => Proxy a
  -> a
  -> Property
prop_inverse _ x = prop1 .&&. prop2
  where
    prop1 = counterexample "left inverse" $
      Nothing =/= pinv x <>? x

    prop2 = counterexample "right inverse" $
      Nothing =/= x <>? pinv x
