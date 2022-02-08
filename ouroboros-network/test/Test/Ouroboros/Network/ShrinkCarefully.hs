{- This module enables us to test that shrinking functions do not
   shrink a value to itself. To use, define a property for your type
   of interest like this:

   prop_shrinkCarefully_T :: ShrinkCarefully T -> Property
   prop_shrinkCarefully_T = prop_shrinkCarefully
-}

module Test.Ouroboros.Network.ShrinkCarefully where

import Data.List
import Text.Pretty.Simple
import Test.QuickCheck

newtype ShrinkCarefully a = ShrinkCarefully a
  deriving (Eq,Show)

instance (Eq a, Arbitrary a) => Arbitrary (ShrinkCarefully a) where
  arbitrary = ShrinkCarefully <$> arbitrary
  shrink (ShrinkCarefully a) = ShrinkCarefully <$> delete a (shrink a)

prop_shrinkCarefully :: (Arbitrary a, Eq a, Show a) => ShrinkCarefully a -> Property
prop_shrinkCarefully (ShrinkCarefully e) =
  whenFail (pPrint e) $
  counterexample (show $ map (==e) (shrink e)) $
  e `notElem` shrink e
