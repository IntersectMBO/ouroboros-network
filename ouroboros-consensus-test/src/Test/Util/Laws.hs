module Test.Util.Laws (
    -- * Properties for @'Semigroup'@ laws
    associativity
    -- * Properties for @'Monoid'@ laws
  , concatenation
  , leftIdentity
  , rightIdentity
    -- * Properties for @'Group'@ laws
  , leftInverse
  , rightInverse
  ) where

import           Data.Group

import           Test.QuickCheck (Property)
import qualified Test.QuickCheck as QC

{------------------------------------------------------------------------------
  Semigroup laws
------------------------------------------------------------------------------}

associativity ::
     ( Semigroup a
     , Show a, Eq a
     )
  => a -> a -> a -> Property
associativity x y z = x <> (y <> z) QC.=== (x <> y) <> z

{------------------------------------------------------------------------------
  Monoid laws
------------------------------------------------------------------------------}

rightIdentity ::
     ( Monoid a
     , Show a, Eq a
     )
  => a
  -> Property
rightIdentity x = x <> mempty QC.=== x

leftIdentity ::
     ( Monoid a
     , Show a, Eq a
     )
  => a
  -> Property
leftIdentity x = mempty <> x QC.=== x

concatenation ::
     ( Monoid a
     , Show a, Eq a
     )
  => [a]
  -> Property
concatenation xs = mconcat xs QC.=== foldr (<>) mempty xs

{------------------------------------------------------------------------------
  Group laws
------------------------------------------------------------------------------}

rightInverse ::
     ( Group a
     , Show a, Eq a
     )
  => a
  -> Property
rightInverse x = x <> invert x QC.=== mempty

leftInverse ::
     ( Group a
     , Show a, Eq a
     )
  => a
  -> Property
leftInverse x = invert x <> x QC.=== mempty
