-- | QuickCheck utilities
module Test.Util.QuickCheck (
    -- * Comparison functions
    lt
  , ge
    -- * Improved variants
  , elements
  ) where

import           GHC.Stack (HasCallStack)

import           Test.QuickCheck hiding (elements)
import qualified Test.QuickCheck as QC

{-------------------------------------------------------------------------------
  Comparison functions
-------------------------------------------------------------------------------}

infix 4 `lt`
infix 4 `ge`

-- | Like '>=', but prints a counterexample when it fails.
ge :: (Ord a, Show a) => a -> a -> Property
x `ge` y = counterexample (show x ++ " < " ++ show y) $ x >= y

-- | Like '<', but prints a counterexample when it fails.
lt :: (Ord a, Show a) => a -> a -> Property
x `lt` y = counterexample (show x ++ " >= " ++ show y) $ x < y

{-------------------------------------------------------------------------------
  Improved variants
-------------------------------------------------------------------------------}

-- | Generates one of the given values. The input list must be non-empty.
--
-- NOTE unlike the standard @elements@, this variant has a 'HasCallStack'
-- constraint, which makes debugging the 'error' much easier.
elements :: HasCallStack => [a] -> Gen a
elements [] = error "Test.Util.QuickCheck.elements used with empty list"
elements xs = QC.elements xs
