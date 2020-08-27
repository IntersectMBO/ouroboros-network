-- | QuickCheck utilities
module Test.Util.QuickCheck (
    -- * Comparison functions
    lt
  , le
  , gt
  , ge
  , expectRight
    -- * Improved variants
  , elements
  , (=:=)
    -- * Convenience
  , collects
  ) where

import           GHC.Stack (HasCallStack)

import           Ouroboros.Consensus.Util (repeatedly)
import           Ouroboros.Consensus.Util.Condense (Condense, condense)

import           Test.QuickCheck hiding (elements)
import qualified Test.QuickCheck as QC

{-------------------------------------------------------------------------------
  Comparison functions
-------------------------------------------------------------------------------}

infix 4 `lt`
infix 4 `le`
infix 4 `gt`
infix 4 `ge`

-- | Like '<', but prints a counterexample when it fails.
lt :: (Ord a, Show a) => a -> a -> Property
x `lt` y = counterexample (show x ++ " >= " ++ show y) $ x < y

-- | Like '<=', but prints a counterexample when it fails.
le :: (Ord a, Show a) => a -> a -> Property
x `le` y = counterexample (show x ++ " > " ++ show y) $ x <= y

-- | Like '>', but prints a counterexample when it fails.
gt :: (Ord a, Show a) => a -> a -> Property
x `gt` y = counterexample (show x ++ " <= " ++ show y) $ x > y

-- | Like '>=', but prints a counterexample when it fails.
ge :: (Ord a, Show a) => a -> a -> Property
x `ge` y = counterexample (show x ++ " < " ++ show y) $ x >= y

-- | Check that we have the expected 'Right' value
--
-- @expectRight b ab@ is roughly equivalent to @Right b === ab@, but avoids an
-- equality constraint on @a@.
expectRight :: (Show a, Show b, Eq b) => b -> Either a b -> Property
expectRight b (Right b') = b === b'
expectRight _ (Left a)   = counterexample ("Unexpected left " ++ show a) $
                             False

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

-- | Like '===', but uses 'Condense' instead of 'Show' when it fails.
infix 4 =:=
(=:=) :: (Eq a, Condense a) => a -> a -> Property
x =:= y =
    counterexample (condense x ++ interpret res ++ condense y) res
  where
    res = x == y
    interpret True  = " == "
    interpret False = " /= "

{-------------------------------------------------------------------------------
  Convenience
-------------------------------------------------------------------------------}

collects :: Show a => [a] -> Property -> Property
collects = repeatedly collect
